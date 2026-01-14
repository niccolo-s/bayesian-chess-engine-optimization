# ==============================================================================
# Task 7: Automated Parameter Tuning (2-Dimensional)
# ==============================================================================

library(Rschach)
library(dplyr)
library(cmdstanr)
library(DiceKriging)
library(tidyverse)
library(metR)
library(ggplot2)


setwd("C:/Users/faust/Desktop/Bayesian Statistics/chess engine project")  #change your working directory when you run 

book_file <- "8moves_v3.epd"
book_fen <- read.csv(book_file, header = FALSE)[[1]]

#### 1. Setting initial values for RFP_intercept and RFP_slope ####

# List of initial candidates
# A , B , C , D 
RFP_int <- numeric(4)
RFP_slo <- numeric(4)
i <- 1

set.seed(123)
while(i <= 4) {
  x1 <- runif(1, -100, 300)
  x2 <- runif(1, 0, 500)
  if(x1 + x2 > 0) {
    RFP_int[i] <- x1
    RFP_slo[i] <- x2
    i <- i+1
  }
}

cbind(RFP_int, RFP_slo)
candidates <- list(
  list(name="A", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0, RFP_intercept=RFP_int[1], RFP_slope=RFP_slo[1])),
  list(name="B", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0, RFP_intercept=RFP_int[2], RFP_slope=RFP_slo[2])),
  list(name="C", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0, RFP_intercept=RFP_int[3], RFP_slope=RFP_slo[3])),
  list(name="D", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0, RFP_intercept=RFP_int[4], RFP_slope=RFP_slo[4]))
)


#### 2. Start initial mini-tournament ####
df <- data.frame(white=character(),
                 black=character(),
                 result=character())

for(i in 1:(length(candidates)-1)){
  for(j in (i+1):length(candidates)){
    
    cand_i <- candidates[[i]]
    cand_j <- candidates[[j]]
    e.cand_i <- Engine(name=cand_i$name, params=cand_i$params)
    e.cand_j <- Engine(name=cand_j$name, params=cand_j$params)
    
    cat(paste0("\n", paste(rep("-", 60), collapse=""), "\n"))
    cat(sprintf("MATCH: %s vs %s \n", cand_i$name, cand_j$name))
    
    n_games=20
    set.seed(123)
    book_samples <- sample(book_fen, size=n_games, replace=F)
    
    for(k in 1:n_games){
      if(k %% 2 == 1){
        white_engine <- e.cand_i
        black_engine <- e.cand_j
      } 
      else{
        white_engine <- e.cand_j
        black_engine <- e.cand_i
      }
      
      current_book_pos <- book_samples[k]
      res <- play.tournament(white_engine, black_engine, 
                             nr_rounds=1L, book=current_book_pos)  # time settings default
      
      for(r in res){
        df <- rbind(df, data.frame(
          white  = r$White,
          black  = r$Black,
          result = r$Result))
      }
    }
  }
}

df

#### 3. Data pre-processing and fitting the model ####
unique_engines <- c("A", "B", "C", "D")
df$outcome <- sapply(df$result, function(x){
  if (x == "1-0") return(3)       # White win
  if (x == "0-1") return(1)       # Black win
  if (x == "1/2-1/2") return(2)   # Draw
})
df$white_id <- match(df$white, unique_engines)
df$black_id <- match(df$black, unique_engines)

df

stan_data <- list(
  N = nrow(df),
  K = length(unique_engines),
  white_id = df$white_id,
  black_id = df$black_id,
  outcome = df$outcome
)

# Build model
mod <- cmdstan_model("elo_model_v1.stan")

# Run MCMC sampling
fit <- mod$sample(
  data = stan_data,
  iter_sampling = 2000,
  iter_warmup = 2000,
  chains = 4,
  parallel_chains = 4,
  seed = 123
)

post_sum <- fit$summary(variables = "rating")

df_results <- data.frame(
  engine = unique_engines,
  mean_rating = post_sum$mean, 
  sd_rating = post_sum$sd)

df_results$RFP_intercept <- sapply(df_results$engine, function(x){
  cand <- candidates[[which(sapply(candidates, function(c) c$name == x))]]
  cand$params$RFP_intercept
})

df_results$RFP_slope <- sapply(df_results$engine, function(x){
  cand <- candidates[[which(sapply(candidates, function(c) c$name == x))]]
  cand$params$RFP_slope
})

df_results
x <- as.matrix(df_results[, -(1:3)])
y <- df_results$mean_rating


#### 4. Gaussian process and Bayesian optimization ####
gp_model <- km(formula=~1, design=x, response=y, 
               covtype="gauss", noise.var=(df_results$sd/2)^2)

x1_grid <- seq(-100, 300, length.out = 200)
x2_grid <- seq(0, 500, length.out = 200)
grid <- expand.grid(RFP_intercept = x1_grid, RFP_slope = x2_grid)
grid <- grid[grid$RFP_intercept + grid$RFP_slope > 0, ]

pred <- predict(gp_model, newdata = grid, type="UK")

# Probability of Improvement and Expected Improvement 
z  <- (pred$mean - max(pred$mean)) / pred$sd

PI <- pnorm(z)
EI <- (pred$mean - max(pred$mean)) * pnorm(z) + pred$sd * dnorm(z)

grid[which.max(EI), ]   
grid[which.max(PI), ] 
# they are different (PI more exploitation, while EI balance between exploration and exploitation)

best <- grid[which.max(EI), ]
cat(sprintf("According to EI\nBest RFP_intercept: %f\nBest RFP_slope: %.f\n", 
            best$RFP_intercept, best$RFP_slope))

# Expected Improvement Heatmap and GP mean contours 
df_plot <- data.frame(
  RFP_intercept = grid$RFP_intercept,
  RFP_slope = grid$RFP_slope,
  mean = pred$mean,
  sd = pred$sd,
  EI = EI
)

df_obs <- data.frame(
  RFP_intercept = x[, 1],
  RFP_slope = x[, 2],
  y = y
)

ggplot(df_plot, aes(RFP_intercept, RFP_slope)) +
  geom_tile(aes(fill = EI)) +
  geom_contour(aes(z = mean), color = "white", bins = 10) +
  geom_point(
    data = df_obs,
    aes(RFP_intercept, RFP_slope),
    color = "orange3", size = 2) +
  geom_point(
    x = best$RFP_intercept,
    y = best$RFP_slope,
    color = "red", size = 3) +
  scale_fill_viridis_c() +
  labs(
   fill = "Expected Improvement") +
  geom_text_contour(
    aes(z = mean),
    color = "white",
    size = 3) +
  theme_minimal()

# According to EI, RFP_intercept = 0.5025126 and RFP_slope = 0 are chosen, 
# add that point in the data set, update the Gaussian process and repeat as a loop.


#### 5. Step 2 ####
df_2 <- data.frame(white=character(),
                 black=character(),
                 result=character())

candidate_2 <- list(name="E", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0,
                                             RFP_intercept=best$RFP_intercept,
                                             RFP_slope=best$RFP_slope))

# Simulate only the games against the new one
for(i in 1:(length(candidates))){
  e.cand_i <- Engine(name=candidates[[i]]$name, params=candidates[[i]]$params)
  new.cand <- Engine(name=candidate_2$name, params=candidate_2$params)
  
  
  cat(paste0("\n", paste(rep("-", 60), collapse=""), "\n"))
  cat(sprintf("MATCH: %s vs %s \n", candidates[[i]]$name, candidate_2$name))
  
  n_games=20
  set.seed(123)
  book_samples <- sample(book_fen, size=n_games, replace=F)
  
  for(k in 1:n_games){
    if(k %% 2 == 1){
      white_engine <- e.cand_i
      black_engine <- new.cand
    } 
    else{
      white_engine <- new.cand
      black_engine <- e.cand_i
    }
    
    current_book_pos <- book_samples[k]
    res <- play.tournament(white_engine, black_engine, 
                           nr_rounds=1L, book=current_book_pos)  # time settings default
    
    for(r in res){
      df_2 <- rbind(df_2, data.frame(
        white  = r$White,
        black  = r$Black,
        result = r$Result))
    }
  }
}

df_2$outcome <- sapply(df_2$result, function(x){
  if (x == "1-0") return(3)       # White win
  if (x == "0-1") return(1)       # Black win
  if (x == "1/2-1/2") return(2)   # Draw
})
unique_engines_2 <- c("A", "B", "C", "D", "E")
df_2$white_id <- match(df_2$white, unique_engines_2)
df_2$black_id <- match(df_2$black, unique_engines_2)

df_2

candidates <- append(candidates, list(candidate_2))  ##  Run it just once!
candidates  # updated 

stan_data <- list(
  N = nrow(df_2),
  K = length(unique_engines_2),
  white_id = df_2$white_id,
  black_id = df_2$black_id,
  outcome = df_2$outcome
)

fit <- mod$sample(
  data = stan_data,
  iter_sampling = 2000,
  iter_warmup = 2000,
  chains = 4,
  parallel_chains = 4,
  seed = 123
)

post_sum <- fit$summary(variables = "rating")

df_2_results <- data.frame(engine = unique_engines_2[5],
                           mean_rating = post_sum$mean[5], 
                           sd_rating = post_sum$sd[5])
df_2_results$RFP_intercept <- best$RFP_intercept
df_2_results$RFP_slope <- best$RFP_slope

df_2_results <- rbind(df_results, df_2_results) ##  Run it just once!

x <- as.matrix(df_2_results[, -(1:3)])
y <- df_2_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model <- km(formula=~1, design=x, response=y, 
               covtype="gauss", noise.var=(df_2_results$sd/2)^2)
pred <- predict(gp_model, newdata = grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z  <- (pred$mean - max(pred$mean)) / pred$sd

PI <- pnorm(z)
EI <- (pred$mean - max(pred$mean)) * pnorm(z) + pred$sd * dnorm(z)

grid[which.max(EI), ]   
grid[which.max(PI), ] 
# they are pretty similar this time 

best <- grid[which.max(EI), ]
cat(sprintf("According to EI\nBest RFP_intercept: %.f\nBest RFP_slope: %.f\n", 
            best$RFP_intercept, best$RFP_slope))

# New Expected Improvement Heatmap 2D
df_2_plot <- data.frame(
  RFP_intercept = grid$RFP_intercept,
  RFP_slope = grid$RFP_slope,
  mean = pred$mean,
  sd = pred$sd,
  EI = EI
)

df_obs <- data.frame(
  RFP_intercept = x[, 1],
  RFP_slope = x[, 2],
  y = y
)

ggplot(df_2_plot, aes(RFP_intercept, RFP_slope)) +
  geom_tile(aes(fill = EI)) +
  geom_contour(aes(z = mean), color = "white", bins = 10) +
  geom_point(
    data = df_obs,
    aes(RFP_intercept, RFP_slope),
    color = "orange3", size = 2) +
  geom_point(
    x = best$RFP_intercept,
    y = best$RFP_slope,
    color = "red", size = 3) +
  scale_fill_viridis_c() +
  labs(
    fill = "Expected Improvement") +
  geom_text_contour(
    aes(z = mean),
    color = "white",
    size = 3) +
  theme_minimal()

# According to EI, RFP_intercept = 300 and RFP_slope = 0 are chosen


#### 6. Step 3 ####
df_3 <- data.frame(white=character(),
                   black=character(),
                   result=character())

candidate_3 <- list(name="F", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0,
                                          RFP_intercept=best$RFP_intercept,
                                          RFP_slope=best$RFP_slope))

# Simulate only the games against the new one
for(i in 1:(length(candidates))){
  e.cand_i <- Engine(name=candidates[[i]]$name, params=candidates[[i]]$params)
  new.cand <- Engine(name=candidate_3$name, params=candidate_3$params)
  
  
  cat(paste0("\n", paste(rep("-", 60), collapse=""), "\n"))
  cat(sprintf("MATCH: %s vs %s \n", candidates[[i]]$name, candidate_3$name))
  
  n_games=20
  set.seed(123)
  book_samples <- sample(book_fen, size=n_games, replace=F)
  
  for(k in 1:n_games){
    if(k %% 2 == 1){
      white_engine <- e.cand_i
      black_engine <- new.cand
    } 
    else{
      white_engine <- new.cand
      black_engine <- e.cand_i
    }
    
    current_book_pos <- book_samples[k]
    res <- play.tournament(white_engine, black_engine, 
                           nr_rounds=1L, book=current_book_pos)  # time settings default
    
    for(r in res){
      df_3 <- rbind(df_3, data.frame(
        white  = r$White,
        black  = r$Black,
        result = r$Result))
    }
  }
}

df_3$outcome <- sapply(df_3$result, function(x){
  if (x == "1-0") return(3)       # White win
  if (x == "0-1") return(1)       # Black win
  if (x == "1/2-1/2") return(2)   # Draw
})
unique_engines_3 <- c("A", "B", "C", "D", "E", "F")
df_3$white_id <- match(df_3$white, unique_engines_3)
df_3$black_id <- match(df_3$black, unique_engines_3)

df_3

candidates <- append(candidates, list(candidate_3))  ##  Run it just once!
candidates  # updated 

stan_data <- list(
  N = nrow(df_3),
  K = length(unique_engines_3),
  white_id = df_3$white_id,
  black_id = df_3$black_id,
  outcome = df_3$outcome
)

fit <- mod$sample(
  data = stan_data,
  iter_sampling = 2000,
  iter_warmup = 2000,
  chains = 4,
  parallel_chains = 4,
  seed = 123
)

post_sum <- fit$summary(variables = "rating")

df_3_results <- data.frame(engine = unique_engines_3[6],
                           mean_rating = post_sum$mean[6], 
                           sd_rating = post_sum$sd[6])
df_3_results$RFP_intercept <- best$RFP_intercept
df_3_results$RFP_slope <- best$RFP_slope

df_3_results <- rbind(df_2_results, df_3_results) ##  Run it just once!

x <- as.matrix(df_3_results[, -(1:3)])
y <- df_3_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model <- km(formula=~1, design=x, response=y, 
               covtype="gauss", noise.var=(df_3_results$sd/2)^2)
pred <- predict(gp_model, newdata = grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z  <- (pred$mean - max(pred$mean)) / pred$sd

PI <- pnorm(z)
EI <- (pred$mean - max(pred$mean)) * pnorm(z) + pred$sd * dnorm(z)

grid[which.max(EI), ]   
grid[which.max(PI), ] 
# PI states as maximum what is just found , while EI is still looking for it

best <- grid[which.max(EI), ]
cat(sprintf("According to EI\nBest RFP_intercept: %.f\nBest RFP_slope: %.f\n", 
            best$RFP_intercept, best$RFP_slope))

# New Expected Improvement Heatmap 2D
df_3_plot <- data.frame(
  RFP_intercept = grid$RFP_intercept,
  RFP_slope = grid$RFP_slope,
  mean = pred$mean,
  sd = pred$sd,
  EI = EI
)

df_obs <- data.frame(
  RFP_intercept = x[, 1],
  RFP_slope = x[, 2],
  y = y
)

ggplot(df_3_plot, aes(RFP_intercept, RFP_slope)) +
  geom_tile(aes(fill = EI)) +
  geom_contour(aes(z = mean), color = "white", bins = 10) +
  geom_point(
    data = df_obs,
    aes(RFP_intercept, RFP_slope),
    color = "orange3", size = 2) +
  geom_point(
    x = best$RFP_intercept,
    y = best$RFP_slope,
    color = "red", size = 3) +
  scale_fill_viridis_c() +
  labs(
    fill = "Expected Improvement") +
  geom_text_contour(
    aes(z = mean),
    color = "white",
    size = 3) +
  theme_minimal()

# According to EI, RFP_intercept = 300 and RFP_slope = 500 are chosen


#### 7. Step 4 ####
df_4 <- data.frame(white=character(),
                   black=character(),
                   result=character())

candidate_4 <- list(name="G", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0,
                                          RFP_intercept=best$RFP_intercept,
                                          RFP_slope=best$RFP_slope))

# Simulate only the games against the new one
for(i in 1:(length(candidates))){
  e.cand_i <- Engine(name=candidates[[i]]$name, params=candidates[[i]]$params)
  new.cand <- Engine(name=candidate_4$name, params=candidate_4$params)
  
  
  cat(paste0("\n", paste(rep("-", 60), collapse=""), "\n"))
  cat(sprintf("MATCH: %s vs %s \n", candidates[[i]]$name, candidate_4$name))
  
  n_games=20
  set.seed(123)
  book_samples <- sample(book_fen, size=n_games, replace=F)
  
  for(k in 1:n_games){
    if(k %% 2 == 1){
      white_engine <- e.cand_i
      black_engine <- new.cand
    } 
    else{
      white_engine <- new.cand
      black_engine <- e.cand_i
    }
    
    current_book_pos <- book_samples[k]
    res <- play.tournament(white_engine, black_engine, 
                           nr_rounds=1L, book=current_book_pos)  # time settings default
    
    for(r in res){
      df_4 <- rbind(df_4, data.frame(
        white  = r$White,
        black  = r$Black,
        result = r$Result))
    }
  }
}

df_4$outcome <- sapply(df_4$result, function(x){
  if (x == "1-0") return(3)       # White win
  if (x == "0-1") return(1)       # Black win
  if (x == "1/2-1/2") return(2)   # Draw
})
unique_engines_4 <- c("A", "B", "C", "D", "E", "F", "G")
df_4$white_id <- match(df_4$white, unique_engines_4)
df_4$black_id <- match(df_4$black, unique_engines_4)

df_4

candidates <- append(candidates, list(candidate_4))  ##  Run it just once!
candidates  # updated 

stan_data <- list(
  N = nrow(df_4),
  K = length(unique_engines_4),
  white_id = df_4$white_id,
  black_id = df_4$black_id,
  outcome = df_4$outcome
)

fit <- mod$sample(
  data = stan_data,
  iter_sampling = 2000,
  iter_warmup = 2000,
  chains = 4,
  parallel_chains = 4,
  seed = 123
)

post_sum <- fit$summary(variables = "rating")

df_4_results <- data.frame(engine = unique_engines_4[7],
                           mean_rating = post_sum$mean[7], 
                           sd_rating = post_sum$sd[7])
df_4_results$RFP_intercept <- best$RFP_intercept
df_4_results$RFP_slope <- best$RFP_slope

df_4_results <- rbind(df_3_results, df_4_results) ##  Run it just once!

x <- as.matrix(df_4_results[, -(1:3)])
y <- df_4_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model <- km(formula=~1, design=x, response=y, 
               covtype="gauss", noise.var=(df_4_results$sd/2)^2)
pred <- predict(gp_model, newdata = grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z  <- (pred$mean - max(pred$mean)) / pred$sd

PI <- pnorm(z)
EI <- (pred$mean - max(pred$mean)) * pnorm(z) + pred$sd * dnorm(z)

grid[which.max(EI), ]   
grid[which.max(PI), ] 
# Both methods coincide and state as maximum what is just found 

best <- grid[which.max(EI), ]
cat(sprintf("According to EI\nBest RFP_intercept: %.f\nBest RFP_slope: %.f\n", 
            best$RFP_intercept, best$RFP_slope))

# New Expected Improvement Heatmap 2D
df_4_plot <- data.frame(
  RFP_intercept = grid$RFP_intercept,
  RFP_slope = grid$RFP_slope,
  mean = pred$mean,
  sd = pred$sd,
  EI = EI
)

df_obs <- data.frame(
  RFP_intercept = x[, 1],
  RFP_slope = x[, 2],
  y = y
)

ggplot(df_4_plot, aes(RFP_intercept, RFP_slope)) +
  geom_tile(aes(fill = EI)) +
  geom_contour(aes(z = mean), color = "white", bins = 10) +
  geom_point(
    data = df_obs,
    aes(RFP_intercept, RFP_slope),
    color = "orange3", size = 2) +
  geom_point(
    x = best$RFP_intercept,
    y = best$RFP_slope,
    color = "red", size = 3) +
  scale_fill_viridis_c() +
  labs(
    fill = "Expected Improvement") +
  geom_text_contour(
    aes(z = mean),
    color = "white",
    size = 3) +
  theme_minimal()

# According to EI, RFP_intercept = 300 and RFP_slope = 0 are chosen. 

save.image(file="environment_task7.RData")