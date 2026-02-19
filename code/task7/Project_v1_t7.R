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
  list(name="A", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.3015075, RFP_intercept=RFP_int[1], RFP_slope=RFP_slo[1])),
  list(name="B", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.3015075, RFP_intercept=RFP_int[2], RFP_slope=RFP_slo[2])),
  list(name="C", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.3015075, RFP_intercept=RFP_int[3], RFP_slope=RFP_slo[3])),
  list(name="D", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.3015075, RFP_intercept=RFP_int[4], RFP_slope=RFP_slo[4]))
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
# they are quite similar 

best <- grid[which.max(EI), ]
cat(sprintf("According to EI\nBest RFP_intercept: %f\nBest RFP_slope: %f\n", 
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

# According to EI, RFP_intercept = 300 and RFP_slope = 0 are chosen, 
# add that point in the data set, update the Gaussian process and repeat as a loop.


#### 5. Step 2 ####
df_2 <- data.frame(white=character(),
                 black=character(),
                 result=character())

candidate_2 <- list(name="E", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.3015075,
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

x_2 <- as.matrix(df_2_results[, -(1:3)])
y_2 <- df_2_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model_2 <- km(formula=~1, design=x_2, response=y_2, 
               covtype="gauss", noise.var=(df_2_results$sd/2)^2)
pred_2 <- predict(gp_model_2, newdata = grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z_2  <- (pred_2$mean - max(pred_2$mean)) / pred_2$sd

PI_2 <- pnorm(z_2)
EI_2 <- (pred_2$mean - max(pred_2$mean)) * pnorm(z_2) + pred_2$sd * dnorm(z_2)

grid[which.max(EI_2), ]   
grid[which.max(PI_2), ] 
# a lot of distance for one paramter 

best_2 <- grid[which.max(EI_2), ]
cat(sprintf("According to EI\nBest RFP_intercept: %f\nBest RFP_slope: %f\n", 
            best_2$RFP_intercept, best_2$RFP_slope))

# New Expected Improvement Heatmap 2D
df_2_plot <- data.frame(
  RFP_intercept = grid$RFP_intercept,
  RFP_slope = grid$RFP_slope,
  mean = pred_2$mean,
  sd = pred_2$sd,
  EI = EI_2
)

df_2_obs <- data.frame(
  RFP_intercept = x_2[, 1],
  RFP_slope = x_2[, 2],
  y = y_2
)

ggplot(df_2_plot, aes(RFP_intercept, RFP_slope)) +
  geom_tile(aes(fill = EI)) +
  geom_contour(aes(z = mean), color = "white", bins = 10) +
  geom_point(
    data = df_2_obs,
    aes(RFP_intercept, RFP_slope),
    color = "orange3", size = 2) +
  geom_point(
    x = best_2$RFP_intercept,
    y = best_2$RFP_slope,
    color = "red", size = 3) +
  scale_fill_viridis_c() +
  labs(
    fill = "Expected Improvement") +
  geom_text_contour(
    aes(z = mean),
    color = "white",
    size = 3) +
  theme_minimal()

# According to EI, RFP_intercept = 0.502513 and RFP_slope = 0 are chosen


#### 6. Step 3 ####
df_3 <- data.frame(white=character(),
                   black=character(),
                   result=character())

candidate_3 <- list(name="F", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.3015075,
                                          RFP_intercept=best_2$RFP_intercept,
                                          RFP_slope=best_2$RFP_slope))

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
df_3_results$RFP_intercept <- best_2$RFP_intercept
df_3_results$RFP_slope <- best_2$RFP_slope

df_3_results <- rbind(df_2_results, df_3_results) ##  Run it just once!

x_3 <- as.matrix(df_3_results[, -(1:3)])
y_3 <- df_3_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model_3 <- km(formula=~1, design=x_3, response=y_3, 
               covtype="gauss", noise.var=(df_3_results$sd/2)^2)
pred_3 <- predict(gp_model_3, newdata = grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z_3  <- (pred_3$mean - max(pred_3$mean)) / pred_3$sd

PI_3 <- pnorm(z_3)
EI_3 <- (pred_3$mean - max(pred_3$mean)) * pnorm(z_3) + pred_3$sd * dnorm(z_3)

grid[which.max(EI_3), ]   
grid[which.max(PI_3), ] 
# different for one paramter 

best_3 <- grid[which.max(EI_3), ]
cat(sprintf("According to EI\nBest RFP_intercept: %.f\nBest RFP_slope: %f\n", 
            best_3$RFP_intercept, best_3$RFP_slope))

# New Expected Improvement Heatmap 2D
df_3_plot <- data.frame(
  RFP_intercept = grid$RFP_intercept,
  RFP_slope = grid$RFP_slope,
  mean = pred_3$mean,
  sd = pred_3$sd,
  EI = EI_3
)

df_3_obs <- data.frame(
  RFP_intercept = x_3[, 1],
  RFP_slope = x_3[, 2],
  y = y_3
)

ggplot(df_3_plot, aes(RFP_intercept, RFP_slope)) +
  geom_tile(aes(fill = EI)) +
  geom_contour(aes(z = mean), color = "white", bins = 10) +
  geom_point(
    data = df_3_obs,
    aes(RFP_intercept, RFP_slope),
    color = "orange3", size = 2) +
  geom_point(
    x = best_3$RFP_intercept,
    y = best_3$RFP_slope,
    color = "red", size = 3) +
  scale_fill_viridis_c() +
  labs(
    fill = "Expected Improvement") +
  geom_text_contour(
    aes(z = mean),
    color = "white",
    size = 3) +
  theme_minimal()

# According to EI, RFP_intercept = 300 and RFP_slope = 145.7286 are chosen


#### 7. Step 4 ####
df_4 <- data.frame(white=character(),
                   black=character(),
                   result=character())

candidate_4 <- list(name="G", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.3015075,
                                          RFP_intercept=best_3$RFP_intercept,
                                          RFP_slope=best_3$RFP_slope))

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
df_4_results$RFP_intercept <- best_3$RFP_intercept
df_4_results$RFP_slope <- best_3$RFP_slope

df_4_results <- rbind(df_3_results, df_4_results) ##  Run it just once!

x_4 <- as.matrix(df_4_results[, -(1:3)])
y_4 <- df_4_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model_4 <- km(formula=~1, design=x_4, response=y_4, 
               covtype="gauss", noise.var=(df_4_results$sd/2)^2)
pred_4 <- predict(gp_model_4, newdata = grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z_4  <- (pred_4$mean - max(pred_4$mean)) / pred_4$sd

PI_4 <- pnorm(z_4)
EI_4 <- (pred_4$mean - max(pred_4$mean)) * pnorm(z_4) + pred_4$sd * dnorm(z_4)

grid[which.max(EI_4), ]   
grid[which.max(PI_4), ] 
# they are very different this time (and EI more exploitaion)

best_4 <- grid[which.max(EI_4), ]
cat(sprintf("According to EI\nBest RFP_intercept: %.2f\nBest RFP_slope: %.f\n", 
            best_4$RFP_intercept, best_4$RFP_slope))

# New Expected Improvement Heatmap 2D
df_4_plot <- data.frame(
  RFP_intercept = grid$RFP_intercept,
  RFP_slope = grid$RFP_slope,
  mean = pred_4$mean,
  sd = pred_4$sd,
  EI = EI_4
)

df_4_obs <- data.frame(
  RFP_intercept = x_4[, 1],
  RFP_slope = x_4[, 2],
  y = y_4
)

ggplot(df_4_plot, aes(RFP_intercept, RFP_slope)) +
  geom_tile(aes(fill = EI)) +
  geom_contour(aes(z = mean), color = "white", bins = 10) +
  geom_point(
    data = df_4_obs,
    aes(RFP_intercept, RFP_slope),
    color = "orange3", size = 2) +
  geom_point(
    x = best_4$RFP_intercept,
    y = best_4$RFP_slope,
    color = "red", size = 3) +
  scale_fill_viridis_c() +
  labs(
    fill = "Expected Improvement") +
  geom_text_contour(
    aes(z = mean),
    color = "white",
    size = 3) +
  theme_minimal()

# According to EI, RFP_intercept = -100 and RFP_slope = 500 are chosen. 

#### 8. Step 5 ####
df_5 <- data.frame(white=character(),
                   black=character(),
                   result=character())

candidate_5 <- list(name="H", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.3015075,
                                          RFP_intercept=best_4$RFP_intercept,
                                          RFP_slope=best_4$RFP_slope))

# Simulate only the games against the new one
for(i in 1:(length(candidates))){
  e.cand_i <- Engine(name=candidates[[i]]$name, params=candidates[[i]]$params)
  new.cand <- Engine(name=candidate_5$name, params=candidate_5$params)
  
  
  cat(paste0("\n", paste(rep("-", 60), collapse=""), "\n"))
  cat(sprintf("MATCH: %s vs %s \n", candidates[[i]]$name, candidate_5$name))
  
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
      df_5 <- rbind(df_5, data.frame(
        white  = r$White,
        black  = r$Black,
        result = r$Result))
    }
  }
}

df_5$outcome <- sapply(df_5$result, function(x){
  if (x == "1-0") return(3)       # White win
  if (x == "0-1") return(1)       # Black win
  if (x == "1/2-1/2") return(2)   # Draw
})
unique_engines_5 <- c("A", "B", "C", "D", "E", "F", "G", "H")
df_5$white_id <- match(df_5$white, unique_engines_5)
df_5$black_id <- match(df_5$black, unique_engines_5)

df_5

candidates <- append(candidates, list(candidate_5))  ##  Run it just once!
candidates  # updated 

stan_data <- list(
  N = nrow(df_5),
  K = length(unique_engines_5),
  white_id = df_5$white_id,
  black_id = df_5$black_id,
  outcome = df_5$outcome
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

df_5_results <- data.frame(engine = unique_engines_5[8],
                           mean_rating = post_sum$mean[8], 
                           sd_rating = post_sum$sd[8])
df_5_results$RFP_intercept <- best_4$RFP_intercept
df_5_results$RFP_slope <- best_4$RFP_slope

df_5_results <- rbind(df_4_results, df_5_results) ##  Run it just once!

x_5 <- as.matrix(df_5_results[, -(1:3)])
y_5 <- df_5_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model_5 <- km(formula=~1, design=x_5, response=y_5, 
                 covtype="gauss", noise.var=(df_5_results$sd/2)^2)
pred_5 <- predict(gp_model_5, newdata = grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z_5  <- (pred_5$mean - max(pred_5$mean)) / pred_5$sd

PI_5 <- pnorm(z_5)
EI_5 <- (pred_5$mean - max(pred_5$mean)) * pnorm(z_5) + pred_5$sd * dnorm(z_5)

grid[which.max(EI_5), ]   
grid[which.max(PI_5), ] 
# they are equal   

best_5 <- grid[which.max(EI_5), ]
cat(sprintf("According to EI\nBest RFP_intercept: %.2f\nBest RFP_slope: %.f\n", 
            best_5$RFP_intercept, best_5$RFP_slope))

# New Expected Improvement Heatmap 2D
df_5_plot <- data.frame(
  RFP_intercept = grid$RFP_intercept,
  RFP_slope = grid$RFP_slope,
  mean = pred_5$mean,
  sd = pred_5$sd,
  EI = EI_5
)

df_5_obs <- data.frame(
  RFP_intercept = x_5[, 1],
  RFP_slope = x_5[, 2],
  y = y_5
)

ggplot(df_5_plot, aes(RFP_intercept, RFP_slope)) +
  geom_tile(aes(fill = EI)) +
  geom_contour(aes(z = mean), color = "white", bins = 10) +
  geom_point(
    data = df_5_obs,
    aes(RFP_intercept, RFP_slope),
    color = "orange3", size = 2) +
  geom_point(
    x = best_5$RFP_intercept,
    y = best_5$RFP_slope,
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
# The optmal values are assumed to be 300 for RFP_intercept and 0 for RFP_slope