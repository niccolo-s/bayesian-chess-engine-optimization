# ==============================================================================
# Task 6: Automated Parameter Tuning
# ==============================================================================

library(Rschach)
library(dplyr)
library(cmdstanr)
library(DiceKriging)
library(tidyverse)

setwd("C:/Users/faust/Desktop/Bayesian Statistics/chess engine project")  #change your working directory when you run 

book_fen <- read.csv("8moves_v3.epd", header = FALSE)[[1]]

#### 1. Setting initial values for LMR_slope ####

# List of initial candidates
# A = 0.2, B = 0.9, C = 1.3, D = 1.6
candidates <- list(
  list(name="A", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.2, RFP_intercept=-30, RFP_slope=150)),
  list(name="B", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=0.9, RFP_intercept=-30, RFP_slope=150)),
  list(name="C", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=1.3, RFP_intercept=-30, RFP_slope=150)),
  list(name="D", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1, LMR_slope=1.6, RFP_intercept=-30, RFP_slope=150))
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

df_results$LMR_slope <- sapply(df_results$engine, function(x){
  cand <- candidates[[which(sapply(candidates, function(c) c$name == x))]]
  cand$params$LMR_slope
})

df_results
x <- as.matrix(df_results$LMR_slope)
colnames(x) <- "LMR_slope"
y <- df_results$mean_rating


#### 4. Gaussian process and Bayesian optimization ####
gp_model <- km(formula=~1, design=x, response=y, 
               covtype="gauss", noise.var=(df_results$sd/2)^2)

x_grid <- as.matrix(seq(0, 2, length.out = 200))
colnames(x_grid) <- "LMR_slope"
pred <- predict(gp_model, newdata = x_grid, type="UK")

# Probability of Improvement and Expected Improvement 
z  <- (pred$mean - max(pred$mean)) / pred$sd

PI <- pnorm(z)
EI <- (pred$mean - max(pred$mean)) * pnorm(z) + pred$sd * dnorm(z)

par(mfrow=c(1, 2))
plot(x_grid, EI, type="l", lwd=2,
     ylab="EI", xlab="LMR_slope")
plot(x_grid, PI, type="l", lwd=2,
     ylab="PI", xlab="LMR_slope")

cat("Best LMR_slope for EI:", x_grid[which.max(EI)]) 
cat("Best LMR_slope for PI:", x_grid[which.max(PI)]) 

# Plot of the function
y_min <- min(pred$mean - qnorm(0.025, lower.tail = F) * pred$sd, df_results$mean_rating)
y_max <- max(pred$mean + qnorm(0.025, lower.tail = F) * pred$sd, df_results$mean_rating)

par(mfrow=c(1, 1))
plot(x_grid, pred$mean, type="l", lwd=2, ylim=c(y_min, y_max),
     ylab="Prediction rating", xlab="LMR_slope", col="blue")
lines(x_grid, pred$mean - qnorm(0.025)*pred$sd, lty=2, lwd=2, col="skyblue")
lines(x_grid, pred$mean + qnorm(0.025)*pred$sd, lty=2, lwd=2, col="skyblue")
points(df_results$LMR_slope, df_results$mean_rating, pch=19, col="red")
abline(v=x_grid[which.max(EI)], col="green3", lwd=2)

# according to both methods, LMR_slope = 0 is chosen, 
# add that point in the data set, update the Gaussian process and repeat as a loop.


#### 5. Step 2 ####
df_2 <- data.frame(white=character(),
                     black=character(),
                     result=character())

candidate_2 <- list(name="E", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1,
                                            LMR_slope=x_grid[which.max(EI)], RFP_intercept=-30, RFP_slope=150))

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
df_2_results$LMR_slope <- x_grid[which.max(EI)]

df_2_results <- rbind(df_results, df_2_results) ##  Run it just once!

x_2 <- as.matrix(df_2_results$LMR_slope)
colnames(x_2) <- "LMR_slope"
y_2 <- df_2_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model_2 <- km(formula=~1, design=x_2, response=y_2, 
               covtype="gauss", noise.var=(df_2_results$sd/2)^2)
pred_2 <- predict(gp_model_2, newdata = x_grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z_2  <- (pred_2$mean - max(pred_2$mean)) / pred_2$sd

PI_2 <- pnorm(z_2)
EI_2 <- (pred_2$mean - max(pred_2$mean)) * pnorm(z_2) + pred_2$sd * dnorm(z_2)

par(mfrow=c(1, 2))
plot(x_grid, EI_2, type="l", lwd=2,
     ylab="EI", xlab="LMR_slope")
plot(x_grid, PI_2, type="l", lwd=2,
     ylab="PI", xlab="LMR_slope")

cat("Best LMR_slope for EI:", x_grid[which.max(EI_2)]) 
cat("Best LMR_slope for PI:", x_grid[which.max(PI_2)]) 
# they are different (PI only exploitation, while EI balance between exploration and exploitation)

# New Plot
y_min_2 <- min(pred_2$mean - qnorm(0.025, lower.tail = F) * pred_2$sd, df_2_results$mean_rating)
y_max_2 <- max(pred_2$mean + qnorm(0.025, lower.tail = F) * pred_2$sd, df_2_results$mean_rating)

par(mfrow=c(1, 1))
plot(x_grid, pred_2$mean, type="l", lwd=2, ylim=c(y_min_2, y_max_2),
     ylab="Prediction rating", xlab="LMR_slope", col="blue")
lines(x_grid, pred_2$mean - qnorm(0.025)*pred_2$sd, lty=2, lwd=2, col="skyblue")
lines(x_grid, pred_2$mean + qnorm(0.025)*pred_2$sd, lty=2, lwd=2, col="skyblue")
points(df_2_results$LMR_slope, df_2_results$mean_rating, pch=19, col="red")
abline(v=x_grid[which.max(EI_2)], col="green3", lwd=2)

# For step 3, according to EI, LMR_slope = 0.3015075 is chosen


#### 6. Step 3 ####
df_3 <- data.frame(white=character(),
                   black=character(),
                   result=character())

candidate_3 <- list(name="F", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1,
                                          LMR_slope=x_grid[which.max(EI_2)], RFP_intercept=-30, RFP_slope=150))

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
df_3_results$LMR_slope <- x_grid[which.max(EI_2)]

df_3_results <- rbind(df_2_results, df_3_results) ##  Run it just once!

x_3 <- as.matrix(df_3_results$LMR_slope)
colnames(x_3) <- "LMR_slope"
y_3 <- df_3_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model_3 <- km(formula=~1, design=x_3, response=y_3, 
               covtype="gauss", noise.var=(df_3_results$sd/2)^2)
pred_3 <- predict(gp_model_3, newdata = x_grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z_3  <- (pred_3$mean - max(pred_3$mean)) / pred_3$sd

PI_3 <- pnorm(z_3)
EI_3 <- (pred_3$mean - max(pred_3$mean)) * pnorm(z_3) + pred_3$sd * dnorm(z_3)

par(mfrow=c(1, 2))
plot(x_grid, EI_3, type="l", lwd=2,
     ylab="EI", xlab="LMR_slope")
plot(x_grid, PI_3, type="l", lwd=2,
     ylab="PI", xlab="LMR_slope")

cat("Best LMR_slope for EI:", x_grid[which.max(EI_3)]) 
cat("Best LMR_slope for PI:", x_grid[which.max(PI_3)]) 
# they are pretty similar 

# New Plot
y_min_3 <- min(pred_3$mean - qnorm(0.025, lower.tail = F) * pred_3$sd, df_3_results$mean_rating)
y_max_3 <- max(pred_3$mean + qnorm(0.025, lower.tail = F) * pred_3$sd, df_3_results$mean_rating)

par(mfrow=c(1, 1))
plot(x_grid, pred_3$mean, type="l", lwd=2, ylim=c(y_min_3, y_max_3),
     ylab="Prediction rating", xlab="LMR_slope", col="blue")
lines(x_grid, pred_3$mean - qnorm(0.025)*pred_3$sd, lty=2, lwd=2, col="skyblue")
lines(x_grid, pred_3$mean + qnorm(0.025)*pred_3$sd, lty=2, lwd=2, col="skyblue")
points(df_3_results$LMR_slope, df_3_results$mean_rating, pch=19, col="red")
abline(v=x_grid[which.max(EI_3)], col="green3", lwd=2)

# For step 4, according to EI, LMR_slope = 0.361809 would be chosen 

#### 7. Step 4 ####
df_4 <- data.frame(white=character(),
                   black=character(),
                   result=character())

candidate_4 <- list(name="G", params=list(NMP_intercept=3, NMP_slope=0, LMR_intercept=1,
                                          LMR_slope=x_grid[which.max(EI_3)], RFP_intercept=-30, RFP_slope=150))

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
df_4_results$LMR_slope <- x_grid[which.max(EI_3)]

df_4_results <- rbind(df_3_results, df_4_results) ##  Run it just once!

x_4 <- as.matrix(df_4_results$LMR_slope)
colnames(x_4) <- "LMR_slope"
y_4 <- df_4_results$mean_rating

# Fit a new Surrogate Model containing the new candidate as well 
gp_model_4 <- km(formula=~1, design=x_4, response=y_4, 
               covtype="gauss", noise.var=(df_4_results$sd/2)^2)
pred_4 <- predict(gp_model_4, newdata = x_grid, type="UK")

# New Probability of Improvement and Expected Improvement 
z_4  <- (pred_4$mean - max(pred_4$mean)) / pred_4$sd

PI_4 <- pnorm(z_4)
EI_4 <- (pred_4$mean - max(pred_4$mean)) * pnorm(z_4) + pred_4$sd * dnorm(z_4)

par(mfrow=c(1, 2))
plot(x_grid, EI_4, type="l", lwd=2,
     ylab="EI", xlab="LMR_slope")
plot(x_grid, PI_4, type="l", lwd=2,
     ylab="PI", xlab="LMR_slope")

cat("Best LMR_slope for EI:", x_grid[which.max(EI_4)]) 
cat("Best LMR_slope for PI:", x_grid[which.max(PI_4)]) 
# EI is indicating again 0 because it wants to be sure due to the presence of the noise, 
# while PI is indicating a new point near to 0

# New Plot
y_min_4 <- min(pred_4$mean - qnorm(0.025, lower.tail = F) * pred_4$sd, df_4_results$mean_rating)
y_max_4 <- max(pred_4$mean + qnorm(0.025, lower.tail = F) * pred_4$sd, df_4_results$mean_rating)

par(mfrow=c(1, 1))
plot(x_grid, pred_4$mean, type="l", lwd=2, ylim=c(y_min_4, y_max_4),
     ylab="Prediction rating", xlab="LMR_slope", col="blue")
lines(x_grid, pred_4$mean - qnorm(0.025)*pred_4$sd, lty=2, lwd=2, col="skyblue")
lines(x_grid, pred_4$mean + qnorm(0.025)*pred_4$sd, lty=2, lwd=2, col="skyblue")
points(df_4_results$LMR_slope, df_4_results$mean_rating, pch=19, col="red")
abline(v=x_grid[which.max(EI_4)], col="green3", lwd=2)
abline(v=x_grid[which.max(PI_4)], col="orange", lwd=2)

# For step 5, according to PI, LMR_slope = 0.120603 would be chosen
# The optimal value is assumed to be LMR_slope = 0.3015075