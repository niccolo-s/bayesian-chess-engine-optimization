# ==============================================================================
# Task 5: Parameter Tuning using Sequential Testing
# ==============================================================================

library(Rschach)
library(dplyr)

# --- SETUP ---
book_file <- "8moves_v3.epd"
if(file.exists(book_file)) {
  book_fen <- readLines(book_file)
} else {
  book_fen <- c("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
}

# --- CANDIDATE DEFINITIONS ---
candidates <- list(
  list(
    name = "Tuning_Candidate",
    params = list(
      NMP_intercept = 4,
      RFP_intercept = 75000,
      LMR_intercept = 1
    )
  ),
  list(
    name = "Validation_Weak",
    params = list(
      NMP_intercept = 8,
      RFP_intercept = 0,
      LMR_intercept = 5
    )
  )
)

# --- SPRT FUNCTION ---
check_sprt <- function(history, E0=10, alpha=0.05, beta=0.05) {
  w_adv <- 0.25
  p_base <- 0.45
  d_scale <- 1.5
  
  bound_L <- log(beta / (1 - alpha))
  bound_U <- log((1 - beta) / alpha)
  LLR <- 0
  
  get_probs <- function(d) {
    ea <- 1/(1+10^(-d/400))
    pd <- p_base * exp(-abs(d)/d_scale)
    return(c((1-pd)*(1-ea), pd, (1-pd)*ea))
  }
  
  pH0 <- get_probs(w_adv)
  pH1 <- get_probs(w_adv + E0)
  
  for (h in history) {
    idx <- 2
    if (h$score == 1) idx <- 3
    if (h$score == 0) idx <- 1
    if (!h$is_white) idx <- 4 - idx
    
    val_H1 <- max(pH1[idx], 1e-10)
    val_H0 <- max(pH0[idx], 1e-10)
    LLR <- LLR + log(val_H1 / val_H0)
  }
  
  if (LLR >= bound_U) return("Significant")
  if (LLR <= bound_L) return("Neutral")
  return("Continue")
}

# --- TESTING EXECUTION ---
all_results <- data.frame()
default_params <- list(NMP_intercept=3, RFP_intercept=70000, LMR_intercept=0)

for (cand in candidates) {
  cat(sprintf("\n%s\nTESTING: %s vs Default\n", paste(rep("=", 60), collapse=""), cand$name))
  
  e_champ <- Engine(name = "SchachMaus", params = default_params)
  e_chal  <- Engine(name = "SchachMaus")
  e_chal$set.params(cand$params)
  
  history <- list()
  status <- "Continue"
  
  while(status == "Continue" && length(history) < 50) {
    fen <- sample(book_fen, 1)
    games <- tryCatch(
      play.tournament(e_chal, e_champ, book=fen, nr_rounds=1L, tc_base=1, tc_inc=0.1),
      error=function(e) NULL
    )
    
    if (is.null(games)) break
    
    res1 <- games[[1]]$result()
    res2 <- games[[2]]$result()
    
    s1 <- 0.5; if(res1=="1-0") s1<-1; if(res1=="0-1") s1<-0
    s2 <- 0.5; if(res2=="1-0") s2<-0; if(res2=="0-1") s2<-1
    
    history[[length(history)+1]] <- list(is_white=TRUE, score=s1)
    history[[length(history)+1]] <- list(is_white=FALSE, score=s2)
    
    all_results <- rbind(all_results, 
                        data.frame(Cand=cand$name, Game=length(history), 
                                 Result=res2, ChalScore=s2))
    
    status <- check_sprt(history)
    cat(sprintf("\rGames: %d | Score: %.1f | Status: %s", 
                length(history), sum(sapply(history, function(x) x$score)), status))
    flush.console()
  }
  
  cat("\n")
  try(e_champ$quit(), silent=TRUE)
  try(e_chal$quit(), silent=TRUE)
  gc()
}

write.csv(all_results, "task5_final_report.csv", row.names=FALSE)
cat("\nComplete. Results saved to 'task5_final_report.csv'.\n")
