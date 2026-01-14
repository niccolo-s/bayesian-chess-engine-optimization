
library(Rschach)
library(dplyr)
library(cmdstanr)

# -----------------------------------------------------------------------------
# 1. SETUP & PARAMETERS
# -----------------------------------------------------------------------------

# Ensure 'fit' object exists (from Task 4)
if (!exists("fit")) {
  stop("Error: 'fit' object not found. Please run Task 2/4 code first!")
}

post_sum <- fit$summary()

# Extract global parameters
global_params <- list(
  white_adv   = post_sum$mean[post_sum$variable == "white_advantage"],
  p_draw_base = post_sum$mean[post_sum$variable == "p_draw_base"],
  draw_scale  = post_sum$mean[post_sum$variable == "draw_scale"]
)

cat("=== Global Parameters from Stan Model ===\n")
print(global_params)

# Load Opening Book
book_file <- "8moves_v3.epd"
if(!file.exists(book_file)) {
  warning("File '8moves_v3.epd' not found! Using fallback start position.")
  book_fen <- c("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
} else {
  book_fen <- readLines(book_file)
}

# Archive for all games
all_games_archive <- data.frame()

# -----------------------------------------------------------------------------
# 2. SPRT FUNCTION
# -----------------------------------------------------------------------------

check_sprt <- function(history, params, E0=20, alpha=0.05, beta=0.05) {
  
  # SPRT Bounds
  bound_A <- log(beta / (1 - alpha))      # Stop & Reject H1 (Fail)
  bound_B <- log((1 - beta) / alpha)      # Stop & Accept H1 (Success)
  LLR <- 0
  
  white_adv   <- params$white_adv
  p_draw_base <- params$p_draw_base
  draw_scale  <- params$draw_scale
  
  for (res in history) {
    # res$is_white: TRUE if Challenger was White
    # res$score: 1 (Win), 0.5 (Draw), 0 (Loss) -- from Challenger perspective
    
    # --- Hypothesis H0: Delta = 0 (No Improvement) ---
    # Diff = (R_White - R_Black) + Adv
    # Under H0, R_Chal == R_Champ. So (R_W - R_B) is always 0.
    diff_H0 <- white_adv
    
    # --- Hypothesis H1: Delta = E0 (Improvement) ---
    if (res$is_white) {
      # Challenger is White. R_Chal = R_Champ + E0.
      # Diff = (R_Chal - R_Champ) + Adv = E0 + Adv
      diff_H1 <- E0 + white_adv
    } else {
      # Challenger is Black.
      # Diff is always from White's perspective (Champion).
      # Diff = (R_Champ - R_Chal) + Adv
      #      = (R_Champ - (R_Champ + E0)) + Adv = -E0 + Adv
      diff_H1 <- -E0 + white_adv
    }
    
    # Probability Calculation (Task 4 Formula)
    calc_probs <- function(d) {
      exp_score <- 1 / (1 + 10^(-d / 400))
      p_draw    <- p_draw_base * exp(-abs(d) / draw_scale)
      p_win  <- (1 - p_draw) * exp_score
      p_loss <- (1 - p_draw) * (1 - exp_score)
      c(p_loss, p_draw, p_win) 
    }
    
    probs_H0 <- calc_probs(diff_H0)
    probs_H1 <- calc_probs(diff_H1)
    
    # Outcome Index: 1=BlackWin, 2=Draw, 3=WhiteWin
    outcome_idx <- 2 
    if (res$is_white) {
      if (res$score == 1) outcome_idx <- 3
      if (res$score == 0) outcome_idx <- 1 
    } else {
      if (res$score == 1) outcome_idx <- 1 
      if (res$score == 0) outcome_idx <- 3 
    }
    
    lik_H1 <- max(probs_H1[outcome_idx], 1e-10)
    lik_H0 <- max(probs_H0[outcome_idx], 1e-10)
    
    LLR <- LLR + log(lik_H1 / lik_H0)
  }
  
  if (LLR >= bound_B) return("H1")
  if (LLR <= bound_A) return("H0")
  return("Undecided")
}

# -----------------------------------------------------------------------------
# 3. TUNING LOOP
# -----------------------------------------------------------------------------

champion_params <- list() 
champion_name <- "Default_SchachMaus"

# Candidate Parameters
candidates <- list(
  list(name="V1_NMP", params=list(NMP_intercept=3)),
  list(name="V2_LMR", params=list(LMR_intercept=1, LMR_slope=0.5)),
  list(name="V3_RFP", params=list(RFP_intercept=-30, RFP_slope=150))
)

cat("\n=== STARTING TUNING TOURNAMENT ===\n")

final_results <- list()

for (cand in candidates) {
  
  cat(paste0("\n", paste(rep("-", 60), collapse=""), "\n"))
  cat(sprintf("MATCH: %s (Challenger) vs %s (Champion)\n", cand$name, champion_name))
  cat("Params:", paste(names(cand$params), cand$params, sep="=", collapse=", "), "\n")
  
  # --- INIT ENGINES ---
  
  # Initialize Champion
  if (length(champion_params) == 0) {
    e_champ <- Engine(name = champion_name)
  } else {
    e_champ <- Engine(name = champion_name, params = champion_params)
  }
  
  # Initialize Challenger
  e_chal <- Engine(name = cand$name, params = cand$params)
  
  history <- list()
  decision <- "Undecided"
  max_games <- 200 
  
  # Game Loop
  while(decision == "Undecided" && length(history) < max_games) {
    
    fen <- sample(book_fen, 1)
    
    # Play 1 Round (2 games)
    res_batch <- tryCatch({
      play.tournament(
        e_chal, e_champ, 
        book = fen, 
        nr_rounds = 1L, 
        tc_base = 0.5, tc_inc = 0.05 
      )
    }, error = function(e) {
      cat("\n[Warning] Engine execution failed:", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(res_batch)) { decision <- "Error"; break; }
    
    # Robust Parsing
    if (!is.data.frame(res_batch)) {
      df_temp <- data.frame(white=character(), result=character(), stringsAsFactors=FALSE)
      for (g in res_batch) {
        w <- tryCatch(g$header("White"), error=function(e) "Unknown")
        r <- tryCatch(g$header("Result"), error=function(e) "*")
        df_temp <- rbind(df_temp, data.frame(white=w, result=r))
      }
      res_batch <- df_temp
    }
    
    if (nrow(res_batch) == 0) next
    
    # Update History
    for (k in 1:nrow(res_batch)) {
      r_str <- res_batch$result[k]
      is_chal_white <- (k %% 2 != 0)
      
      score <- 0.5 
      if (r_str == "1-0") score <- ifelse(is_chal_white, 1, 0)
      if (r_str == "0-1") score <- ifelse(is_chal_white, 0, 1)
      
      history[[length(history)+1]] <- list(is_white=is_chal_white, score=score)
      
      # Archive Data
      all_games_archive <- bind_rows(all_games_archive, data.frame(
        Round_ID = length(final_results) + 1,
        Challenger = cand$name,
        Champion = champion_name,
        Is_Chal_White = is_chal_white,
        Result = r_str,
        Score = score,
        Param_Set = paste(names(cand$params), cand$params, sep="=", collapse=";")
      ))
    }
    
    # Check SPRT (E0=100, beta=0.2 for faster rejection)
    decision <- check_sprt(history, params=global_params, E0=30, alpha=0.1, beta=0.1)
    cat(sprintf("\rGames: %d | Status: %s", length(history), decision))
    flush.console()
  }
  
  cat("\nMatch Decision:", decision, "\n")
  
  # Update Champion Logic
  if (decision == "H1") {
    cat(sprintf(">>> SUCCESS! %s promotes to Champion!\n", cand$name))
    champion_params <- cand$params
    champion_name <- cand$name
  } else {
    cat(sprintf(">>> FAIL. %s remains Champion.\n", champion_name))
  }
  
  final_results[[cand$name]] <- decision
  
  Sys.sleep(0.5)
}

# Save Results
write.csv(all_games_archive, "task5_tuning_archive.csv", row.names=FALSE)
cat("\n=== TUNING COMPLETE ===\n")
