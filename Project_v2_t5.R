# ==============================================================================
# Task 5: Manual Tuning (Sequential Testing with Rschach)
# ==============================================================================

library(Rschach)
library(dplyr)
library(cmdstanr)

# -----------------------------------------------------------------------------
# 1. SETUP & PARAMETERS
# -----------------------------------------------------------------------------

# Check if the 'fit' object from Task 2 exists (required for model parameters)
if (!exists("fit")) stop("Error: Run Task 2 first to generate the 'fit' object!")

# Extract global parameters from the fitted Stan model
post_sum <- fit$summary()

global_params <- list(
  white_adv   = post_sum$mean[post_sum$variable == "white_advantage"],
  p_draw_base = post_sum$mean[post_sum$variable == "p_draw_base"],
  draw_scale  = post_sum$mean[post_sum$variable == "draw_scale"]
)

cat("=== Global Parameters from Stan Model ===\n")
print(global_params)

# Load the opening book (FEN strings)
book_file <- "8moves_v3.epd"
if(!file.exists(book_file)) {
  warning("File 8moves_v3.epd not found! Using simplified start positions.")
  book_fen <- c("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
} else {
  book_fen <- read.csv(book_file, header = FALSE)[[1]]
}

# -----------------------------------------------------------------------------
# 2. SPRT FUNCTION (Consistent with Task 4 Logic)
# -----------------------------------------------------------------------------

check_sprt <- function(history, params, E0=10, alpha=0.05, beta=0.05) {
  
  # Define SPRT Boundaries (Stop Thresholds)
  bound_A <- log(beta / (1 - alpha))      # Threshold for H0 (New engine is NOT better)
  bound_B <- log((1 - beta) / alpha)      # Threshold for H1 (New engine IS better)
  LLR <- 0                                # Log-Likelihood Ratio accumulator
  
  # Retrieve parameters from the list
  white_adv   <- params$white_adv
  p_draw_base <- params$p_draw_base
  draw_scale  <- params$draw_scale
  
  # NOTE ON TIME CONTROL (IMPORTANT):
  # Even though games are played with a time control (e.g., 0.5+0.05),
  # we set diff_tc = 0 here.
  # Reason: We do not have the 'beta' parameter (time sensitivity) for the NEW engine
  # because it hasn't been modeled yet. We assume its time management is similar 
  # to the base engine, so the rating difference comes purely from strength, not time handling.
  diff_tc <- 0 
  
  for (res in history) {
    # 'res$score' is the point result for the CHALLENGER (1=Win, 0.5=Draw, 0=Loss)
    
    # Map result to Stan Model Outcome Indices:
    # 1 = Black Win
    # 2 = Draw
    # 3 = White Win
    outcome_idx <- 2 # Default to Draw
    
    if (res$is_white) {
      # Challenger played WHITE
      if (res$score == 1) outcome_idx <- 3 # White (Challenger) Won
      if (res$score == 0) outcome_idx <- 1 # Black (Champion) Won
    } else {
      # Challenger played BLACK
      if (res$score == 1) outcome_idx <- 1 # Black (Challenger) Won
      if (res$score == 0) outcome_idx <- 3 # White (Champion) Won
    }
    
    # --- Hypothesis H0: No Improvement (Delta = 0) ---
    # Under H0, the rating difference is 0.
    if (res$is_white) {
      # Challenger (White) vs Champion (Black)
      # Diff = (Challenger - Champion) + adv = 0 + adv
      diff_H0 <- 0 + diff_tc + white_adv
    } else {
      # Champion (White) vs Challenger (Black)
      # Diff = (Champion - Challenger) + adv = 0 + adv
      # Note: Even if roles are reversed, if ratings are equal, diff is 0.
      diff_H0 <- -diff_tc + white_adv
    }
    
    # --- Hypothesis H1: Improvement (Challenger is stronger by E0) ---
    if (res$is_white) {
      # Challenger (White) is stronger by E0
      # Diff = E0 + adv
      diff_H1 <- E0 + diff_tc + white_adv
    } else {
      # Champion (White) is weaker (Challenger is Black and stronger)
      # Diff = (Champion - Challenger) + adv = -E0 + adv
      diff_H1 <- -E0 - diff_tc + white_adv
    }
    
    # Probability Calculation Helper (Formula from Stan Model)
    calc_probs <- function(d) {
      exp_score <- 1 / (1 + 10^(-d / 400))
      p_draw    <- p_draw_base * exp(-abs(d) / draw_scale)
      # Returns vector: [Prob(BlackWin), Prob(Draw), Prob(WhiteWin)]
      c((1 - p_draw) * (1 - exp_score), p_draw, (1 - p_draw) * exp_score)
    }
    
    probs_H0 <- calc_probs(diff_H0)
    probs_H1 <- calc_probs(diff_H1)
    
    # Update LLR (Log-Likelihood Ratio)
    # Use max(..., 1e-10) to avoid log(0) errors
    lik_H1 <- max(probs_H1[outcome_idx], 1e-10)
    lik_H0 <- max(probs_H0[outcome_idx], 1e-10)
    
    LLR <- LLR + log(lik_H1 / lik_H0)
  }
  
  # Check stopping conditions
  if (LLR >= bound_B) return("H1") # Accept H1 (Challenger is better)
  if (LLR <= bound_A) return("H0") # Accept H0 (Challenger is not better)
  return("Undecided")              # Continue testing
}

# -----------------------------------------------------------------------------
# 3. TUNING CONFIGURATION
# -----------------------------------------------------------------------------

# Initial Champion (Default Stockfish with no custom parameters)
champion_params <- list() 
champion_name <- "Default_Stockfish"

# List of Candidates to test (Challengers)
# These modify specific internal parameters (NMP, LMR, RFP)
candidates <- list(
  list(name="V1_NMP", params=list(NMP_intercept=3, NMP_slope=0)),
  list(name="V2_LMR", params=list(LMR_intercept=1, LMR_slope=0.5)),
  list(name="V3_RFP", params=list(RFP_intercept=-30, RFP_slope=150)),
  list(name="V4_Combo", params=list(NMP_intercept=3, LMR_intercept=1, RFP_intercept=-30, RFP_slope=150))
)

# -----------------------------------------------------------------------------
# 4. MAIN TUNING LOOP
# -----------------------------------------------------------------------------

cat("\n=== STARTING TUNING TOURNAMENT ===\n")
cat("SPRT Settings: E0=10, Alpha=0.05, Beta=0.05\n")

final_results <- list()

# Iterate through every candidate in our list
for (cand in candidates) {
  
  cat(paste0("\n", paste(rep("-", 60), collapse=""), "\n"))
  cat(sprintf("MATCH: %s (Challenger) vs %s (Champion)\n", cand$name, champion_name))
  cat("Testing Params:", paste(names(cand$params), cand$params, sep="=", collapse=", "), "\n")
  
  # --- INIT ENGINES ---
  
  # Initialize Champion Engine
  if (length(champion_params) == 0) {
    e.champ <- Engine("Stockfish") # Default settings
  } else {
    e.champ <- Engine("Stockfish", params = champion_params) # Custom settings
  }
  
  # Initialize Challenger Engine
  # Rschach requires parameters to be a named list
  if (is.null(names(cand$params))) stop("Challenger parameters must be named!")
  e.chal <- Engine("Stockfish", params = cand$params)
  
  # Reset match history for this pair
  history <- list()
  decision <- "Undecided"
  games_played <- 0
  
  # --- GAME LOOP ---
  # Keep playing until a decision is made or we reach the safety limit (400 games)
  while(decision == "Undecided" && games_played < 400) {
    
    # Sample a random opening position from the book
    current_book_pos <- sample(book_fen, 1) 
    
    # 1. Play a Round
    # play.tournament automatically plays 2 games per round (swapping colors)
    # We use tryCatch to prevent the script from crashing if the engine hangs/crashes
    res_batch <- tryCatch({
      play.tournament(
        e.chal, e.champ,       # e.chal is Engine #1, e.champ is Engine #2
        book = current_book_pos, 
        nr_rounds = 1L,        # 1 Round = 2 Games (White/Black swap)
        tc_base = 0.5,         # Time: 0.5 seconds (Very fast for testing)
        tc_inc = 0.05           
      )
    }, error = function(e) {
      cat("\nError inside play.tournament:", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(res_batch)) { decision <- "Error"; break; }
    
    # 2. Parse Results (Robust Fix)
    # Sometimes Rschach returns a list of Game objects instead of a data frame.
    # This block ensures we always work with a clean data frame.
    if (!is.data.frame(res_batch)) {
      df_temp <- data.frame(white=character(), result=character(), stringsAsFactors=FALSE)
      for (g in res_batch) {
        w <- tryCatch(g$header("White"), error=function(e) "Unknown")
        r <- tryCatch(g$header("Result"), error=function(e) "*")
        df_temp <- rbind(df_temp, data.frame(white=w, result=r))
      }
      res_batch <- df_temp
    }
    
    if (nrow(res_batch) == 0) next # Skip if empty result
    
    # 3. Update History
    # We need to record the result from the Challenger's perspective.
    # Logic: In Round 1, Game 1 (Index 1) -> Engine #1 (Challenger) is White.
    #        In Round 1, Game 2 (Index 2) -> Engine #1 (Challenger) is Black.
    
    for (k in 1:nrow(res_batch)) {
      res_str <- res_batch$result[k]
      
      # Determine if Challenger was White based on game index
      is_chal_white <- (k %% 2 != 0) 
      
      # Calculate Score for Challenger
      score <- 0.5 # Default Draw
      if (res_str == "1-0") score <- ifelse(is_chal_white, 1, 0)
      if (res_str == "0-1") score <- ifelse(is_chal_white, 0, 1)
      
      # Add to history list
      history[[length(history)+1]] <- list(is_white=is_chal_white, score=score)
    }
    
    # Check SPRT status
    games_played <- length(history)
    decision <- check_sprt(history, params=global_params)
    
    # Print progress (overwrite line with \r)
    cat(sprintf("\rGames: %d | Status: %s", games_played, decision))
    flush.console()
  }
  
  cat("\n")
  
  # --- ACTION AFTER MATCH ---
  if (decision == "H1") {
    cat(sprintf("--> VICTORY! %s is better. Promoting to Champion.\n", cand$name))
    # Challenger wins -> They become the new Champion for the next match
    champion_params <- cand$params
    champion_name <- cand$name
  } else if (decision == "H0") {
    cat(sprintf("--> DEFEAT. %s failed to beat Champion.\n", cand$name))
    # Challenger lost -> Current Champion stays
  } else {
    cat(sprintf("--> UNDECIDED (Limit reached). Keeping current Champion.\n"))
  }
  
  # Save result for summary
  final_results[[cand$name]] <- list(
    decision = decision, 
    games = games_played,
    champion_after = champion_name
  )
}

cat("\n=== TUNING FINISHED ===\n")
cat("Final Champion Engine:", champion_name, "\n")
cat("Best Parameters Found:", paste(names(champion_params), champion_params, sep="=", collapse=", "), "\n")

