# ==============================================================================
# Task 5: Manual Tuning (Rschach Syntax Fixed)
# ==============================================================================

library(Rschach)
library(dplyr)

# 1. SETUP & PARAMETERS -------------------------------------------------------
if (!exists("fit")) stop("Run Task 1/2 first to get the 'fit' object!")

post_sum <- fit$summary()
global_params <- list(
  white_adv   = post_sum$mean[post_sum$variable == "white_advantage"],
  p_draw_base = post_sum$mean[post_sum$variable == "p_draw_base"],
  draw_scale  = post_sum$mean[post_sum$variable == "draw_scale"]
)

# Load opening book exactly as in the example
book_file <- "8moves_v3.epd"
if(!file.exists(book_file)) stop("File 8moves_v3.epd not found!")
book_fen <- read.csv(book_file, header = FALSE)[[1]]

# 2. SPRT FUNCTION (UNCHANGED) ------------------------------------------------
check_sprt <- function(history, E0=10, alpha=0.05, beta=0.05, params) {
  
  bound_A <- log(beta / (1 - alpha))
  bound_B <- log((1 - beta) / alpha)
  LLR <- 0
  
  for (res in history) {
    # res$score is points for Challenger (1, 0.5, 0)
    outcome_idx <- 2
    if (res$is_white) {
      if (res$score == 1) outcome_idx <- 3
      if (res$score == 0) outcome_idx <- 1
    } else {
      if (res$score == 1) outcome_idx <- 1
      if (res$score == 0) outcome_idx <- 3
    }
    
    adv <- params$white_adv
    diff_H0 <- ifelse(res$is_white, adv, -adv)
    diff_H1 <- ifelse(res$is_white, E0 + adv, -E0 + adv)
    
    get_p <- function(d) {
      es <- 1 / (1 + 10^(-d/400))
      pd <- params$p_draw_base * exp(-abs(d)/params$draw_scale)
      c((1-pd)*(1-es), pd, (1-pd)*es)
    }
    
    p0 <- get_p(diff_H0)
    p1 <- get_p(diff_H1)
    LLR <- LLR + log(max(p1[outcome_idx], 1e-10) / max(p0[outcome_idx], 1e-10))
  }
  
  if (LLR >= bound_B) return("H1")
  if (LLR <= bound_A) return("H0")
  return("Undecided")
}

# 3. TUNING CONFIGURATION -----------------------------------------------------

champion_params <- list() # Empty list = Default params
champion_name <- "Champion"

# Candidates to test (from Task 6 hint)
candidates <- list(
  list(name="V1_NMP", params=list(NMP_intercept=3, NMP_slope=0)),
  list(name="V2_LMR", params=list(LMR_intercept=1, LMR_slope=0.5)),
  list(name="V3_RFP", params=list(RFP_intercept=-30, RFP_slope=150)),
  list(name="V4_Combo", params=list(NMP_intercept=3, LMR_intercept=1, RFP_intercept=-30, RFP_slope=150))
)


# 4. MAIN TUNING LOOP (DEBUGGED & ROBUST) -------------------------------------

cat("\n=== STARTING TUNING TOURNAMENT ===\n")

for (cand in candidates) {
  
  cat(paste0("\n----------------------------------------------------------\n"))
  cat(sprintf("MATCH: %s (Challenger) vs %s (Champion)\n", cand$name, champion_name))
  
  # --- DEBUG PRINT ---
  # Provjeravamo strukturu parametara prije kreiranja enginea
  cat("DEBUG: Challenger Params:\n")
  print(cand$params)
  cat("DEBUG: Names of Challenger Params:", paste(names(cand$params), collapse=", "), "\n")
  
  # --- INIT CHAMPION ---
  # Ako je lista prazna (duljina 0), NE šaljemo argument 'params' uopće.
  # Ovo sprječava grešku "Parameter list must be named" za prazne liste.
  if (length(champion_params) == 0) {
    cat("DEBUG: Init Champion (Default)\n")
    e.champ <- Engine(champion_name)
  } else {
    cat("DEBUG: Init Champion (Custom)\n")
    e.champ <- Engine(champion_name, params = champion_params)
  }
  
  # --- INIT CHALLENGER ---
  # Ista logika: provjeravamo ima li imena prije slanja
  if (length(cand$params) == 0) {
    # Ovo se ne bi smjelo dogoditi za Challengera, ali za svaki slučaj
    e.chal <- Engine(cand$name)
  } else {
    # Provjera jesu li parametri imenovani (ključno za Rschach!)
    if (is.null(names(cand$params))) {
      stop("CRITICAL ERROR: Challenger parameters are not named! Check 'candidates' list definition.")
    }
    cat("DEBUG: Init Challenger (Custom)\n")
    e.chal <- Engine(cand$name, params = cand$params)
  }
  
  history <- list()
  decision <- "Undecided"
  games_played <- 0
  
  while(decision == "Undecided" && games_played < 200) {
    
    current_book_pos <- sample(book_fen, 2) 
    
    # 1. Play Tournament
    # Pazi: play.tournament nekad vraća listu igara, nekad data frame.
    # Najsigurnije je spremiti u varijablu i provjeriti klasu.
    res_batch <- tryCatch({
      play.tournament(
        e.chal, e.champ,       
        book = current_book_pos, 
        nr_rounds = 1L,        
        tc_base = 1,           
        tc_inc = 0.1           
      )
    }, error = function(e) {
      cat("\nError inside play.tournament:", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(res_batch)) { decision <- "Error"; break; }
    
    # 2. Convert to DataFrame (Fix for "argument of length 0")
    # Ako je res_batch lista (Game objects), moramo izvući rezultate.
    # Ako je već data frame, ovo će samo proći.
    
    if (!is.data.frame(res_batch)) {
      # Pretpostavka: res_batch je lista Game objekata
      # Rschach obično ima metodu result() ili header() na game objektu
      
      # Ručno kreiramo data frame iz liste igara
      df_temp <- data.frame(white=character(), result=character(), stringsAsFactors=FALSE)
      
      for (g in res_batch) {
        # Pokušaj izvući headere
        # Sintaksa može varirati: g$header("White") ili g$white
        w <- tryCatch(g$header("White"), error=function(e) "Unknown")
        r <- tryCatch(g$header("Result"), error=function(e) "*")
        
        df_temp <- rbind(df_temp, data.frame(white=w, result=r))
      }
      res_batch <- df_temp
    }
    
    # Debug: Provjera je li sad data frame pun
    if (nrow(res_batch) == 0) {
      cat("\nWARNING: play.tournament returned 0 results! Retrying...\n")
      next # Preskoči ovu iteraciju
    }
    
    # 3. Parse results (Standard logic)
    for (k in 1:nrow(res_batch)) {
      is_chal_white <- (res_batch$white[k] == cand$name)
      res_str <- res_batch$result[k]
      
      score <- 0.5
      if (res_str == "1-0") score <- ifelse(is_chal_white, 1, 0)
      if (res_str == "0-1") score <- ifelse(is_chal_white, 0, 1)
      
      history[[length(history)+1]] <- list(is_white=is_chal_white, score=score)
    }
    
    games_played <- length(history)
    decision <- check_sprt(history, params=global_params)
    
    cat(sprintf("\rGames: %d | Status: %s", games_played, decision))
    flush.console()
  }
  
  
  cat("\n")
  
  if (decision == "H1") {
    cat(sprintf("--> WINNER: %s is better! Promoting to Champion.\n", cand$name))
    champion_params <- cand$params
    champion_name <- cand$name
  } else if (decision == "H0") {
    cat(sprintf("--> DEFEAT: %s is NOT better.\n", cand$name))
  } else {
    cat("--> TIMEOUT/ERROR: Keeping current champion.\n")
  }
}

cat("\n=== TUNING FINISHED ===\n")

cat("Final Champion:", champion_name, "\n")
cat("Final Params:", paste(names(champion_params), champion_params, sep="=", collapse=", "), "\n")

