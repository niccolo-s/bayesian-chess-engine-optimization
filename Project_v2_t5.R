library(Rschach)
library(dplyr)

# --- SETUP ---
book_file <- "8moves_v3.epd"
if(file.exists(book_file)) {
  book_fen <- readLines(book_file)
} else {
  warning("Book file not found! Using startpos.")
  book_fen <- c("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
}

# --- CANDIDATE DEFINITIONS ---
# POPRAVLJENE VRIJEDNOSTI PREMA ZADATKU
candidates <- list(
  list(
    name = "Tuning_Candidate",
    params = list(
      NMP_intercept = 4,    # Malo agresivnije (Default je obicno 3)
      RFP_intercept = 0,    # U rasponu -100 do 300 (Default je npr -30)
      LMR_intercept = 1     # (Default je 0 ili 1)
    )
  ),
  list(
    name = "Validation_Diff",
    params = list(
      NMP_intercept = 3,    # Default
      RFP_intercept = 100,  # Značajna promjena unutar raspona
      LMR_intercept = 0
    )
  )
)

# --- SPRT FUNCTION ---
# Tvoja logika za SPRT i color flipping je dobra!
check_sprt <- function(history, E0=10, alpha=0.05, beta=0.05) {
  # Parametri za logističku distribuciju (Elo)
  # Napomena: Ovi 'magic numbers' (0.25, 0.45) su heuristike za remi šansu.
  # Za zadatak su vjerojatno OK, ali pazi da znaš što znače ako te pitaju.
  w_adv <- 0.25 
  p_base <- 0.45 
  d_scale <- 1.5 
  
  bound_L <- log(beta / (1 - alpha))
  bound_U <- log((1 - beta) / alpha)
  LLR <- 0
  
  get_probs <- function(d) {
    ea <- 1/(1+10^(-d/400))
    pd <- p_base * exp(-abs(d)/d_scale)
    return(c((1-pd)*(1-ea), pd, (1-pd)*ea)) # Loss, Draw, Win (iz perspektive Bijelog)
  }
  
  pH0 <- get_probs(w_adv)       # Null hypothesis (Elo diff = 0)
  pH1 <- get_probs(w_adv + E0)  # Alt hypothesis (Elo diff = E0)
  
  for (h in history) {
    idx <- 2 # Default Draw
    if (h$score == 1) idx <- 3 # Win
    if (h$score == 0) idx <- 1 # Loss
    
    # Ako kandidat NIJE bio bijeli, moramo "okrenuti" ploču za vjerojatnosti
    # Jer pH0/pH1 su izračunati za Bijelog.
    if (!h$is_white) idx <- 4 - idx 
    
    val_H1 <- max(pH1[idx], 1e-10)
    val_H0 <- max(pH0[idx], 1e-10)
    LLR <- LLR + log(val_H1 / val_H0)
  }
  
  if (LLR >= bound_U) return("Significant (H1 accepted)")
  if (LLR <= bound_L) return("Neutral (H0 accepted)")
  return("Continue")
}

# --- TESTING EXECUTION ---
all_results <- data.frame()
# Default params according to Task 6 Hint
default_params <- list(NMP_intercept=3, RFP_intercept=-30, LMR_intercept=0)

for (cand in candidates) {
  cat(sprintf("\n%s\nTESTING: %s vs Default\nParams: %s\n", 
              paste(rep("=", 60), collapse=""), 
              cand$name,
              paste(names(cand$params), cand$params, sep="=", collapse=", ")))
  
  e_champ <- Engine(name = "SchachMaus", params = default_params)
  e_chal  <- Engine(name = "SchachMaus")
  e_chal$set.params(cand$params)
  
  history <- list()
  status <- "Continue"
  
  # POVEĆAN LIMIT NA 500 IGARA
  while(status == "Continue" && length(history) < 500) {
    
    # Osigurač ako je book prazan
    fen <- if(length(book_fen)>0) sample(book_fen, 1) else NULL
    
    games <- tryCatch(
      play.tournament(e_chal, e_champ, book=fen, nr_rounds=1L, tc_base=1, tc_inc=0.1),
      error=function(e) { cat("Error in game:", e$message, "\n"); return(NULL) }
    )
    
    if (is.null(games)) break
    
    # play.tournament s nr_rounds=1 igra 2 igre (A vs B, B vs A)
    # Pretpostavka: games[[1]] je Chal(White) vs Champ(Black)
    #               games[[2]] je Champ(White) vs Chal(Black)
    
    res1 <- games[[1]]$result()
    res2 <- games[[2]]$result()
    
    # Score for Candidate (Chal)
    s1 <- 0.5; if(res1=="1-0") s1<-1; if(res1=="0-1") s1<-0 # Chal is White
    s2 <- 0.5; if(res2=="1-0") s2<-0; if(res2=="0-1") s2<-1 # Chal is Black (0-1 means Black wins)
    
    history[[length(history)+1]] <- list(is_white=TRUE, score=s1)
    history[[length(history)+1]] <- list(is_white=FALSE, score=s2)
    
    # SPREMANJE SVIH PARAMETARA
    # Kreiramo red s rezultatima i parametrima kandidata
    row_data <- data.frame(
      Cand = cand$name,
      Game_ID = length(history), # Globalni ID u matchu
      Is_White = c(TRUE, FALSE),
      Score = c(s1, s2),
      NMP = cand$params$NMP_intercept,
      RFP = cand$params$RFP_intercept,
      LMR = cand$params$LMR_intercept
    )
    
    all_results <- rbind(all_results, row_data)
    
    status <- check_sprt(history)
    
    # Ispis stanja svakih 10 igara da ne spama konzolu previše
    if (length(history) %% 10 == 0) {
        cat(sprintf("\rGames: %d | Total Score: %.1f | LLR Status: %s", 
                    length(history), sum(sapply(history, function(x) x$score)), status))
        flush.console()
    }
  }
  
  cat(sprintf("\nFinal Status for %s: %s after %d games.\n", cand$name, status, length(history)))
  
  try(e_champ$quit(), silent=TRUE)
  try(e_chal$quit(), silent=TRUE)
  gc() # Garbage collector
}

write.csv(all_results, "task5_final_report.csv", row.names=FALSE)
cat("\nComplete. Results saved to 'task5_final_report.csv'.\n")
