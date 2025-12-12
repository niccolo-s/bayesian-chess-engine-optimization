library(Rschach)

# Read the opening book (character vector of FEN string)
book <- read.csv("8moves_v3.epd", head = FALSE)[[1]]

# Print the first 8 string
head(book, 8)
# and display the chess board position setup
positions(head(book, 8))

# Instantiate an engine with default parameters named "base"
e.base <- Engine("base")
# another engine instance with specific parameters
e.new <- Engine("new", params = list(NMP_intercept = 2, NMP_slope = 0.34))

# Get a list of the new engines parameters and their values
e.new$params()

# Play one game of chess between the two engines with time control 1+0.1
(game <- play.game(
    white = e.base,
    black = e.new,
    startpos = book[1],
    tc_base = 1,
    tc_inc = 0.1
))

# Now, play a tournament with repeated positions (that is each starting position
# is played twice, where for the second game the engine switch sides (colors).
# This is the default to ensure fair comparison)
(games <- play.tournament(e.base, e.new, book = book, nr_rounds = 3L))

# Same tournament to file in PGN format
pgn(games, file = "games.pgn")
