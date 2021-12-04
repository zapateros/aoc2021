# The zero in the input was bugging me today. 
input <- readLines("input/input_day_04.txt")
bingo <- as.numeric(unlist(strsplit(input[1], ",")))
max_rounds <- length(bingo)
board_separators <- which(input == "")
height <- board_separators[2] - board_separators[1] - 1

all_rounds <- data.frame()
for(i in 1:length(board_separators)){
  # Create board
  board_strings <- input[(board_separators[i] + 1):(board_separators[i] + height)]
  board_strings_split <- unlist(strsplit(board_strings, " "))               
  board_strings_split <- as.numeric(board_strings_split[nzchar(board_strings_split)])
  board <- matrix(board_strings_split, byrow = TRUE, nrow = height)
  
  # Bingo rounds
  chosen <- data.frame(row = NULL, col = NULL)
  draw <- 1
  while(TRUE){
    index <- which(board == bingo[draw], arr.ind = TRUE)
    board[index] <- -1
    chosen <- rbind(chosen, index)
    if(any(table(chosen$col) == 5) | any(table(chosen$row) == 5)){
      break
    }
    draw <- draw + 1
    
  }
  board[board == -1] <- 0
  score <- sum(board) * bingo[draw]
  all_rounds <- rbind(all_rounds, data.frame(win_round = draw, score = score))
}

# Star 1
output <- all_rounds$score[which.min(all_rounds$win_round)]
cat("Day 4 Star 1:", output)

# Star 2
output <- all_rounds$score[which.max(all_rounds$win_round)]
cat("Day 4 Star 2:", output)
