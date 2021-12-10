input    <- readLines("input/input_day_10.txt")
closings <- c(")", "]", "}", ">")
points   <- c(3, 57, 1197, 25137)
opens    <- c("(", "[", "{", "<")
points2  <- c(1, 2, 3, 4)
scores2  <- NULL
score    <- 0
for(i in 1:length(input)){
  line <- input[i]
  
  line_new <- line
  while(TRUE){
    line <- line_new
    line_new <- gsub("\\{}|\\[]|<>|\\()", "", line)
    if(nchar(line) == nchar(line_new)){
      break
    }
  }
  firstClosingIndex <- unlist(gregexpr("}|]|)|>", line))[1]
  
  if(firstClosingIndex != -1){
    wrongClosing <- substring(line, firstClosingIndex, firstClosingIndex)
    score <- score + points[closings == wrongClosing]
  }else{
    lengthChars <- nchar(line)
    score2 <- 0
    for(n in lengthChars:1){
      score2 <- score2 * 5
      score2 <- score2 + points2[opens == substring(line, n, n)]
    }
    scores2 <- c(scores2, score2)
  }
}
# Star 1
output <- score
cat("Day 10 Star 1:", output)

# Star 2
scores2_ord <- scores2[order(scores2)]
output <- scores2_ord[(length(scores2_ord) + 1)/2]
cat("Day 10 Star 2:", output)
