rolled <- 0
turn <- 1
score <- c(0, 0)
space <- c(7, 2)
die <- c(1, 2, 3)
while(TRUE){
  space[turn] <- (space[turn] + sum(die) - 1) %% 10 + 1
  score[turn] <- score[turn] + space[turn]
  rolled <- rolled + 1
  die <- (die + 3 -1) %% 100 + 1
  turn <- turn %% 2 + 1
  if(any(score > 999)){
    break
  }
  
}

output <- 3 * rolled * min(score)
cat("Day 21 Star 1:", output)

# Star 2
possibleOutcomes <- rowSums(expand.grid(1:3, 1:3, 1:3))
posThrows <- table(possibleOutcomes)
throws <- as.numeric(names(posThrows))
posThrows <-c(1,3,6,7,6,3,1)
throws <- c(3,4,5,6,7,8,9)
roll <- function(player, space, die, score, multiplier){
  
  newSpace <- (space[player] + die - 1) %% 10 + 1
  newScore <- score[player] + newSpace
  
  if(newScore < winScore){
    score[player] <- newScore
    space[player] <- newSpace
    newPlayer <- player %% 2 + 1
    
    for(i in 1:length(throws)){
      newMultiplier <- as.integer64.integer64(multiplier * posThrows[i])
      if(is.na(newMultiplier)){
        cat(multiplier,":",posThrows[i], "\n")
      }
      roll(newPlayer, space, throws[i], score, newMultiplier)
    }
  }else{
    wins[player] <<- wins[player] + multiplier
  }
}

winScore <- 21
wins <- c(0, 0)
space <- c(7, 2)
score <- c(0,0)

time <- Sys.time()
for(j in 1:length(throws)){
  roll(1, space, throws[j], score, posThrows[j])
  cat(as.character(Sys.time() - time), "  ",j,"\n")
}

output <- as.character(max(wins))
cat("Day 21 Star 2:", output)
