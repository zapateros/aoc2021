input <- readLines("input/input_day_18.txt")
fishes <- strsplit(input, "")
fish1 <- fishes[[1]]

for(i in 2:length(fishes)){
  fish <- c("[", fish1, ",", fishes[[i]], "]")
  while(TRUE){
    
    # Check for nested pair in 4
    nest      <- 0
    i         <- 1
    nestIndex <- FALSE
    while(TRUE){
      fishChar <- fish[i]
      if(fishChar == "["){
        nest <- nest + 1
      }else if(fishChar == "]"){
        nest <- nest - 1
      }
      if(nest == 5){
        nestIndex <- i
        break
      }
      i <- i + 1
      if(i == length(fish)){
        break
      }
    }
    
    # if nested in 4, explode it. Else, check for split possibility
    if(nestIndex != FALSE){
      
      x <- as.numeric(fish[nestIndex + 1])
      y <- as.numeric(fish[nestIndex + 3])
      
      digits <- grep("[[:digit:]]",fish)
      
      right <- digits[digits > nestIndex]
      left  <- digits[digits < nestIndex]
      
      if(length(left) > 0){
        leftAddIndex <- left[length(left)]
        fish[leftAddIndex] <- as.numeric(fish[leftAddIndex]) + x
      }
      
      if(length(right) > 2){
        rightAddIndex <- right[3]
        fish[rightAddIndex] <- as.numeric(fish[rightAddIndex]) + y
      }
      
      fish[nestIndex] <- "0"
      fish <- fish[-c((nestIndex + 1):(nestIndex + 4))]
      
    }else{
      # Check for split possibility
      digits <- grep("[[:digit:]]",fish)
      digitNums <- as.numeric(fish[digits])
      splitNums <- which(digitNums > 9)
      if(length(splitNums) > 0){
        
        splitNum <- digitNums[splitNums[1]]
        xNew <- floor(splitNum / 2)
        yNew <- ceiling(splitNum / 2)
        charsAdd <- c("[",xNew, ",", yNew, "]")
        
        addIndex <- digits[splitNums[1]]
        fish <- c(fish[1:(addIndex - 1)], charsAdd, fish[(addIndex + 1):length(fish)])
        
      }else{
        break
      }
    }
  }
  fish1 <- fish
}



# calculate magnitudes
outFish <- paste(fish1, collapse = "")
while(TRUE){
  matches <- unlist(regmatches(outFish, gregexpr("\\[\\d+[\\,]\\d+\\]", outFish)))
  if(length(matches) == 0){
    break
  }
  match <- matches[1]
  nums  <- as.numeric(unlist(regmatches(match, gregexpr("\\d+", match))))
  newNum <- sum(nums * c(3, 2))
  outFish <- gsub(paste0("\\", match), as.character(newNum), outFish)
}
magnitude <- as.numeric(outFish)
output <- magnitude
cat("Day 18 Star 1:", output)


# Star 2
calculateFish <- function(fish, fish2){
  fish <- c("[", fish1, ",", fish2, "]")
  while(TRUE){
    
    # Check for nested pair in 4
    nest      <- 0
    i         <- 1
    nestIndex <- FALSE
    while(TRUE){
      fishChar <- fish[i]
      if(fishChar == "["){
        nest <- nest + 1
      }else if(fishChar == "]"){
        nest <- nest - 1
      }
      if(nest == 5){
        nestIndex <- i
        break
      }
      i <- i + 1
      if(i == length(fish)){
        break
      }
    }
    
    # if nested in 4, explode it. Else, check for split possibility
    if(nestIndex != FALSE){
      
      x <- as.numeric(fish[nestIndex + 1])
      y <- as.numeric(fish[nestIndex + 3])
      
      digits <- grep("[[:digit:]]",fish)
      
      right <- digits[digits > nestIndex]
      left  <- digits[digits < nestIndex]
      
      if(length(left) > 0){
        leftAddIndex <- left[length(left)]
        fish[leftAddIndex] <- as.numeric(fish[leftAddIndex]) + x
      }
      
      if(length(right) > 2){
        rightAddIndex <- right[3]
        fish[rightAddIndex] <- as.numeric(fish[rightAddIndex]) + y
      }
      
      fish[nestIndex] <- "0"
      fish <- fish[-c((nestIndex + 1):(nestIndex + 4))]
      
    }else{
      # Check for split possibility
      digits <- grep("[[:digit:]]",fish)
      digitNums <- as.numeric(fish[digits])
      splitNums <- which(digitNums > 9)
      if(length(splitNums) > 0){
        
        splitNum <- digitNums[splitNums[1]]
        xNew <- floor(splitNum / 2)
        yNew <- ceiling(splitNum / 2)
        charsAdd <- c("[",xNew, ",", yNew, "]")
        
        addIndex <- digits[splitNums[1]]
        fish <- c(fish[1:(addIndex - 1)], charsAdd, fish[(addIndex + 1):length(fish)])
        
      }else{
        break
      }
    }
  }
  
  # calculate magnitudes
  outFish <- paste(fish, collapse = "")
  while(TRUE){
    matches <- unlist(regmatches(outFish, gregexpr("\\[\\d+[\\,]\\d+\\]", outFish)))
    if(length(matches) == 0){
      break
    }
    match <- matches[1]
    nums  <- as.numeric(unlist(regmatches(match, gregexpr("\\d+", match))))
    newNum <- sum(nums * c(3, 2))
    outFish <- gsub(paste0("\\", match), as.character(newNum), outFish)
  }
  magnitude <- as.numeric(outFish)
  magnitude
}

maxMagnitude <- 0
for(i in 1:(length(fishes) - 1)){
  for(j in ((i + 1):length(fishes))){
    mag1 <- calculateFish(fishes[[i]], fishes[[j]])
    mag2 <- calculateFish(fishes[[j]], fishes[[i]])
    maxMagnitude <- max(maxMagnitude, mag1, mag2)
  }
}
output <- maxMagnitude
cat("Day 18 Star 2:", output)
