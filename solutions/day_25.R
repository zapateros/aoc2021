# not fast en elegant but to conclude advent of code 2021 it's fine with me!
input <- readLines("input/input_day_25.txt")
w <- nchar(input[1])
d <- length(input)
mat <- matrix(unlist(strsplit(input, "")), byrow = TRUE, nrow = d)
south <- which(mat == "v", arr.ind = TRUE)
east <- which(mat == ">", arr.ind = TRUE)

cucumbers <- rbind(south, east)


k <- 1
while(TRUE){
  stepsEast <- 0
  eastNew <- NULL
  for(i in 1:nrow(east)){
    cucumber <- east[i,]  
    nextSpot <- c(cucumber[1], cucumber[2] %% w + 1)
    if(any(cucumbers[,1] == nextSpot[1] & cucumbers[,2] == nextSpot[2])){
      eastNew <- rbind(eastNew, cucumber)
    }else{
      eastNew <- rbind(eastNew, nextSpot)
      stepsEast <- stepsEast + 1
    }
  }
  east <- eastNew
  cucumbers <- rbind(south, east)
  stepsSouth <- 0
  southNew <- NULL
  for(j in 1:nrow(south)){
    cucumber <- south[j,]
    nextSpot <- c(cucumber[1] %% d + 1, cucumber[2])
    if(any(cucumbers[,1] == nextSpot[1] & cucumbers[,2] == nextSpot[2])){
      southNew <- rbind(southNew, cucumber)
    }else{
      southNew <- rbind(southNew, nextSpot)
      stepsSouth <- stepsSouth + 1
    }
  }
  south <- southNew
  cucumbers <- rbind(south, east)
  if(sum(c(stepsEast, stepsSouth)) == 0){
    break
  }
  k <- k + 1
}
output <- k
cat("Day 25 Star 1: ", output)
