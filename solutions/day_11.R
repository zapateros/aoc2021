# This solution for both star 1 and 2 only works if result for part 2 is larger than 100. otherwise the loop ends before it gets to 100 and therefore star 1 is wrong.
input <- readLines("input/input_day_11.txt")
mat   <- matrix(as.numeric(unlist(strsplit(input, ""))), byrow = T, nrow = length(input))

# Create adjacency list on cellnumber
dim     <- length(input)
length  <- dim * dim
mat.pad = rbind(NA, cbind(NA, matrix(c(1:(length)), ncol = dim), NA), NA)
ind     = 2:(dim + 1) # row/column indices of the "middle"
neighbours =  cbind(N  = as.vector(mat.pad[ind - 1, ind    ]),
              NE = as.vector(mat.pad[ind - 1, ind + 1]),
              E  = as.vector(mat.pad[ind    , ind + 1]),
              SE = as.vector(mat.pad[ind + 1, ind + 1]),
              S  = as.vector(mat.pad[ind + 1, ind    ]),
              SW = as.vector(mat.pad[ind + 1, ind - 1]),
              W  = as.vector(mat.pad[ind    , ind - 1]),
              NW = as.vector(mat.pad[ind - 1, ind - 1]))
neighbourList <- apply(neighbours, 1, function(x){x[!is.na(x)]})

nFlashes <- 0
maxStep  <- 100
step     <- 0
while(TRUE){
  step <- step + 1
  
  # Step 1: Add one to all
  mat <- mat + 1
  
  # Step 2: Do all flashes
  flashed <- NULL
  if(any(mat == 10)){
    
    while(TRUE){
      flashIndexes <- which(mat > 9)
      possibleFlashIndexes <- flashIndexes[!flashIndexes %in% flashed]
      if(length(possibleFlashIndexes) > 0){
        flashIndex <- possibleFlashIndexes[1]
        flashNeighbours <- unlist(neighbourList[flashIndex])
        mat[flashNeighbours] <- mat[flashNeighbours] + 1
        flashed <- c(flashed, flashIndex)
        if(step <= maxStep){
          nFlashes <- nFlashes + 1
        }
      }else{
        break
      }
    }
  }
  if(length(flashed) == 100){
    break
  }
  
  # Step 3: set all flashed to zero
  mat[mat > 9] <- 0
}

# Star 1
cat("Day 11 Star 1:", nFlashes)

# Star 2
cat("Day 11 Star 2:", step)
