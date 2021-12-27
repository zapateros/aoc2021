# This is for part 2. Use one loop for part one and cut off at 50
input <- readLines("input/input_day_20.txt")
split <- which(input == "")
inputString <- paste(input[1:(split - 1)], collapse = "")
enhancing <- unlist(gregexpr("#", inputString)[1])

inputMatrix <- matrix(unlist(strsplit(input[(split + 1):length(input)], "")), 
                      byrow = TRUE, ncol = nchar(input[split + 1]))
indices <- data.frame(which(inputMatrix == "#", arr.ind = TRUE))
time <- Sys.time()
for(i in 1:50){
  newIndices <- NULL
  for(x in (min(indices$col) - 3):(max(indices$col) + 3)){
    for(y in (min(indices$row) - 3):(max(indices$row) + 3)){
      
      t1 <- any((indices$row == y - 1) & (indices$col == x - 1))
      t2 <- any((indices$row == y - 1) & (indices$col == x))
      t3 <- any((indices$row == y - 1) & (indices$col == x + 1))
      
      t4 <- any((indices$row == y) & (indices$col == x - 1))
      t5 <- any((indices$row == y) & (indices$col == x))
      t6 <- any((indices$row == y) & (indices$col == x + 1))
      
      t7 <- any((indices$row == y + 1) & (indices$col == x - 1))
      t8 <- any((indices$row == y + 1) & (indices$col == x))
      t9 <- any((indices$row == y + 1) & (indices$col == x + 1))
      
      
      t <- c(t1, t2, t3, t4, t5, t6, t7, t8, t9)
      
      enhanceIndex <- sum(t * 2^(8:0)) + 1
      
      if(enhanceIndex %in% enhancing){
        newIndices <- rbind(newIndices, data.frame(row = y, col = x))
      }
      
    }
  }
  
  # if even i, cut off edges
  if((i %% 2) == 0){
    newIndices <- newIndices[newIndices$row> -i & newIndices$row <(101 + i) & newIndices$col> -i & newIndices$col <(101 + i),]
  }
  
  
  #newIndices <- newIndices[newIndices$row> 0 & newIndices$row <101 & newIndices$col> 0 & newIndices$col <101,]
  
  
  indices <- newIndices
  cat(i,": ", as.character(Sys.time() - time), "\n")
}
Sys.time() - time


dim(newIndices[newIndices$row> -2 & newIndices$row <103 & newIndices$col> -2 & newIndices$col <103,])
