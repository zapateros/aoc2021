input <- readLines("input/input_day_09.txt")
dim   <- length(input)
mat   <- matrix(as.numeric(unlist(strsplit(input, ""))), byrow = TRUE, nrow = dim)

# Create neighbourlist with cellnumber
mat.pad = rbind(NA, cbind(NA, matrix(c(1:(dim * dim)), ncol = dim), NA), NA)
ind = 2:(dim + 1) # row/column indices of the "middle"
neighbours = cbind(N  = as.vector(mat.pad[ind - 1, ind    ]),
              E  = as.vector(mat.pad[ind    , ind + 1]),
              S  = as.vector(mat.pad[ind + 1, ind    ]),
              W  = as.vector(mat.pad[ind    , ind - 1]))
nb_list <- apply(neighbours, 1, function(x){x[!is.na(x)]})

# Star 1
minima  <- sapply(1:length(mat), function(x){all(mat[x] < mat[unlist(nb_list[x])])})              
output <- sum(mat[minima] + 1)                 
cat("Day 9 Star 1:", output)

# Star 2
bassinSizes <- NULL
bassinCells <- which(mat < 9)
while(length(bassinCells) > 0){
  # Find cellnumbers of cluster of picked cell
  findCluster <- function(cell, cluster = cell){
    nbs <- nb_list[[cell]]
    for(nb in nbs){
      if(mat[nb] != 9 & !(nb %in% cluster)){
        found   <- findCluster(nb, c(cluster, nb))
        cluster <- unique(c(cluster, found))
      }
    }
    return(cluster)
  }
  chosenCluster <- findCluster(bassinCells[1])
  clusterSize   <- length(chosenCluster)
  bassinSizes   <- c(bassinSizes, clusterSize)
  bassinCells   <- bassinCells[!bassinCells %in% chosenCluster]
}

output <- prod(bassinSizes[rev(order(bassinSizes))][1:3])
cat("Day 9 Star 2:", output)
