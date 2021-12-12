# Not a very clean and fast solution but it works
input       <- readLines("input/input_day_12.txt")
mat         <- matrix(unlist(strsplit(input, "-")), byrow = TRUE, nrow = length(input))
matBothWays <- rbind(mat, mat[,c(2,1)])
caves       <- setNames(data.frame(matBothWays), c("from", "to"))
caves$from  <- as.character(caves$from)
caves$to    <- as.character(caves$to)

isBig <- function(string){
  string == toupper(string)
}

# I checked big caves are not connected to other big caves,  therefore no infinite paths
path  <- "start"
count <- 0
findPath <- function(path){
  cave <- path[length(path)]
  # Check possible next caves
  possiblePathsAll <- unique(caves$to[caves$from == cave])
  checks <- sapply(possiblePathsAll, function(x){isBig(x) | (!isBig(x) & !(x %in% path))})
  possiblePaths <- possiblePathsAll[checks]
  if(length(possiblePaths) > 0){
    for(x in possiblePaths){
      path1 <- c(path, x)  
      if(x == "end"){
        count <<- count + 1
      }else{
        findPath(path1)
      }
    }
  }
}

findPath(path)
output <- count
cat("Day 12 Star 1:", output)

# STAR 2
path  <- "start"
count <- 0
findPath <- function(path){
  cave <- path[length(path)]
  # Check possible next caves
  possiblePathsAll <- unique(caves$to[caves$from == cave])
  lowerInPath      <- path[!(isBig(path) | path == "start")]
  if(length(lowerInPath)>0){
    tab <- table(lowerInPath)
    if(any(tab > 1)){
      possiblePaths <- possiblePathsAll[!possiblePathsAll == "start"]
      possiblePaths <- possiblePaths[!possiblePaths %in% names(tab)]
    }else{
      possiblePaths <- possiblePathsAll[!(possiblePathsAll == "start")]
    }
  }else{
    possiblePaths <- possiblePathsAll[!(possiblePathsAll == "start")]
  }
  if(length(possiblePaths) > 0){
    for(x in possiblePaths){
      path1 <- c(path, x)  
      if(x == "end"){
        count <<- count + 1
      }else{
        findPath(path1)
      }
    }
  }
}

findPath(path)
output <- count
cat("Day 12 Star 2:", output)
