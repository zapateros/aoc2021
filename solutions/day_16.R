# This one I really liked. For part two I first created a dataframe with the structure and then analysed it to find the answer

bitHex <- "0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111"
mat <- matrix(unlist(strsplit(gsub("\n", " = ", bitHex), " = ")), byrow = TRUE, ncol = 2)
input <- readLines("input/input_day_16.txt")
bits <- NULL
for(i in 1:nchar(input)){
  bits <- c(bits, as.numeric(unlist(strsplit(mat[mat[,1] == substr(input, i, i),2], ""))))
}

bit3 <- 2^(2:0)
bit11 <- 2^(10:0)
bit15 <- 2^(14:0)  


# Star 1
package       <- bits
packageLength <- length(package)
i             <- 1
totalVersion  <- 0
while(TRUE){
  version      <- sum(package[i:(i+2)] * bit3)
  id           <- sum(package[(i+3):(i+5)] * bit3)
  totalVersion <- totalVersion + version
  if(id == 4){
    i <- i + 6
    while(TRUE){
      group <- package[i:(i + 4)]
      if(group[1] == 1){
        i <- i + 5
      }else{
        i <- i + 5
        break
      }
    }
  }else{
    lengthTypeID <- package[i + 6]
    if(lengthTypeID == 0){
      i <- i + 22
    }else if(lengthTypeID == 1){
      i <- i + 18
    }
  }
  if(i > (packageLength - 7)){
    break
  }
}
output <- totalVersion
cat("Day 16 Star 1:", output)


# Star 2
calcNumeric <- function(x){
  sum(2^((length(x) - 1):0) * x) 
}
input <- readLines("input/input_day_16.txt")
bits <- NULL
for(i in 1:nchar(input)){
  bits <- c(bits, as.numeric(unlist(strsplit(mat[mat[,1] == substr(input, i, i),2], ""))))
}

package       <- bits
packageLength <- length(package)
i         <- 1
structure <- NULL
while(TRUE){
  version     <- sum(package[i:(i+2)] * bit3)
  id          <- sum(package[(i+3):(i+5)] * bit3)
  structureAdd <- data.frame(i = i, id = id, value = NA, bits = NA, packets = NA)

  if(id == 4){
    i <- i + 6
    literal <- NULL
    while(TRUE){
      group <- package[i:(i + 4)]
      literal <- c(literal, group[2:5])
      if(group[1] == 1){
        i <- i + 5
      }else{
        i <- i + 5
        break
      }
    }
    structureAdd$value <- calcNumeric(literal)
  }else{
    lengthTypeID <- package[i + 6]
    if(lengthTypeID == 0){
      structureAdd$bits <- calcNumeric(package[(i + 7):(i + 21)]) + 21
      i <- i + 22
    }else if(lengthTypeID == 1){
      structureAdd$packets <- calcNumeric(package[(i + 7):(i + 17)])
      i <- i + 18
    }
  }
  
  structure <- rbind(structure, structureAdd)
  if(i > (packageLength - 7)){
    break
  }
}

j <- 1
while(TRUE){
  
  structureRow <- structure[j,]
  if(!is.na(structureRow$bits) | !is.na(structureRow$packets)){
    
    if(!is.na(structureRow$bits)){
      packetRows <- which(structure$i > structureRow$i & structure$i < (structureRow$i + structureRow$bits))
    }else if(!is.na(structureRow$packets)){
      packetRows <- (j + 1):(j + structureRow$packets)
    }
    
    values <- structure$value[packetRows]
    if(any(is.na(values))){
      j <- j + 1
    }else{
      
      if(structureRow$id == 0){
        newValue <- sum(values)
      }else if(structureRow$id == 1){
        newValue <- prod(values)
      }else if(structureRow$id == 2){
        newValue <- min(values)
      }else if(structureRow$id == 3){
        newValue <- max(values)
      }else if(structureRow$id == 5){
        if(values[1] > values[2]){
          newValue <- 1
        }else{
          newValue <- 0
        }
      }else if(structureRow$id == 6){
        if(values[1] < values[2]){
          newValue <- 1
        }else{
          newValue <- 0
        }
      }else if(structureRow$id == 7){
        if(values[1] == values[2]){
          newValue <- 1
        }else{
          newValue <- 0
        }
      }
      structure$value[j] <- newValue
      structure$packets[j] <- NA
      structure$bits[j] <- NA
      structure <- structure[-packetRows,]
      
      #cat(paste(structure[1,], sep = ","), "\n")
      
      if(nrow(structure) == 1){
        break
      }
      j <- 1 
    }  
  }else{
    j <- j + 1
  }
}
output <- as.character(structure$value)
cat("Day 16 Star 2:", output)
