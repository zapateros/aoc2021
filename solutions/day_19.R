input <- readLines("input/input_day_19.txt")
input1 <- input[-grep("scanner", input)]
input1[input1 == ""] <- "new"
input2 <- paste0(input1, collapse=",")
scannerStrings <- unlist(strsplit(input2, ",new,"))

# Make rotation matrices
theta <- pi/2
phi <- 0
gamma <- 0
rotate <- function(theta, phi, gamma){
  mat <- rbind(c(cos(theta)*cos(phi), sin(gamma) *sin(theta) * cos(phi) - cos(gamma)*sin(phi), cos(gamma) * sin(theta) *cos(phi) + sin(gamma)*sin(phi)),
        c(cos(theta) * sin(phi), sin(gamma)*sin(theta)*sin(phi) + cos(gamma) * cos(phi), cos(gamma)*sin(theta)*sin(phi) -sin(gamma) * cos(phi)),
        c(-sin(theta), sin(gamma)*cos(theta), cos(gamma)*cos(theta)))
  round(mat)
}
all <- NULL
for(x in 0:3){
  for(y in 0:3){
    for(z in 0:3){
      all <- rbind(all, c(rotate(x * pi / 2, y * pi / 2, z * pi / 2) %*% 1:3))
    }
  }
}
uniqueRotations <- unique(all)
directions <- uniqueRotations / abs(uniqueRotations)
rotations <- abs(uniqueRotations)

checkRotation <- function(tryRotation){
  n <- 7000
  found <- c(FALSE, FALSE, FALSE)
  x <- -n
  while(x < n){
    xx <- tryRotation$x - x
    if(sum(allBeacons$x %in% xx) > 11){
      
      found[1] <- TRUE
      break
    }
    x <- x + 1
  } 
  y <- -n
  while(y < n){
    yy <- tryRotation$y - y
    if(sum(allBeacons$y %in% yy) > 11){
      
      found[2] <- TRUE
      break
    }
    y <- y + 1
  } 
  
  z <- -n
  while(z < n){
    zz <- tryRotation$z - z
    if(sum(allBeacons$z %in% zz) > 11){
      
      found[3] <- TRUE
      break
    }
    z <- z + 1
  } 
  
  if(sum(found) == 3){
    c(x,y,z)
  }else{
    FALSE
  }
}

# takes 7 minutes
allBeacons <- setNames(data.frame(matrix(as.numeric(unlist(strsplit(scannerStrings[1], ","))), 
                                         byrow = TRUE, ncol = 3)), c("x", "y", "z"))
allScanners <- NULL
tries <- c(2:26)
k <- 1
while(TRUE){
  i <- tries[k]
  cat("try: ", i,"\n")
  newScanner <- setNames(data.frame(matrix(as.numeric(unlist(strsplit(scannerStrings[i], ","))), 
                                           byrow = TRUE, ncol = 3)), c("x", "y", "z"))
  j <- 1
  while(TRUE){
    relRotation <- rotations[j,]
    relDirection <- directions[j,]
    tryMat <- data.frame(x = relDirection[1] * newScanner[,relRotation[1]], 
                         y = relDirection[2] * newScanner[,relRotation[2]],
                         z = relDirection[3] * newScanner[,relRotation[3]])
    
    out <- checkRotation(tryMat)
    if(length(out) == 3){
      allScanners <- rbind(allScanners, out)
      beacons <- tryMat - matrix(rep(out, nrow(tryMat)), byrow = TRUE, ncol = 3)
      allBeacons <- unique(rbind(allBeacons, beacons))
      tries <- tries[-k]
      if(k > length(tries)){
        k <- 1
      }
      
      break
    }
    j <- j + 1
    if(j == 25){
      cat(i, "Not found", "\n")
      k <- k + 1
      if(k > length(tries)){
        k <- 1
      }
      break
    }
  }
  if(length(tries) == 0){
    break
  }
  
}

# Star 1
output <- nrow(allBeacons)
cat("Day 19 Star 1:", output)

# Star 2
maxDistance <- 0
for(first in 1:(nrow(allScanners) -1)){  
  for(second in (first + 1):nrow(allScanners)){   
    x1 <- allScanners[first,]
    x2 <- allScanners[second,]
    manDist <- sum(abs(x1 - x2))
    maxDistance <- max(manDist, maxDistance)
  }  
}

output <- maxDistance
cat("Day 19 Star 2:", output)
