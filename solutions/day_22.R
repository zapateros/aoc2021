# This one took me too long. In the end I'm still not completely sure this is the right way but it works (slow..)
# It helped writing some 2D examples.
# I didn't clean the code yet but I just want to forget this one. Also I'm not even sure this was the code that I used in the end but who cares

input <- readLines("input/input_day_22.txt")
time <- Sys.time()
cubes <- NULL
for(i in input){
  on <- grepl("on", i)
  i <- unlist(strsplit(gsub("[^0-9.,-]", "", i), ","))
  x1 <- as.numeric(unlist(strsplit(i[1], "\\.\\.")))
  x <- c(x1[1]:x1[2])
  y1 <- as.numeric(unlist(strsplit(i[2], "\\.\\.")))
  y <- c(y1[1]:y1[2])
  z1 <- as.numeric(unlist(strsplit(i[3], "\\.\\.")))
  z <- c(z1[1]:z1[2])
  gr <- expand.grid(x, y, z)
  strs <- apply(gr, 1, paste, collapse = ",")
  if(on){
    cubes <- unique(c(cubes, strs))
  }else{
    cubes <- cubes[!(cubes %in% strs)]
  }
}
output <- length(cubes)
cat("Day 22 Star 1:", output)
Sys.time() - time



# Star 2 (veeeeery slow method, but it works)
mat <- NULL
for(i in 1:length(input)){
  on <- grepl("on", input[i]) * 1
  numbers <- unlist(strsplit(gsub("[^0-9.,-]", "", input[i]), ","))
  x1 <- as.numeric(unlist(strsplit(numbers[1], "\\.\\.")))
  y1 <- as.numeric(unlist(strsplit(numbers[2], "\\.\\.")))
  z1 <- as.numeric(unlist(strsplit(numbers[3], "\\.\\.")))
  mat <- rbind(mat, data.frame(onOff = on, xFrom = x1[1], xTo = x1[2], yFrom = y1[1], yTo = y1[2], zFrom = z1[1], zTo = z1[2]))
}

checkOverlap <- function(cube1, cube2){
  
  x1 <- c(cube1$xFrom, cube1$xTo)
  y1 <- c(cube1$yFrom, cube1$yTo)
  z1 <- c(cube1$zFrom, cube1$zTo)
  
  x2 <- c(cube2$xFrom, cube2$xTo)
  y2 <- c(cube2$yFrom, cube2$yTo)
  z2 <- c(cube2$zFrom, cube2$zTo)
  
  # overlap x
  ox1 <- min(x2) >= min(x1) & min(x2) <= max(x1) # min(x2):min(max(x1), max(x2))
  ox2 <- min(x1) >= min(x2) & min(x1) <= max(x2) # min(x1):min(max(x2), max(x1))
  
  # overlap y
  oy1 <- min(y2) >= min(y1) & min(y2) <= max(y1) # min(y2):min(max(y1), max(y2))
  oy2 <- min(y1) >= min(y2) & min(y1) <= max(y2) # min(y1):min(max(y2), max(y1))
  
  # overlap z
  oz1 <- min(z2) >= min(z1) & min(z2) <= max(z1) # min(z2):min(max(z1), max(z2))
  oz2 <- min(z1) >= min(z2) & min(z1) <= max(z2) # min(z1):min(max(z2), max(z1))
  
  if((ox1 | ox2) & (oy1 | oy2) & (oz1 | oz2)){
    
    xNewFrom <- c(min(x2), min(x1))[c(ox1, ox2)][1]
    xNewTo   <- min(c(max(x1), max(x2)))
    
    yNewFrom <- c(min(y2), min(y1))[c(oy1, oy2)][1]
    yNewTo   <- min(c(max(y1), max(y2))) 
    
    zNewFrom <- c(min(z2), min(z1))[c(oz1, oz2)][1]
    zNewTo   <- min(c(max(z1), max(z2)))
    
    newCube <- data.frame(xFrom = xNewFrom, xTo = xNewTo, yFrom = yNewFrom, yTo = yNewTo, zFrom = zNewFrom, zTo = zNewTo)
    return(newCube)
  }else{
    return(NULL)
  }
}


calculateSize <- function(cube){
  sum((abs(cube$xTo - cube$xFrom) + 1) * (abs(cube$yTo - cube$yFrom) + 1) * (abs(cube$zTo - cube$zFrom) + 1))
}


time <- Sys.time()
cubesPlus <- mat[1, -1]
cubesMin  <- NULL


for(j in 2:nrow(mat)){
  
  
  onOff <- mat$onOff[j]
  cubeNew <- mat[j, -1]
  
  if(onOff == 1){
    minuses <- NULL
    for(i in 1:nrow(cubesPlus)){
      overlapCube <- checkOverlap(cubeNew, cubesPlus[i,])
      minuses <- rbind(minuses, overlapCube)
    }
    
    pluses <- NULL
    if(length(cubesMin) > 0){
      for(i in 1:nrow(cubesMin)){
        overlapCube <- checkOverlap(cubeNew, cubesMin[i,])
        pluses <- rbind(pluses, overlapCube)
        
      }
    }
    
    cubesPlus <- rbind(cubesPlus, cubeNew)
    cubesPlus <- rbind(cubesPlus, pluses)
    cubesMin <- rbind(cubesMin, minuses)
    
    
    
  }else if(onOff == 0){
    
    # find overlaps with cubesPlus
    minuses <- NULL
    for(i in 1:nrow(cubesPlus)){
      overlapCube <- checkOverlap(cubeNew, cubesPlus[i,])
      minuses <- rbind(minuses, overlapCube)
    }
    
    # find overlaps with Overlaps because they will be counted double
    pluses <- NULL
    for(i in 1:nrow(cubesMin)){
      overlapCube <- checkOverlap(cubeNew, cubesMin[i,])
      pluses <- rbind(pluses, overlapCube)
    }
    
    cubesPlus <- rbind(cubesPlus, pluses)
    cubesMin <- rbind(cubesMin, minuses)
    
  }
  
  cat(j, ": ", Sys.time() - time, "\n")
  
}
Sys.time() - time

as.character(calculateSize(cubesPlus) - calculateSize(cubesMin))
