# Not very clean but I have other things to do
# target : x=201..230, y=-99..-65
xTarget <- c(201:230)
yTarget <- c(-99:-65)

xTargetMin <- 201
xTargetMax <- 230
yTargetMin <- -99

yMaxAll <- 0
allRounds <- NULL
for(xvinit in 1:250){
  for(yvinit in -100:500){
    y <- x <- 0
    yMax <- 0
    xv <- xvinit
    yv <- yvinit
    while(x <= xTargetMax & y >= yTargetMin){
      x <- x + xv
      y <- y + yv
      yMax <- max(c(y, yMax))
      if(xv > 0){
        xv <- xv - 1
      }else if(xv < 0){
        xv <- xv + 1
      }
      yv <- yv - 1
      if((x %in% xTarget) & (y %in% yTarget)){
        allRounds <- rbind(allRounds, c(xvinit, yvinit))
        yMaxAll <- max(yMax, yMaxAll)
        break
        
      }
      if(xv == 0 & x < xTargetMin){
        break
      }
    }
  }
}
# Star 1
output <- yMaxAll
cat("Day 17 Star 1:", output)

# Star 2
output <- nrow(allRounds)
cat("Day 17 Star 2:", output)
