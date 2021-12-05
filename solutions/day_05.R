input <- readLines("input/input_day_05.txt")
mat   <- matrix(as.numeric(unlist(strsplit(gsub(" -> ", ",", input), ","))), byrow = TRUE, ncol = 4)

points      <- NULL
points_diag <- NULL
for(i in 1:nrow(mat)){
  x1 <- mat[i, 1]
  y1 <- mat[i, 2]
  x2 <- mat[i, 3]
  y2 <- mat[i, 4]
  if(x1 == x2){
    points <- rbind(points, cbind(x1, y1:y2))
  }else if(y1 == y2){
    points <- rbind(points, cbind(x1:x2, y1))
  }else if(abs(x1 - x2) == abs(y1 - y2)){ 
    # Diagonals for Star 2
    points_diag <- rbind(points_diag, cbind(x1:x2, y1:y2))
  } 
}

# Star 1
output <- nrow(unique(points[duplicated(points),]))
cat("Day 5 Star 1:", output)

# Star 2
combined_points <- rbind(points, points_diag)
output <- nrow(unique(combined_points[duplicated(combined_points),]))
cat("Day 5 Star 2:", output)
