# This is for star 2. for star 1 just do one loop
input <- readLines("input/input_day_13.txt")
split <- which(input == "")
dotsInput <- input[1:split-1]
folds <- gsub("fold along ", "", input[(split+1):length(input)])

dots1 <- lapply(dotsInput, function(x){1 + as.numeric(unlist(strsplit(x,  ",")))})
dots <- matrix(unlist(dots1), byrow = TRUE, ncol = 2)


for(i in 1:length(folds)){
  fold <- folds[i]
  
  if(grepl("x=", fold)){
    foldLine <- as.numeric(gsub("x=", "", fold)) + 1
    indices <- dots[,1] > foldLine
    dots[indices, 1] <-  foldLine - (dots[indices, 1] - foldLine)
  }else if(grepl("y=", fold)){
    foldLine <- as.numeric(gsub("y=", "", fold)) + 1
    indices <- dots[,2] > foldLine
    dots[indices, 2] <-  foldLine - (dots[indices, 2] - foldLine)
  }
  cat(nrow(unique(dots)), "\n")
  
}
plot(dots[,1], -1 * dots[,2])
