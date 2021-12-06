input  <- as.numeric(unlist(strsplit(readLines("input/input_day_06.txt"),",")))
school <- c(0, unname(table(input)), 0, 0, 0)

schoolSize <- function(days, school){
  for(day in 1:days){
    school <- c(school[2:9], school[1])
    school[7] <- school[7] + school[9]
  }
  as.character(sum(school))
}

# Star 1
output <- schoolSize(80, school)
cat("Day 6 Star 1:", output)

# Star 2
output <- schoolSize(256, school)
cat("Day 6 Star 2:", output)
