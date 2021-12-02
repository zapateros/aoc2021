input <- readLines("input/input_day_02.txt")
forward_inds <- grep("forward", input)
forward <- input[forward_inds]
up_down <- input[-forward_inds]

horizontal <- sum(as.numeric(gsub("forward ", "", forward)))
vertical <- sum(as.numeric(gsub("up ", "-", gsub("down ", "", up_down))))
output <- horizontal * vertical
cat("Day 2 Star 1: ", output)

# Star 2
aim  <- 0
hor  <- 0
dept <- 0
for(x in input){
  if(grepl("forward", x)){
    hor_x <- as.numeric(gsub("forward ", "", x))
    hor <- hor + hor_x
    dept <- dept + hor_x * aim
  }else if(grepl("down", x)){
    aim <- aim + as.numeric(gsub("down ","", x))
  }else if(grepl("up", x)){
    aim <- aim - as.numeric(gsub("up ", "", x))
  }
}
output <- hor * dept
cat("Day 2 Star 2: ", output)
