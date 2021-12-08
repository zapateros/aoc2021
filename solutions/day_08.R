# Not a very elegant solution, but it works. I just implemented every manual step
input      <- readLines("input/input_day_08.txt")
mat        <- setNames(data.frame(matrix(trimws(unlist(strsplit(input, "\\|"))), byrow = TRUE, ncol = 2)), c("input", "output"))
mat$input  <- as.character(mat$input)
mat$output <- as.character(mat$output)

# Star 1
counts <- lapply(mat$output, function(x){nchar(unlist(strsplit(x, " ")))})
aggs   <- table(unlist(counts))
output <- sum(aggs[which(names(aggs) %in% c(2, 3, 4, 7))])
cat("Day 8 Star 1:", output)

# Star 2
# I call the edges:
#     t
# lt      rt
#     m
# lb      rb
#     b
edges <- c("t", "rt", "lt", "m", "rb", "lb", "b")
codes <- NULL
for(i in 1:nrow(mat)){
  inp <- unlist(strsplit(mat$input[i], " "))
  out <- unlist(strsplit(mat$output[i], " "))
  ls  <- vector("list", 7)
  names(ls) <- edges
  
  inpSplit   <- strsplit(inp, "")
  inpLengths <- nchar(inp)
  
  # Determine letters and edges
  # rt and rb have just 2 letters (number 1)
  ls$rt <- ls$rb <- unlist(inpSplit[inpLengths == 2])
  
  # t: 3letter different from 2 letter (number 7)
  charn3 <- unlist(inpSplit[inpLengths == 3])
  ls$t <- charn3[!charn3 %in% ls$rt]
  
  # lt and m have 4 chars, different from rt and rb (number 4)
  charn4 <- unlist(inpSplit[inpLengths == 4])
  ls$lt  <- ls$m <- charn4[!charn4 %in% ls$rt]
  
  # lb and b, 7 chars (number 8)
  charn7 <- unlist(inpSplit[inpLengths == 7])
  ls$lb  <- ls$b <- charn7[!charn7 %in% unique(unlist(ls))]
  
  # m and lt can be found by checking which chars6 is missing one of the letters of lt and m
  ls$lt <- ls$lt[c(all(grepl(ls$lt[1], inp[inpLengths == 6])), all(grepl(ls$lt[2], inp[inpLengths == 6])))]
  ls$m  <- ls$m[ls$m != ls$lt]
  
  # lb and b can be found by checking which charn6 is missing one of the letters lb and b
  ls$b  <- ls$lb[c(all(grepl(ls$lb[1], inp[inpLengths == 6])), all(grepl(ls$lb[2], inp[inpLengths == 6])))]
  ls$lb <- ls$lb[ls$lb != ls$b]
  
  # rt and rb can be found by checking which charn6 is missing one of the letters rt and rb
  ls$rb <- ls$rb[c(all(grepl(ls$rb[1], inp[inpLengths == 6])), all(grepl(ls$rb[2], inp[inpLengths == 6])))]
  ls$rt <- ls$rt[ls$rt != ls$rb]
  
  digits       <- list()
  digits$zero  <- c(ls$t, ls$rt, ls$rb, ls$b, ls$lb, ls$lt)
  digits$one   <- c(ls$rt, ls$rb)
  digits$two   <- c(ls$t, ls$rt, ls$m, ls$lb, ls$b)
  digits$three <- c(ls$t, ls$rt, ls$m, ls$rb, ls$b)
  digits$four  <- c(ls$lt, ls$m, ls$rt, ls$rb)
  digits$five  <- c(ls$t, ls$lt, ls$m, ls$rb, ls$b)
  digits$six   <- c(ls$t, ls$lt, ls$m, ls$lb, ls$rb, ls$b)
  digits$seven <- c(ls$t, ls$rt, ls$rb)
  digits$eight <- unname(unlist(ls))
  digits$nine  <- c(ls$t, ls$rt, ls$lt, ls$m, ls$rb, ls$b)
  
  # Check output code
  code <- sapply(out, function(y){
    outChars  <- unlist(strsplit(y, ""))
    digitChar <- which(sapply(digits, function(x){all(x %in% outChars) & all(outChars %in% x)}) == TRUE)
    digit     <- digitChar - 1
    return(digit)
  })
  codes <- c(codes, as.numeric(paste0(code, collapse = "")))
}
output <- sum(codes)
cat("Day 8 Star 2:", output)
