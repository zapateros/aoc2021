input <- readLines("input/input_day_14.txt")
split <- which(input == "")
poly  <- input[1:split-1]
polymers <- NULL
for(i in 1:(nchar(poly) - 1)){
  polymers <- c(polymers, substr(poly, i, i + 1))
}
polyTab <- data.frame(table(polymers))
polyTab$polymers <- as.character(polyTab$polymers)
polyTab$Freq <- as.numeric(polyTab$Freq)

pairs <- matrix(unlist(strsplit(input[(split+1):length(input)], " -> ")), byrow = TRUE, ncol = 2)
pairs <- cbind(pairs, paste0(substr(pairs[,1], 1, 1), pairs[,2]))
pairs <- cbind(pairs, paste0(pairs[,2], substr(pairs[,1], 2, 2)))
pairs <- setNames(data.frame(pairs), c("pair", "subsitute", "subpair1", "subpair2"))

lastChar <- substring(poly, nchar(poly))

insertPolymers <- function(nRounds){
  polyTabNew <- polyTab
  for(step in 1:nRounds){
    for(n in 1:nrow(pairs)){
      tryPair <- pairs$pair[n]
      if(tryPair %in% polyTab$polymers){
        amount <- polyTab$Freq[polyTab$polymers == tryPair]
        index  <- which(polyTabNew$polymers == tryPair)
        polyTabNew$Freq[index] <- polyTabNew$Freq[index] - amount
        polyTabNew <- rbind(polyTabNew, data.frame(polymers = pairs$subpair1[n], Freq = amount), 
                            data.frame(polymers = pairs$subpair2[n], Freq = amount))
        polyTabNew <- aggregate(.~polymers, polyTabNew, sum)
        polyTabNew <- polyTabNew[!polyTabNew$Freq == 0,]
      }
    }
    polyTab <- polyTabNew
  }
  polyTab$polymers <- substr(polyTab$polymers, 1, 1)
  aggregated  <- aggregate(.~polymers, polyTab, sum)
  lastCharInd <- which(aggregated$polymers == lastChar)
  aggregated$Freq[lastCharInd] <- aggregated$Freq[lastCharInd] + 1
  
  return(as.character(max(aggregated$Freq) - min(aggregated$Freq)))
}

# Star 1:
output <- insertPolymers(10)
cat("Day 14 Star 1:", output)

# Star 2:
output <- insertPolymers(40)
cat("Day 14 Star 2:", output)
