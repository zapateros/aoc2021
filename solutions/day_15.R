library(igraph)
input <- readLines("input/input_day_15.txt")
dim <- length(input)
mat <- matrix(as.numeric(unlist(strsplit(input, ""))), byrow = TRUE, nrow = dim)

mat.pad = rbind(NA, cbind(NA, matrix(c(1:(dim * dim)), ncol = dim), NA), NA)
ind = 2:(dim + 1) # row/column indices of the "middle"
neighbours = cbind(N  = as.vector(mat.pad[ind - 1, ind    ]),
                   E  = as.vector(mat.pad[ind    , ind + 1]),
                   S  = as.vector(mat.pad[ind + 1, ind    ]),
                   W  = as.vector(mat.pad[ind    , ind - 1]))
nb_list <- apply(neighbours, 1, function(x){x[!is.na(x)]})
adj <- NULL
for(i in 1:length(nb_list)){
  adj <- rbind(adj, cbind(i, nb_list[[i]], mat[nb_list[[i]]]))
}
g <- graph_from_edgelist(adj[,1:2], directed = TRUE)
E(g)$weights <- adj[,3]

end <- dim*dim
sp <- unlist(shortest_paths(g, from = 1, to = end, weights = E(g)$weights)$vpath)
count <- 0
for(j in 1:(length(sp) - 1)){
  x <- sp[j]
  y <- sp[j + 1]
  count <- count + adj[adj[,1] == x & adj[,2] == y, 3]
}
output <- count
cat("Day 15 Star 1:", output)





input <- readLines("input/input_day_15.txt")
dim <- length(input)
mat <- matrix(as.numeric(unlist(strsplit(input, ""))), byrow = TRUE, nrow = dim)

times <- 5
newMat <- do.call(cbind, replicate(times, do.call(rbind, replicate(times, mat, simplify=FALSE)), simplify=FALSE))



addVert <- do.call(cbind, replicate(500, rep(0:4, each = 100), simplify=FALSE))
addHori <- do.call(rbind, replicate(500, rep(0:4, each = 100), simplify=FALSE))
addBoth <- addVert + addHori

newMatAdd <- (newMat + addBoth - 1)%%9 + 1



dim <- 500
mat.pad = rbind(NA, cbind(NA, matrix(c(1:(dim * dim)), ncol = dim), NA), NA)
ind = 2:(dim + 1) # row/column indices of the "middle"
neighbours = cbind(N  = as.vector(mat.pad[ind - 1, ind    ]),
                   E  = as.vector(mat.pad[ind    , ind + 1]),
                   S  = as.vector(mat.pad[ind + 1, ind    ]),
                   W  = as.vector(mat.pad[ind    , ind - 1]))
nb_list <- apply(neighbours, 1, function(x){x[!is.na(x)]})
adj <- NULL
for(i in 1:length(nb_list)){
  adj <- rbind(adj, cbind(i, nb_list[[i]], newMatAdd[nb_list[[i]]]))
}
g <- graph_from_edgelist(adj[,1:2], directed = TRUE)
E(g)$weights <- adj[,3]

end <- dim*dim
sp <- unlist(shortest_paths(g, from = 1, to = end, weights = E(g)$weights)$vpath)
count <- 0
for(j in 1:(length(sp) - 1)){
  x <- sp[j]
  y <- sp[j + 1]
  count <- count + adj[adj[,1] == x & adj[,2] == y, 3]
  
}
output <- count
cat("Day 15 Star 2:", output)
