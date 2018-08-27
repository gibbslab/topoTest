library(igraph)

input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net",format = "ncol")
loops <- TRUE

ddist <- sort(as.vector(degree(input,mode = "total")),decreasing = TRUE)

empty <- make_empty_graph(n = vcount(input),directed = FALSE)

nodes <- seq(vcount(input))
val <- sample(nodes,1)

for(a in seq(length(ddist))){
  for(i in seq(ddist[a])){
    edge <- sample(val,1)
    if(loops  == TRUE){
      while(edge == val){
        edge <- sample(val,1)
      }
    }
    empty <- empty + edge(val,edge)
  }
}
