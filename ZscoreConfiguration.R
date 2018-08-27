library(igraph)

input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net",format = "ncol")
loops <- FALSE

ddist <- as.data.frame.numeric(table(sort(as.vector(degree(input,mode = "total")),decreasing = FALSE)))

empty <- make_empty_graph(n = vcount(input),directed = FALSE)

nodes <- seq(vcount(input))

sapply(ddist,function(x){
  
})

