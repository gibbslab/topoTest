library(igraph)

input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net",format = "ncol")
ddist <- sort(as.vector(degree(input,mode = "total")),decreasing = TRUE)

empty <- make_empty_graph(n = vcount(input),directed = FALSE)

nodes <- seq(vcount(input))
