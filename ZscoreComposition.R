library(igraph)

input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net",format = "ncol")
loops <- FALSE

ddist <- sort(as.vector(degree(input,mode = "total")),decreasing = TRUE)

empty <- make_empty_graph(n = vcount(input),directed = FALSE)

nodes_in <- nodes_out <- seq(vcount(input))
edge_list <- NULL

for(a in ddist){
  val <- sample(nodes_in,1)
  nodes_in <- nodes_in[!nodes_in %in% val]
  
  edge <- sample(nodes_out,a)
  
  for(b in edge){
    edge_list <- c(edge_list,val,b)
    if(empty[b,val] == 1){
    b <- sample(nodes_out,1)
    }
  }
  add_edges(empty,edge_list)
}
