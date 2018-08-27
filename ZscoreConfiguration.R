library(igraph)

input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net",format = "ncol")
loops <- FALSE

ddist <- table(sort(as.vector(degree(input,mode = "total")),decreasing = FALSE))

empty <- make_empty_graph(n = vcount(input),directed = FALSE)

nodes <- seq(vcount(input))

for(a in ddist){
  val <- sample(nodes,1)
  nodes <- nodes[!nodes %in% val]
  
  edge <- sample(nodes,a)
  
  for(b in edge){
    edge_list <- c(edge_list,val,b)
  }
}

empty <- empty %>% add_edges(edge_list)
