library(igraph)

input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net",format = "ncol")
loops <- FALSE

ddist <- as.data.frame.numeric(table(sort(as.vector(degree(input,mode = "total")),decreasing = FALSE)))

empty <- make_empty_graph(n = vcount(input),directed = FALSE)

nodes <- seq(vcount(input))

for(i in seq(dim(ddist)[1])){
  alfa <- as.numeric(names(ddist[i,])) - max(degree(empty))
  for(j in seq(alfa)){
    if(length(nodes)%%2==0){
      resample <- sample(nodes,length(nodes))
    }else{
      resample <- sample(nodes,length(nodes)-1)
    }
    empty <- empty %>% add_edges(resample)
  }
  
  del <- sample(nodes,ddist[i,])
  nodes <- nodes[!nodes %in% del]
}



last <- which(as.vector(sort(degree(input))) != sort(degree(empty)))

empty <- empty %>% add_edges(last)





as.vector(sort(degree(input))) != sort(degree(empty))
sort(degree(empty))
