library(igraph)

input <- read.graph("~/Documentos/Data3.0/PD/ParkinsonN.net",format = "ncol")

ddist <- as.data.frame.numeric(table(sort(as.vector(degree(input,mode = "total")),decreasing = FALSE)))

empty <- make_empty_graph(n = vcount(input),directed = FALSE)

nodes <- seq(vcount(input))

for(i in seq(dim(ddist)[1])){
  alfa <- as.numeric(names(ddist[i,])) - max(degree(empty))
  for(j in seq(alfa)){
    resample <- sample(nodes,length(nodes))
    if(length(resample)%%2!=0){resample<-resample[1:length(resample)-1]}
    empty <- empty %>% add_edges(resample)
  }
  
  del <- sample(nodes,ddist[i,])
  nodes <- nodes[!nodes %in% del]
}

control <- TRUE

while(control){
  last <- which(as.vector(sort(degree(input))) != sort(degree(empty)))
  id_last <- sort(degree(empty))[last]
  final_vector <- unique(na.omit(match(id_last,degree(empty))))
  
  if(length(final_vector)%%2==0){
    empty <- empty %>% add_edges(final_vector[1:length(final_vector)])
  }else if(length(final_vector) == 1){
    empty <- empty %>% add_edges(c(final_vector,final_vector))
  }else{
    empty <- empty %>% add_edges(final_vector[1:length(final_vector)-1]) 
  }
  if(length(final_vector)==0){control <- FALSE}
}




as.vector(sort(degree(input))) != sort(degree(empty))
sort(degree(empty))
which(degree(empty) %in% id_last)
