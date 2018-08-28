library(igraph)

input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net",format = "ncol")
ddist <- as.data.frame.numeric(table(sort(as.vector(degree(input,mode = "total")),decreasing = FALSE)))

new_collector <- vector()

test <- "diameter"

for(run in seq(1000)){
  
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
  
  while(TRUE){
    last <- which(as.vector(sort(degree(input))) != sort(degree(empty)))
    id_last <- sort(degree(empty))[last]
    final_vector <- unique(na.omit(match(id_last,degree(empty))))
    
    if(length(final_vector)==0 | ecount(empty) > vcount(input)){break}
    
    if(length(final_vector)%%2==0){
      empty <- empty %>% add_edges(final_vector[1:length(final_vector)])
    }else if(length(final_vector) == 1){
      empty <- empty %>% add_edges(c(final_vector,final_vector))
    }else{
      empty <- empty %>% add_edges(final_vector[1:length(final_vector)-1]) 
    }
  }
  if(test=="clustering"){
    new_new_collector <- c(new_collector,transitivity(empty))
  }else if(test=="diameter"){
    new_collector <- c(new_collector,diameter(empty))
  }else{
    stop("Please use a valid option")
  }
  
}

if(test=="clustering"){
  Zscore <- (transitivity(input) - mean(new_collector))/sd(new_collector)
}else{
  Zscore <- (diameter(input) - mean(new_collector))/sd(new_collector)
}

