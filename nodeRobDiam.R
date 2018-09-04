library(igraph)

input <- read.graph("~/Documentos/Coquito/MM_coquitoCyto.txt","ncol")
input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net","ncol")

fraction <- 0.05
by <- 0.005
centrality <- NULL

hubs <- names(sort(degree(input),decreasing = TRUE))
random <- sample(hubs,length(hubs))

input.hubs <- input.random <- input
before_array <- 0

diam.hub <- diam.ranom <- diameter(input)


result <- data.frame(seq(0,fraction,by))
names(result) <- "fr"

if(centrality == "clustering"){
  diam.centrality <- diameter(input)
  input.centrality <- input
  clustering <- data.frame(transitivity(input,type = "local",isolates = "zero"),names(V(input)))
  names(clustering) <- c("value","ID")
  clustering <- clustering[with(clustering,order(clustering$value,decreasing = TRUE)),]$ID
}else if(centrality == "betwenness"){
  diam.centrality <- diameter(input)
  input.centrality <- input
  betweenness <- names(sort(betweenness(input,directed = FALSE),decreasing = TRUE))
}else if(file.exists(centrality)){
  diam.centrality <- diameter(input)
  input.centrality <- input
  file.centrality <- read.table(centrality)
}else if(!is.null(centrality)){
  stop("The selected option is invalid")
}

for(i in seq(by,fraction,by)){
  to_delete <- round(vcount(input)*i)
  
  if(to_delete > before_array){
    input.hubs <- input.hubs - vertices(as.vector(na.omit(hubs[(before_array+1):to_delete])))
    input.random <- input.random - vertices(as.vector(na.omit(random[(before_array+1):to_delete])))
  }
  
  
  
  diam.hub <- c(diam.hub,diameter(input.hubs))
  diam.ranom <- c(diam.ranom,diameter(input.random))
  
  before_array <- to_delete
}

result$f_hub <- diam.hub
result$f_random <- diam.ranom


