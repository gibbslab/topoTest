library(igraph)

input <- read.graph("~/Documentos/Coquito/MM_coquitoCyto.txt","ncol")
input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net","ncol")

fraction <- 0.05

hubs <- names(sort(degree(input),decreasing = TRUE))
random <- sample(hubs,length(hubs))

input.hubs <- input.random <- input
before_array <- 0

for(i in seq(0.005,fraction,0.005)){
  to_delete <- round(vcount(input)*i)
  print(to_delete)
  input.hubs <- input.hubs - vertices(as.vector(na.omit(hubs[before_array+1:to_delete])))
  
  
  diam <- diameter(input.robus)
  
  before_array <- to_delete
}

as.vector(na.omit(hubs[before_array+1:to_delete]))

clustering <- data.frame(transitivity(input,type = "local",isolates = "zero"),names(V(input)))
names(clustering) <- c("value","ID")
clustering <- clustering[with(clustering,order(clustering$value,decreasing = TRUE)),]$ID
betweenness <- names(sort(betweenness(input,directed = FALSE),decreasing = TRUE))







seq(0,fraction,0.005)