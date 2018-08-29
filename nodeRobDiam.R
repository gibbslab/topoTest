library(igraph)

input <- read.graph("~/Documentos/Coquito/MM_coquitoCyto.txt","ncol")

fraction <- 0.05

hubs <- names(sort(degree(input),decreasing = TRUE))
input.hubs <- input

for(i in seq(0.005,fraction,0.005)){
  to_delete <- hubs[1:round(vcount(input)*i)]
  input.hubs <- input.hubs - vertices(to_delete)
}

clustering <- data.frame(transitivity(input,type = "local",isolates = "zero"),names(V(input)))
names(clustering) <- c("value","ID")
clustering <- clustering[with(clustering,order(clustering$value,decreasing = TRUE)),]$ID
betweenness <- names(sort(betweenness(input,directed = FALSE),decreasing = TRUE))
random <- sample(hubs,length(hubs))






seq(0,fraction,0.005)