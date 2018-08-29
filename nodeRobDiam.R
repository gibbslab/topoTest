library(igraph)

input <- read.graph("~/Documentos/Coquito/MM_coquitoCyto.txt","ncol")

hubs <- names(sort(degree(input),decreasing = TRUE))
clustering <- data.frame(transitivity(input,type = "local",isolates = "zero"),names(V(input)))
names(clustering) <- c("value","ID")
clustering <- clustering[sort(clust)]
betweenness <- names(sort(betweenness(input,directed = FALSE),decreasing = TRUE))
random <- sample(hubs,length(hubs))

clustering[sort(clustering$value,decreasing = TRUE),]