library(igraph)

input <- read.graph("~/Documentos/Coquito/MM_coquitoCyto.txt","ncol")

hubs <- names(sort(degree(input),decreasing = TRUE))
clustering <- data.frame(transitivity(input,type = "local",isolates = "zero"),names(V(input)))
