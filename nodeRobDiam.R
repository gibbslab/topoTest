library(igraph)

input <- read.graph("~/Documentos/Coquito/MM_coquitoCyto.txt","ncol")
input <- read.graph("~/Documentos/Data3.0/AZ/AlzheimerN.net","ncol")

fraction <- 0.05
by <- 0.005
centrality <- "betweenness"
vector.name <- NULL

hubs <- names(sort(degree(input),decreasing = TRUE))
random <- sample(hubs,length(hubs))

input.hubs <- input.random <- input
before_array <- 0

diam.hub <- diam.random <- diameter(input)


result <- data.frame(seq(0,fraction,by))
names(result) <- "fr"

if(centrality == "clustering"){
  diam.centrality <- diameter(input)
  input.centrality <- input
  clustering <- data.frame(transitivity(input,type = "local",isolates = "zero"),names(V(input)))
  names(clustering) <- c("value","ID")
  data.centrality <- clustering[with(clustering,order(clustering$value,decreasing = TRUE)),]$ID
}else if(centrality == "betweenness"){
  diam.centrality <- diameter(input)
  input.centrality <- input
  data.centrality <- names(sort(betweenness(input,directed = FALSE),decreasing = TRUE))
}else if(is.vector(centrality) & length(centrality) == vcount(input)){
  diam.centrality <- diameter(input)
  input.centrality <- input
  data.centrality <- centrality
}else if(!is.null(centrality)){
  stop("The selected option is invalid")
}

for(i in seq(by,fraction,by)){
  to_delete <- round(vcount(input)*i)
  
  if(to_delete > before_array){
    input.hubs <- input.hubs - vertices(as.vector(na.omit(hubs[(before_array+1):to_delete])))
    input.random <- input.random - vertices(as.vector(na.omit(random[(before_array+1):to_delete])))
  }
  
  if(!is.null(centrality)){
    if(to_delete > before_array){
      input.centrality <- input.centrality - vertices(as.vector(na.omit(data.centrality[(before_array+1):to_delete])))
    }
    diam.centrality <- c(diam.centrality,diameter(input.centrality))
  }
  
  diam.hub <- c(diam.hub,diameter(input.hubs))
  diam.random <- c(diam.random,diameter(input.random))
  
  before_array <- to_delete
}

if(centrality != "clustering" & centrality != "betweenness"){
  centrality <- vector.name
}

result$f_hub <- diam.hub
result$f_random <- diam.random
result[[paste0("f_",centrality)]] <- diam.hub

