# el <- matrix( c("foo", "bar", "bar", "foobar"), nc = 2, byrow = TRUE)
# net1=graph_from_edgelist(el)
# plot(net1)
# 
# nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
# links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
# net2 <-graph_from_data_frame(d=links,vertices = nodes,directed = TRUE )
#   
# plot(net2)
# 
# vertices=V(net2)

library('igraph')
n3=data.frame(id=1:4,n=c(rep(1,3),20),label=c("a","b","c","invisible"),shapes=rep('circle',4))
e3=data.frame(from=c(1:3,1:3),to=c(rep(4,3),2:4))
net<-graph_from_data_frame(d = e3, vertices = n3,directed = TRUE)
V(net)$size <- V(net)$n*50
plot(net,vertex.shape='circle',vertex.size=V(net)$size,alpha=0.1)

