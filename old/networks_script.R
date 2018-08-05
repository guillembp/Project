library(igraph)
library(dplyr)
getwd()
setwd('~')
edge_list <- read.csv2('Desktop/networks_data/email-Eu-core.txt', sep = ' ')
communities <- read.csv2('Desktop/networks_data/email-Eu-core-department-labels.txt', sep = ' ')
colnames(communities) <- c('Node','Community')

# Create a strongly connected graph using depth first search
graphed_network <- graph_from_data_frame(d = edge_list, directed = FALSE)
G <- dfs(graph = graphed_network, root=159, neimode = 'all', unreachable = FALSE, order = TRUE, order.out = FALSE, father = FALSE, dist = FALSE, in.callback = NULL, out.callback = NULL, extra = NULL, rho = parent.frame())

# Generate adjacency matrix from the edge list
A <- as_adjacency_matrix(G, type = 'both', sparse=FALSE)

Nodes <- 40
MaxEdgesPerNode <- 50

# Ravasz algorithm for community clustering, dendrogram plotting
X <- matrix(0, nrow = Nodes, ncol = Nodes)
# network_edges <- data.frame(X0=integer(), X1, integer())

for(i in 1:Nodes){
  print(paste0("Row: ",i))
  for(j in 1:Nodes){
    if(i!=j){
      print(j)
      di <- 0
      dj <- 0
      J <- 0
      for(k in 1:Nodes){
        print(k)
        J <- J+A[i,k]*A[j,k]
        di <- di+A[i,k] 
        dj <- dj+A[j,k]
      }
      if(J>0){
        X[i,j] <- J/(min(di,dj))
        print('positive')
      } else {
        X[i,j] <- J/(min(di,dj)+1)
        print('negative')
      }
    }
  }
}

Adj <- A[1:40,1:40]

Adj <- Adj*upper.tri(Adj, diag = FALSE)

g <- graph.adjacency(Adj)
g <- simplify(graph=g, remove.multiple = TRUE, remove.loops = TRUE)
e <- get.edgelist(g)

# prepare hierarchical cluster
hc = hclust(dist(X))
# dendrogram with labels at the same level
plot(hc, hang = -1)

pg <- graph_from_data_frame(d = e, directed = FALSE)
# plot function with edge.label added
plot(pg)

# Remove 1/10th of the edges at random
smp_size <- floor(0.1 * nrow(edgelist_short))

