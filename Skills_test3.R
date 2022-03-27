# load required packages
library(ggraph)
library(igraph)
library(tidygraph)
library(dplyr)
library(patchwork)
library(glue)


# •	Simulate networks using two different models: 
# 1.	Erdös-Renyi graph G(n,p)

n = 50   # number of nodes
c = 2.5     # average degree
p = c/n   #connection probability

adj = matrix(0, n, n)     # create adjacency matrix

for(.i in 2:n){
  for(.j in 1:(.i-1)){    
    adj[.i, .j] = rbinom(1,1, p) 
    
  }
}

adj_mat <- adj + t(adj) # calculate the transpose

(ErdosRenyi_graph = graph_from_adjacency_matrix(adj_mat))

set.seed(1) # set seed for reproducing
# create plot of network
ER_plot <- ggraph(ErdosRenyi_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  labs(title = paste0("Erdos-Renyi Random Graph ",   
                      "(n = ", n, 
                      ", p = ", p, ")"))

ER_plot

# 2.	Watts-Strogatz graph (small world)
set.seed(2) #set seed for reproducing

n <- 20 # order of the graph
dim <- 1 # dimension of original grid
nei <- 4 # number of neighbors in original grid
p <- 0.3 # probability

# creating an example of model
(WS_graph <- sample_smallworld(dim, n, nei, p))

#create plot of netword
WS_plot <- plot(WS_graph, 
     layout = layout_in_circle, 
     vertex.size = 2, 
     vertex.label = NA, 
     edge.lty = 3,
     main = glue('Watts-Strogatz on ', n, ' Nodes and Probability ', p),
     sub = glue('Starting with a ', dim , 
                '-Dimensional Lattice \n Where Each Node Has ', nei, 
                ' Neighbors Clockwise Originally'))

# •	Use the network statistics to compare the two models, and answer the following questions:
# 1.	What is the effect of choosing different parameters in the models?
# 2.	How do the node degree distributions differ in the different models?
# 3.	How does the clustering coefficient vary across the models?
  