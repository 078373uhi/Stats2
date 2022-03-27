# load required packages
library(ggraph)
library(igraph)
library(tidygraph)
library(dplyr)
library(patchwork)


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

set.seed(1)
ER_plot <- ggraph(ErdosRenyi_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  labs(title = paste0("Erdos-Renyi Random Graph ",   
                      "(n = ", n, 
                      ", p = ", p, ")"))

ER_plot

# 2.	Watts-Strogatz graph (small world)
