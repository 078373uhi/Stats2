# load required packages
library(ggraph)
library(igraph)
library(tidygraph)
library(dplyr)
library(patchwork)
library(glue)


# •	Simulate networks using two different models: 
# 1.	Erdös-Renyi graph G(n,p)

n = 20   # number of nodes
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
dim <- 1 # dimension of grid
nei <- 4 # number of neighbors in grid
p <- 0.3 # probability

# creating an example of model
(WS_graph <- sample_smallworld(dim, n, nei, p))

#create plot of network
plot(WS_graph, 
     layout = layout_in_circle, 
     vertex.size = 2, 
     vertex.label = NA, 
     edge.lty = 3,
     main = glue('Watts-Strogatz on ', n, ' Nodes and Probability ', p),
     sub = glue('Starting with a ', dim , 
                '-Dimensional Lattice \n Where Each Node Has ', nei, 
                ' Neighbors Clockwise'))

# •	Use the network statistics to compare the two models, and answer the following questions:
# 1.	What is the effect of choosing different parameters in the models?


# Erdös-Renyi
# different number of nodes
# model with 10 nodes
n = 10   # number of nodes
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

set.seed(6) # set seed for reproducing
# create plot of network
n10 <- ggraph(ErdosRenyi_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  labs(title = paste0("Erdos-Renyi Graphs ",   
                      "(n = ", n, 
                      ", p = ", p, ")"))

# model with 20 nodes
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

set.seed(7) # set seed for reproducing
# create plot of network
n50 <- ggraph(ErdosRenyi_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  labs(title = paste0("(n = ", n, 
                      ", p = ", p, ")"))

# model with 100 nodes
n = 100   # number of nodes
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

set.seed(8) # set seed for reproducing
# create plot of network
n100 <- ggraph(ErdosRenyi_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  labs(title = paste0("(n = ", n, 
                      ", p = ", p, ")"))

# display plots
n10 + n50 + n100


# different average degree/probability
# model with average 1 degree
n = 20   # number of nodes
c = 1     # average degree
p = c/n   #connection probability

adj = matrix(0, n, n)     # create adjacency matrix

for(.i in 2:n){
  for(.j in 1:(.i-1)){    
    adj[.i, .j] = rbinom(1,1, p) 
    
  }
}

adj_mat <- adj + t(adj) # calculate the transpose

(ErdosRenyi_graph = graph_from_adjacency_matrix(adj_mat))

set.seed(9) # set seed for reproducing
# create plot of network
c1 <- ggraph(ErdosRenyi_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  labs(title = paste0("Erdos-Renyi Graphs ",   
                      "(n = ", n, 
                      ", p = ", p, ")"))

# model with average 5 degrees
n = 20   # number of nodes
c = 5     # average degree
p = c/n   #connection probability

adj = matrix(0, n, n)     # create adjacency matrix

for(.i in 2:n){
  for(.j in 1:(.i-1)){    
    adj[.i, .j] = rbinom(1,1, p) 
    
  }
}

adj_mat <- adj + t(adj) # calculate the transpose

(ErdosRenyi_graph = graph_from_adjacency_matrix(adj_mat))

set.seed(10) # set seed for reproducing
# create plot of network
c5 <- ggraph(ErdosRenyi_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  labs(title = paste0("(n = ", n, 
                      ", p = ", p, ")"))

# model with average 10 degrees
n = 20   # number of nodes
c = 10     # average degree
p = c/n   #connection probability

adj = matrix(0, n, n)     # create adjacency matrix

for(.i in 2:n){
  for(.j in 1:(.i-1)){    
    adj[.i, .j] = rbinom(1,1, p) 
    
  }
}

adj_mat <- adj + t(adj) # calculate the transpose

(ErdosRenyi_graph = graph_from_adjacency_matrix(adj_mat))

set.seed(11) # set seed for reproducing
# create plot of network
c10 <- ggraph(ErdosRenyi_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  labs(title = paste0("(n = ", n, 
                      ", p = ", p, ")"))

# display plots
c1 + c5 + c10

# Small world
# different probablities
set.seed(3)
n <- 20 # order of the graph
dim <- 1 # dimension of original grid
nei <- 4 # number of neighbors in original grid

for (p in c(0, 0.2, 0.5, 0.6, 0.9, 1)) {
  # creating model
  sw_graph <- sample_smallworld(dim, n, nei, p)
  
  plot(sw_graph, 
       layout = layout_in_circle, 
       vertex.size = 2, 
       vertex.label = NA, 
       edge.lty = 3,
       main = glue('Watts-Strogatz on ', n, ' Nodes and Probability ', p),
       sub = glue('Starting with a ', dim , 
                  '-Dimensional Lattice \n Where Each Node Has ', nei, 
                  ' Neighbors Clockwise Originally'))
  
  writeLines('\n')
  
}

# different neighbours
set.seed(4)
n <- 20 # order of the graph
dim <- 1 # dimension of original grid
p <- 0.5 # probability

for (nei in c(0, 2, 5, 10)) {
  # creating model
  sw_graph <- sample_smallworld(dim, n, nei, p)
  
  plot(sw_graph, 
       layout = layout_in_circle, 
       vertex.size = 2, 
       vertex.label = NA, 
       edge.lty = 3,
       main = glue('Watts-Strogatz on ', n, ' Nodes and Probability ', p),
       sub = glue('Starting with a ', dim , 
                  '-Dimensional Lattice \n Where Each Node Has ', nei, 
                  ' Neighbors Clockwise Originally'))
  
  writeLines('\n')
  
}

# different number nodes
set.seed(5)
nei <- 3 # order of the graph
dim <- 1 # dimension of original grid
p <- 0.5 # probability

for (n in c(2, 5, 10, 15, 20)) {
  # creating model
  sw_graph <- sample_smallworld(dim, n, nei, p)
  
  plot(sw_graph, 
       layout = layout_in_circle, 
       vertex.size = 2, 
       vertex.label = NA, 
       edge.lty = 3,
       main = glue('Watts-Strogatz on ', n, ' Nodes and Probability ', p),
       sub = glue('Starting with a ', dim , 
                  '-Dimensional Lattice \n Where Each Node Has ', nei, 
                  ' Neighbors Clockwise Originally'))
  
  writeLines('\n')
  
}

# 2.	How do the node degree distributions differ in the different models?
# this shows the indegree (incoming edges) of each model
sort(degree(ErdosRenyi_graph, mode="in"))
sort(degree(WS_graph, mode="in"))

# this shows the outdegree (outgoing edges) of each model
sort(degree(ErdosRenyi_graph, mode="out"))
sort(degree(WS_graph, mode="out"))

# this shows the total degree of each model. It can be seen that the Erdös-Renyi  
# model has more than the Watts-Strogatz.  
sort(degree(ErdosRenyi_graph, mode="total"))
sort(degree(WS_graph, mode="total"))

# this shows the closeness, or how many steps are needed to access all other nodes
# from any particular node. The values are similar for each model though the 
# values for the Erdös-Renyi model are slightly higher.  Higher values mean nodes
# are more spread out.
sort(closeness(ErdosRenyi_graph, normalized=TRUE))
sort(closeness(WS_graph, normalized=TRUE))

# this shows betweenness or the number of shortest paths between nodes.  The 
# Watts-Strogatz model shows shorter paths.
sort(betweenness(ErdosRenyi_graph))
sort(betweenness(WS_graph))

# plots of centrality
ErdosRenyi_tdy <- as_tbl_graph(ErdosRenyi_graph)
ER_plot <- ErdosRenyi_tdy %>% 
  activate(nodes) %>%
  mutate(pagerank = centrality_pagerank()) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness()) %>%
  ggraph() +
  geom_edge_link(aes(alpha = betweenness)) +
  geom_node_point(aes(size = pagerank, colour = pagerank)) +
  # discrete colour legend
  scale_color_gradient(guide = 'legend') +
  labs(title = "Erdos-Renyi centrality plot")

WS_tdy <- as_tbl_graph(WS_graph)
WS_plot <- WS_tdy %>% 
  activate(nodes) %>%
  mutate(pagerank = centrality_pagerank()) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness()) %>%
  ggraph() +
  geom_edge_link(aes(alpha = betweenness)) +
  geom_node_point(aes(size = pagerank, colour = pagerank)) +
  # discrete colour legend
  scale_color_gradient(guide = 'legend') +
  labs(title = "Watts-Strogatz centrality plot")

# compare centrality plots
ER_plot + WS_plot

# 3.	How does the clustering coefficient vary across the models?
  