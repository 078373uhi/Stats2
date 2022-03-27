library(ggraph)
library(igraph)

# load the data
starwars_edgelist <- read.csv("starwars_edgelist.csv")
starwars_vertices <- read.csv("starwars_vertices.csv")

# create undirected graph object
starwars_graph <- graph_from_data_frame(
  d = starwars_edgelist,
  vertices = starwars_vertices,
  directed = FALSE
)

starwars_graph

sort(degree(starwars_graph))

sort(degree(starwars_graph, mode="in"))

sort(degree(starwars_graph, mode="out"))

sort(degree(starwars_graph, mode="total"))

sort(strength(starwars_graph))

sort(closeness(starwars_graph, normalized=TRUE))

sort(betweenness(starwars_graph))

sort(eigen_centrality(starwars_graph)$vector)

sort(page_rank(starwars_graph)$vector)

sort(authority_score(starwars_graph)$vector)

neighbors(starwars_graph, 
          v=which(V(starwars_graph)$name=="DARTH VADER"))

ego(starwars_graph, order=2, 
    nodes=which(V(starwars_graph)$name=="DARTH VADER"))

library(tidygraph) # make tidy graphs from igraph
library(dplyr)

starwars_tdy <- as_tbl_graph(starwars_graph)

starwars_tdy %>% 
  activate(nodes) %>%
  mutate(pagerank = centrality_pagerank()) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness()) %>%
  ggraph() +
  geom_edge_link(aes(alpha = betweenness)) +
  geom_node_point(aes(size = pagerank, colour = pagerank)) + 
  geom_node_text(aes(label = name), repel=TRUE) +
  # discrete colour legend
  scale_color_gradient(guide = 'legend')

# community level
components(starwars_graph)

set.seed(123)
swcomp <- components(starwars_graph)

ggraph(starwars_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue",size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() 

giant <- decompose(starwars_graph)[[1]]
components(giant)

cluster_walktrap(giant)

cluster_walktrap(giant, steps=10)

giant

coreness(starwars_graph)

which(coreness(starwars_graph)==6) # what is the core of the network?

which(coreness(starwars_graph)==1) # what is the periphery of the network?

g <- make_ring(10)
g <- add_edges(g, c(1,2, 2,3, 1,3))
coreness(g)

# community detection - all nodes

starwars_tdy %>% 
  activate(nodes) %>%
  mutate(community = as.factor(group_louvain())) %>% # cluster louvain algorithm
  ggraph() + 
  geom_edge_link() + 
  geom_node_point(aes(colour = community), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) 

# network level
diameter(starwars_graph, directed=FALSE, weights=NA)

get_diameter(starwars_graph, directed=FALSE, weights=NA)

mean_distance(starwars_graph, directed=FALSE)

dist <- distances(starwars_graph, weights=NA)

dist[1:5,1:5] # select the first five

edge_density(starwars_graph)

# 22*21 possible edges / 2 because it's undirected = 231 possible edges
# but only 60 exist
60/((22*21)/2)

reciprocity(starwars_graph)

transitivity(starwars_graph)

# random networks
# Erdős-Rényi Model
n = 100   # n = total node count.
c = 3     # c = average degree.
p = c/n   # p = connection probability

adj = matrix(0, n, n)     # We create an adjacency matrix
# We start with no connections

for(.i in 2:n){
  for(.j in 1:(.i-1)){    # For each pair of vertices (.i, .j):
    adj[.i, .j] = rbinom(1,1, p) # we add the edge with probability p
    
  }
}

adj_mat <- adj + t(adj) # t() calculate the transpose

(ErdosRenyi_graph = graph_from_adjacency_matrix(adj_mat))

set.seed(123)
ggraph(ErdosRenyi_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  labs(title = paste0("Erdos-Renyi Random Graph ",   
                      "(n = ", n, 
                      ", p = ", p, ")"))





n = 10   # n = total node count.
p = 0.4   # p = connection probability

adj1 = matrix(0, n, n)     # 3 matrices with same parameters
adj2 = matrix(0, n, n) 
adj3 = matrix(0, n, n) 

for(.i in 2:n){
  for(.j in 1:(.i-1)){    
    adj1[.i, .j] = rbinom(1,1, p) 
    adj2[.i, .j] = rbinom(1,1, p)
    adj3[.i, .j] = rbinom(1,1, p)
  }
}

adj_mat1 <- adj1 + t(adj1) 
adj_mat2 <- adj2 + t(adj2) 
adj_mat3 <- adj3 + t(adj3) 

# Make graph objects
er_graph1 = graph_from_adjacency_matrix(adj_mat1)
er_graph2 = graph_from_adjacency_matrix(adj_mat2)
er_graph3 = graph_from_adjacency_matrix(adj_mat3)

# Plot
library(patchwork)

set.seed(123)
g1 <- ggraph(er_graph1, layout = "circle") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  theme_void()

g2 <- ggraph(er_graph2, layout = "circle") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  theme_void()

g3 <- ggraph(er_graph3, layout = "circle") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(colour = "lightblue", size = 5, 
                  show.legend = FALSE) +
  theme_void()


g1 +      labs(title = paste0("Erdos-Renyi Random Graph ",   
                              "(n = ", n, 
                              ", p = ", p, ")")) +
  g2 + g3 
