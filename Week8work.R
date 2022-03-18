library(ggraph)
library(igraph)

install.packages("igraphdata")
library(igraphdata) # for the data
install.packages("visNetwork")
library(visNetwork) # for the visualization

data("Koenigsberg")

nodes <- data.frame(
  id = V(Koenigsberg)$name, 
  label =  V(Koenigsberg)$name
)

edges = data.frame(
  from = c(rep("Altstadt-Loebenicht", 3), rep("Kneiphof", 3), "Vorstadt-Haberberg"),
  to = c(rep("Kneiphof", 2), rep("Lomse", 2), rep("Vorstadt-Haberberg", 2), "Lomse")
)

visNetwork(nodes, edges) |> 
  visLayout(randomSeed = 1)

#Creating Networks in R
# Edgelists

contacts <- data.frame(
  from = c("Sian","Sian","Danny","Danny","Danny","Mina"),
  to = c("Danny","Peter","Peter","Mina","Hamza","Hamza")
)

contacts

contacts_edgelist <- as.matrix(contacts)

contacts_undir <- graph_from_edgelist(el = contacts_edgelist,
                                      directed = FALSE)

contacts_dir  <- graph_from_edgelist(el = contacts_edgelist,
                                     directed = TRUE)

contacts_undir

contacts_dir

# Adjacency Matrix
# create 3x3 adjacency matrix
flight_data <- matrix(c(0, 5, 2, 4, 0, 0, 4, 1, 0), nrow = 3, ncol = 3)
rownames(flight_data) <- c("EDI", "BHX", "BRS") # Edinburgh, Birmingham, Bristol
colnames(flight_data) <- rownames(flight_data)

flight_data

# create multigraph from adjacency matrix
(flightgraph <- graph_from_adjacency_matrix(
  adjmatrix = flight_data,
  mode = "directed"
))

ggraph(flightgraph, layout = 'kk') +
  geom_edge_fan(arrow = arrow(length = unit(4, 'mm')),
                end_cap = circle(5, 'mm')) +
  geom_node_point(size = 14, color = "pink") +
  geom_node_text(aes(label = name)) +
  theme_void()

# create weighted graph 
(flightgraph_weighted <- graph_from_adjacency_matrix(
  adjmatrix = flight_data,
  mode = "directed",
  weighted = TRUE
))

ggraph(flightgraph_weighted, layout = 'kk') +
  geom_edge_fan(arrow = arrow(length = unit(4, 'mm')),
                end_cap = circle(5, 'mm'),aes(edge_alpha = weight)) +
  geom_node_point(size = 14, color = "pink") +
  geom_node_text(aes(label = name)) +
  labs(legend.position="none") 

# simplified graph
(flightgraph_simple <- simplify(
  flightgraph
))

ggraph(flightgraph_simple, layout = 'kk') +
  geom_edge_fan(arrow = arrow(length = unit(4, 'mm')),
                end_cap = circle(5, 'mm')) +
  geom_node_point(size = 14, color = "pink") +
  geom_node_text(aes(label = name)) +
  labs(legend.position="none") +
  theme_void()

#Dataframe
# edge dataframe
(edge_df <- data.frame(
  from = c("Sian","Sian","Danny","Danny","Hamza","Mina"),
  to = c("Jordan","Peter","Jake","Peter","Jordan","Jake")
))

# vertex dataframe
(vertex_df <- data.frame(
  name = c("Sian","Danny","Hamza","Mina","Jordan","Peter","Jake"),
  type = c(rep(TRUE, 4), rep(FALSE, 3)),
  Dept = c(rep("A",4), rep("B",3))
))

# create graph
(companygraph <- graph_from_data_frame(
  d = edge_df,
  directed = FALSE,
  vertices = vertex_df
))

# set layout to bipartite
lo <- layout_as_bipartite(companygraph)
lo <- lo[,c(2,1)]

ggraph(companygraph, layout = lo) +
  geom_edge_link(color = "blue") +
  geom_node_point(size = 14, aes(color = as.factor(Dept))) +
  geom_node_text(aes(label = name)) +
  labs(color = "") +
  theme_void()

bands <- data.frame(
  from = c(rep("Best\nBoy Bands", 3), rep("Backstreet\nBoys", 5), 
           rep("Boyzone", 5), rep("Take\nThat", 5)),
  to = c("Backstreet\nBoys", "Boyzone", "Take\nThat", 
         "Nick\nCarter", "Brian\nLittrell", "AJ\nMcLean", "Kevin\nRichardson", "Howie\nDorough",
         "Ronan\nKeating", "Shane\nLynch", "Keith\nDuffy", "Mikey\nGraham", "Stephen\nGately",
         "Robbie\nWilliams", "Gary\nBarlow", "Mark\nOwen", "Jason\nOrange", "Howard\nDonald"
  )
) |> 
  as.matrix()
bands <- igraph::graph_from_edgelist(bands)
ggraph(bands, layout = "dendrogram") +
  geom_edge_elbow(color = "blue") +
  geom_node_point(color = "pink", size = 10) +
  geom_node_text(aes(label = name), size = 2.5) +
  theme_void()

#Adding Properties to the vertices and edges

# dataframe of edges and properties 
(edge_transfers <- data.frame(
  from = c("A", "A", "B", "B"),
  to = c("A", "B", "A", "C"),
  cur = c("USD", "USD", "GBP", "GBP"),
  amt = c(150000, 570000, 230000, 175000)
))


# dataframe of vertices and properties 
(vertex_transfers <- data.frame(
  name = c("A", "B", "C"),
  loc = c("USA", "UK", "France")
))

# create graph
(gtransfers <- graph_from_data_frame(
  d = edge_transfers,
  directed = TRUE,
  vertices = vertex_transfers
))

V(gtransfers)

E(gtransfers)

V(gtransfers)$name

E(gtransfers)$amt

# create unweighted graph from routes edgelist
edge_routes <- data.frame(
  from = c("EDI", "EDI","BHX","BHX", "BRS"), # Edinburgh, Birmingham, Bristol
  to = c("BHX", "BRS", "EDI","BRS","EDI")
)

(edge_routes <- as.matrix((edge_routes)))

# add weights as an edge property
flightsgraph <- igraph::graph_from_edgelist(
  el = edge_routes,
  directed = TRUE
)

# add weights as an edge property
E(flightsgraph)$weight <- c(4, 4, 5, 1, 2)

# view flightsgraph
flightsgraph

# edge dataframe
edge_df <- data.frame(
  from = c("Sian","Sian","Danny","Danny","Hamza","Mina"),
  to = c("Jordan","Peter","Jake","Peter","Jordan","Jake")
)

# vertex dataframe
vertex_df <- data.frame(
  name = c("Sian","Danny","Hamza","Mina","Jordan","Peter","Jake"),
  Dept = c(rep("A",4), rep("B",3))
)

# create graph
(companygraph <- graph_from_data_frame(
  d = edge_df,
  directed = FALSE,
  vertices = vertex_df
))

V(companygraph)$type <- V(companygraph)$Dept

companygraph
