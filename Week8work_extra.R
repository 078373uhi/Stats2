library(ggraph)
library(igraph)
library(igraphdata) # for datasets (not sure if we'll use it!)

# get karate data
karate_edgelist <- karate <- read.csv("karate.csv")


head(karate)

(karate <- graph_from_data_frame(karate_edgelist, 
                                 directed = FALSE))

#igraph

# default plot with igraph

# set seed for reproducibility
set.seed(123)

# create random layout (there are other layouts you can try)
l <- layout_randomly(karate)

# plot with random layout
plot(karate, layout = l)

# only store a label if Mr Hi or John A
V(karate)$label <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),  
                          V(karate)$name,
                          "")

# change label font color, size and font family 
# (selected font family needs to be installed on system)
V(karate)$label.color <- "black"
V(karate)$label.cex <- 0.8
# V(karate)$label.family <- "arial"

plot(karate, layout = l)

# different colors and shapes for Mr Hi and and John A
V(karate)$color <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "lightblue", 
                          "pink")

V(karate)$shape <- ifelse(V(karate)$name %in% c("Mr Hi", "John A"),
                          "square", 
                          "circle")


plot(karate, layout = l)

# change color and linetype of all edges
E(karate)$color <- "blue"
E(karate)$lty <- "dashed"

plot(karate, layout = l)

# circle layout
set.seed(123)
circ <- layout_in_circle(karate)
plot(karate, layout = circ)

# sphere layout
set.seed(123)
sph <- layout_on_sphere(karate)
plot(karate, layout = sph)

# F-R algorithm
set.seed(123)
fr <- layout_with_fr(karate)
plot(karate, layout = fr)

# K-K algorithm
set.seed(123)
kk <- layout_with_kk(karate)
plot(karate, layout = kk)

#ggraph
# set seed for reproducibility
set.seed(123)

# visualise using ggraph with fr layout
ggraph(karate, layout = "fr") +
  geom_edge_link() +
  geom_node_point() 

# added colour, remove grey background, add title

set.seed(123)
ggraph(karate, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(color = "blue", size = 5) +
  labs(title = "Zachary's Karate Club Network")

# colour nodes according to 'leader' or not.
V(karate)$leader <- ifelse(
  V(karate)$name %in% c("Mr Hi", "John A"), 1, 0
)

set.seed(123)
ggraph(karate, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(color = as.factor(leader)), size = 5, 
                  show.legend = FALSE)  + 
  labs(title = "Zachary's Karate Club Network")

# get the data

work_edgelist <- read.csv("workfrance_edgelist.csv")
work_vertices <- read.csv("workfrance_vertices.csv")


head(work_edgelist)

head(work_vertices)

# create undirected graph object
work_graph <- graph_from_data_frame(
  d = work_edgelist,
  vertices = work_vertices,
  directed = FALSE
)

work_graph

set.seed(123)
ggraph(work_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(color = "blue", size = 5)

set.seed(123)
ggraph(work_graph, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7, aes(width = mins), 
                 show.legend = FALSE) +
  geom_node_point(aes(color = dept), size = 5) +
  labs(color = "Department") +
  labs(title = "Spatial co-location of employees in a workplace")

# get data
londontube_vertices <- read.csv("londontube_vertices.csv")
londontube_edgelist <- read.csv("londontube_edgelist.csv")

head(londontube_vertices)
head(londontube_edgelist)

library(dplyr)
# create a set of distinct line names and linecolors to use
lines <- londontube_edgelist %>% 
  distinct(line, linecolor)

# create graph object
tubegraph <- graph_from_data_frame(
  d = londontube_edgelist, 
  vertices = londontube_vertices,
  directed = FALSE
)

tubegraph

# visualize tube graph using linecolors for edge color
set.seed(123)
ggraph(tubegraph) + 
  geom_node_point(color = "black", size = 1) +
  geom_edge_link(aes(color = line), width = 1) +
  scale_edge_color_manual(name = "Line",
                          values = lines$linecolor) 

# reorganize to include longitude and latitude for start and end
new_edgelist <- londontube_edgelist %>%
  inner_join(londontube_vertices %>% 
               select(id, latitude, longitude), 
             by = c("from" = "id")) %>% 
  rename(lat_from = latitude, lon_from = longitude) %>% 
  inner_join(londontube_vertices %>% 
               select(id, latitude, longitude), 
             by = c("to" = "id")) %>% 
  rename(lat_to = latitude, lon_to = longitude)

# view
head(new_edgelist)

# recreate graph object to capture additional edge data
tubegraph <- graph_from_data_frame(
  d = new_edgelist, 
  vertices = londontube_vertices,
  directed = FALSE
)

# visualize tube graph using linecolors for edge color
set.seed(123)
ggraph(tubegraph) + 
  geom_node_point(aes(x = longitude, y = latitude), 
                  color = "black", size = 1) +
  geom_edge_link(aes(x = lon_from, y = lat_from,
                     xend = lon_to, yend = lat_to,
                     color = line), width = 1) +
  scale_edge_color_manual(name = "Line",
                          values = lines$linecolor)+
  theme_void()

#visNetwork
library(visNetwork)

nodes <- data.frame(
  id = 1:4,
  label = c("David", "Jack", "Mina", "Jane")
)

edges <- data.frame(
  from = c(1, 1, 1, 4, 4),
  to = c(2, 3, 4, 2, 3)
)

visNetwork(nodes, edges) %>%  # No need to create iGraph object
  visLayout(randomSeed = 123)

# colour nodes according to 'leader' or not.
V(karate)$leader <- ifelse(
  V(karate)$name %in% c("Mr Hi", "John A"), 1, 0
)

set.seed(123)
ggraph(karate, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(color = as.factor(leader)), size = 5, 
                  show.legend = FALSE) + 
  labs(title = "Zachary's Karate Club Network")

