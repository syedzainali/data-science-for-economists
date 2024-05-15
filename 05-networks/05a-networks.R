
### set wd to source file dir
setwd("~/work/Teaching/data-science-for-economists/05-networks/")

library(tidyverse)
library(igraph)
library(data.table)

set.seed(1234)
edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 1, 4, 2, 1))
node_list <- tibble(id = 1:4)
g <- igraph::graph_from_data_frame(d = edge_list, vertices = node_list)

plot(g, edge.arrow.size = 0.7, vertex.label.cex=3)
igraph::as_adjacency_matrix(g)

# create the same network from the adj. matrix
adjmatrix <- matrix(c(0,1,0,0,
                      1,0,0,1,
                      0,1,0,0,
                      1,0,0,0), nrow = 4, ncol = 4, byrow = TRUE)
g <- graph_from_adjacency_matrix(
  adjmatrix,
  mode = c("directed"),
  weighted = NULL,
  diag = TRUE,
  add.colnames = NULL,
  add.rownames = NA
)
plot(g, edge.arrow.size = 0.7, vertex.label.cex=3)

# try yourself using the figure!

# path, walks, length
igraph::all_simple_paths(g, 3, 1)
igraph::shortest_paths(g, 3, 1)
plot(g)

# type of graphs
full_graph <- make_full_graph(6, directed = FALSE, loops = FALSE)
plot(full_graph, vertex.label.cex=3)
star <- make_star(6, mode = c("mutual"), center = 1)
plot(star, edge.arrow.size = 0.7)

# generate a dataframe to represents all the edges of your bipartite ntw
d <- data.frame(country=c("DEU", "DEU", "FRA", "FRA", "CAN","CAN", "USA"), 
                        trade_agr=c("CETA","EU", "EU", "CETA","CETA","USMCA","USMCA"))
# trasform it in a graph
g <- graph_from_data_frame(d, directed = FALSE)
as_data_frame(g, what="vertices")
# define color and shape mappings to distinguish nodes type
V(g)$label <- V(g)$name
V(g)$type <- 1
V(g)[name %in% d$trade_agr]$type <- 2
col <- c("steelblue", "orange")
shape <- c("circle", "square")

# plot it!
plot(g,
     vertex.color = col[V(g)$type],
     vertex.shape = shape[V(g)$type], vertex.label.cex=3
)

# WEIGHTED NETWORK
# Mock example, assign random weight (1-5) according to certain probability distr.

set.seed(1234)
adjm <- matrix(sample(0:5, 25, replace=TRUE,
                      prob=c(0.9,0.02,0.02,0.02,0.02,0.02)), ncol=5)
g2 <- graph_from_adjacency_matrix(adjm, weighted=TRUE)
plot(g2, vertex.label = V(g)$name, vertex.label.cex=1)
E(g2)$weight

# R read all what comes next the first 2 columns as attributes of edges, in this case they are weight
edgelist <- read.table(text = "
V1 V2 weight
                       A B 1
                       B C 8
                       C D 6
                       D E 9
                       C F 12
                       F G 15",header=T)
g <- graph_from_data_frame(edgelist)
is_weighted(g)



# Exercise: input either through adj matrix or as a dataframe the network as plotted above

#SOLUTIONS TO EXERCISE
# create a network, inspect the adj. matrix and plot
adjmatrix <- matrix(c(0,1,1,0,0,
                      1,0,1,0,0,
                      0,0,0,0,0,
                      0,0,1,0,0,
                      0,0,1,0,0), nrow = 5, ncol = 5, byrow = TRUE)
g <- graph_from_adjacency_matrix(
  adjmatrix,
  mode = c("directed", "undirected", "max", "min", "upper", "lower", "plus"),
  weighted = NULL,
  diag = TRUE,
  add.colnames = NULL,
  add.rownames = NA
)
plot(g, vertex.label.cex=3)

edge_list <- tibble(from = c(5,5, 6, 6, 8, 9), to = c(6, 7, 7, 5, 7, 7))
node_list <- tibble(id = 5:9)
g<- igraph::graph_from_data_frame(d = edge_list, vertices = node_list, directed = TRUE)
igraph::get.adjacency(g)
plot(g, edge.arrow.size = 0.7)

