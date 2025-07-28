install.packages("devtools")
install.packages("usethis")
library('devtools')
install_github("biometry/bipartite/bipartite")

library(bipartite)

library(readr)
ma_red <- read_csv("Documents/ma_red.csv")
View(ma_red)

install.packages("igraph")
library(igraph)
data.matrix(ma_red)

graph_from_adjacency_matrix(as.matrix(ma_red), mode = "undirected", diag = FALSE)
