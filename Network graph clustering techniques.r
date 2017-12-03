library(igraph)
setwd("D:\\goto\\Topic Analysis Experiments")
datable <- read.csv("AdjacencyMatrix A.csv")
the_wordgraph <- graph_from_adjacency_matrix(datable, weighted = TRUE, mode = "lower")
net_clusters <- cluster_walktrap(the_wordgraph, weights = E(graph)$weight, steps = 4,
merges = TRUE, modularity = TRUE, membership = TRUE)
cluster_results <- cbind(V(the_wordgraph)$name, net_clusters$membership)
write.csv(cluster_results, file ="ClusterResults A, walktrap.csv")
net_clusters

#SPINGLASS
net_clusters <- cluster_spinglass(the_wordgraph, weights = NULL, vertex = NULL, spins = 25,
parupdate = FALSE, start.temp = 1, stop.temp = 0.01, cool.fact = 0.99,
update.rule = c("config", "random", "simple"), gamma = 1,
implementation = c("orig", "neg"), gamma.minus = 1)
cluster_results <- cbind(V(the_wordgraph)$name, net_clusters$membership)
write.csv(cluster_results, file ="ClusterResults A, spinglass.csv")
net_clusters

#OPTIMAL METHOD
net_clusters <- cluster_optimal(the_wordgraph, weights = NULL)
cluster_results <- cbind(V(the_wordgraph)$name, net_clusters$membership)
write.csv(cluster_results, file ="ClusterResults A, optimal.csv")
net_clusters

#INFOMAP
net_clusters <- cluster_infomap(the_wordgraph, e.weights = NULL, v.weights = NULL, nb.trials = 10,
modularity = TRUE)
cluster_results <- cbind(V(the_wordgraph)$name, net_clusters$membership)
write.csv(cluster_results, file ="ClusterResults A, infomap.csv")
net_clusters

#EDGE BETWEENNESS
net_clusters <- cluster_edge_betweenness(the_wordgraph, weights = E(graph)$weight, directed = TRUE,
edge.betweenness = TRUE, merges = TRUE, bridges = TRUE,
modularity = TRUE, membership = TRUE)
cluster_results <- cbind(V(the_wordgraph)$name, net_clusters$membership)
write.csv(cluster_results, file ="ClusterResults A, edgebetween.csv")
net_clusters
