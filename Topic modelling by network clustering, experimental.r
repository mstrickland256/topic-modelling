#EXORDIUM
library(tm)
library(textmineR)
library(bigmemory)
library(igraph)

#CREATING CUSTOMIZED STOPWORDS LIST
stopwords <- read.csv("stopwords.csv", header = FALSE)
stopwords <- as.character(stopwords$V1)
flibbertigibbet <- c(stopwords, stopwords())

#ACCESSING FILES
docs <- Corpus(DirSource("whatever"))

#PREPROCESSING
docs <- tm_map(docs, content_transformer(tolower))
removeSpecialChars <- function(x) gsub("[^a-zA-Z ]", "", x)
docs <- tm_map(docs, removeSpecialChars)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, flibbertigibbet)

#SETTING AND INSPECTING MATRIX
dtm <- DocumentTermMatrix(docs)
dim(dtm)
dtm <- weightTf(dtm, normalize = TRUE)
#colnames(dtm) [100:110]
#inspect(dtm) [1:10, 101:110]
#top100terms <- findFreqTerms(dtm, 100)
dtmDense <- removeSparseTerms(dtm, 0.98)   #removes terms absent in whatever percentage of documents; max allowed sparsity percentage is 0.998
dim(dtmDense)
dtmFinal <- as.big.matrix(x = as.matrix(dtmDense))
dtmFinal <- as.matrix(dtmFinal)

#CONVERTING TO COÖCCURRENCE MATRIX
cooccurrences <- Dtm2Tcm(dtmFinal)
write.csv(as.matrix(cooccurrences), file = "AdjacencyMatrix.csv")

#BUILDING NETWORK
the_wordgraph <- graph_from_adjacency_matrix(cooccurrences, weighted = TRUE, mode = "lower")
net_clusters <- cluster_louvain(the_wordgraph, weights = NULL)
cluster_results <- cbind(V(the_wordgraph)$name, net_clusters$membership)
write.csv(cluster_results, file ="ClusterResults.csv")
fluke <- neighbors(the_wordgraph, "china", mode = c("out", "in", "all", "total"))
neighs <- cbind(V(the_wordgraph)$name, fluke$membership)
write.csv(neighs, file ="Neighbors.csv")
