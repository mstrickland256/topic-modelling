#EXORDIUM
library(tm)
library(topicmodels)

setwd("/users/shimiaoning/pdf") #Adjust this as necessary, obviously.

Sys.setlocale(locale="C")


#INPUT
goodpath <- "/users/shimiaoning/Ukoe"
docs <- Corpus(DirSource(goodpath), readerControl = list(reader=readPDF))

# alternate method
# filenames <- list.files(getwd(),pattern="*.pdf")
# files <- readPDF(control = list(text = "-layout"))(elem = list(uri = filename),
#                                                 language = "en",
#                                                 id = "id1")


#TEXT CLEANING
docs <- tm_map(docs, content_transformer(tolower), lazy=TRUE)
docs <- tm_map(docs, removePunctuation, lazy=TRUE)
docs <- tm_map(docs, removeNumbers, lazy=TRUE)
docs <- tm_map(docs, removeWords, stopwords("english"), lazy=TRUE)
docs <- tm_map(docs, stripWhitespace, lazy=TRUE)
#docs <- tm_map(docs, stemDocument, lazy=TRUE)


#DOCUMENT TERM MATRIX FORMATION
dtm <- DocumentTermMatrix(docs)
#rownames(dtm) <- filenames
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq, decreasing = TRUE)
freq[ord]


#GIBBS PARAMETERS
burnin <- 4000
iter <- 300
thin <- 500
seed <- list(2003, 5, 63, 100001, 765)
nstart <- 5
best <- TRUE


#NO. OF TOPICS
k <- 5


#CLEARING OUT DOCUMENTS WITHOUT TERMS FROM MATRIX
rowTotals <- apply(dtm , 1, sum)
dtm <- dtm[rowTotals > 0, ]

#LDA PROPER
ldaOut <- LDA(dtm, k, method="Gibbs", control=list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin = thin))
ldaOut.terms <- as.matrix(terms(ldaOut, 6)) #2nd argument here denotes the number of terms to display per topic.
write.csv(ldaOut.terms, file = paste("LDAGibbs", k, "TopicsTerms.csv"))
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities, file = paste("LDAGibbs", k, "TopicProbabilities.csv"))
