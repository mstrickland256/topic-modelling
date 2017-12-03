#EXORDIUM
library(tm)
library(textmineR)
library(bigmemory)

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
dtm <- weightTf(dtm, normalize = TRUE)
dim(dtm)
#colnames(dtm) [100:110]
#inspect(dtm) [1:10, 101:110]
#top100terms <- findFreqTerms(dtm, 100)
dtm <- removeSparseTerms(dtm, 0.98)   #removes terms absent in whatever percentage of documents; max allowed sparsity percentage is 0.998.

#EXPLORATORY ANALYSIS OF TEXT
findAssocs(dtm, c("china", "europe"), c(0.7, 0.5)) #Finds associations with the given term(s), following number(s) is/are the correlation limit(s) to be used.
findFreqTerms(dtm, 2, 3) #First number is lower frequency bound, second is higher frequency bound; for the latter, may supply "Inf" if desired.
findMostFreqTerms(dtm, n = 10L) #Second term is optional, specifies the number of terms to be limited to.
Zipf_plot(dtm) #Plotting logarithm of frequency against logarithm of rank.
Heaps_plot(dtm) #As Heap's law says vocabulary size grows polynomially with text size, this plots logarithm of vocabulary size against logarithm of text size.
