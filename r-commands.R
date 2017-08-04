setwd("C:\\a_orgs\\carleton\\hist3814\\module4")
# give yourself as much memory as you've got
options(java.parameters = "-Xmx5120m")
#install.packages('rJava')
library('rJava')
## from http://cran.r-project.org/web/packages/mallet/mallet.pdf
#install.packages('mallet')
library('mallet')
#install.packages('RCurl')
library('RCurl')
x <- getURL("https://raw.githubusercontent.com/shawngraham/exercise/gh-pages/CND.csv", .opts = list(ssl.verifypeer = FALSE))
documents <- read.csv(text = x, col.names=c("Article_ID", "Newspaper Title", "Newspaper City", "Newspaper Province", "Newspaper Country", "Year", "Month", "Day", "Article Type", "Text", "Keywords"), colClasses=rep("character", 3), sep=",", quote="")
counts <- table(documents$Newspaper.City)
barplot(counts, main="Cities", xlab="Number of Articles")
countsYear <- table(documents$Year)
barplot(countsYear, main="Year", xlab="Number of Articles")
mallet.instances <- mallet.import(documents$Article_ID, documents$Text, "C:\\a_orgs\\carleton\\hist3814\\module4\\stopwords.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#set the number of desired topics
num.topics <- 50
topic.model <- MalletLDA(num.topics)
topic.model$loadDocuments(mallet.instances)
## Get the vocabulary, and some statistics about word frequencies.
## These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)
write.csv(word.freqs, "cnd-word-freqs.csv" )
countWords <- table(word.freqs$term.freq)
barplot(countWords, main="term.freq", xlab="Number of words")

## Optimize hyperparameters every 20 iterations,
## after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)
## Now train a model. Note that hyperparameter optimization is on, by default.
## We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(1000)
## Run through a few iterations where we pick the best topic for each token,
## rather than sampling from the posterior distribution.
topic.model$maximize(10)
## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
mallet.top.words(topic.model, topic.words[7,])

topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "cnd-topics-docs.csv" ) 
## Get a vector containing short names for the topics
topics.labels <- rep("", num.topics)
for (topic in 1:num.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels

write.csv(topics.labels, "cnd-topics-labels.csv")

#install.packages('wordcloud')


#library(wordcloud)
library(wordcloud)


for(i in 1:num.topics){
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words[i,], 25)
  print(wordcloud(topic.top.words$words,
                  topic.top.words$weights,
                  c(4,.8), rot.per=0,
                  random.order=F))
}



## cluster based on shared words
plot(hclust(dist(topic.words)), labels=topics.labels)



topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$article_id

#install.packages("cluster")

library(cluster)
topic_df_dist <- as.matrix(daisy(t(topic_docs), metric = "euclidean", stand = TRUE))
# Change row values to zero if less than row minimum plus row standard deviation
# keep only closely related documents and avoid a dense spagetti diagram
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] <- 0

#install.packages("igraph")

library(igraph)
g <- as.undirected(graph.adjacency(topic_df_dist))
layout1 <- layout.fruchterman.reingold(g, niter=500)
plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1, vertex.color= "grey", edge.arrow.size = 0, vertex.label.dist=0.5, vertex.label = NA)

write.graph(g, file="cnd.graphml", format="graphml")

