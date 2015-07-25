setwd("~/git/DataScienceCapstone/final/en_US")
# Read files into variables
readLines("en_US.news.txt") -> en_US_news
## Warning: incomplete final line found on 'en_US.news.txt'
readLines("en_US.blogs.txt") -> en_US_blogs
readLines("en_US.twitter.txt") -> en_US_twitter

library(plyr)
library(knitr)
## Create the list for summary
list_en<-list(en_US_news, en_US_blogs,en_US_twitter)
names(list_en)<- c("en_US_news", "en_US_blogs","en_US_twitter")

## Define the function for Word-counting
WordCounting<-function(x) length(unlist(strsplit(x,split = " ")))

## Output the summary:Object size,Lines,Words
ldply(list_en,c("object.size","length","WordCounting"))->output
names(output) <- c("Object","Size in bytes", "Line Counts", "Word Counts")
kable(output)




library(tm)
merged <- paste(newsData[1:10000], blogData[1:10000], twitterData[1:10000])
corpus <- VCorpus(VectorSource(merged))

corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
library(RWeka)

corpusDf <-data.frame(text=unlist(sapply(corpus, 
                                         `[`, "content")), stringsAsFactors=F)

findNGrams <- function(corp, grams) {
  ngram <- NGramTokenizer(corp, Weka_control(min = grams, max = grams,
                                             delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngram2 <- data.frame(table(ngram))
  #pick only top 25
  ngram3 <- ngram2[order(ngram2$Freq,decreasing = TRUE),][1:100,]
  colnames(ngram3) <- c("String","Count")
  ngram3
}

TwoGrams <- findNGrams(corpusDf, 2)
ThreeGrams <- findNGrams(corpusDf, 3)
FourGrams <- findNGrams(corpusDf, 4)




par(mfrow = c(1, 1))

barplot(TwoGrams[1:20,2], cex.names=0.5, names.arg=TwoGrams[1:20,1], col="red", main="2-Grams", las=2)
barplot(ThreeGrams[1:20,2], cex.names=0.5, names.arg=ThreeGrams[1:20,1], col="green", main="3-Grams", las=2)
barplot(FourGrams[1:20,2], cex.names=0.5, names.arg=FourGrams[1:20,1], col="blue", main="4-Grams", las=2)
