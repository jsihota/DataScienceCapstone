setwd("~/git/DataScienceCapstone/final/en_US")
# Read files into variables
newsData <- readLines(file("en_US.news.txt"))
blogData <- readLines(file("en_US.blogs.txt"))
twitterData <- readLines(file("en_US.twitter.txt"))

print(paste("News Data Length = ", length(newsData),
            ", News Blog Length = ", length(blogData),
            ", News twitter Length = ", length(twitterData)
))

lengths <- c(length(blogData),length(newsData),length(twitterData))
lengths <- data.frame(lengths)
lengths$names <- c("blogs","news","twitter")
ggplot(lengths,aes(x=names,y=lengths)) +
  geom_bar(stat='identity',fill='cornsilk',color='grey60') + 
  xlab('Source') + ylab('Total Lines') + coord_flip() + 
  geom_text(aes(label=format(lengths,big.mark=","),size=3),vjust=-0.2) +
  scale_y_continuous(limits=c(0,2600000)) +
  theme(legend.position='none') + 
  ggtitle('Total Line Count by Text Source')



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
```