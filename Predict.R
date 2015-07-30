# setwd("~/git/DataScienceCapstone")
# setwd("~/git/DataScienceCapstone/final/en_US")
# # Read files into variables
# en_USnewsData <- readLines(file("en_US.news.txt"))
# en_USblogData <- readLines(file("en_US.blogs.txt"))
# en_UStwitterData <- readLines(file("en_US.twitter.txt"))
# 
# setwd("~/git/DataScienceCapstone/final/de_DE")
# # Read files into variables
# de_DEnewsData <- readLines(file("de_DE.news.txt"))
# de_DEblogData <- readLines(file("de_DE.blogs.txt"))
# de_DEtwitterData <- readLines(file("de_DE.twitter.txt"))
# 
# setwd("~/git/DataScienceCapstone/final/fi_FI")
# # Read files into variables
# fi_FInewsData <- readLines(file("fi_FI.news.txt"))
# fi_FIblogData <- readLines(file("fi_FI.blogs.txt"))
# fi_FItwitterData <- readLines(file("fi_FI.twitter.txt"))
# 
# setwd("~/git/DataScienceCapstone/final/ru_RU")
# # Read files into variables
# ru_RUnewsData <- readLines(file("ru_RU.news.txt"))
# ru_RUblogData <- readLines(file("ru_RU.blogs.txt"))
# ru_RUtwitterData <- readLines(file("ru_RU.twitter.txt"))
# 
# 
# 
# setwd("~/git/DataScienceCapstone")
# merged <- paste(en_USnewsData[1:1000], en_USblogData[1:1000], en_UStwitterData[1:1000],de_DEnewsData[1:1000], de_DEblogData[1:1000], de_DEtwitterData[1:1000],fi_FInewsData[1:1000], fi_FIblogData[1:1000], fi_FItwitterData[1:1000],ru_RUnewsData[1:1000], ru_RUblogData[1:1000], ru_RUtwitterData[1:1000])
# write(merged, "merged.txt")

setwd("~/git/DataScienceCapstone")
merged <- readLines(file("merged.txt"))

library(tm)
corpus <- VCorpus(VectorSource(merged))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())

corpusDf <-data.frame(text=unlist(sapply(corpus, 
                                         `[`, "content")), stringsAsFactors=F)

library(RWeka)
findNGrams <- function(corp, grams) {
  ngram <- NGramTokenizer(corp, Weka_control(min = grams, max = grams,
                                             delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngram2 <- data.frame(table(ngram))
  ngram3 <- ngram2[order(ngram2$Freq,decreasing = TRUE),]
  colnames(ngram3) <- c("String","Count")
  ngram3
}


#install.packages("hash")
library("hash")
ngram <- NGramTokenizer(corpusDf, Weka_control(min = 3, max = 3,
                                           delimiters = " \\r\\n\\t.,;:\"()?!"))
tokenHash <- hash()
for (n in ngram) {
  splitString = unlist(strsplit(n, " "))
  token <- paste(splitString[1], splitString[2], sep=" ")
  if (all( has.key( token, tokenHash ) ))
  {
    print("has key")
    
  }
  else
  {
    print("New")
    nextHash <- hash()
    set(tokenHash, key=token, values=splitString[3])
    
  }
  
}



ngram2 <- data.frame(table(ngram))
ngram3 <- ngram2[order(ngram2$Freq,decreasing = TRUE),]
colnames(ngram3) <- c("String","Count")
ngram3

ThreeGrams[,1][1]

for(i in 1:length(ThreeGrams[,1])){
  x = ThreeGrams[,1][i]
  y = ThreeGrams[,2][i]
  print(y)
  print(x)
}




# http://www.decontextualize.com/teaching/rwet/n-grams-and-markov-chains/
#http://www.umiacs.umd.edu/~jimmylin/CMSC723-2009-Fall/session9-slides.pdf