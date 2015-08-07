library(tm)
library(RWeka)
library(Matrix)
library(data.table)
library(stringi)
library(stringr)
library(scales)
library(SnowballC)
setwd("C:\\Git\\DataScienceCapstone")
text <- readLines(file("merged.txt"))

setwd("C:\\Git\\DataScienceCapstone\\final\\en_US")
# Read files into variables
newsData <- readLines(file("en_US.news.txt"))
blogData <- readLines(file("en_US.blogs.txt"))
twitterData <- readLines(file("en_US.twitter.txt"))

head(newsData)

rm (text)
gc()
vectorS <- VectorSource(newsData)
rm (newsData)
gc()
corpus <- VCorpus(vectorS)

gc()
vectorS <- VectorSource(blogData)
rm (blogData)
gc()
corpusblogData <- VCorpus(vectorS)

gc()
vectorS <- VectorSource(twitterData)
rm (twitterData)
gc()
corpustwitterData <- VCorpus(vectorS)
gc()


saveRDS(corpus, "corpusNewsData.rds")
saveRDS(corpus, "corpusBlogData.rds")

corpus2 <- readRDS("corpusBlogData.rds")

stopifnot (is.character (text))
stopifnot (length (text) > 0)


corpus2 <- tm_map (corpus2, content_transformer (stri_trans_tolower))
corpus2 <- tm_map (corpus2, content_transformer (remove_nonprint))    
corpus2 <- tm_map (corpus2, removePunctuation)
corpus2 <- tm_map (corpus2, removeNumbers)
corpus2 <- tm_map (corpus2, stripWhitespace)
corpus2 <- tm_map (corpus2, content_transformer (stri_trim_both))
master <- c(corpus, corpus2)
rm (corpus2)
rm (corpus)
gc()

options (mc.cores = 1)

tok <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))
tdm <- TermDocumentMatrix (master, control = list (tokenize = tok))  

