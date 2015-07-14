#install.packages("tm")
#install.packages("RWeka")

library(tm)
library(RWeka)



setwd("~/class/DataScienceCapstone")
# download file from URL
if (!file.exists("Coursera-SwiftKey.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile="Coursera-SwiftKey.zip",method = "curl")
}
# unzip file
if (!file.exists("final")) {
  unzip("Coursera-SwiftKey.zip")
}

# OTHER GOOD Source will be google search terms, whatsup text messages 
#load files
en_US_twitter = read.table("final/en_US/en_US.twitter.txt", sep='\t', skipNul = TRUE,quote = "",nrows = 500)
en_US_news = read.table("final/en_US/en_US.news.txt", sep='\t', skipNul = TRUE,quote = "",nrows = 500) 
en_US_blogs = read.table("final/en_US/en_US.blogs.txt", sep='\t', skipNul = TRUE,quote = "",nrows = 500) 

dataset<-rbind(en_US_twitter, en_US_news)
en_US_text <-rbind(dataset, en_US_blogs)
rm(dataset)




TrimSpace <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}


TrimNonAlphanumeric<- function( x ) {
  gsub("[^[:alnum:] ]", "", x)
}

TrigramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 2, max = 6))


CleanUpEnglish <- function(x, output) {
  text <- tolower(TrimSpace(TrimNonAlphanumeric(x[1])))
  tdm <- TermDocumentMatrix(Corpus(VectorSource(text)), 
                            control = list(tokenize = TrigramTokenizer))
  inspect(tdm)
  
}

apply(en_US_text, 1, CleanUpEnglish, output = 'outputfile')






