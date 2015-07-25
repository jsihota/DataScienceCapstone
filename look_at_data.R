#install.packages("tm")
#install.packages("RWeka")
#install.packages("stargazer")

library(tm)
library(RWeka)
library(ggplot2)



setwd("~/git/DataScienceCapstone")
# download file from URL
if (!file.exists("Coursera-SwiftKey.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile="Coursera-SwiftKey.zip",method = "curl")
}
# unzip file
if (!file.exists("final")) {
  unzip("Coursera-SwiftKey.zip")
}


setwd("~/git/DataScienceCapstone/final/en_US")
# Read files into variables
newsData <- readLines(file("en_US.news.txt"))
blogData <- readLines(file("en_US.blogs.txt"))
twitterData <- readLines(file("en_US.twitter.txt"))

print(paste("News Data Length = ", length(newsData),
            ", News Blog Length = ", length(blogData),
            ", News twitter Length = ", length(twitterData)
))







# OTHER GOOD Source will be google search terms, whatsup text messages 
#load files
en_US_twitter = read.table("final/en_US/en_US.twitter.txt", sep='\t', skipNul = TRUE,quote = "")
en_US_news = read.table("final/en_US/en_US.news.txt", sep='\t', skipNul = TRUE,quote = "") 
en_US_blogs = read.table("final/en_US/en_US.blogs.txt", sep='\t', skipNul = TRUE,quote = "") 

dataset<-rbind(en_US_twitter, en_US_news)
en_US_text <-rbind(dataset, en_US_blogs)
rm(dataset)


length(en_US_blogs)
lengths <- c(length(en_US_blogs),length(en_US_news),length(en_US_twitter))
lengths <- data.frame(lengths)
lengths$names <- c("blogs","news","twitter")
ggplot(lengths,aes(x=names,y=lengths)) +
  geom_bar(stat='identity',fill='cornsilk',color='grey60') + 
  xlab('Source') + ylab('Total Lines') + coord_flip() + 
  geom_text(aes(label=format(lengths,big.mark=","),size=3),labels=comma,vjust=-0.2) +
  scale_y_continuous(limits=c(0,2600000),labels=comma) +
  theme(legend.position='none') + 
  ggtitle('Total Line Count by Text Source')




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






