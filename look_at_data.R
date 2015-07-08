library(knitr)
options(digits = 7)
opts_chunk$set(fig.width=10)
library(R.utils, warn.conflicts = FALSE, quietly=TRUE)
library(data.table, warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly=TRUE)

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
en_US_blogs = read.table("final/en_US/en_US.blogs.txt") 


en_US_news = read.table("final/en_US/en_US.news.txt") 
en_US_twitter = read.table("final/en_US/en_US.twitter.txt") 