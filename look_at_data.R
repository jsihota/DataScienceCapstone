
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
en_US_twitter = read.table("final/en_US/en_US.twitter.txt", sep='\t', skipNul = TRUE,quote = "") 
str(en_US_twitter)

TrimSpace <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}


TrimNonAlphanumeric<- function( x ) {
  gsub("[^[:alnum:] ]", "", x)
}

CleanUpEnglish <- function(x, output) {
  text <- TrimSpace(TrimNonAlphanumeric(x[1]))
  print(text)
}

apply(en_US_twitter, 1, CleanUpEnglish, output = 'outputfile')






