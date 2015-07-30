setwd("~/git/DataScienceCapstone")
merged <- readLines(file("merged.txt"))

library(tm)
library(RWeka)
#install.packages("Matrix")
#install.packages("stringi")
library("Matrix")
library("data.table")
library("stringi")


create_corpus <- function (text) {
  stopifnot (is.character (text))
  stopifnot (length (text) > 0)
  
  corpus <- Corpus (VectorSource (text))
  corpus <- tm_map (corpus, content_transformer (stri_trans_tolower))
  corpus <- tm_map (corpus, content_transformer (remove_nonprint))    
  corpus <- tm_map (corpus, removePunctuation)
  corpus <- tm_map (corpus, removeNumbers)
  #corpus <- tm_map (corpus, stemDocument, language = "english")
  corpus <- tm_map (corpus, stripWhitespace)
  corpus <- tm_map (corpus, content_transformer (stri_trim_both))
  
  return (corpus)
}

create_tdm <- function (corpus, ngram_min = 1, ngram_max = 3) {
  options(mc.cores=1)
  tok <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = ngram_min, max = ngram_max))
  tdm <- TermDocumentMatrix (corpus, control = list (tokenize = tok))    
  
  return (tdm)
}



create_model <- function (tdm, cutoff = 3) {
  
  # count the total number of occurances across the corpus
  dims <- c(tdm$nrow, tdm$ncol)
  mx <- sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v, dims=dims, dimnames=tdm$dimnames)
  counts <- rowSums (mx)
  model <- data.table (phrase = rownames (mx), count = counts)
  
  # split the phrase into previous and next
  model [, prev_words := except_last_word (phrase), by = phrase]
  model [, next_word := last_word (phrase), by = phrase]
  
  # keep only the most frequently occuring prev_word for each base_word
  model <- model [, list (
    next_word = next_word [which.max (count)], 
    count     = max (count)
  ), by = prev_words]
  
  # exclude any ngrams that occur less than the cut-off frequncy
  model <- model [ count >= cutoff ]
  
  # uni-gram, bi-gram or tri-gram?
  model [, gram := sapply (strsplit (prev_words, split = " "), length) + 1 ]
  
}

split_on_space <- function (x) {
  result <- unlist (strsplit (x, split = "[ ]+"))
  result [nchar (result) > 0]
}

except_last_word <- function (phrase) {
  words <- split_on_space (phrase)
  paste (words [1:length (words)-1], collapse = " ")
}


last_word <- function (phrase) {
  words <- split_on_space (phrase)
  words [length (words)]
}

remove_nonprint <- function (x) gsub ("[^[:print:]]+", "", x)
predict_next_word <- function (phrase, model, n = 3) {
  
  # sanity checks
  stopifnot (is.character (phrase))
  stopifnot (length (phrase) == 1)
  
  # uses 'create_corpus' to apply all of the same pre-processing on the 
  # input phrase as was applied to the training data
  clean_phrase <- sapply (create_corpus (phrase), function (x) x$content)
  
  # break the sentence into its component words
  previous <- unlist (strsplit (clean_phrase, split = " ", fixed = TRUE))
  len <- length (previous)
  
  prediction <- NULL
  for (i in n:1) {
    
    # ensure there are enough previous words 
    # for example, a trigram model needs 2 previous words
    if (len >= i-1) {
      
      # grab the last 'i-1' words
      base <- tail (previous, i-1)
      base <- paste (base, collapse = " ")
      
      prediction <- model [prev_words == base, next_word]
      if (length (prediction) > 0) {
        #message (sprintf ("%s-gram: '%s' -> '%s'", i, base, prediction))
        break
      }
    }
  }
  
  return (prediction)
}
corpus <- create_corpus(merged)
tdm <- create_tdm(corpus)
tokenModel <- create_model(tdm)
predict_next_word ("Bill falls down the", tokenModel, n = 4)