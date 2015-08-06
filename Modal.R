
#install.packages("SnowballC")
library(tm)
library(RWeka)
library(Matrix)
library(data.table)
library(stringi)
library(stringr)
library(scales)
library(SnowballC)
# stringr, ggplot2, tm, RWeka, SnowballC, Matrix, scales, stringi


create_corpus <- function (text) {
  stopifnot (is.character (text))
  stopifnot (length (text) > 0)
  
  corpus <- Corpus (VectorSource (text))
  corpus <- tm_map (corpus, content_transformer (stri_trans_tolower))
  corpus <- tm_map (corpus, content_transformer (remove_nonprint))    
  corpus <- tm_map (corpus, removePunctuation)
  corpus <- tm_map (corpus, removeNumbers)
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

classname <- function (name) paste (class (get (name)), collapse = " ")

#
# if a data set does not already exist, create and
# cache it.  if the data set already exists, do nothing.
#
cache_if_missing <- function (name, data, 
                              cache_write = TRUE) {
  
  if (!data.exists(name)) {
    message ("creating data set: ", name, " @ ", Sys.time())
    
    # assign the data set to the given name
    assign (name, data, inherits = TRUE)
    
    # cache the data set, if allowed
    if (cache_write) {
      cache (name)   
      message (sprintf ("saving to cache: %s [%s] @ %s", name, classname (name), Sys.time()))
    }
    
  } else {
    message (sprintf ("loaded from cache: %s [%s] @ %s", name, classname (name), Sys.time()))
  }
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

lines <- function () {
  setwd("C:\\Git\\DataScienceCapstone")
  text <- readLines(file("merged.txt"))
}

data.exists <- function (data.name) {
  stopifnot (is.character (data.name))
  exists (data.name) && !is.function ( get (data.name))
}

build.modal <- function () {
  token <- lines()
  cache_if_missing ("train.corpus", { create_corpus (token) })
  if (data.exists("token")){
    rm (token)
    gc ()
  }

  options (mc.cores = 1)
  # create a tdm for training with 1-grams, 2-grams, and 3-grams
  cache_if_missing ("train.tdm", create_tdm (train.corpus))
  # clean-up
  if (data.exists("train.corpus")){
    rm (train.corpus)
  }
  
  # create and cache the n-gram model
  cache_if_missing ("model", create_model (train.tdm, cutoff = 3))
  # clean-up
  
  if (data.exists(train.tdm)){
    rm (train.tdm)
  }
}

if (!exists("model")){
  model <- readRDS("predictmodel.rds")
}

if(!exists("model"))
{
  model <- build.modal()
  saveRDS(model, "predictmodel.rds")
}
predict_next_word("Jack jumps highest of all.",model)


