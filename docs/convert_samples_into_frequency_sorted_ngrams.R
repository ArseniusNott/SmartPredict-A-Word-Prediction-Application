# set seed
set.seed(1234)
setwd("~/projects/week_3/")

# libraries
library(readr) # /
library(dplyr) # /
library(tidytext) # /
library(stringr)
library(tidyr)
library(lexicon)
library(ggplot2)
library(textclean)

# file names
blog_cleaned_file_name <- "./data/blog_cleaned_small.csv"
twitter_cleaned_file_name <- "./data/twitter_cleaned_small.csv"
news_cleaned_file_name <- "./data/news_cleaned_small.csv"

# functions
tokenize_ngram <- function(data, n = 1, retokenize = FALSE) {
  if (retokenize == TRUE) {
    ngrams <- data %>%
      unnest_tokens(output = ngrams, 
                    input = ngrams, 
                    token = "ngrams",
                    n = n,
                    to_lower = TRUE)
  } else {
    ngrams <- data %>%
      unnest_tokens(output = ngrams, 
                    input = lines, 
                    token = "ngrams",
                    n = n,
                    to_lower = TRUE)
  }
  
  return(ngrams)
}

separate_ngrams = function(tokenized_ngram, n) {
  ngram_columns <- paste("word", 1:n, sep = "")
  tokenized_ngram %>%
    separate(ngrams, ngram_columns, sep = " ")
}

# sort by frequency and separate ngrams into word columns
sort_by_frequency <- function(tokenized_ngram, n) {
  # if one word per token
  if (n == 1) {
    tokenized_ngram %>%
      count(ngrams, sort = TRUE)
  } else {
    tokenized_ngram %>%
      count(ngrams, sort = TRUE) %>%
      separate_ngrams(n)
  }
}

# save as CSV
save_as_csv <- function(data, file_path) {
  write_csv(data, path = file_path)
}

blog_cleaned <- read_csv(file = blog_cleaned_file_name, col_names = TRUE, 
                         col_types = cols(
                           ngrams = col_character(),
                           is_english = col_logical()
                         ))
twitter_cleaned <- read_csv(file = twitter_cleaned_file_name, col_names = TRUE, 
                            col_types = cols(
                              ngrams = col_character(),
                              is_english = col_logical()
                            ))
news_cleaned <- read_csv(file = twitter_cleaned_file_name, col_names = TRUE, 
                         col_types = cols(
                           ngrams = col_character(),
                           is_english = col_logical()
                         ))

corpora_one_gram <- bind_rows(blog_cleaned, twitter_cleaned, news_cleaned) %>%
  filter(is_english == TRUE) %>%
  mutate(is_english = NULL)
corpora_two_gram <- tokenize_ngram(data = corpora_one_gram, n = 2, 
                                   retokenize = TRUE)
corpora_three_gram <- tokenize_ngram(data = corpora_one_gram, n = 3, 
                                     retokenize = TRUE)
corpora_four_gram <- tokenize_ngram(data = corpora_one_gram, n = 4, 
                                    retokenize = TRUE)
corpora_five_gram <- tokenize_ngram(data = corpora_one_gram, n = 5, 
                                    retokenize = TRUE)

# frequency sort
corpora_one_gram_frequency_sorted <- 
  sort_by_frequency(corpora_one_gram, n = 1)
corpora_two_gram_frequency_sorted <- 
  sort_by_frequency(corpora_two_gram, n = 2)
corpora_three_gram_frequency_sorted <- 
  sort_by_frequency(corpora_three_gram, n = 3)
corpora_four_gram_frequency_sorted <- 
  sort_by_frequency(corpora_four_gram, n = 4)
corpora_five_gram_frequency_sorted <- 
  sort_by_frequency(corpora_five_gram, n = 5)

# save as .csv files
save_as_csv(corpora_one_gram_frequency_sorted, 
            file_path = "./data/corpora_one_gram_frequency_sorted.csv")
save_as_csv(corpora_two_gram_frequency_sorted, 
            file_path = "./data/corpora_two_gram_frequency_sorted.csv")
save_as_csv(corpora_three_gram_frequency_sorted, 
            file_path = "./data/corpora_three_gram_frequency_sorted.csv")
save_as_csv(corpora_four_gram_frequency_sorted, 
            file_path = "./data/corpora_four_gram_frequency_sorted.csv")
save_as_csv(corpora_five_gram_frequency_sorted, 
            file_path = "./data/corpora_five_gram_frequency_sorted.csv")