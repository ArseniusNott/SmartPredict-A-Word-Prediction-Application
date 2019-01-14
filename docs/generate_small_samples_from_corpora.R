set.seed(1234)
options(width = 100)

# libraries
library(readr)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(lexicon)
library(textclean)

# save as CSV
save_as_csv <- function(data, file_path) {
  write_csv(data, path = file_path)
}

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

remove_tokens_with_numbers <- function(tokenized_corpus) {
  # remove tokens that are pure digits
  # stop words numbers
  custom_stop_words_digits <- 
    tokenized_corpus %>%
    filter(str_detect(ngrams, "\\w*[0-9]+\\w*\\s*")) %>%
    pull(var = ngrams) %>%
    unique()
  
  # convert to data_frame
  custom_stop_words_digits <- data_frame(word = custom_stop_words_digits)
  
  # remove pure number tokens
  pure_number_token_removed <- tokenized_corpus %>%
    anti_join(custom_stop_words_digits, by = c("ngrams" = "word"))
  
  return(pure_number_token_removed)
}

remove_profane_tokens <- function(tokenized_corpus) {
  # stop words profanity
  # unique profane words from the following sources:
  # (1) Alejandro U. Alvarez's List of Profane Words
  # (2) Stackoverflow user2592414's List of Profane Words
  # (3) bannedwordlist.com's List of Profane Words
  # (4) Google's List of Profane Words
  custom_stop_words_profanity <- 
    rbind(
      data_frame(word = profanity_alvarez)[, 1],
      data_frame(word = profanity_arr_bad)[, 1],
      data_frame(word = profanity_banned)[, 1],
      data_frame(word = profanity_racist)[, 1],
      data_frame(word = profanity_zac_anger)[, 1]
    ) %>%
    unique()
  
  profane_words_removed <- 
    tokenized_corpus %>%
    anti_join(custom_stop_words_profanity, by = c("ngrams" = "word"))
  
  return(profane_words_removed)
}

remove_tokens_with_special_characters <- function(tokenized_corpus) {
  # remove tokens with any special characters
  # EXCEPT APOSTROPHE
  
  custom_stop_words_special_characters <- 
    tokenized_corpus %>%
    filter(str_detect(ngrams, "[^('\\p{Alphabetic}{1,})[:^punct:]]")) %>%
    pull(var = ngrams) %>%
    unique()
  
  if (length(custom_stop_words_special_characters) == 0) {
    print("No special characters found")
    return()
  }
  
  # convert to data_frame
  custom_stop_words_special_characters <- 
    data_frame(word = custom_stop_words_special_characters)
  
  # remove pure number tokens
  special_characters_token_removed <- tokenized_corpus %>%
    anti_join(custom_stop_words_special_characters, by = c("ngrams" = "word"))
  
  return(special_characters_token_removed)
}


# tokenized clean corpus
# NOTE: version without expanding contractions
tokenize_and_clean_corpus <- 
  function(input_file_location, size = 0.25, 
           custom_key_contractions = key_contractions,
           english_language = grady_augmented) {
    set.seed(1234)
    raw_lines <- read_lines(file = input_file_location, n_max = -1, progress = FALSE)
    
    # convert raw data to data_frame that adheres to tidy format
    raw_df <- data_frame(lines = raw_lines)
    
    # get a fraction of raw df (default is 25%)
    sample_df <- sample_frac(tbl = raw_df, size = size)
    
    # tokenize corpus 1-gram
    one_gram <- tokenize_ngram(data = sample_df, n = 1)
    
    # convert apostrophe [’] to 0027
    one_gram <- 
      one_gram %>%
      mutate(ngrams = gsub(pattern = "’", "'", ngrams))
    
    # remove tokens with numbers
    one_gram <- remove_tokens_with_numbers(one_gram)
    
    # remove tokens with special characters
    one_gram <- remove_tokens_with_special_characters(one_gram)
    
    # remove tokens that are profane
    one_gram <- remove_profane_tokens(one_gram)
    
    # add column that would initially determine if the word is english or not
    one_gram <-
      one_gram %>%
      mutate(is_english = ngrams %in% english_language)
    
    return(one_gram)
  }

change_contraction <- 
  function (custom_key_contractions, contraction_to_change, new_value) {
    custom_key_contractions %>%
      mutate(expanded = replace(expanded, contraction == contraction_to_change, 
                                new_value))
  }

custom_key_contractions_second_iteration <- function() {
  # custom_key_contractions
  custom_key_contractions <- key_contractions
  custom_key_contractions <- 
    rbind(custom_key_contractions, 
          # FIRST ITERATION
          # contractions from first iteration (blog)
          c(contraction = "here's", expanded = "here is"),
          c(contraction = "it'd", expanded = "it would"),
          c(contraction = "that'd", expanded = "that would"),
          c(contraction = "there'd", expanded = "there would"),
          c(contraction = "y'all", expanded = "you and all"),
          c(contraction = "needn't", expanded = "need not"),
          c(contraction = "gov't", expanded = "government"),
          c(contraction = "n't", expanded = "not"),
          c(contraction = "ya'll", expanded = "you and all"),
          c(contraction = "those'll", expanded = "those will"),
          c(contraction = "this'll", expanded = "this will"),
          c(contraction = "than'll", expanded = "than will"),
          c(contraction = "c'mon", expanded = "come on"),
          c(contraction = "qur'an", expanded = "quran"),
          # additional from twitter
          c(contraction = "where'd", expanded = "where would"),
          c(contraction = "con't", expanded = "continued"),
          c(contraction = "nat'l", expanded = "national"),
          c(contraction = "int'l", expanded = "international"),
          c(contraction = "i'l", expanded = "i will"),
          c(contraction = "li'l", expanded = "little"),
          c(contraction = "add'l", expanded = "additional"),
          c(contraction = "ma'am", expanded = "madam"),
          # SECOND ITERATION
          # additional from blog
          c(contraction = "y'know", expanded = "you know"),
          c(contraction = "not've", expanded = "not have"),
          c(contraction = "that've", expanded = "that have"),
          c(contraction = "should've", expanded = "should have"),
          c(contraction = "may've", expanded = "may have"),
          c(contraction = "ne'er", expanded = "never"),
          c(contraction = "e're", expanded = "ever"),
          c(contraction = "whene'er", expanded = "whenever"),
          # additional from twitter
          c(contraction = "cont'd", expanded = "continued"),
          c(contraction = "how're", expanded = "how are"),
          c(contraction = "there're", expanded = "there are"),
          c(contraction = "where're", expanded = "when are"),
          c(contraction = "why're", expanded = "why are"),
          c(contraction = "that're", expanded = "that are"),
          c(contraction = "how've", expanded = "how have"),
          c(contraction = "there've", expanded = "there have"),
          c(contraction = "may've", expanded = "may have"),
          c(contraction = "she've", expanded = "she have"),
          c(contraction = "all've", expanded = "all have"),
          # additional from news
          c(contraction = "hawai'i", expanded = "hawaii"))
  
  return(custom_key_contractions)
}

# update can't to cannot NOT can not
custom_key_contractions_v2 <- change_contraction(custom_key_contractions_second_iteration(), 
                                              "can't", "cannot")
save_as_csv(custom_key_contractions_v2, file_path = "./data/custom_key_contractions_v2.csv")

# custom english dictionary
custom_english_dictionary <- read_csv("./data/custom_english_dictionary.csv")
# add common key contractions to custom english dictionary
custom_english_dictionary_v2 <- 
  data_frame(ngrams = c(custom_english_dictionary$ngrams, 
                        custom_key_contractions_v2$contraction))
save_as_csv(custom_english_dictionary_v2, 
            file_path = "./data/custom_english_dictionary_v2.csv")



# small sample per corpus

# raw file locations
blog_us_raw_input_file <- "~/Dropbox/week_2/data/en_US.blogs.txt"
twitter_us_raw_input_file <- "~/Dropbox/week_2/data/en_US.twitter.txt"
news_us_raw_input_file <- "~/Dropbox/week_2/data/en_US.news.txt"

# Use the custom_english_dictionary and custom_key_contractions to reclean
# blog, twitter and news corpora (0.27, 0.22, 0.24) for the data analysis part
# now, we reduce them by .20 so the sample now for blog, twitter and news corpora
# will be 0.7, 0.02 and 0.04 
blog_cleaned_small <- tokenize_and_clean_corpus(
  input_file_location = blog_us_raw_input_file, 
  size = 0.07, 
  custom_key_contractions = custom_key_contractions_v2, 
  english_language = custom_english_dictionary_v2$ngrams)
# save to file
write_csv(blog_cleaned_small, 
          path = "./data/blog_cleaned_small.csv")

twitter_cleaned_small <- tokenize_and_clean_corpus(
  input_file_location = twitter_us_raw_input_file, 
  size = 0.02, 
  custom_key_contractions = custom_key_contractions_v2, 
  english_language = custom_english_dictionary_v2$ngrams)
# save to file
write_csv(twitter_cleaned_small, 
          path = "./data/twitter_cleaned_small.csv")

news_cleaned_small <- tokenize_and_clean_corpus(
  input_file_location = news_us_raw_input_file, 
  size = 0.04, 
  custom_key_contractions = custom_key_contractions_v2, 
  english_language = custom_english_dictionary_v2$ngrams)
# save to file
write_csv(news_cleaned_small, 
          path = "./data/news_cleaned_small.csv")
