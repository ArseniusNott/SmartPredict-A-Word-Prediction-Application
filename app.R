# libraries
library(readr)
library(stringr)
library(dplyr)
library(tidytext)
library(tidyr)
library(lexicon)
library(shiny)

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

corpora_one_gram_frequency_sorted <- 
  read_csv("./data/corpora_one_gram_frequency_sorted.csv", col_names = TRUE,
           col_types = cols(
             ngrams = col_character(),
             n = col_double()
           ))
corpora_two_gram_frequency_sorted <- 
  read_csv("./data/corpora_two_gram_frequency_sorted.csv", col_names = TRUE,
           col_types = cols(
             word1 = col_character(),
             word2 = col_character(),
             n = col_double()
           ))
corpora_three_gram_frequency_sorted <- 
  read_csv("./data/corpora_three_gram_frequency_sorted.csv", col_names = TRUE,
           col_types = cols(
             word1 = col_character(),
             word2 = col_character(),
             word3 = col_character(),
             n = col_double()
           ))
corpora_four_gram_frequency_sorted <- 
  read_csv("./data/corpora_four_gram_frequency_sorted.csv", col_names = TRUE,
           col_types = cols(
             word1 = col_character(),
             word2 = col_character(),
             word3 = col_character(),
             word4 = col_character(),
             n = col_double()
           ))
corpora_five_gram_frequency_sorted <- 
  read_csv("./data/corpora_five_gram_frequency_sorted.csv", col_names = TRUE,
           col_types = cols(
             word1 = col_character(),
             word2 = col_character(),
             word3 = col_character(),
             word4 = col_character(),
             word5 = col_character(),
             n = col_double()
           ))
predict_the_next_word <- function(to_predict) {
  to_predict_word_count <- str_count(to_predict, "\\S+")
  
  predicted_words <- character()
  
  if (to_predict_word_count == 1) {
    word_1 <- tolower(to_predict)
    
    predicted_words <- (corpora_two_gram_frequency_sorted %>%
                          filter(word1 == word_1) %>%
                          head(3))$word2
    
  } 
  else if (to_predict_word_count == 2) {
    two_words <- 
      data_frame(lines = to_predict) %>%
      tokenize_ngram(n = 2) %>%
      separate_ngrams(2)
    
    word_1 <- two_words$word1
    word_2 <- two_words$word2
    
    predicted_words <- (corpora_three_gram_frequency_sorted %>%
                          filter(word1 == word_1 & word2 == word_2) %>%
                          head(3))$word3
    
    # if the predicted words are already three
    if (length(predicted_words) == 3) {
      return(predicted_words)
    } else {
      # back off to two-grams using 1 word
      to_append <- (corpora_two_gram_frequency_sorted %>%
                      filter(word1 == word_2) %>%
                      head(3))$word2
      
      predicted_words <- c(predicted_words, to_append) %>% unique()
    }
  } 
  else if (to_predict_word_count == 3) {
    three_words <- 
      data_frame(lines = to_predict) %>%
      tokenize_ngram(n = 3) %>%
      separate_ngrams(3)
    
    # separate words into individual variables
    word_1 <- three_words$word1
    word_2 <- three_words$word2
    word_3 <- three_words$word3
    
    predicted_words <- (corpora_four_gram_frequency_sorted %>%
                          filter(word1 == word_1 & word2 == word_2 &
                                   word3 == word_3) %>%
                          head(3))$word4
    
    # if the predicted words are already three
    if (length(predicted_words) == 3) {
      return(predicted_words)
    } else {
      # back off to three-grams using 2 words
      to_append <- (corpora_three_gram_frequency_sorted %>%
                      filter(word1 == word_2 & word2 == word_3) %>%
                      head(3))$word3
      
      predicted_words <- c(predicted_words, to_append) %>% unique()
      
      if (length(predicted_words) >= 3) {
        return(head(predicted_words, n = 3))
      } else {
        # back off to two-grams using one word
        to_append <- (corpora_two_gram_frequency_sorted %>%
                        filter(word1 == word_3) %>%
                        head(3))$word2
        # append to predicted words
        predicted_words <- c(predicted_words, to_append) %>% unique()
      }
    }
    
    
  } 
  else if (to_predict_word_count >= 4) {
    last_four_words <- 
      data_frame(lines = to_predict) %>%
      tokenize_ngram(n = 1) %>%
      tail(4) %>%
      tokenize_ngram(n = 4, retokenize = TRUE) %>%
      separate_ngrams(4)
    
    # the last four words separated into variables
    word_1 = last_four_words$word1
    word_2 = last_four_words$word2
    word_3 = last_four_words$word3
    word_4 = last_four_words$word4
    
    predicted_words <- (corpora_five_gram_frequency_sorted %>%
                          filter(word1 == word_1 & word2 == word_2 &
                                   word3 == word_3 & word4 == word_4) %>%
                          head(3))$word5 
    
    # if the predicted words are already three
    if (length(predicted_words) == 3) {
      return(predicted_words)
    } else {
      # back off to four-grams using three words
      to_append <- (corpora_four_gram_frequency_sorted %>%
                      filter(word1 == word_2 & word2 == word_3 &
                               word3 == word_4) %>%
                      head(3))$word4
      
      predicted_words <- c(predicted_words, to_append) %>% unique()
      
      if (length(predicted_words) >= 3) {
        return(head(predicted_words, n = 3))
      } else {
        # if predicted words are not enough, back-off to three grams
        # using last two words
        to_append <- (corpora_three_gram_frequency_sorted %>%
                        filter(word1 == word_3 & word2 == word_4) %>%
                        head(3))$word3
        
        predicted_words <- c(predicted_words, to_append) %>% unique()
        
        # return if we already have three word results
        if (length(predicted_words) >= 3) {
          return(head(predicted_words, n = 3))
        } else {
          # back off to two-grams using one word if still we don't have 
          # enough results
          to_append <- (corpora_two_gram_frequency_sorted %>%
                          filter(word1 == word_4) %>%
                          head(3))$word2
          # append to predicted words
          predicted_words <- c(predicted_words, to_append) %>% unique()
        }
      }
    }
  }
  
  # If no words are predicted, return "the" (being 'the is the most common word in 
  # English').
  if (length(predicted_words) == 0) {
    return(c("the", "to", "and"))
  } else {
    # return top 3 words
    return(head(predicted_words, n = 3))
  }
}


server <- function(input, output, session) {
  observe({
    to_predict <- trimws(input$to_predict_text_box)
    words_predicted <- predict_the_next_word(to_predict)
    
    first_word <- ifelse(!is.na(words_predicted[1]), 
                         yes = words_predicted[1], "")
    second_word <- ifelse(!is.na(words_predicted[2]), 
                          yes = words_predicted[2], "")
    third_word <- ifelse(!is.na(words_predicted[3]), 
                         yes = words_predicted[3], "")
    
    updateTextInput(session, "first_word", value = first_word)
    updateTextInput(session, "second_word", value = second_word)
    updateTextInput(session, "third_word", value = third_word)
  })
}

shinyApp(ui = htmlTemplate("www/index.html"), server)