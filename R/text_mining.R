# https://www.tidytextmining.com/sentiment.html

library(tidytext)

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books()

tidy_books_word <- tidy_books %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()



tidy_books_senti <- tidy_books_word %>%
  unnest_tokens(word, text)
