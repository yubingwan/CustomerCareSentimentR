## code to prepare `DATASET` dataset goes here
# install.packages("tidytext")
# install.packages("textdata")
library(tidyverse)
library(tidytext)
library(textdata)

# List the file names
file_names <- list.files(
  file.path(getwd(), "data-raw"), pattern = "*.txt", recursive = T, full.names = T)

# Read the content of each file and create a data frame
data_list <- lapply(file_names, function(file) {
  text <- readLines(file, warn = FALSE)  # Read the content of the file
  data.frame(filepath = file, text = paste(text, collapse = "\n"), stringsAsFactors = FALSE)
})

# Combine the data frames into a single data frame
df <- do.call(rbind, data_list) %>% 
  mutate(filename = basename(filepath)) %>% 
  select(filename, text, filepath)

# View(stop_words) 
df_tokens <- df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% ## filter stop words by tidytest data
  group_by(filename) %>% 
  mutate(linenumber = row_number()) %>%
  ungroup() 

# Assign sentiment scores using the "bing" lexicon
df_senti_bing <- df_tokens %>%
  inner_join(get_sentiments("bing"))

article_sentiment_bing <- df_senti_bing %>%
  inner_join(get_sentiments("bing")) %>%
  count(filename, sentiment) %>% # index = linenumber %/% 80, 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(article_sentiment_bing, aes(sentiment, filename, fill = filename)) +
  geom_col(show.legend = FALSE) 

df_senti_afinn <- df_tokens %>%
  inner_join(get_sentiments("afinn"))

nrc_senti <- get_sentiments("nrc")
table(nrc_senti$sentiment) 

bing_senti <- get_sentiments("bing")
afinn_senti <- get_sentiments("afinn")


nrc_joy <- filter(nrc_senti, sentiment == "joy")

df_senti_nrc <- df_tokens %>%
  inner_join(get_sentiments("nrc"))

# Summarize sentiment scores for each document
df_sentiment_summary <- df_sentiment %>%
  count(word, sort = TRUE) %>%
  group_by(filename) %>%
  summarise(sentiment_score = sum(sentiment))

# View the resulting data frame
print(df)
