## code to prepare `DATASET` dataset goes here
# install.packages("tidytext")
# install.packages("textdata")
# install.packages("purrr")

library(tidyverse)
library(tidytext)
library(textdata)
library(purrr)

# List the file names
file_names <- list.files(
  file.path(getwd(), "data-raw"), pattern = "*.txt", recursive = T, full.names = T)

# Read the content of each file and create a data frame
data_list <- lapply(file_names, function(file) {
  text <- readLines(file, warn = FALSE)  # Read the content of the file
  data.frame(filepath = file, text = paste(text, collapse = "\n"), stringsAsFactors = FALSE)
})

# Combine the data frames into a single data frame
df <- purrr::map_dfr(data_list, rbind) %>% 
  mutate(filename = basename(filepath)) %>% 
  select(filename, text, filepath) %>%
  mutate(word_count_raw = str_count(text, "\\S+")) 

df_unnest <- df %>% 
  unnest_tokens(word, text)

df_unnest_sum <- df_unnest %>% 
  group_by(filename) %>% 
  summarize(word_count_all = n(), word_count_all_unique = n_distinct(word)) %>% 
  ungroup()

# View(stop_words) 
df_tokens <- df_unnest %>% 
  anti_join(stop_words) %>% ## filter stop words by tidytest data
  group_by(filename) %>% 
  mutate(linenumber = row_number()) %>%
  ungroup() 

df_tokens_sum <- df_tokens %>% 
  group_by(filename) %>% 
  summarize(word_count_token = n(), word_count_token_unique = n_distinct(word)) %>% 
  ungroup()

################################################################################
# Assign sentiment scores using the "bing" lexicon
df_senti_bing <- df_tokens %>%
  inner_join(get_sentiments("bing"))

df_senti_bing_score <- df_senti_bing %>%
  count(filename, sentiment) %>% # index = linenumber %/% 80, 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  rename_at(vars(-matches("filename")), ~str_c(.x, "_score_bing"))

df_senti_bing_sum <- df_senti_bing %>% 
  group_by(filename) %>% 
  summarize(word_count_bing_senti = n(), word_count_bing_senti_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_bing_pos_sum <- df_senti_bing %>% 
  filter(sentiment == "positive") %>% 
  group_by(filename) %>% 
  summarize(word_count_bing_positive = n(), word_count_bing_positive_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_bing_neg_sum <- df_senti_bing %>% 
  filter(sentiment == "negative") %>% 
  group_by(filename) %>% 
  summarize(word_count_bing_negative = n(), word_count_bing_negative_unique = n_distinct(word)) %>% 
  ungroup()

################################################################################
df_senti_afinn <- df_tokens %>%
  inner_join(get_sentiments("afinn")) %>% 
  mutate(sentiment = case_when(value > 0 ~ "positive", value < 0 ~ "negative", TRUE ~ NA_character_))

df_senti_afinn_score <- df_senti_afinn %>%
  group_by(filename) %>%
  summarize(
    positive_score_afinn = sum(ifelse(value > 0, value, 0)),
    negative_score_afinn = sum(ifelse(value < 0, value, 0)),
    sentiment_score_afinn = sum(value)) %>% 
  ungroup()
  
df_senti_afinn_sum <- df_senti_afinn %>% 
  group_by(filename) %>% 
  summarize(word_count_afinn_senti = n(), word_count_afinn_senti_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_afinn_pos_sum <- df_senti_afinn %>% 
  filter(sentiment == "positive") %>% 
  group_by(filename) %>% 
  summarize(word_count_afinn_positive = n(), word_count_afinn_positive_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_afinn_neg_sum <- df_senti_afinn %>% 
  filter(sentiment == "negative") %>% 
  group_by(filename) %>% 
  summarize(word_count_afinn_negative = n(), word_count_afinn_negative_unique = n_distinct(word)) %>% 
  ungroup()


################################################################################
nrc_senti <- get_sentiments("nrc")
table(nrc_senti$sentiment) 

nrc_pos_neg <- distinct(filter(nrc_senti, sentiment %in% c("positive", "negative"))) %>% 
  filter(!word %in% word[duplicated(word)])

df_senti_nrc <- df_tokens %>%
  inner_join(nrc_pos_neg)

df_senti_nrc_score <- df_senti_nrc %>%
  count(filename, sentiment) %>% # index = linenumber %/% 80, 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  rename_at(vars(-matches("filename")), ~str_c(.x, "_score_nrc"))

df_senti_nrc_sum <- df_senti_nrc %>% 
  group_by(filename) %>% 
  summarize(word_count_nrc_senti = n(), word_count_nrc_senti_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_nrc_pos_sum <- df_senti_nrc %>% 
  filter(sentiment == "positive") %>% 
  group_by(filename) %>% 
  summarize(word_count_nrc_positive = n(), word_count_nrc_positive_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_nrc_neg_sum <- df_senti_nrc %>% 
  filter(sentiment == "negative") %>% 
  group_by(filename) %>% 
  summarize(word_count_nrc_negative = n(), word_count_nrc_negative_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_nrc_trust_sum <- df_tokens %>%
  inner_join(filter(nrc_senti, sentiment %in% c("trust"))) %>% 
  group_by(filename) %>% 
  summarize(word_count_nrc_trust = n(), word_count_nrc_trust_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_nrc_surprise_sum <- df_tokens %>%
  inner_join(filter(nrc_senti, sentiment %in% c("surprise"))) %>% 
  group_by(filename) %>% 
  summarize(word_count_nrc_surprise = n(), word_count_nrc_surprise_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_nrc_anticipation_sum <- df_tokens %>%
  inner_join(filter(nrc_senti, sentiment %in% c("anticipation"))) %>% 
  group_by(filename) %>% 
  summarize(word_count_nrc_anticipation = n(), word_count_nrc_anticipation_unique = n_distinct(word)) %>% 
  ungroup()

################################################################################
loughran_senti <- get_sentiments("loughran")
table(loughran_senti$sentiment) 

loughran_pos_neg <- distinct(filter(loughran_senti, sentiment %in% c("positive", "negative"))) %>% 
  filter(!word %in% word[duplicated(word)])

df_senti_loughran <- df_tokens %>%
  inner_join(loughran_pos_neg)

df_senti_loughran_score <- df_senti_loughran %>%
  count(filename, sentiment) %>% # index = linenumber %/% 80, 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  rename_at(vars(-matches("filename")), ~str_c(.x, "_score_loughran"))

df_senti_loughran_sum <- df_senti_loughran %>% 
  group_by(filename) %>% 
  summarize(word_count_loughran_senti = n(), word_count_loughran_senti_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_loughran_pos_sum <- df_senti_loughran %>% 
  filter(sentiment == "positive") %>% 
  group_by(filename) %>% 
  summarize(word_count_loughran_positive = n(), word_count_loughran_positive_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_loughran_neg_sum <- df_senti_loughran %>% 
  filter(sentiment == "negative") %>% 
  group_by(filename) %>% 
  summarize(word_count_loughran_negative = n(), word_count_loughran_negative_unique = n_distinct(word)) %>% 
  ungroup()

df_senti_loughran_litigious_sum <- df_tokens %>%
  inner_join(filter(loughran_senti, sentiment %in% c("litigious"))) %>% 
  group_by(filename) %>% 
  summarize(word_count_loughran_litigious = n(), word_count_loughran_litigious_unique = n_distinct(word)) %>% 
  ungroup()


article_bing <- df_unnest_sum %>% 
  left_join(df_tokens_sum) %>% 
  left_join(df_senti_bing_score) %>% 
  left_join(df_senti_bing_sum) %>% 
  left_join(df_senti_bing_pos_sum) %>% 
  left_join(df_senti_bing_neg_sum) %>%
  left_join(df_senti_afinn_score) %>% 
  left_join(df_senti_afinn_sum) %>% 
  left_join(df_senti_afinn_pos_sum) %>% 
  left_join(df_senti_afinn_neg_sum) %>%
  left_join(df_senti_nrc_score) %>% 
  left_join(df_senti_nrc_sum) %>% 
  left_join(df_senti_nrc_pos_sum) %>% 
  left_join(df_senti_nrc_neg_sum) %>%
  left_join(df_senti_nrc_trust_sum) %>%
  left_join(df_senti_nrc_surprise_sum) %>%
  left_join(df_senti_nrc_anticipation_sum) %>%
  left_join(df_senti_loughran_score) %>% 
  left_join(df_senti_loughran_sum) %>% 
  left_join(df_senti_loughran_pos_sum) %>% 
  left_join(df_senti_loughran_neg_sum) %>%
  left_join(df_senti_loughran_litigious_sum) %>%
  mutate_all(~replace(., is.na(.), 0))

write_csv(article_bing, "data-interm/article_word_counts_sentiment_scores_20230728.csv")


colnm_dictionary <- data.frame(
  colnames = names(article_bing)[-1], 
  explain = c(
    "total words in each article", 
    "total unique words in each article", 
    "total filtered words that can be considered as tokens to map with words in the 4 lexicons", 
    "total filtered unique words that can be considered as tokens to map with words in the 4 lexicons", 
    "negative sentiment score of each article mapped to bing lexicon (this is equal to column word_count_bing_negative, as in bing lexions each word was equally asigned a weight/score 1/-1 for positive or negative sentiment)", 
    "positive sentiment score of each article mapped to bing lexicon (this is equal to column word_count_bing_positive, as in bing lexions each word was equally asigned a weight/score 1/-1 for positive or negative sentiment)", 
    "overall sentiment score of articles by bing lexicon, i.e. positive_score_bing - negative_score_bing", 
    "total words selected as sentiment indicators by bing lexicon", 
    "total unique words selected as sentiment indicators by bing lexicon", 
    "total words selected as POSITIVE sentiment indicators by bing lexicon", 
    "total unique words selected as POSITIVE sentiment indicators by bing lexicon", 
    "total words selected as NEGATIVE sentiment indicators by bing lexicon", 
    "total unique words selected as NEGATIVE sentiment indicators by bing lexicon", 
    "positive sentiment score of each article mapped to afinn lexicon (this is NOT equal to column word_count_afinn_negative, as in this lexions each word was NOT equally asigned a weight/score 1/-1 for positive or negative sentiment)", 
    "negative sentiment score of each article mapped to afinn lexicon (this is NOT equal to column word_count_afinn_positive, as in this lexions each word was NOT equally asigned a weight/score 1/-1 for positive or negative sentiment)", 
    "overall sentiment score of articles by afinn lexicon, i.e. positive_score_afinn + negative_score_afinn",
    "total words selected as sentiment indicators by afinn lexicon", 
    "total unique words selected as sentiment indicators by afinn lexicon", 
    "total words selected as POSITIVE sentiment indicators by afinn lexicon", 
    "total unique words selected as POSITIVE sentiment indicators by afinn lexicon", 
    "total words selected as NEGATIVE sentiment indicators by afinn lexicon", 
    "total unique words selected as NEGATIVE sentiment indicators by afinn lexicon", 
    "this is similar as for bing lexicon",
    "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "this is similar as for bing lexicon", 
    "", "", "", "", "", "", "", "", "", ""
  )
)

write_csv(colnm_dictionary, "data-interm/article_word_counts_sentiment_scores_colname_dictionary.csv")

################################################################################

library(ggplot2)
ggplot(article_sentiment_bing, aes(sentiment, filename, fill = filename)) +
  geom_col(show.legend = FALSE) 

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
