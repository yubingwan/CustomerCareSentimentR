# https://www.tidytextmining.com/sentiment.html

library(tidytext)

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

write_csv(afinn, "data-raw/2_sentiment_libraries/afinn.csv")
write_csv(bing, "data-raw/2_sentiment_libraries/bing.csv")
write_csv(nrc, "data-raw/2_sentiment_libraries/nrc.csv")
