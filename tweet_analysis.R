#TEXT MINING
library(tidyverse)
library(lubridate)
library(scales)
library(dslabs)
data("trump_tweets")
head(trump_tweets)
trump_tweets %>% tail
names(trump_tweets)
?trump_tweets
trump_tweets$text[16413] %>%
  str_wrap(width = options()$width) %>%
  cat
trump_tweets %>% count(source) %>% 
  arrange(desc(n)) %>%
  head(5)
campaign_tweets <- trump_tweets %>%
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") &
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at) %>%
  as_tibble()
campaign_tweets%>%head()
sum(is.na(campaign_tweets$source))

campaign_tweets %>% 
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source)%>%
  mutate(percent = n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(hour, percent, color = source))+
  geom_line() +
  geom_point()+
  scale_y_continuous(labels = percent_format())+
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

library(tidytext)
poem <- c("Roses are red,", "Violets are blue,", 
          "Sugar is sweet,", "And so are you.")  
example <- tibble(line = c(1,2,3,4), text = poem)  
example  %>% unnest_tokens(word, text)

#Now tweets 
i <- 3008
campaign_tweets$text[i] %>% str_wrap(width = 65) %>%
  cat()

  
links <- "https://t.co/[A-Za-z\\d]+|&amp;"
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  pull(word)

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, "")) %>%
  unnest_tokens(word, text, token = "tweets")

tweet_words %>% count(word) %>% 
  arrange(desc(n))
stop_words

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word & 
           !str_detect(word, "^\\d+$")) %>% 
  mutate(word = str_replace(word, "^'", ""))

tweet_words %>% count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  pivot_wider(names_from = "source", values_from = "n", values_fill = 0) %>%
  mutate(or = (Android + 0.5)/(sum(Android) - Android + 0.5)/
           ((iPhone + 0.5)/(sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% filter(Android+iPhone >100) %>%
  arrange(desc(or))









  