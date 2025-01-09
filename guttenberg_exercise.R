library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
data("gutenberg_metadata")
str(gutenberg_metadata)

x <- str_detect(gutenberg_metadata$title, "Pride and Prejudice")
sum(x, na.rm = T)    
gutenberg_works(x)

book <- gutenberg_download(1342, mirror="http://mirrors.xmission.com/gutenberg/")

words <- book %>% unnest_tokens(word, text)
nrow(words)

sig_words <- words %>% 
  filter(!word %in% stop_words$word)
nrow(sig_words)

dgt <-  str_detect(sig_words$word, "\\d")
sum(dgt)
pure_words <- sig_words$word[!dgt]
length(pure_words)

word_table <- as.data.frame(table(pure_words))
more_100 <- word_table %>% filter(Freq > 100)
nrow(more_100)
more_100 <- more_100 %>% arrange(desc(Freq))
more_100


words <- words %>% anti_join(stop_words)
afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(words, afinn, by = "word" )
nrow(afinn_sentiments)
afinn_sentiments %>% filter(value>= 0) %>% nrow()
afinn_sentiments %>% filter(value == 4) %>% nrow()
