try(require(dplyr) || install.packages("dplyr"))
library(dplyr)

require(tidytext) || install.packages("tidytext")
library(tidytext)

try(require(tidyr) || install.packages("tidyr"))
library(tidyr)

require(tibble)

text <- c("Laura is the mother of a newborn and, like many mothers, felt strongly about providing her baby the best, most nutritious food options available. Her challenge was to research, find, purchase and repurchase nutritious options, with far too many choices, conflicting advice and amid the chaos of parenting a small child. The chaos spanned from diapers to Cheerios, and even when she felt certain about her choice, she then had to navigate when, where and how to get it at the best price and greatest convenience. Laura's experience provides the perfect illustration of the power, and predicament, of customer journey analytics as it relates to digital commerce and marketing.")

text  # view the text read-in.

# build dataframe out of input text to start tidytext processing.
textdf = data_frame(text = text) 
textdf     # yields 1x1 tibble.  



textdf %>% unnest_tokens(word, text) %>% head()  # tokenizing words


# tokenizing sentences
textdf %>% unnest_tokens(sentence, text, token = "sentences") %>% head()


(textdf %>% unnest_tokens(sentence, text, token = "sentences"))$sentence[2]     # e.g., show 2nd sentence.


# tokenizing ngrams
textdf %>% unnest_tokens(ngram, text, token = "ngrams", n = 2) %>% head()    # yields (#tokens -1 ) bigrams

# now do count() & ID most occurring bigrams etc.
(textdf %>% unnest_tokens(ngram, text, token = "ngrams", n = 2))$ngram[5:10] 


# example reading in an amazon nokia corpus?
a0 = readLines('https://github.com/sudhir-voleti/sample-data-sets/raw/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')
# a0 = readLines(file.choose())    # alternately, do this to read file from local machine

text <- a0
text  =  gsub("<.*?>", " ", text)              # regex for removing HTML tags
length(text)


textdf = data_frame(text = text) # yields 120x1 tibble. i.e., each doc = 1 row here.


# trying some tokenization ops
(textdf %>% unnest_tokens(word, text) %>% head())[,1]     # word tokenization 

textdf %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) %>%   #counts & sorts no. of occurrences of each item in 'word' column 
  rename(count = n) %>%     # renames the count column from 'n' (default name) to 'count'.
  head() 

data(stop_words)


textdf %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) %>%   
  rename(count = n) %>%
  anti_join(stop_words) %>%    # try ?anti_join
  head()


