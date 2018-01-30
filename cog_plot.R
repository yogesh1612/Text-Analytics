
nokia <- readLines('https://github.com/sudhir-voleti/sample-data-sets/raw/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')
text <- nokia

# Function 1 
clean_text_tokenizer <- function(text){ 
  if (!require(tidytext)) {install.packages("tidytext")}
  if (!require(dplyr)) {install.packages("dypyr")}
  if (!require(tm)) {install.packages("tm")}
  temp  =  gsub("(<.*>)|([^[:alnum:]])", " ", text)         
  temp  =  iconv(temp, "latin1", "ASCII", sub="") 
  temp  =  tolower(temp)                          
  temp  =  gsub("\\d", "", temp)                           
  temp =  stripWhitespace(temp)
  temp  =  gsub("^\\s+|\\s+$", "", temp)          
  temp <- data_frame(document = 1:120, text = temp)
  temp <- temp %>% unnest_tokens(word, text)
  my_stop_words <- c("phone","samsung", "phones") # my stope words defined
  custom_stop_words <- bind_rows(data_frame(word = my_stop_words, 
                                            lexicon = c("custom")), stop_words) 
  custom_stop_words  = unique(custom_stop_words) # de duplicating
  
  temp <- temp %>%
    anti_join(custom_stop_words)
  return(temp) }




# Function 2 A - foor creating DTM matrix 
dtm_creator <- function(cleaned_text){
  if (!require(tidytext)) {install.packages("tidytext")}
  if (!require(dplyr)) {install.packages("dypyr")}
  if (!require(tm)) {install.packages("tm")}
  temp <- cleaned_text %>% count(document, word, sort = TRUE) %>%
    ungroup()
  # creating DTM 
  dtm_temp <- temp %>%
    cast_sparse(document, word, n)
  return(dtm_temp) } 


# Function 2 B - for TF-IDF Matrix

tf_idf_creator <- function(cleaned_text){
  if (!require(tidytext)) {install.packages("tidytext")}
  if (!require(dplyr)) {install.packages("dypyr")}
  if (!require(tm)) {install.packages("tm")}
  
  temp <- cleaned_text  %>% count(document, word, sort = TRUE) %>%
    ungroup()
  
  total_temp <- temp %>%  group_by(document) %>% 
    summarize(total = sum(n))  
  
  temp <- left_join(temp, total_temp)
  
  # creating TF-IDF matrix 
  temp <- temp %>%
    bind_tf_idf(word, document, n)
  
  tf_tdf_matrix <- temp %>%
    cast_sparse(document, word, tf_idf)
  
  return(tf_tdf_matrix) }


# Function 3 
plot_wrd_cloud = function(dtm_temp){    
  if (!require(wordcloud)) {install.packages("wordcloud")}
  dtm = as.matrix(dtm_temp)  
  dtm_colsum = apply(dtm, 2, sum)  
  min_word = min(50, length(dtm_colsum))   
  words = colnames(dtm)[1:min_word] 
  freq = 10 * dtm_colsum/mean(dtm_colsum)  
  wordcloud(words,  
            freq,           
            scale = c(8, 0.3),  
            colors=1:10)       
}  

# Running all three functions together through piping

#nokia %>% clean_text_tokenizer() %>% dtm_creator() %>% plot_wrd_cloud 
#nokia %>% clean_text_tokenizer() %>% tf_idf_creator() %>% plot_wrd_cloud 
cleaned_text <- clean_text_tokenizer(nokia)
head(cleaned_text)
dim(cleaned_text)

cleaned_dtm <- dtm_creator(cleaned_text)
head(cleaned_dtm)
class(cleaned_dtm)

cleaned_tfidf <- tf_idf_creator(cleaned_text)
head(cleaned_tfidf)
class(cleaned_tfidf)

plot_wrd_cloud(cleaned_dtm)
library(wordcloud)


cog_plot<- function(text,min_n){
  if (!require(widyr)) {install.packages("widyr")}
  if (!require(ggplot2)) {install.packages("ggplot2")}
  if (!require(ggraph)) {install.packages("ggraph")}
  if (!require(igraph)) {install.packages("igraph")}
  
    cleaned_text <- clean_text_tokenizer(text)
    dsc_word_pair <- cleaned_text %>% pairwise_count(word,document,sort=TRUE,upper = FALSE)
    dsc_word_pair %>%
      filter(n >= min_n) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE,
                     point.padding = unit(0.2, "lines")) +
      theme_void()
    
}


cog_plot(nokia,5)
