
# Load a real-world corpus & walk-thru the workflow below
nokia <- readLines('https://github.com/sudhir-voleti/sample-data-sets/raw/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')


# Function 1 to clean Text
clean_text_tokenizer <- function(text){ # pass text vector as an argument
  #load essential packages
  if (!require(tidytext)) {install.packages("tidytext")}
  if (!require(dplyr)) {install.packages("dplyr")}
  if (!require(tm)) {install.packages("tm")}
  
  
  temp  =  gsub("(<.*>)|([^[:alnum:]])", " ", text) # Regex to remove html tags       
  temp  =  iconv(temp, "latin1", "ASCII", sub="")   # Keep only ASCII characters
  temp  =  tolower(temp)                            # Convert to lower case characters
  temp  =  gsub("\\d", "", temp)                    # removing all numbers   
  temp =  stripWhitespace(temp)                     # removing whitespaces
  temp  =  gsub("^\\s+|\\s+$", "", temp)            # removing leading and trailing whitespaces
  temp <- data_frame(temp)
  temp <- tibble::rowid_to_column(temp, "document")
  #temp <- data_frame(document = 1:120, text = temp)
  temp <- temp %>% unnest_tokens(word, temp)
  stopwords = tm::stopwords('english') 
  
  # temp  =  removeWords(temp,stopwords)            # removing stopwords created above
  
 # my_stop_words <- c("phone","samsung", "phones") # my stope words defined
  # custom_stop_words <- bind_rows(data_frame(word = my_stop_words, 
                                         # lexicon = c("custom")), stop_words) 
  # custom_stop_words  = unique(custom_stop_words) # de duplicating
  temp <- temp %>%
    anti_join(stop_words)
  
  return(temp) }


# test above functions
# cleaned_text <- clean_text_tokenizer(nokia)


# Function 2 Create a DTM
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
 
 
 # Function 3 -  TF-IDF Matrix
 
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


# Function 4 Plot word cloud
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

nokia %>% clean_text_tokenizer() %>% dtm_creator() %>% plot_wrd_cloud 
nokia %>% clean_text_tokenizer() %>% tf_idf_creator() %>% plot_wrd_cloud 




