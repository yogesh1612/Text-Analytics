# Load a real-world corpus & walk-thru the workflow below
nokia <- readLines('https://github.com/sudhir-voleti/sample-data-sets/raw/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')
class(nokia)
head(nokia)
length(nokia)

x = nokia[1]    # use x as sample input and execute below func line by line to see what it does.

Clean_Text <- function(x){   # x = a text doc
  
  require("tm")    # always invoke required libraries
  
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  
  # removing standard english stopwords like 'the', 'an' etc
  stopwords = tm::stopwords('english')      # tm's inbuilt stopword list; 'tokenizer' too has a stopwords() func, hence 'tm::'
  
  x  =  removeWords(x,stopwords)            # removing stopwords created above
  x  =  stripWhitespace(x)                  # removing white space
  #  x  =  stemDocument(x)                   # can stem doc if needed. Will need Snowball libraries if so.
  
  return(x) }    # Clean_Text() func ends

# Try the full func on one doc x:
system.time({ x = Clean_Text(x) })
x

# note syntax if unfamiliar with apply()
system.time({
  nokia_cleaned = lapply(nokia, function(x) {Clean_Text(x)})
}) 



if (!(require(rJava))) {install.packages("rJava")}
if (!(require(RWeka))) {install.packages("RWeka")}

require(RWeka)    # calls rJava as a dependency
# define specs for your n-gram pull below
ngram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) #invoking RWeka for ngram tokenizing   
system.time({  bigram_nokia = ngram(x)  })  

class(bigram_nokia)

length(bigram_nokia)   # how many bigrams in all?


bigram_nokia[1:15]     # view a few


nokia_cleaned = sapply(nokia, Clean_Text, simplify=TRUE)    # check '?sapply'
class(nokia_cleaned)
length(nokia_cleaned)

corpus = nokia_cleaned   # use this line by line in func below to see what it does



Build_DTM <- function(corpus){
  
  require(tm)
  corpus1 = Corpus(VectorSource(corpus))   # formats text input for tm feed
  dtm = DocumentTermMatrix(corpus1); dim(dtm)
  rownames(dtm) = seq(1:length(corpus)) 
  
  # remove blank docs and blank cols
  if_empty_rows = (rowSums(as.matrix(dtm)) == 0)   # logical vector if_empty
  if_empty_cols = (colSums(as.matrix(dtm)) == 0)   # logical vector if_empty
  dtm = dtm[!(if_empty_rows), !(if_empty_cols)]   # drop any row with no tokens
  dim(dtm)
  
  # we can weigh the DTM differentially, e.g., under TF-IDF weighing (more on this later)
  dtm_idf = t(weightTfIdf(t(dtm), normalize = T)) 
  
  # rearrange terms in descending order of Tf and return
  decr_colsum = order(colSums(as.matrix(dtm)), decreasing=TRUE) 
  dtm_ordered = dtm[, decr_colsum]
  return(dtm_ordered)   }    # func ends



if (!(require(wordcloud))) {install.packages("worcloud")}
library(wordcloud)

Plot_Wordc = function(dtm){    # plot wordcloud func opens. 
  
  dtm = as.matrix(dtm)  
  dtm_colsum = apply(dtm, 2, sum)  
  
  min1 = min(100, length(dtm_colsum))  # no more than 100 terms in wordcloud  
  words = colnames(dtm)[1:min1]  
  freq = 10 * dtm_colsum/mean(dtm_colsum)  # rescaling for better viewing
  
  # if (max(freq) > 100) {freq = log(100* freq/max(freq)) } 
  
  wordcloud(words,  # wordcloud func begins
            freq,           
            scale = c(8, 0.3),  # can change this to adjust font scale
            colors=1:10)        # randomly choose between 10 colors
  
} # func ends

# try func on nokia dtm output
Plot_Wordc(dtm_nokia)



