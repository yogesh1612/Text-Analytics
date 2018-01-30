rm(list = ls()) # clears workspace
library("stringr")

Tokenize_String <- function(string){
  
  temp <- tolower(string)   # Lowercase  
  
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ") # anything not alphabetical followed by a space, replace!   
  
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ") # collapse one or more spaces into one space.   
  
  temp <- stringr::str_split(temp, " ")[[1]]
  
  indexes <- which(temp == "")
  if(length(indexes) > 0){ temp <- temp[-indexes]  } 
  return(temp)
  
} 

# This below is the Tokenizer for text blocks
Tokenize_Text_Block <- function(text){
  
  # Check to see if there is any text at all with another conditional
  if(length(text) == 0){ cat("There was no text in this document! \n"); stop }
  
  # Get rid of blank lines
  indexes <- which(text == "")
  if(length(indexes) > 0){ text <- text[-indexes]   }  
  
  # Loop through the lines in the text and use the append() function to 
  clean_text <- Tokenize_String(text[1])
  if (length(text)>1){  
    
    for(i in 2:length(text)){
      # add them to a vector 
      clean_text <- append(clean_text, Tokenize_String(text[i])) }   # i loop ends
    
  } # if condn ends
  
  # Calculate the number of tokens and unique tokens and return them in a named list object.
  num_tok <- length(clean_text)
  num_uniq <- length(unique(clean_text))
  
  to_return <- list(num_tokens = num_tok,   # output is list with 3 elements
                    unique_tokens = num_uniq, 
                    text = clean_text)    
  
  return(to_return)    
} # Tokenize_Text_Block func ends


# Reading in an ISB example (first two paras from Wiki)
text.para1 <- "Indian School of Business (ISB), is one of the prestigious business schools in India and is rated amongst the best in the world .The institute has various management programs with the PGP as its flagship course .Admission to the coveted management program is rigorous and has also the distinction of having one of the most stringent selectivity ratio internationally . The student cohort has a diverse mix with students coming in from top Indian and International Universities like the IITs , BITS Pilani, NITs , SRCC , ISI etc. There are students who are doctors , people from defence establishments ,sportsman and who have excelled in various other professions .ISB has integrated campuses at Mohali, Punjab and Hyderabad, Telangana, and is a non profit organization.[2] The school was founded by two senior executives of McKinsey & Company with the backing of government and is governed by a board comprising both Indian and non-Indian businessmen."
text.para2 <- "ISB has been ranked 27th in the world in the 2017 Financial Times Global MBA Rankings.[3] It is the first business school in Indian subcontinent to be accredited by the Association to Advance Collegiate Schools of Business.[4] In 2008, it became the youngest institution to find a place in global MBA rankings when it was ranked 20.[5] Indian School of Business accepts both GMAT and GRE scores for the admission process."


# tokenize these paragraphs
out.para1 = Tokenize_Text_Block(text.para1)
out.para2 = Tokenize_Text_Block(text.para2)
str(out.para1)   # check the structure or str() of the output 


# Merge the two token sets
merged.token.sets = unique(c(out.para1$text, out.para2$text))
length(merged.token.sets)

# since R is not good at dynamic allocation of 
# define the dtm object to populate
dtm.fp = matrix(0, nrow = 2, ncol = length(merged.token.sets))
row.names(dtm.fp) = seq(1:2)
colnames(dtm.fp) = merged.token.sets
dim(dtm.fp)

# define rows in a list    
docs.list = list(out.para1$text, out.para2$text)

# populate the dtm
for (i1 in 1:length(merged.token.sets)){    # looping over tokens
  
  for (i2 in 1:2){    # loop over documents 
    
    dtm.fp[i2, i1] = length(grep(merged.token.sets[19], docs.list[[2]]))
    
  }} # both i1, i2 loops end

dtm.fp[, 1:15]    # view first 15 cols of the DTM

# build matrix to hold (n-1) bigrams
n = length(out.para1$text); n
bigram.mat = matrix(0, nrow = (n-1), ncol = 2)

# loop over all tokens now
for (i1 in 1:(n-1)){
  
  bigram.mat[i1, 1] = out.para1$text[i1]
  bigram.mat[i1, 2] = out.para1$text[i1+1]
}

head(bigram.mat)

stitched = paste(bigram.mat[,1], bigram.mat[,2], sep=".")
bigram_df = data.frame(unigram1 = bigram.mat[,1], unigram2 = bigram.mat[,2], bigram = stitched)
head(bigram_df)


a0 = unique.matrix(bigram.mat); dim(a0)   # how many bigrams occurred multiple times.


# the same for trigrams is just as easy
trigram.mat = matrix(0, nrow = (n-2), ncol = 3)

for (i1 in 1:(n-2)){
  trigram.mat[i1, 1] = out.para1$text[i1]
  trigram.mat[i1, 2] = out.para1$text[i1+1]
  trigram.mat[i1, 3] = out.para1$text[i1+2]
}

head(trigram.mat)
