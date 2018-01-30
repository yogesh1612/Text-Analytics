install.packages("stringr")
library("stringr")
my_string <- "My PHONE number is +91 40 2318 7106, is it?!"
my_string
lower_string <- tolower(my_string)   # makes lower case
lower_string

second_string <- "OK folks, Second sentence, coming up."; 
second_string


my_string <- paste(my_string,    # try ?paste
                   second_string, 
                   sep = " ")
my_string

my_string_vector <- str_split(my_string, "!")[[1]]

my_string_vector

grep("\\?", my_string_vector)    # gives location of vector that contains match

grepl("\\?", my_string_vector[1])   # logical grep() for binary T/F output


str_replace_all(my_string, "e","___")

my_string


# using regex to extract all numbers from a string
str_extract_all(my_string,"[0-9]+") [[1]]
# regex "[0-9]+" translates to "match any substring that is one or more contiguous numbers"



str_extract_all(my_string, "[^a-zA-Z\\s]+") # every char not in a-z, A-Z or space \\s

str_extract_all(my_string, "[\\d]+") # every char that is a digit

## Cleaning Text and Tokenization from First Principles (using Regex) ##

# how to read in and clean a raw input text file.
clean_string <- function(string){
  
  require(stringr)
  
  temp <- tolower(string)   # Lowercase
  
  # Remove everything that is not a number or letter (may want to keep more stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ") # anything not alphabetical followed by a space, replace!
  # Shrink down to just one white space using '+' regex or for repeats >1
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ") # collapse one or more spaces into one space.
  
  # Split it (into tokens? yup.)
  temp <- stringr::str_split(temp, " ")[[1]]
  
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
  
} 


sentence <- "All the world's a stage, and all the men and women merely players: they have their exits and their entrances; and one man in his time plays many parts.' "
clean_sentence <- clean_string(sentence)   # invoking the function
print(clean_sentence)   # view func output

unique(clean_sentence)  # de-duplicated version.


