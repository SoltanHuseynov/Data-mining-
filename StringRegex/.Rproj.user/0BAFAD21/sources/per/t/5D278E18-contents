if(!require(stringi)) install.packages("stringi")
if(!require(stringr)) install.packages("stringr")
if(!require(tm)) install.packages("tm")
if(!require(NLP)) install.packages("NLP")
if(!require(tidytext)) install.packages("tidytext")
if(!require(dplyr)) install.packages("dplyr")

library(dplyr)
library(tidytext)
library(tm)
library(NLP)
library(stringi)
library(stringr)

#gereksiz işaretleri silme function
replace_all <-function(input_string){

      input_string<- str_replace_all(input_string,"http[^[:space:]]*", "")
      input_string<- str_replace_all(input_string,"#\\S+", "")
      input_string<- str_replace_all(input_string,"#\\S+", "")
      input_string<- str_replace_all(input_string,"[[:punct:][:blank:]]+"," ")

      input_string<- str_to_lower(input_string,"tr")
      input_string<- removeNumbers(input_string)

      input_string<- str_replace_all(input_string,"[<].*[>]"," ")
      input_string<- gsub("\uFFFD","",input_string,fixed = TRUE)
      input_string<- gsub("\n","",input_string,fixed = TRUE)
      input_string<- str_replace_all(input_string,"[^[:alnum:]]", " ")

  return(input_string)
}

# gereksiz kelimeleri silme function
removeVoca <-function(input_data,stopwords){
  input_data %>%
    # text İD'ni değiştirerek word İD'ni atayak
    unnest_tokens(word,text) %>%
    anti_join(stopwords,by="word")

}
