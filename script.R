library(tidyverse)
library(tidytext)
library(textstem)
library(tm)
library(data.table)
library(randomForest)
set.seed(42)

### Data Extraction ####
# dataset is from http://ai.stanford.edu/~amaas/data/sentiment/
extract_rating <- function(file_name){
     rating <- regmatches(file_name, regexpr("([0-9])(?=\\.txt)", file_name, perl=T))
     if(rating == "0") "10" else rating
}
to_dt <- function(files_list){
     l <- lapply(files_list, function(x){c(readLines(x), extract_rating(x))})
     data.table(matrix(unlist(l), nrow=length(l), byrow=T))[
          sample(1:.N)][ #shuffle rows
          ,ID:=.I][ #add ID 
          ,c(3,1,2)] #move ID column to the front
}
files_list_train_pos <- paste0("data/train/pos/", list.files("data/train/pos"))
files_list_train_neg <- paste0("data/train/neg/", list.files("data/train/neg"))
files_list_test_pos <- paste0("data/train/pos/", list.files("data/train/pos"))
files_list_test_neg <- paste0("data/train/pos/", list.files("data/train/pos"))

train <- to_dt(c(files_list_train_neg, files_list_train_pos)) 
test <- to_dt(c(files_list_test_neg, files_list_test_pos)) 

### Data Preprocessing ####
train_dtm <- train %>% 
     unnest_tokens(word, V1) %>% 
     anti_join(stop_words) %>% 
     filter(!grepl("br", word, perl=T)) %>%
     mutate(word = lemmatize_words(word)) %>%
     count(ID, word) %>% 
     bind_tf_idf(word, ID, n) %>% 
     cast_dtm(document = ID, term = word, 
              value = n, weighting = tm::weightTfIdf) %>%
     removeSparseTerms(.95)

### Train CV Split ####
train_x <- train_dtm[1:20000,] %>% as.matrix() %>% as.data.table()
train_y <- train[1:20000,"V2"]
cv_x <- train_dtm[20001:25000,] %>% as.matrix() %>% as.data.table()
cv_y <- train[20001:25000,"V2"]

to_binary <- function(y){
     #converts multiclass labels to binary labels
     y %>% unlist() %>%
          lapply(function(x){if(as.numeric(x) > 5) 1 else 0})
}

### Training ####
rfc <- randomForest(x = train_x, 
                    y = train_y %>% to_binary() %>% unlist() %>% as.factor(),
                    ntree = 50)

cv_pred_y <- rfc %>% predict(cv_x)
rfc_acc <- sum(as.numeric(as.character(cv_pred_y)) == unlist(to_binary(cv_y))) / nrow(cv_y)
