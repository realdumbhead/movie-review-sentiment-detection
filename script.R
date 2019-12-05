library(tidyverse)
library(data.table)
set.seed(42)

### Data Extraction ####
# dataset is from http://ai.stanford.edu/~amaas/data/sentiment/
extract_rating <- function(file_name){
     regmatches(file_name, regexpr("([0-9])(?=\\.txt)", file_name, perl = T))
}
to_dt <- function(files_list){
     l <- lapply(files_list, function(x){c(readLines(x), extract_rating(x))})
     data.table(matrix(unlist(l), nrow=length(l), byrow=T))
}
files_list_train_pos <- paste0("data/train/pos/", list.files("data/train/pos"))
files_list_train_neg <- paste0("data/train/neg/", list.files("data/train/neg"))
files_list_test_pos <- paste0("data/train/pos/", list.files("data/train/pos"))
files_list_test_neg <- paste0("data/train/pos/", list.files("data/train/pos"))


train <- rbind(to_dt(files_list_train_neg), to_dt(files_list_train_pos)) %>% slice(sample(1:n()))
test <- rbind(to_dt(files_list_test_neg), to_dt(files_list_test_pos)) %>% slice(sample(1:n()))




