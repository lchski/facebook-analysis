library(tidyverse)
library(lubridate)
library(jsonlite)

library(helpers)

message_folders_prefix <- "data/source/2020-01-29-facebook-lucascherkewski/messages/inbox/"

message_folders <- fs::dir_ls(message_folders_prefix) %>%
  enframe(name = NULL) %>%
  mutate(value = as.character(value)) %>%
  mutate(value = str_remove(value, fixed(message_folders_prefix))) %>%
  rename(folder = value)
