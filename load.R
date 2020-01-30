library(tidyverse)
library(lubridate)
library(jsonlite)
library(tidytext)

library(helpers)

source("lib/helpers.R")

message_folders_prefix <- "data/source/2020-01-29-facebook-lucascherkewski/messages/inbox/"

message_folders <- fs::dir_ls(message_folders_prefix) %>%
  enframe(name = NULL) %>%
  mutate(value = as.character(value)) %>%
  mutate(value = str_remove(value, fixed(message_folders_prefix))) %>%
  rename(folder = value) %>%
  mutate(file_count = map_int(
    folder,
    ~ fs::dir_ls(paste0(message_folders_prefix, .x, "/"), regexp = "\\.json$") %>%
      enframe(name = NULL) %>%
      summarize(count = n()) %>%
      pull(count)
  )) %>%
  mutate(max_msg_count = 10000 * file_count)
