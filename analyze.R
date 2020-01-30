library(tidytext)

gce_msgs %>%
  summarize_messages(sender_name)

el_msgs %>%
  summarize_messages(sender_name)
