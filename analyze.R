library(tidytext)

gce_msgs

gce_msgs_words <- gce_msgs %>% unnest_tokens(word, content)

gce_msgs %>%
  left_join(
    gce_msgs_words %>%
      group_by(msg_id) %>%
      summarize(count = n())
  )

gce_msgs %>%
  left_join(
    gce_msgs %>%
      unnest_tokens(word, content) %>% ## can also be `gce_msgs_words`
      group_by(msg_id) %>%
      summarize(count = n())
  ) %>%
  group_by(sender_name) %>%
  summarize(messages = n(), words = sum(count, na.rm = TRUE)) %>%
  mutate(messages_prop = messages / sum(messages), words_prop = words / sum(words)) %>%
  arrange(-words_prop) %>%
  select(sender_name, messages, messages_prop, words, words_prop)
