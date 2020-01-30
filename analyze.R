gce_msgs %>%
  summarize_messages(sender_name)

el_msgs %>%
  summarize_messages(sender_name)

gce_msgs %>%
  ggplot(aes(x = month_fct, fill = factor(year))) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Purples") +
  theme_minimal()

gce_msgs %>%
  ggplot(aes(x = hour, y = n_words, fill = sender_name)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Purples") +
  theme_minimal()

gce_msgs %>%
  ggplot(aes(x = timestamp, y = mins_until_next_message)) +
  geom_hex() +
  geom_smooth() +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%Y %b") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

gce_msgs %>%
  filter(! is.na(mins_until_next_message)) %>%
  mutate(mins_unm_bin = cut(
    mins_until_next_message,
    c(0, 1, 5, 10, 15, 30, 45, 60, 90, 120, 240, 480, 1440, Inf)
  ))

gce_msgs %>%
  filter(! is.na(mins_until_next_message)) %>%
  mutate(mins_unm_bin = cut(
    mins_until_next_message,
    c(0, 1, 5, 10, 15, 30, 45, 60, 90, 120, 240, 480, 1440, Inf)
  )) %>%
  ggplot(aes(x = mins_unm_bin, fill = as_factor(year))) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



gce_msgs %>%
  filter(! is.na(mins_until_next_message)) %>%
  mutate(mins_unm_bin = cut(
    mins_until_next_message,
    c(0, 1, 5, 10, 15, 30, 45, 60, 90, 120, 240, 480, 1440, Inf)
  )) %>%
  filter(year == 2018 & month == 10) %>%
  ggplot(aes(x = mins_unm_bin)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

gce_msgs %>%
  filter(! is.na(mins_until_next_message)) %>%
  mutate(mins_unm_bin = cut(
    mins_until_next_message,
    c(0, 1, 5, 10, 15, 30, 45, 60, 90, 120, 240, 480, 1440, Inf)
  )) %>%
  filter(year == 2018 & month == 10) %>%
  ggplot(aes(x = timestamp, y = mins_unm_bin)) +
  geom_point()



## get last message in each thread
gce_msgs %>%
  group_by(thread_id) %>%
  top_n(1, wt = msg_id) %>%
  select(msg_id, timestamp, thread_id, sender_name, is_thread_start, content)

