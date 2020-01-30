library(tidytext)

gce_msgs %>%
  summarize_messages(sender_name)

el_msgs %>%
  summarize_messages(sender_name)

gce_msgs %>%
  ggplot(aes(x = month_fct, fill = factor(year))) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Purples") +
  theme_minimal()
