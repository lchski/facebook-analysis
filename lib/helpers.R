
read_message_folder <- function(message_folder) {
  messages_to_return <- fs::dir_ls(paste0(message_folders_prefix, message_folder, "/"), regexp = "\\.json$") %>%
    map_dfr(~ read_json(.x, flatten = TRUE)$messages %>%
              transpose() %>%
              as_tibble() %>%
              unnest(cols = c(sender_name, timestamp_ms, content, type)),
            .id = "source"
    ) %>%
    mutate(source = str_remove(source, fixed(message_folders_prefix))) %>%
    separate(source, into = c("source_folder", "source_file"), sep = "/") %>%
    mutate(
      timestamp = as_datetime(as.POSIXct(
        timestamp_ms / 1000,
        origin = "1970-01-01",
        tz = "America/Toronto"
      ), tz = "America/Toronto")
    ) %>%
    mutate_at(vars(timestamp), list(year = year, month = month, day = day, hour = hour, minute = minute)) %>%
    mutate(
      month_fct = month(timestamp, label = TRUE),
      wday = wday(timestamp, week_start = 1),
      wday_fct = wday(timestamp, label = TRUE, week_start = 1)
    ) %>%
    arrange(timestamp) %>%
    mutate(
      mins_until_next_message = time_length(interval(timestamp, lead(timestamp)), "minutes")
    ) %>%
    mutate(
      mins_since_last_message = lag(mins_until_next_message)
    ) %>%
    mutate(is_thread_start = sender_name != lag(sender_name)) %>%
    mutate(is_thread_start = ifelse(is.na(is_thread_start), TRUE, is_thread_start)) %>%
    mutate(msg_id = row_number()) %>%
    left_join(
      gce_msgs %>%
        select(msg_id, timestamp, is_thread_start) %>%
        filter(is_thread_start) %>%
        mutate(thread_id = row_number())
    ) %>%
    fill(thread_id) %>%
    select(
      msg_id,
      source_folder,
      source_file,
      timestamp_ms,
      timestamp:mins_since_last_message,
      sender_name,
      thread_id,
      is_thread_start,
      type,
      content
    )

  messages_to_return <- messages_to_return %>%
    left_join(
      messages_to_return %>%
        unnest_tokens(word, content) %>%
        group_by(msg_id) %>%
        summarize(n_words = n())
    )
  
  messages_to_return  
}

extract_threads <- function(messages) {
  messages %>%
    group_by(thread_id, sender_name) %>%
    summarize(
      n_msgs = n(),
      n_words = sum(n_words),
      msg_id_start = min(msg_id),
      msg_id_end = max(msg_id),
      timestamp_start = min(timestamp),
      timestamp_end = max(timestamp)
    ) %>%
    ungroup() %>%
    mutate(
      mins_thread_length = time_length(interval(timestamp_start, timestamp_end), "minutes"),
      mins_until_next_thread = time_length(interval(timestamp_end, lead(timestamp_start)), "minutes")
    ) %>%
    mutate_at(vars(timestamp_start), list(year_start = year, month_start = month, day_start = day, hour_start = hour, minute_start = minute)) %>%
    mutate(
      month_start_fct = month(timestamp_start, label = TRUE),
      wday_start = wday(timestamp_start, week_start = 1),
      wday_start_fct = wday(timestamp_start, label = TRUE, week_start = 1)
    )
}


summarize_messages <- function(dataset, ...) {
  dataset %>%
    group_by(...) %>%
    summarize(messages = n(), words = sum(n_words, na.rm = TRUE)) %>%
    mutate(messages_prop = messages / sum(messages), words_prop = words / sum(words)) %>%
    arrange(-words_prop) %>%
    select(..., messages, messages_prop, words, words_prop)
}

lookup_message_folder <- function(string_to_search) {
  message_folders %>%
    filter(str_detect(folder, string_to_search))
}

