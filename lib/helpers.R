
read_message_folder <- function(message_folder) {
  fs::dir_ls(paste0(message_folders_prefix, message_folder, "/"), regexp = "\\.json$") %>%
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
    arrange(timestamp) %>%
    mutate(msg_id = row_number()) %>%
    select(msg_id, source_folder, source_file, timestamp_ms, timestamp:minute, sender_name, type, content)
}



remove_fr_columns <- function(dataset) {
  dataset %>%
    select(
      -DESCRIP_F,
      -TITLE_F,
      -DEPT_F,
      -INDICATORFRA,
      -SUBINDICATORFRA
    )
}
