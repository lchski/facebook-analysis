
read_message_folder <- function(message_folder) {
  fs::dir_ls(paste0(message_folders_prefix, message_folder, "/"), regexp = "\\.json$") %>%
    map_dfr(~ read_json(.x, flatten = TRUE)$messages %>%
              transpose() %>%
              as_tibble() %>%
              unnest(cols = c(sender_name, timestamp_ms, content, type)),
            .id = "source"
    ) %>%
    mutate(
      timestamp = as_datetime(as.POSIXct(
        timestamp_ms / 1000,
        origin = "1970-01-01",
        timezone = "America/Toronto"
      ))
    )
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
