suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(httr2)
  library(lubridate)
  library(knitr)
})

# Library of weekdays
weekdays <- c("monday",
              "tuesday",
              "wednesday",
              "thursday",
              "friday",
              "saturday",
              "sunday")

seasports_extract <- function(url) {
  
  # Extract
  req <- request(url)
  resp <- req_perform(req)
  resp_html <- resp_body_html(resp)
  
  sports_table <- resp_html %>%
    html_element(".methode-table") %>%
    html_table()
  
  # Transform
  clean_sports_table <- sports_table %>%
    mutate(
      game_day = ifelse(tolower(X1) %in% weekdays, X1, NA),
      game_time = ifelse(str_detect(X1, "^\\d"), X1, NA),
      league = ifelse(is.na(game_time), X1, NA),
      game = ifelse(!is.na(game_time), X2, NA),
      tv = ifelse(!is.na(game_time), X3, NA),
      radio = ifelse(!is.na(game_time), X4, NA)
    ) %>%
    fill(league, .direction = "down") %>%
    fill(game_day, .direction = "down") %>%
    filter(!is.na(game_time)) %>%
    select(game_day, game_time, league, game, tv, radio)
  
  
  time_sports_table <- clean_sports_table %>%
    mutate(
      game_time = str_replace_all(game_time, fixed("."), ""),
      game_time = format(parse_date_time(game_time, orders = c("HM%p", "H%p")), "%H:%M:%S")
    ) %>%
    arrange(game_time, league, game)
  
  return(time_sports_table)
}