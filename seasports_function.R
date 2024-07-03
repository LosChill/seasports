suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(polite)
  library(lubridate)
  library(knitr)
})

# Library of weekdays ----
weekdays <- c("monday",
              "tuesday",
              "wednesday",
              "thursday",
              "friday",
              "saturday",
              "sunday")

## For column presentation ----
upper_weekdays <- toupper(weekdays)
## For regex evaluation ----
regex_weekdays <- paste(tolower(weekdays), collapse = "|")

# Extract Function ----
seasports_extract <- function(url_var) {
  
  ## Scrape url
  session <- bow(url_var)
  resp_html <- scrape(session)
  
  ## Extract and combine tables
  sports_table <- resp_html %>%
    html_elements(".methode-table") %>%
    map(html_table) %>%
    bind_rows()
  
  return(sports_table)
}

# Transform Function ----
seasports_transform <- function(tibble_var) {

  ## Mutate Day and League rows to columns
  clean_sports_table <- tibble_var %>%
    mutate(
      game_day = ifelse(str_detect(tolower(X1), regex_weekdays), X1, NA),
      game_time = case_when(
        str_detect(X1, "^\\d") ~ X1,
        str_detect(tolower(X1), "noon") ~ "12 p.m.",
        TRUE ~ NA
        ),
      league = ifelse(is.na(game_time), X1, NA),
      game = ifelse(!is.na(game_time), X2, NA),
      tv = ifelse(!is.na(game_time), X3, NA),
      radio = ifelse(!is.na(game_time), X4, NA)
      ) %>%
    fill(league, .direction = "down") %>%
    fill(game_day, .direction = "down") %>%
    filter(!is.na(game_time)) %>%
    select(game_day, game_time, league, game, tv, radio)
  
  ## Convert time format
  time_sports_table <- clean_sports_table %>%
    mutate(
      game_time = str_replace_all(game_time, fixed("."), ""),
      game_time = str_replace_all(game_time, fixed("*"), ""),
      game_time = format(parse_date_time(game_time, orders = c("HM%p", "H%p")), "%H:%M:%S")
      )

  ## Create and add day_id column
  game_day_order <- unique(time_sports_table$game_day)
  time_sports_table <- time_sports_table %>%
    mutate(day_id = match(game_day, game_day_order))
  
  ## Sort and reorder
  time_sports_table <- time_sports_table %>%
    relocate(day_id, .before = 1) %>%
    arrange(day_id, game_time, league)
  
  return(time_sports_table)
}
