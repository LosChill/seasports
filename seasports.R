library(knitr)

source("~/Documents/Computing/projects/seasports/seasports_function.R")

# Seattle Times Sports Page URL
url <- "https://www.seattletimes.com/nation-world/sports-on-tv-radio-2/"

# Run function
sports_table <- seasports_extract(url)

time_sports_table <- seasports_transform(sports_table)

kable(time_sports_table, format = "rst")