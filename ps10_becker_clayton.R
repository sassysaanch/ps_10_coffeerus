################################################################################
##
## [ PROJ ] < Problem set 10 >
## [ FILE ] < ps10_becker_clayton.R >
## [ AUTH ] < cnbecker14 >
## [ INIT ] < March 22, 2023 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)

## ---------------------------
## directory paths
## ---------------------------

data_dir <- file.path(".","data")

## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 1 

events <- readRDS(file = url("https://github.com/anyone-can-cook/rclass2/raw/main/data/ps10_events.RDS"))

# Question 2 

print(events$address)
writeLines(events$address)

# Question 3

str_view(string = events$address, pattern = "(.+)\\n(.+),\\s(\\w\\w)\\s(\\d{5})")

# Question 4

loc_matches <- str_match(string = events$address, pattern = "(.+)\\n(.+),\\s(\\w\\w)\\s(\\d{5})")

# Question 5

events <- events %>% mutate(
  event_address = loc_matches[,2],
  event_city = loc_matches[,3],
  event_state = loc_matches[,4],
  event_zip = loc_matches[,5]
)

## -----------------------------------------------------------------------------
## Part 3 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 1

str_view(string = events$date, pattern = "(\\d+)[/](\\d+)[/](\\d+)")
str_match(string = events$date, pattern = "(\\d+)[/](\\d+)[/](\\d+)")

# Question 2

events <- events %>% mutate(
  event_date = str_replace(string = date, 
                           pattern = "(\\d+)[/](\\d+)[/](\\d+)", 
                           replacement = "20\\3-\\1-\\2")
)

# Question 3

time_matches <- str_match(string = events$time, pattern = "(\\d{2}):(\\d{2})\\s([AP]M)")

# Question 4

events <- events %>% mutate(
  hour = as.double(time_matches[,2]), 
  minute = time_matches[,3],
  ampm = time_matches[,4]
)

# Question 5

events <- events %>% mutate(
  hour24 = if_else(ampm == "PM", hour + 12, hour),
  hour24 = if_else(hour24 == 24, 12, hour24)
)

# Question 6 

events <- events %>% mutate(
  hour24 = str_pad(string = hour24, width = 2, side = "left", pad = "0")
)

# Question 7

events <- events %>% mutate(
  event_time = str_c(hour24,":",minute,":","00"),
  event_datetime = str_c(event_date," ",event_time)
)

# Question 8

results <- events %>% select(event_datetime, event_date, event_time, event_location, event_address, 
                             event_city, event_state, event_zip)

# Question 9 

9. Write your `results` object to a CSV file named `events_<your_name>.csv` 
(fill in your name or initials). Save the file inside your `data_dir`.

results %>% write_csv(file = file.path(data_dir, "clayton_becker.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
