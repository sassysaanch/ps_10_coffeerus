################################################################################
##
## [ PROJ ] < Problem set #10 >
## [ FILE ] < Homework Edu 260B >
## [ AUTH ] < Saanchi Shah/ sassysaanch >
## [ INIT ] < 2/22/2023 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(dplyr)
library(tidyverse)
library(stringr)

## ---------------------------
## directory paths
## ---------------------------
data_dir <- file.path("./ps10_shah_Saanchi/data")

## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments
## -----------------------------------------------------------------------------
# Q1. 
events <- readRDS(url("https://github.com/anyone-can-cook/rclass2/raw/main/data/ps10_events.RDS"))

# Q2.
print(events$address)
writeLines(events$address)

# Q3.

str_view(string = events$address, pattern = "(.+)\\n(\\w+),\\s(\\w\\w)\\s(\\d{5})")

# Q4
loc_matches <- str_match(string = events$address, pattern = "(.+)\\n(.+),\\s(\\w\\w)\\s(\\d{5})")

# Q5
events = events %>% 
  mutate(
    event_address = loc_matches[,2],
    event_city = loc_matches[,3],
    event_state = loc_matches[,4],
    event_zip = loc_matches[,5]
  )

## -----------------------------------------------------------------------------
## Part 3 - Label each question using comments
## -----------------------------------------------------------------------------

# Q1 
str_view(string = events$date, pattern = "(\\d+)[-/\\.](\\d+)[-/\\.](\\d+)")
str_match(string = events$date, pattern = "(\\d+)[-/\\.](\\d+)[-/\\.](\\d+)")

# Q2
events = events %>% 
  mutate(
    event_date = str_replace(string = events$date, pattern = "(\\d+)[-/\\.](\\d+)[-/\\.](\\d+)",
                             replacement = "20\\3\\-\\1\\-\\2"))

# Q3

time_matches <- str_match(string = events$time, pattern = "(\\d{2}):(\\d{2})\\s([AP]M)")

str_view_all(string = events$time, pattern = "(\\d\\d):(\\d\\d)\\s([AM]P)")

# Q4

events = events %>% 
  mutate(hour = as.double(time_matches[,2]),
         minute = time_matches[,3],
         ampm = time_matches[,4])

# Q5
events = events %>% 
  mutate(hournew = if_else(ampm == "PM", 
                           hour + 12, hour),
         hour24 = if_else(hournew == '24', 12, hournew))

# Q6

events$hour24 <- str_pad(events$hour24, 2, side = "left", pad = 0)

# Q7

events = events %>% 
  mutate(
    event_time = str_c(hour24, ":", minute, ":", '00'),
    event_datetime = str_c(event_date, " ", event_time)) %>% 
  select(-hournew)

# Q8

results <- 
  events %>% 
  select(
    event_datetime,
    event_date,
    event_time,
    event_location,
    event_address,
    event_city,
    event_state,
    event_zip
  )

# Q9

write_csv(results, file.path(data_dir, 'events_saanchi.csv'))


## -----------------------------------------------------------------------------
## Part 4 - Label each question using comments
## -----------------------------------------------------------------------------

# Learned : https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/452
# Response: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/446

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
