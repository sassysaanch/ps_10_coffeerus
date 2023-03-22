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

1. In this section, you will be parsing the date and time information for the events. First, write a 
regular expression for the `date` column that contains 3 capturing groups for matching the following 
items:
  
- 1st capturing group: Month
- 2nd capturing group: Day
- 3rd capturing group: Year

Use both `str_view()` and `str_match()` to help you write your regex and visualize your matches.

str_view()

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
