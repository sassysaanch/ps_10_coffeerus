################################################################################
##
## [ PROJ ] < Problem set #10 >
## [ FILE ] < pa9_Cousins_SarahJ >
## [ AUTH ] < Sarah J. Cousins/SJC0usins >
## [ INIT ] < Due 3/24/23 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(stringr)

## ---------------------------
## directory paths
## ---------------------------
data_dir <- file.path('.', 'data')


## -----------------------------------------------------------------------------
## Part I: Setting up repository
## -----------------------------------------------------------------------------

#Done!

## -----------------------------------------------------------------------------
## Part II: Parsing event locations
## -----------------------------------------------------------------------------

#Q 2.1

#Use readRDS() and url() to load in the recruiting events data and save as object named events
events <- readRDS(file = url("https://github.com/anyone-can-cook/rclass2/raw/main/data/ps10_events.RDS"))


#Q 2.2

#Use print() and writeLines() to print out the address variable of events to see how it looks.
print(events$address)
writeLines(events$address)


#Q 2.3

#Use str_view() to help you write a regular expression that contains 4 capturing groups for matching the following items from the address variable:
#1st capturing group: Street address
#2nd capturing group: City
#3rd capturing group: 2-letter state code
#4th capturing group: 5-digit zip code (do not include the 4-digit extension if there is one)
str_view(string = events$address, pattern = "(.+)\\n(.+),\\s(\\w\\w)\\s(\\d{5})")


#Q 2.4

#Now, plug the regular expression you wrote in the previous step into str_match() to obtain a character matrix containing the matches. Save this object as loc_matches. The 1st column of the matrix should contain the full match, and the next 4 columns contain matches for each capturing group.

loc_matches <- str_match(string = events$address, pattern = "(.+)\\n(.+),\\s(\\w\\w)\\s(\\d{5})")


#Q 2.5
#You will now add the following 4 columns to the events dataframe, which should contain the respective information from loc_matches:
#event_address: Street address
#event_city: City
#event_state: 2-letter state code
#event_zip: 5-digit zip code

events <- events %>% mutate(
  event_address = loc_matches[,2],
  event_city = loc_matches[,3],
  event_state = loc_matches[,4],
  event_zip = loc_matches[,5]
)


## -----------------------------------------------------------------------------
## Part III: Parsing event date and time
## -----------------------------------------------------------------------------

#Q 3.1

#Expression for the date column that contains 3 capturing groups for matching the following items:
#1st capturing group: Month
#2nd capturing group: Day
#3rd capturing group: Year
#Use both str_view() and str_match() to help you write your regex and visualize your matches.

str_view(string = events$date, pattern = "(\\d+)[/](\\d+)[/](\\d+)")
str_match(string = events$date, pattern = "(\\d+)[/](\\d+)[/](\\d+)")


#Q 3.2

#Using str_replace() and your regular expression from the previous step, add a new column to events called event_date where each element contains the date in the format: YYYY-MM-DD
#Hint: Use the replacement argument in str_replace() and backreferences to format the date and assign it to the event_date column in events.

events <- events %>% mutate(
  event_date = str_replace(string = date, 
                           pattern = "(\\d+)[/](\\d+)[/](\\d+)", 
                           replacement = "20\\3-\\1-\\2")
)


#Q 3.3

#Now, write a regular expression for the time column that contains the following 3 capturing groups:
#1st capturing group: Hour
#2nd capturing group: Minute
#3rd capturing group: AM/PM
#Use str_match() to obtain a character matrix of your matches and save it to time_matches.

time_matches <- str_match(string = events$time, pattern = "(\\d{2}):(\\d{2})\\s([AP]M)")


#Q 3.4

#4. Add the following 3 columns to the events dataframe, which should contain the respective information from the time_matches character matrix:

events <- events %>% mutate(
  hour = as.double(time_matches[,2]),  #hour: Hour -- #Use as.double() to convert the hour to double type before assigning it as the hour column in events. This will allow us to perform arithmetic operations on it in the next step.
  minute = time_matches[,3],  #minute: Minute
  ampm = time_matches[,4]   #ampm: AM/PM
)


#Q 3.5

#24-hour clock instead of 12-hour clock using AM/PM. 
#new variable called hour24 to the events dataframe that will be the 24-hour clock version of hour.
#Hint: If the hour is a PM hour and it is not 12 PM (noon), add 12 to the hour (e.g., 2 PM should  become 2 + 12 = 14). This step does not involve regex. You could use an if_else statement inside your mutate() function.

events <- events %>% mutate(
  hour24 = if_else(ampm == "PM", hour + 12, hour),
  hour24 = if_else(hour24 == 24, 12, hour24)
)


#Q 3.6

#Use str_pad() to pad the hour24 variable you created in the previous step so that it is always 2 digits long (e.g., 8 would become 08, etc.)

events <- events %>% mutate(
  hour24 = str_pad(string = hour24, width = 2, side = "left", pad = "0")
)


#Q 3.7
#Now, add the following 2 variables to the events dataframe:
#event_time: 24-hour clock time in the format: HH:MM:SS (second can be 00)
#event_datetime: Date and time in the format: YYYY-MM-DD HH:MM:SS

events <- events %>% mutate(
  event_time = str_c(hour24,":",minute,":","00"),
  event_datetime = str_c(event_date," ",event_time)
)


#Q 3.8
#Finally, select only the following variables from events and save it to a new object called results: event_datetime, event_date, event_time, event_location, event_address, event_city, event_state, event_zip

results <- events %>% select(event_datetime, event_date, event_time, event_location, event_address, 
                             event_city, event_state, event_zip)

#Q 3.9

#Write your results object to a CSV file named events_<your_name>.csv (fill in your name or initials). Save the file inside your data_dir.

results %>% write_csv(file = file.path(data_dir, "Cousins_SarahJ.csv"))


## -----------------------------------------------------------------------------
##  Part V: Create a GitHub issue
## -----------------------------------------------------------------------------

#Posted: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/454
#Replied: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/418


## -----------------------------------------------------------------------------
## GIT TERMINAL COMMANDS // STEPS
## -----------------------------------------------------------------------------
#since always start at ps2...
cd..
cd..
cd ps10
#initating and linking remote to local
git clone https://github.com/anyone-can-cook/ps_10_coffeerus
cd ps_10_coffeerus
#creating dev branch
git checkout -b ps10dev_Cousins_Sarah
#blah blah, doing assignment but now ready to send to remote/coffeerus

#confirming i'm on dev branch -- could also use git status
git branch -a
#adding while on my dev branch
#from dev branch [[check to see what actually needs to be pushed once complete]]
git add ps10_Cousins_SarahJ.R
git add data/Cousins_SarahJ.csv
git status
git commit -m "inital commit and merge"
#switch to main, pull, merge and push
git checkout main
git pull
git merge ps10dev_Cousins_Sarah
git push


## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
