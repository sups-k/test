# Ways to read a file
dat <- read.csv("murders.csv")

# With readr
dat <- read_csv("murders.csv")
## This automatically converts the data into a tibble which tells you data types
## of the columns

## readxl::read_excel() has a function to look through different sheets

## as.Date("1970-01-01") result is 0 because this is the day computer is invented
## It counts how many days have passed since day 0.

library(lubridate)
mydate <- make_date(2003, 5, 7)
# YYYY MM DD format
year(mydate)
# 2003
month(mydate)
# 5
day(mydate)
# 7
ymd("2003-5-7") # convert to date in YYYY-MM-DD format
# "2003-05-07" OR see below
ymd("2003-May-7")

ydm("2003-5-7") # convert to YYYY-DD-MM
# 2003-07-05
wday(mydate) # weekday; 1 = Sunday
# 6
today()
# "2024-02-08"
wday(today(), label = TRUE)
# Thu
yday(today())
# 39
round_date(today(), unit = "week")
# "2024-02-11"


# Read lines without worrying about format. Can specify number of lines to read with "n"
readLines("calificaciones.csv", n = 3) # not ASCII or UTF8
# UTF8 is default unicode. Contains non-English alphabet (like Spanish)

guess_encoding("murders.csv")
# ASCII, confidence = 1
guess_encoding("calcificaciones.csv")
#ISO-8859-1, confidence = 0.92

# Thus, specify the locale to display correctly in R.
# Also can change what decimal point looks like in the data
read_csv("calcificaciones.csv", locale = locale(encoding = "ISO-8859-1",
                                                decimal_mark = ","))


parse_date(dat$f.n., format = "%d de %B de %Y", locale = locale(date_names = "es"))


dat$estampa >= make_date(2023, 9, 22)
tz(dat$estampa)
with_tz(dat$estampa, tz = "EST") >= make_date(2023, 9, 22)
# Default time zone is always UTC (time in London)
# FALSE = submitted on time

## Problem set 4? or 3?

#1
# In Terminal:
# mv census_key.txt census_key.R
# In R:
source("census_key.R")
# This will read the census key as "census_key"
# or
census_key <- "bd00a4130b3eb9d98ccb631252b74597a4687b3d"

#2: Google "US census api"
# Under vintage 2021, there is an "API Call" which you need. Also read "Example Call"
api <- "https://api.census.gov/data/2021/pep/population"

#3: We want the data from the census from 2020 & 2021
library(httr2)
request <- request(api) |>
  req_url_query(get = I("POP_2020,POP_2021,NAME"),
                `for` = I("state:*"),
                key = census_key)
# It makes an HTML link for getting the data
# Request has not been yet though
# I() means do not convert the string, keep in original form. "Inhibit interpretation conversion of objects"

#4: Make request
request <- request |> req_perform()
# Status: 200 OK
# Content-Type: application/json
# Body: In memory (2112 bytes)

# Now JSON is the official format for application data (API data), even if they are tables

#5:
library(jsonlite)
population <- request |> resp_body_string() |>
  fromJSON(flatten = TRUE)
# Save as RDA so as to not keep requesting data from the census website
save(population, file = "population.rda")

#6:
library(janitor)
population <- population |> row_to_names(1) |>
  as_tibble() |>
  select(-state) |>
  rename(state_name = NAME) |>
  # row_to_names(n) is a janitor function that defines the nth row as the header row
  pivot_longer(-state_name, names_to = "year", values_to = "population") |>
  mutate(year = str_remove(year, "POP_")) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]) |>
# Match the state names in df to US state abbreviations and add them to the df
  mutate(state = case_when(
    state_name == "District of Columbia" ~"DC",
    state_name == "Puerto Rico" ~ "PR",
    .default = state))

#8, 9, 17 practice "join"  
