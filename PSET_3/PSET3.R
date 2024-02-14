library(httr2)
library(janitor)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(viridis)

# Read census key
source("~/Downloads/test/PSET_3/census_key.R")

api <- "https://api.census.gov/data/2021/pep/population"

# Write request
request <- request(api) |>
  req_url_query(get = I("POP_2020,POP_2021,NAME"),
                `for` = I("state:*"),
                key = census_key)

# Submit request
request <- request |> req_perform()

# Extract population information
population <- request |> resp_body_string() |>
  fromJSON(flatten = TRUE)
# Save as RDA
save(population, file = "~/Downloads/test/PSET_3/population.rda")

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

# Barplot of 2021 populations by state
population |> dplyr::filter(year == 2021) |> 
  ggplot(aes(x = state, y = population, fill = state)) +
  geom_bar(stat = "identity") +
  scale_fill_hue(c=40) +
  xlab("US State") +
  ylab("Population in 2021") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar plot of percent population change
population |> pivot_wider(names_from = year, values_from = population) |> 
  mutate(pop_change = ( (`2021` - `2020`)*100/`2020` )) |> 
  ggplot(aes(x = reorder(state, pop_change), y = pop_change)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  coord_flip() +
  labs(title = "Percentage Change in State Populations (2020-2021)",
       x = "State",
       y = "Percent Change") +
  theme_minimal()

# Add the following region numbers to population
cdc_regions_list <- list(
  "1" = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont"),
  "2" = c("New Jersey", "New York", "Puerto Rico", "Virgin Islands"),
  "3" = c("Delaware", "District of Columbia", "Maryland", "Pennsylvania", "Virginia", "West Virginia"),
  "4" = c("Alabama", "Florida", "Georgia", "Kentucky", "Mississippi", "North Carolina", "South Carolina", "Tennessee"),
  "5" = c("Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin"),
  "6" = c("Arkansas", "Louisiana", "New Mexico", "Oklahoma", "Texas"),
  "7" = c("Iowa", "Kansas", "Missouri", "Nebraska"),
  "8" = c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming"),
  "9" = c("Arizona", "California", "Hawaii", "Nevada", "American Samoa", "Commonwealth of the Northern Mariana Islands", "Federated States of Micronesia", "Guam", "Marshall Islands", "Republic of Palau"),
  "10" = c("Alaska", "Idaho", "Oregon", "Washington"))

cdc_regions <- do.call(rbind, lapply(names(cdc_regions_list), function(region) {
  data.frame(region = region, state_name = cdc_regions_list[[region]])
})) |>
  mutate(region = factor(as.numeric(region)))

population <- left_join(x = population, y = cdc_regions, by = "state_name")

# COVID-19 APIs
cases_api <- "https://data.cdc.gov/resource/pwn4-m3yp.json"
hosp_api <- "https://data.cdc.gov/resource/39z2-9zu6.json"
deaths_api <- "https://data.cdc.gov/resource/r8kw-7aab.json"
vac_api <- "https://data.cdc.gov/resource/rh2h-3yt2.json"


# Submit request & extract information
cases <- request(cases_api) |> 
  req_url_query("$limit" = 10000000) |> 
  req_perform() |> 
  resp_body_string() |> 
  fromJSON(flatten = TRUE)

# Filter data
cases_filtered <- cases |>
  semi_join(population, by = "state") |> 
  mutate(epid_week = epiweek(end_date), epid_year = epiyear(end_date), new_cases = as.numeric(new_cases)) |>
  filter(epid_year == 2020 | epid_year == 2021) |>
  arrange(state, as_date(end_date)) |> 
  select(state, epid_week, epid_year, new_cases) |>
  filter(!is.na(new_cases)) |> 
  group_by(state, epid_week, epid_year) |> 
  summarize(new_cases = sum(new_cases))

get_cdc_data <- function(api){
  
  # Write request
  df <- request(api) |> 
    req_url_query("$limit" = 10000000) |> 
    req_perform() |> 
    resp_body_string() |> 
    fromJSON(flatten = TRUE)
  
  # Return data frame
  return(df)
}

# Get hospitalisation data
hosp_raw <- get_cdc_data(hosp_api)

# Filter data
hosp <- hosp_raw |> 
  select(collection_date, jurisdiction, total_hospitalized_covid) |> 
  rename(state = jurisdiction) |> 
  filter(!str_detect(state, "Region")) |> 
  semi_join(population, by = "state") |>
  mutate(epid_week = epiweek(collection_date),
         epid_year = epiyear(collection_date),
         total_hospitalized_covid = as.numeric(total_hospitalized_covid))

weekly_hosp <- hosp |>
  group_by(state, epid_week, epid_year) |>
  mutate(num_collection_dates = n()) |>
  ungroup() |>
  # Filter out weeks with less than 7 collection dates
  filter(num_collection_dates >= 7) |>
  # Summarize total cases per week
  group_by(state, epid_week, epid_year) |>
  summarise(total_hospitalised = sum(total_hospitalized_covid), .groups = "drop")

hosp <- weekly_hosp |> 
  filter(!is.na(total_hospitalised)) |> 
  filter(epid_year %in% c(2020, 2021))

rm(weekly_hosp)
gc()

# COVID-19 Deaths
deaths_raw <- get_cdc_data(deaths_api)
deaths <- deaths_raw |> 
  rename(state_name = state) |> 
  mutate(state = state.abb[match(state_name, state.name)]) |>
  mutate(state = case_when(
    state_name == "District of Columbia" ~"DC",
    state_name == "Puerto Rico" ~ "PR",
    .default = state)) |> 
  mutate(epid_week = epiweek(end_date), epid_year = epiyear(end_date)) |> 
  semi_join(population, by = "state") |>
  filter(epid_year %in% c(2020, 2021)) |> 
  select(state, epid_week, epid_year, covid_19_deaths) |>
  mutate(across(-state, as.numeric)) |> 
  filter(!is.na(covid_19_deaths)) |> 
  group_by(state, epid_week, epid_year) |> 
  summarize(total_deaths = sum(covid_19_deaths))

# Vaccination Data - location, mmwr_week, epid_year (from epiyear(date)), 
vax_raw <- get_cdc_data(vac_api)

vax <- vax_raw |> 
  rename(state = location) |> 
  select(state, date, date_type, series_complete_daily, booster_daily) |> 
  mutate(epid_year = epiyear(date), epid_week = epiweek(date)) |> 
  mutate(across(c(epid_year, series_complete_daily, booster_daily), as.numeric)) |>
  filter(epid_year %in% c(2020, 2021)) |> 
  filter(series_complete_daily >= 0, booster_daily >= 0) |>
  group_by(state, epid_week, epid_year) |> 
  summarize(total_series = sum(series_complete_daily), total_booster = sum(booster_daily), .groups = "drop") |> 
  filter(!is.na(total_series), !is.na(total_booster))


# Ready to join tables 
all_dates <- data.frame(date = seq(make_date(2020, 1, 25), make_date(2021, 12, 31), by = "week")) |>
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1)) |>
  mutate(epid_year = epiyear(date), epid_week = epiweek(date)) 

dates_and_pop <- cross_join(population, all_dates)

dat <- dates_and_pop |> 
  left_join(cases_filtered, by = c("state", "epid_week", "epid_year")) |> 
  left_join(hosp, by = c("state", "epid_week", "epid_year")) |> 
  left_join(deaths, by = c("state", "epid_week", "epid_year")) |> 
  left_join(vax, by = c("state", "epid_week", "epid_year")) |> 
  distinct()



# Plot trend with ggplot2
dat |>
  ggplot(aes(x = date, y = (new_cases / population), group = state, color = region)) +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  labs(x = "Year", y = "Cases per Person", color = "Region", title = "Trend Lines of COVID-19 Cases") +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal()


dat <- dat |> 
  mutate(hosp_rate = (total_hospitalised / population) * 100000,
         death_rate = (total_deaths / population) * 100000)
dat_long <- dat |> 
  pivot_longer(cols = c(hosp_rate, death_rate), values_to = "rate", names_to = "outcome")

dat_long |> ggplot(aes(x = date, y = rate, color = outcome)) +
  geom_smooth(na.rm = TRUE) +
  labs(x = "Year", y = "Rate per 100,000", color = "Outcome") +
  theme_minimal() +
  facet_wrap(~outcome, nrow = 2)




# Filter data for the specified period and compute death rate
death_rates <- dat |> 
  filter(date >= make_date(2021, 3, 1) & date <= make_date(2021, 9, 1)) |>
  group_by(state) |>
  summarize(death_rate = mean(total_deaths, na.rm = TRUE)*n() / population[1]*100000)


# Compute vaccination rate by September 1st
vaccination_rate <- dat |>
  filter(date <= make_date(2021, 9, 1)) |>
  group_by(state) |>
  filter(!is.na(total_series)) |> 
  summarize(vax_rate = max(total_series) / (population[1] * 100))

# Merge death_rates and vaccination_rate data frames
death_vac_rates <- inner_join(death_rates, vaccination_rate, by = "state")

# Plot deaths per day per 100,000 people against vaccination rate
ggplot(death_vac_rates, aes(x = death_rate, y = vax_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(x = "Deaths per day per 100,000 people",
       y = "Vaccination rate",
       title = "Deaths vs. Vaccination Rate by State",
       subtitle = "January 1 to July 1, 2021 for deaths, Vaccination rate by September 1, 2021") +
  theme_minimal()






# Filter data for the specified period and compute booster rate
booster_rate <- dat |>
  filter(date >= make_date(2021, 10, 1) & date <= make_date(2021, 12, 31)) |> 
  group_by(state) |>
  filter(!is.na(total_booster)) |> 
  summarize(boo_rate = max(total_booster) / (population[1] * 100))


# Merge booster_rate and death_rate data frames
death_boo_rates <- inner_join(death_rates, booster_rate, by = "state")

# Plot booster rate against vaccination rate
ggplot(death_boo_rates, aes(x = death_rate, y = boo_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(x = "Deaths per day per 100,000 people",
       y = "Booster rate",
       title = "Deaths vs. Booster Rate by State",
       subtitle = "October 1 to December 31, 2021 for booster, January 1 to July 1, 2021 for deaths") +
  theme_minimal()
