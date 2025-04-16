### this script is to clean the price data
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(geobr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(glmnet)
library(fixest)
library(MASS)


# load the data
cepea_cattle_prices <- readRDS("data/cepea_cattle_prices.rds")
state_avgs = read_csv("data/state_avg.csv")
livestock_state_carcass = read_csv("data/livestock_state_carcass.csv")

### DATA PRE-PROCESSING

# aggregate state averages to quarterly level
state_avgs <- state_avgs %>%
  mutate(quarter = case_when(
    month %in% c(1, 2, 3)  ~ 1,
    month %in% c(4, 5, 6)  ~ 2,
    month %in% c(7, 8, 9)  ~ 3,
    month %in% c(10, 11, 12) ~ 4
  )) %>%
  group_by(year, state_code, quarter) %>%
  summarise(across(w_avg, mean, na.rm = TRUE), .groups = "drop")

# clean the data so it is usable
cepea_cattle_prices = cepea_cattle_prices %>% drop_na() # it has a really weird naming??
cepea_cattle_prices <- cepea_cattle_prices %>%
  mutate(
    month_year = dmy(month_year),  # Convert to date format
    month = day(month_year),       # Extract month, they use a different month/day/year format so im using the day function here
    year = year(month_year)        # Extract year
  ) %>%
  dplyr::select(-month_year)

cepea_cattle_prices$state_code_name <- sub(".*\\((.*)\\).*", "\\1", cepea_cattle_prices$city_fat) # seperate the state initials
ibge_codes <- c( # use this to match initials to state codes
  AC = 12, AL = 27, AM = 13, AP = 16, BA = 29,
  CE = 23, DF = 53, ES = 32, GO = 52, MA = 21,
  MT = 51, MS = 50, MG = 31, PA = 15, PB = 25,
  PE = 26, PI = 22, PR = 41, RJ = 33, RN = 24,
  RS = 43, RO = 11, RR = 14, SC = 42, SP = 35,
  SE = 28, TO = 17
)
cepea_cattle_prices$state_code <- ibge_codes[cepea_cattle_prices$state_code_name] # convert to codes

# make sure there is a quarter column in cattle prices
cepea_cattle_prices <- cepea_cattle_prices %>%
  mutate(quarter = case_when(
    month >= 1 & month <= 3  ~ 1,
    month >= 4 & month <= 6  ~ 2,
    month >= 7 & month <= 9  ~ 3,
    month >= 10 & month <= 12 ~ 4
  ))

# Join the data with state_avg
regression_data = left_join(cepea_cattle_prices, state_avgs, by = c("month", "year", "state_code"))
regression_data = regression_data %>% drop_na()#  %>% select(-c(geom)) # for now drop all of the rows that have any NA values in them

# Join with carcass data
livestock_state_carcass <- livestock_state_carcass %>% mutate(quarter = as.double(quarter)) # renaming to match with other data
regression_data = left_join(regression_data, livestock_state_carcass, by = c("quarter", "state_code", "year"))
regression_data = regression_data %>% drop_na()
regression_data$city_fat <- gsub("[() ]", "", regression_data$city_fat)
#regression_data$city_fat <- as.factor(regression_data$city_fat)
regression_data$state_code_name <- as.factor(regression_data$state_code_name)
regression_data <- regression_data %>%
  group_by(year, state_code) %>%
  mutate(w_avg = mean(w_avg, na.rm = TRUE)) %>%
  ungroup()

# save the data for later use
write.csv(regression_data, "data/regression_data.csv", row.names = FALSE)
