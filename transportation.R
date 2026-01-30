library(tidycensus)
library(tidyverse)
library(gitcreds)
library(writexl)

####set personal access token (current PAT expires Sun, Mar 29 2026) and Census API Key####

#To set github PAT:
#gitcreds_set()

#if you do not have a census api key set globally in R, you will need to do that via: 
# census_api_key("YOUR KEY GOES HERE", install = TRUE)

####CMRPC Communities listed in a vector####
communities <- c("Auburn", "Barre", "Berlin", "Blackstone", "Boylston", "Brookfield", "Charlton", "Douglas", "Dudley", "East Brookfield", "Grafton", "Hardwick", "Holden", "Hopedale", "Leicester", "Mendon", "Millbury", "Millville", "New Braintree", "North Brookfield", "Northborough", "Northbridge", "Oakham", "Oxford", "Paxton", "Princeton", "Rutland", "Shrewsbury", "Southbridge", "Spencer", "Sturbridge", "Sutton", "Upton", "Uxbridge", "Warren", "Webster", "West Boylston", "West Brookfield", "Westborough", "Worcester")


####Initial setup - var list, global vars####

##best to use detailecd tables for MA Towns. Can also use Data Profiles, but beware that variable ids may not be consistent between years.

#load variables for a particular year
vars_2023 <- load_variables(2023, "acs5", cache = TRUE)

#years for variable pulls
years <- c(2010:2023)

#excel and csv file location
xlsx_path <- "data/transportation/xlsx/"

csv_path <- "data/transportation/csv/"

####Commute Mode to work####

#variables from ACS
commute_mode_vars_all <- vars_2023 |> 
  filter(str_detect(name, regex("B08301", ignore_case = TRUE ))) 

#just the ones we want
commute_mode_vars_select <- commute_mode_vars_all |> 
  filter(
    name %in% c("B08301_003", "B08301_004", "B08301_010", "B08301_016", "B08301_017", "B08301_018", "B08301_019", "B08301_020", "B08301_021")
  )|> 
  mutate(label_short = str_remove(label, "Estimate!!Total:!!"),
         label_short = str_replace(label_short, ":!!", " - "),
         label_short = str_remove(label_short, ":"))

#just the ones we want in a vector for use in function
commute_mode_vars_sub <- commute_mode_vars_all |> 
  filter(
    name %in% c("B08301_003", "B08301_004", "B08301_010", "B08301_016", "B08301_017", "B08301_018", "B08301_019", "B08301_020", "B08301_021")
  ) |> 
  pull(name)

#pull data - set up as a function

pull_commute_mode <- function(yr) {
  get_acs(
    geography = "county subdivision",
    variables = commute_mode_vars_sub,
    state = "MA",
    county = "Worcester",
    year = yr,
    survey = "acs5"
  ) |> 
    mutate(NAME = str_remove(NAME, " town, Worcester County, Massachusetts"),
           year = yr) |> 
    filter(NAME %in% communities)
}

#call function with years as input
commute_mode_all <- map_dfr(years, pull_commute_mode)

#join label from vars
commute_mode_all <- commute_mode_all |> 
  left_join(commute_mode_vars_select |> select(label_short, name), join_by(variable == name))

#export to csv and excel
write_xlsx(commute_mode_all, paste(xlsx_path,"commute_mode.xlsx", sep = ""))

write_csv(commute_mode_all, paste(csv_path, "commute_mode.csv", sep = ""))

####Vehicles available####

#variables from ACS
vehicle_avail_vars_all <- vars_2023 |> 
  filter(str_detect(name, regex("B08201", ignore_case = TRUE ))) 

#just the ones we want
vehicle_avail_vars_select <- vehicle_avail_vars_all |> 
  filter(
    name %in% c("B08201_002", "B08201_003", "B08201_004", "B08201_005", "B08201_006")
  )|> 
  mutate(label_short = str_remove(label, "Estimate!!Total:!!"),
         label_short = str_replace(label_short, ":!!", " - "),
         label_short = str_remove(label_short, ":"))

#just the ones we want in a vector for use in function
vehicle_avail_vars_sub <- vehicle_avail_vars_select |> 
  pull(name)

#pull data - set up as a function

pull_vehicle_avail <- function(yr) {
  get_acs(
    geography = "county subdivision",
    variables = vehicle_avail_vars_sub,
    state = "MA",
    county = "Worcester",
    year = yr,
    survey = "acs5"
  ) |> 
    mutate(NAME = str_remove(NAME, " town, Worcester County, Massachusetts"),
           year = yr) |> 
    filter(NAME %in% communities)
}

#call function with years as input
vehicle_avail_all <- map_dfr(years, pull_vehicle_avail)

#join label from vars
vehicle_avail_all <- vehicle_avail_all |> 
  left_join(vehicle_avail_vars_select |> select(label_short, name), join_by(variable == name))

#export to csv and excel
write_xlsx(vehicle_avail_all, paste(xlsx_path,"vehicle_avail.xlsx", sep = ""))

write_csv(vehicle_avail_all, paste(csv_path, "vehicle_avail.csv", sep = ""))



####Travel time to work####
#table B08303

#variables from ACS
travel_time_vars_all <- vars_2023 |> 
  filter(str_detect(name, regex("B08303", ignore_case = TRUE ))) 

#just the ones we want
travel_time_vars_select <- travel_time_vars_all |> 
  filter(
    name %in% c("B08303_002","B08303_003","B08303_004","B08303_005", "B08303_006", "B08303_007", "B08303_008", "B08303_009", "B08303_010", "B08303_011", "B08303_012", "B08303_013")
  )|> 
  mutate(label_short = str_remove(label, "Estimate!!Total:!!"),
         label_short = str_replace(label_short, ":!!", " - "),
         label_short = str_remove(label_short, ":"))

#just the ones we want in a vector for use in function
travel_time_vars_sub <- travel_time_vars_select |> 
  pull(name)

#pull data - set up as a function

pull_travel_time <- function(yr) {
  get_acs(
    geography = "county subdivision",
    variables = travel_time_vars_sub,
    state = "MA",
    county = "Worcester",
    year = yr,
    survey = "acs5"
  ) |> 
    mutate(NAME = str_remove(NAME, " town, Worcester County, Massachusetts"),
           year = yr) |> 
    filter(NAME %in% communities)
}

#call function with years as input
travel_time_all <- map_dfr(years, pull_travel_time)

#join label from vars
travel_time_all <- travel_time_all |> 
  left_join(travel_time_vars_select |> select(label_short, name), join_by(variable == name))

#export to csv and excel
write_xlsx(travel_time_all, paste(xlsx_path,"travel_time.xlsx", sep = ""))

write_csv(travel_time_all, paste(csv_path, "travel_time.csv", sep = ""))
