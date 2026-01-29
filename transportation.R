library(tidycensus)
library(tidyverse)
library(gitcreds)

####set personal access token (current PAT expires Sun, Mar 29 2026) and Census API Key####

#To set github PAT:
#gitcreds_set()

#if you do not have a census api key set globally in R, you will need to do that via: 
# census_api_key("YOUR KEY GOES HERE", install = TRUE)

####CMRPC Communities listed in a vector####
communities <- c("Auburn", "Barre", "Berlin", "Blackstone", "Boylston", "Brookfield", "Charlton", "Douglas", "Dudley", "East Brookfield", "Grafton", "Hardwick", "Holden", "Hopedale", "Leicester", "Mendon", "Millbury", "Millville", "New Braintree", "North Brookfield", "Northborough", "Northbridge", "Oakham", "Oxford", "Paxton", "Princeton", "Rutland", "Shrewsbury", "Southbridge", "Spencer", "Sturbridge", "Sutton", "Upton", "Uxbridge", "Warren", "Webster", "West Boylston", "West Brookfield", "Westborough", "Worcester")



####Initial setup to get consistent variable ids across time####

##best to use detailecd tables for MA Towns. Can also use Data Profiles, but beware that variable ids may not be consistent between years.

#load variables for a particular year
vars_2023 <- load_variables(2023, "acs5", cache = TRUE)

#years for variable pulls
years <- c(2010:2023)


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




####Pull vehicles available####