library(tidycensus)
library(tidyverse)
library(writexl)

####set personal access token (current PAT expires Sun, Mar 29 2026) and Census API Key####

#To set github PAT:
#gitcreds::gitcreds_set()

#if you do not have a census api key set globally in R, you will need to do that via: 
# census_api_key("YOUR KEY GOES HERE", install = TRUE)


####Initial setup - var list, global vars and functions####

communities <- c("Auburn", "Barre", "Berlin", "Blackstone", "Boylston", "Brookfield", "Charlton", "Douglas", "Dudley", "East Brookfield", "Grafton", "Hardwick", "Holden", "Hopedale", "Leicester", "Mendon", "Millbury", "Millville", "New Braintree", "North Brookfield", "Northborough", "Northbridge", "Oakham", "Oxford", "Paxton", "Princeton", "Rutland", "Shrewsbury", "Southbridge", "Spencer", "Sturbridge", "Sutton", "Upton", "Uxbridge", "Warren", "Webster", "West Boylston", "West Brookfield", "Westborough", "Worcester")

##best to use detailecd tables for MA Towns. Can also use Data Profiles, but beware that variable ids may not be consistent between years.

#load variables for a particular year
vars_2023 <- load_variables(2023, "acs5", cache = TRUE)

#years for variable pulls
years <- c(2010:2023)

#excel and csv file location
xlsx_path <- "data/transportation/xlsx/"
csv_path <- "data/transportation/csv/"

#global export function
export_csv_xlsx <- function(df, filename, csv_path, xlsx_path) {
  
  # ensure directories exist
  dir.create(csv_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(xlsx_path, recursive = TRUE, showWarnings = FALSE)
  
  # build full file paths
  csv_file  <- file.path(csv_path,  paste0(filename, ".csv"))
  xlsx_file <- file.path(xlsx_path, paste0(filename, ".xlsx"))
  
  # write files
  readr::write_csv(df, csv_file)
  writexl::write_xlsx(df, xlsx_file)
  
  invisible(list(csv = csv_file, xlsx = xlsx_file))
}

#global pull function
pull_acs_multiyear <- function(var_ids, years,
                               geography = "county subdivision",
                               state = "MA",
                               county = "Worcester",
                               survey = "acs5",
                               communities) {
  
  pull_one_year <- function(yr) {
    get_acs(
      geography = geography,
      variables = var_ids,
      state = state,
      county = county,
      year = yr,
      survey = survey
    ) |>
      mutate(
        NAME = NAME |>
          str_extract("^[^,]+") |>
          str_remove("\\s+(town|city)$") |>
          str_trim(),
        year = yr
      ) |>
      filter(NAME %in% communities)
  }
  
  map_dfr(years, pull_one_year)
}

#global label lookup column function
make_label_lookup <- function(vars_tbl, prefix = "Estimate!!Total:!!") {
  vars_tbl |>
    transmute(
      name,
      label_short = label |>
        str_remove(prefix) |>
        str_replace(":!!", " - ") |>
        str_remove(":") |>
        str_trim()
    )
}


####Commute Mode to work####

#variables from ACS
commute_mode_vars_all <- vars_2023 |> 
  filter(str_detect(name, regex("B08301", ignore_case = TRUE ))) 

#just the ones we want
commute_mode_vars_select <- commute_mode_vars_all |> 
  filter(
    name %in% c("B08301_003", "B08301_004", "B08301_010", "B08301_016", "B08301_017", "B08301_018", "B08301_019", "B08301_020", "B08301_021")
  ) |> 
  left_join(make_label_lookup(commute_mode_vars_all), by = "name")

#vector for use in function
commute_mode_vars_sub <- commute_mode_vars_select |> 
  pull(name)

#pull data using our global function

commute_mode_all <- pull_acs_multiyear(
  var_ids = commute_mode_vars_sub,
  years = years,
  communities = communities
)

#join short label from vars
commute_mode_all <- commute_mode_all |> 
  left_join(commute_mode_vars_select |> select(label_short, name), join_by(variable == name))

#export to csv and excel
export_csv_xlsx(
  df = commute_mode_all,
  filename = "commute_mode",
  csv_path = csv_path,
  xlsx_path = xlsx_path
)


####Vehicles available####

#variables from ACS
vehicle_avail_vars_all <- vars_2023 |> 
  filter(str_detect(name, regex("B08201", ignore_case = TRUE ))) 

#just the ones we want
vehicle_avail_vars_select <- vehicle_avail_vars_all |> 
  filter(
    name %in% c("B08201_002", "B08201_003", "B08201_004", "B08201_005", "B08201_006")
  )|> 
  left_join(make_label_lookup(vehicle_avail_vars_all), by = "name")
  
#just the ones we want in a vector for use in function
vehicle_avail_vars_sub <- vehicle_avail_vars_select |> 
  pull(name)

#pull data using our global function

vehicle_avail_all <- pull_acs_multiyear(
  var_ids = vehicle_avail_vars_sub,
  years = years,
  communities = communities
)

#join short label from vars
vehicle_avail_all <- vehicle_avail_all |> 
  left_join(vehicle_avail_vars_select |> select(label_short, name), join_by(variable == name))

#export to csv and excel
export_csv_xlsx(
  df = vehicle_avail_all,
  filename = "vehicle_avail",
  csv_path = csv_path,
  xlsx_path = xlsx_path
)


####Travel time to work####

#variables from ACS
travel_time_vars_all <- vars_2023 |> 
  filter(str_detect(name, regex("B08303", ignore_case = TRUE ))) 

#just the ones we want
travel_time_vars_select <- travel_time_vars_all |> 
  filter(
    name %in% c("B08303_002","B08303_003","B08303_004","B08303_005", "B08303_006", "B08303_007", "B08303_008", "B08303_009", "B08303_010", "B08303_011", "B08303_012", "B08303_013")
  )|> 
  left_join(make_label_lookup(travel_time_vars_all), by = "name")

#just the ones we want in a vector for use in function
travel_time_vars_sub <- travel_time_vars_select |> 
  pull(name)

#pull data using our global function

travel_time_all <- pull_acs_multiyear(
  var_ids = travel_time_vars_sub,
  years = years,
  communities = communities
)

#join short label from vars
travel_time_all <- travel_time_all |> 
  left_join(travel_time_vars_select |> select(label_short, name), join_by(variable == name))

#export to csv and excel
export_csv_xlsx(
  df = travel_time_all,
  filename = "travel_time",
  csv_path = csv_path,
  xlsx_path = xlsx_path
)
