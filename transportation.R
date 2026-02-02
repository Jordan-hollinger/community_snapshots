library(tidycensus)
library(tidyverse)
library(writexl)

####set personal access token (current PAT expires Sun, Mar 29 2026) and Census API Key####
#To set github PAT:
#gitcreds::gitcreds_set()

#if you do not have a census api key set globally in R, you will need to do that via: 
# census_api_key("YOUR KEY GOES HERE", install = TRUE)


####load and find the vars you want####

#Important note: best to use detailed tables for MA Towns when pulling multiple years. Can also use Data Profiles, but beware that variable ids may not be consistent between years.
vars_2023 <- load_variables(2023, "acs5", cache = TRUE)

#Exploration helper (run when adding new topics) - swap in table your interest in
vars_B08303 <- vars_2023 |> filter(str_starts(name, "B08303"))


####Global variables/objects####

#All Towns/Cities
communities <- c("Auburn", "Barre", "Berlin", "Blackstone", "Boylston", "Brookfield", "Charlton", "Douglas", "Dudley", "East Brookfield", "Grafton", "Hardwick", "Holden", "Hopedale", "Leicester", "Mendon", "Millbury", "Millville", "New Braintree", "North Brookfield", "Northborough", "Northbridge", "Oakham", "Oxford", "Paxton", "Princeton", "Rutland", "Shrewsbury", "Southbridge", "Spencer", "Sturbridge", "Sutton", "Upton", "Uxbridge", "Warren", "Webster", "West Boylston", "West Brookfield", "Westborough", "Worcester")

#Years to pull
years <- c(2010:2023)

#Topic specs -- add in the topics and variables that you need to pull
topic_specs <- list(
  list(
    topic = "commute_mode",
    table = "B08301",
    vars  = c("B08301_003", "B08301_004", "B08301_010", "B08301_016",
              "B08301_017", "B08301_018", "B08301_019", "B08301_020", "B08301_021")
  ),
  list(
    topic = "vehicle_avail",
    table = "B08201",
    vars  = c("B08201_002", "B08201_003", "B08201_004", "B08201_005", "B08201_006")
  ),
  list(
    topic = "travel_time",
    table = "B08303",
    vars  = c("B08303_002","B08303_003","B08303_004","B08303_005","B08303_006",
              "B08303_007","B08303_008","B08303_009","B08303_010","B08303_011",
              "B08303_012","B08303_013")
  )
)

#excel and csv file location
xlsx_path <- "data/transportation/xlsx/"
csv_path <- "data/transportation/csv/"


####Functions####

#Missing year checker for variables
check_vars_by_year <- function(spec, years, survey = "acs5") {
  map_dfr(years, function(yr) {
    vars_yr <- load_variables(yr, survey, cache = TRUE)
    
    missing <- setdiff(spec$vars, vars_yr$name)
    
    tibble(
      topic = spec$topic,
      year = yr,
      n_missing = length(missing),
      missing_vars = paste(missing, collapse = ", ")
    )
  }) |>
    filter(n_missing > 0)
}

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

#run everything
run_topic <- function(spec, vars_master, years, communities, csv_path, xlsx_path, survey = "acs5") {
  
  # 1) Identify years where any requested variable is missing
  missing_report <- check_vars_by_year(spec, years, survey = survey)
  
  bad_years <- missing_report |>
    distinct(year) |>
    pull(year)
  
  good_years <- setdiff(years, bad_years)
  
  if (length(good_years) == 0) {
    warning(sprintf("Topic '%s': no usable years (all years missing at least one variable).", spec$topic))
    return(list(data = tibble(), missing = missing_report))
  }
  
  if (length(bad_years) > 0) {
    message(sprintf(
      "Topic '%s': skipped %d year(s): %s",
      spec$topic, length(bad_years), paste(sort(bad_years), collapse = ", ")
    ))
  }
  
  # 2) Build variable label lookup from your master year (2023)
  vars_all <- vars_master |>
    filter(str_starts(name, spec$table))
  
  vars_select <- vars_all |>
    filter(name %in% spec$vars) |>
    left_join(make_label_lookup(vars_all), by = "name")
  
  # 3) Pull only good years and attach labels
  df <- pull_acs_multiyear(
    var_ids = vars_select$name,
    years = good_years,
    communities = communities,
    survey = survey
  ) |>
    left_join(vars_select |> select(name, label_short),
              join_by(variable == name))
  
  # 4) Export
  export_csv_xlsx(
    df = df,
    filename = spec$topic,
    csv_path = csv_path,
    xlsx_path = xlsx_path
  )
  
  # Return both data + report so you can inspect/export later
  list(data = df, missing = missing_report)
}



####Run Data Pull for all topics####
topic_results <- map(
  topic_specs,
  run_topic,
  vars_master = vars_2023,
  years = years,
  communities = communities,
  csv_path = csv_path,
  xlsx_path = xlsx_path
)

names(topic_results) <- map_chr(topic_specs, "topic")


missing_report_all <- map_dfr(topic_results, "missing")

# Optional: export the missing report so it’s documented
export_csv_xlsx(
  df = missing_report_all,
  filename = "acs_missing_variable_report",
  csv_path = csv_path,
  xlsx_path = xlsx_path
)

