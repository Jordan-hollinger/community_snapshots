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

#Exploration helper (run when adding new topics) - swap in table your interested in
table_temp <- vars_2023 |> filter(str_starts(name, "B25041"))

temp_vars <- table_temp$name[-1]
temp_vars
####Global variables/objects####

#All Towns/Cities
communities <- c("Auburn", "Barre", "Berlin", "Blackstone", "Boylston", "Brookfield", "Charlton", "Douglas", "Dudley", "East Brookfield", "Grafton", "Hardwick", "Holden", "Hopedale", "Leicester", "Mendon", "Millbury", "Millville", "New Braintree", "North Brookfield", "Northborough", "Northbridge", "Oakham", "Oxford", "Paxton", "Princeton", "Rutland", "Shrewsbury", "Southbridge", "Spencer", "Sturbridge", "Sutton", "Upton", "Uxbridge", "Warren", "Webster", "West Boylston", "West Brookfield", "Westborough", "Worcester")

#Years to pull
years <- c(2010:2023)

#Topic specs -- add in the topics and variables that you need to pull
topic_specs <- list(
  list(
    topic = "units_in_structure",
    table = "B25024",
    vars  = c("B25024_002", "B25024_003", "B25024_004", "B25024_005", "B25024_006", "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25024_011")
  ),
  list(
    topic = "year_built",
    table = "B25034",
    vars  = c("B25034_002", "B25034_003", "B25034_004", "B25034_005", "B25034_006", "B25034_007", "B25034_008", "B25034_009", "B25034_010", "B25034_011")
  ),
  list(
    topic = "occupancy",
    table = "B25002",
    vars  = c("B25002_002", "B25002_003")
  ),
  list(
    topic = "bedrooms",
    table = "B25041",
    vars  = c("B25041_002", "B25041_003", "B25041_004", "B25041_005", "B25041_006", "B25041_007")
    )
)

xlsx_path <- "data/housing/xlsx/"
csv_path  <- "data/housing/csv/"

#### Functions ####

# Missing year checker for variables
check_vars_by_year <- function(spec, years, survey = "acs5") {
  purrr::map_dfr(years, function(yr) {
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

# Export helper
export_csv_xlsx <- function(df, filename, csv_path, xlsx_path) {
  dir.create(csv_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(xlsx_path, recursive = TRUE, showWarnings = FALSE)
  
  csv_file  <- file.path(csv_path,  paste0(filename, ".csv"))
  xlsx_file <- file.path(xlsx_path, paste0(filename, ".xlsx"))
  
  readr::write_csv(df, csv_file)
  writexl::write_xlsx(df, xlsx_file)
  
  invisible(list(csv = csv_file, xlsx = xlsx_file))
}

# Label lookup helper
make_label_lookup <- function(vars_tbl) {
  vars_tbl |>
    transmute(
      name,
      label_short = label |>
        # Remove either "Estimate!!Total:!!" or "Estimate!!Total!!"
        str_remove("^Estimate!!Total:??!!") |>
        # Convert ACS label separators into something readable
        str_replace_all("!!", " - ") |>
        # Clean up any stray punctuation/spacing
        str_replace_all("\\s*-\\s*-\\s*", " - ") |>
        str_trim()
    )
}

# Multiyear pull where each year gets labels from THAT YEAR
pull_acs_multiyear_labeled <- function(spec, years,
                                       geography = "county subdivision",
                                       state = "MA",
                                       county = "Worcester",
                                       survey = "acs5",
                                       communities) {
  pull_one_year <- function(yr) {
    
    df <- get_acs(
      geography = geography,
      variables = spec$vars,
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
    
    # labels for this year
    vars_yr_tbl <- load_variables(yr, survey, cache = TRUE) |>
      filter(str_starts(name, spec$table)) |>
      filter(name %in% spec$vars)
    
    label_lookup_yr <- make_label_lookup(vars_yr_tbl)
    
    df |>
      left_join(label_lookup_yr, join_by(variable == name))
  }
  
  purrr::map_dfr(years, pull_one_year)
}

# Run one topic (skip years with missing vars, label per-year)
run_topic <- function(spec, years, communities, csv_path, xlsx_path, survey = "acs5") {
  
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
  
  df <- pull_acs_multiyear_labeled(
    spec = spec,
    years = good_years,
    communities = communities,
    survey = survey
  )
  
  export_csv_xlsx(
    df = df,
    filename = spec$topic,
    csv_path = csv_path,
    xlsx_path = xlsx_path
  )
  
  list(data = df, missing = missing_report)
}

#### Run Data Pull for all topics ####
topic_results <- purrr::map(
  topic_specs,
  run_topic,
  years = years,
  communities = communities,
  csv_path = csv_path,
  xlsx_path = xlsx_path
)

names(topic_results) <- purrr::map_chr(topic_specs, "topic")

missing_report_all <- purrr::map_dfr(topic_results, "missing")

# Export missing report (documented)
export_csv_xlsx(
  df = missing_report_all,
  filename = "acs_missing_variable_report",
  csv_path = csv_path,
  xlsx_path = xlsx_path
)

#### Append all topic CSVs for Shiny (exclude report + previous combined file) ####
csv_dir <- "data/housing/csv"

topic_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE) |>
  purrr::keep(~ !basename(.x) %in% c("acs_missing_variable_report.csv", "all_topics_long.csv"))

all_topics_long <- topic_files |>
  purrr::map_dfr(
    ~ readr::read_csv(.x, show_col_types = FALSE) |>
      mutate(topic = tools::file_path_sans_ext(basename(.x)))
  )

readr::write_csv(all_topics_long, file.path(csv_dir, "all_topics_long.csv"))
