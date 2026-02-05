library(tidycensus)
library(tidyverse)
library(writexl)

#### set personal access token and Census API Key ####
# gitcreds::gitcreds_set()
# census_api_key("YOUR KEY GOES HERE", install = TRUE)

#### load variables for exploration (optional) ####
vars_all <- load_variables(2010, "acs5", cache = TRUE)

# Exploration helper (swap in any table ID you want to inspect)
vars_B08303 <- vars_2023 |> filter(str_starts(name, "B08303"))

#### Global variables/objects ####

communities <- c(
  "Auburn", "Barre", "Berlin", "Blackstone", "Boylston", "Brookfield", "Charlton",
  "Douglas", "Dudley", "East Brookfield", "Grafton", "Hardwick", "Holden", "Hopedale",
  "Leicester", "Mendon", "Millbury", "Millville", "New Braintree", "North Brookfield",
  "Northborough", "Northbridge", "Oakham", "Oxford", "Paxton", "Princeton", "Rutland",
  "Shrewsbury", "Southbridge", "Spencer", "Sturbridge", "Sutton", "Upton", "Uxbridge",
  "Warren", "Webster", "West Boylston", "West Brookfield", "Westborough", "Worcester"
)

years <- 2010:2023

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

xlsx_path <- "data/transportation/xlsx/"
csv_path  <- "data/transportation/csv/"

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
    
    # Load variable metadata for this year and table
    vars_yr_tbl <- load_variables(yr, survey, cache = TRUE) |>
      filter(str_starts(name, spec$table))
    
    # Only request vars that exist in this year
    vars_available <- intersect(spec$vars, vars_yr_tbl$name)
    
    # If none exist for this year, return empty
    if (length(vars_available) == 0) {
      return(tibble())
    }
    
    # Pull ACS for available vars only
    df <- get_acs(
      geography = geography,
      variables = vars_available,
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
    
    # Year-specific labels for available vars
    label_lookup_yr <- make_label_lookup(vars_yr_tbl) |>
      filter(name %in% vars_available) |>
      select(name, label_short)
    
    df |>
      left_join(label_lookup_yr, join_by(variable == name))
  }
  
  purrr::map_dfr(years, pull_one_year)
}

# Run one topic (skip years with missing vars, label per-year)
run_topic <- function(spec, years, communities, csv_path, xlsx_path, survey = "acs5") {
  
  # report missing vars by year (still produced/exported later)
  missing_report <- check_vars_by_year(spec, years, survey = survey)
  
  # Pull all years, but allow each year to pull only the vars that exist
  df <- pull_acs_multiyear_labeled(
    spec = spec,
    years = years,
    communities = communities,
    survey = survey
  )
  
  # Optional: message summary (no skipping, just reporting)
  years_with_missing <- missing_report |>
    distinct(year) |>
    pull(year)
  
  if (length(years_with_missing) > 0) {
    message(sprintf(
      "Topic '%s': %d year(s) have missing vars (pulled available vars anyway): %s",
      spec$topic, length(years_with_missing), paste(sort(years_with_missing), collapse = ", ")
    ))
  }
  
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

#check missing vars
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
csv_dir <- "data/transportation/csv"

topic_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE) |>
  purrr::keep(~ !basename(.x) %in% c("acs_missing_variable_report.csv", "all_topics_long.csv"))

all_topics_long <- topic_files |>
  purrr::map_dfr(
    ~ readr::read_csv(.x, show_col_types = FALSE) |>
      mutate(topic = tools::file_path_sans_ext(basename(.x)))
  )

readr::write_csv(all_topics_long, file.path(csv_dir, "all_topics_long.csv"))
