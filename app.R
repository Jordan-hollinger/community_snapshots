# app.R

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(scales)
library(plotly)
library(shinyWidgets)

DATA_PATH <- "data/transportation/csv/all_topics_long.csv"
MAX_TOWNS <- 5

raw <- read_csv(
  DATA_PATH,
  show_col_types = FALSE,
  progress = FALSE
)

df <- raw |>
  mutate(
    town  = str_trim(str_remove(as.character(NAME), ",.*$")),
    year  = suppressWarnings(as.integer(year)),
    value = suppressWarnings(as.numeric(estimate)),
    moe   = suppressWarnings(as.numeric(moe)),
    label = as.character(label_short),
    topic_raw = as.character(topic),
    
    town_key  = str_to_lower(str_trim(town)),
    topic_key = str_to_lower(str_trim(topic_raw))
  ) |>
  filter(
    !is.na(town_key), town_key != "",
    !is.na(topic_key), topic_key != "",
    !is.na(year),
    !is.na(label), label != "",
    !is.na(value),
    is.finite(value)
  )

if (nrow(df) == 0) stop("After cleaning, df has 0 rows. Check required columns and missing values.")

# ---- Topic dropdown: show sentence case, store raw key ----
topic_keys   <- sort(unique(df$topic_key))
topic_labels <- str_to_sentence(gsub("_", " ", topic_keys))
topic_choices_named <- setNames(topic_keys, topic_labels)  # names shown, values stored

# ---- Town dropdown: show town name, store town_key ----
town_lookup <- df |>
  distinct(town_key, town) |>
  arrange(town)
town_choices_named <- setNames(town_lookup$town_key, town_lookup$town)

year_choices <- sort(unique(df$year))

default_topic <- if (length(topic_keys) > 0) topic_keys[[1]] else NULL
default_year  <- if (length(year_choices) > 0) max(year_choices, na.rm = TRUE) else NULL
default_town  <- if (length(town_choices_named) > 0) unname(town_choices_named)[[1]] else NULL

# ---- Fixed palette for up to 5 towns (your current approach) ----
town_palette <- c(
  "#264653",
  "#2a9d8f",
  "#e9c46a",
  "#f4a261",
  "#e76f51"
)

ui <- navbarPage(
  title = "Community Snapshots (Prototype)",
  
  # ------------------- Transportation tab (LIVE) -------------------
  tabPanel(
    "Transportation",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "topic_key", "Topic",
          choices = topic_choices_named,
          selected = default_topic
        ),
        
        selectInput(
          "year", "Year",
          choices = year_choices,
          selected = default_year
        ),
        
        pickerInput(
          inputId = "town_keys",
          label   = "Town(s)",
          choices = town_choices_named,
          selected = default_town,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            size = 10
          )
        ),
        
        radioButtons(
          "metric", "Display",
          choices = c(
            "Raw estimate" = "raw",
            "Percent of total (within town)" = "pct"
          ),
          selected = "raw"
        ),
        
        checkboxInput("sort_bars", "Sort bars (descending)", TRUE),
        
        hr(),
        checkboxInput("show_table", "Show data table", value = FALSE),
        
        hr(),
        strong("Diagnostics"),
        verbatimTextOutput("diag")
      ),
      
      mainPanel(
        plotlyOutput("bar_plot", height = 560),
        
        conditionalPanel(
          condition = "input.show_table == true",
          hr(),
          h4("Filtered data preview"),
          tableOutput("preview")
        )
      )
    )
  ),
  
  # ------------------- Housing tab (placeholder) -------------------
  tabPanel(
    "Housing",
    fluidRow(
      column(
        8,
        h3("Housing"),
        p("Coming soon."),
        p("This tab is a placeholder so you can see the multi-domain layout. Once you have housing data, we’ll wire in the same pattern: filters on the left, interactive chart(s) on the right.")
      )
    )
  ),
  
  # ------------------- Municipal Finance tab (placeholder) -------------------
  tabPanel(
    "Municipal Finance",
    fluidRow(
      column(
        8,
        h3("Municipal Finance"),
        p("Coming soon."),
        p("When you're ready, we can add a finance dataset and either reuse the same bar-chart framework or build finance-specific visuals (e.g., trends over time, per-capita metrics, comparisons).")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- Cap towns at MAX_TOWNS ---
  last_valid_towns <- reactiveVal(default_town)
  
  observeEvent(input$town_keys, {
    req(input$town_keys)
    
    if (length(input$town_keys) > MAX_TOWNS) {
      showNotification(
        paste0("Please select no more than ", MAX_TOWNS, " towns for comparison."),
        type = "warning",
        duration = 4
      )
      updatePickerInput(session, inputId = "town_keys", selected = last_valid_towns())
    } else {
      last_valid_towns(input$town_keys)
    }
  }, ignoreInit = TRUE)
  
  match_counts <- reactive({
    req(input$topic_key, input$year, input$town_keys)
    
    n_total <- nrow(df)
    n_topic <- df |> filter(topic_key == input$topic_key) |> nrow()
    n_year  <- df |> filter(topic_key == input$topic_key, year == as.integer(input$year)) |> nrow()
    n_town  <- df |> filter(topic_key == input$topic_key, year == as.integer(input$year), town_key %in% input$town_keys) |> nrow()
    
    list(total = n_total, after_topic = n_topic, after_year = n_year, after_town = n_town)
  })
  
  output$diag <- renderText({
    req(input$topic_key, input$year, input$town_keys)
    m <- match_counts()
    paste0(
      "Rows in df: ", m$total, "\n",
      "After topic filter: ", m$after_topic, "\n",
      "After year filter:  ", m$after_year, "\n",
      "After town filter:  ", m$after_town, "\n\n",
      "Selected topic_key: ", input$topic_key, "\n",
      "Selected year:      ", input$year, "\n",
      "Selected town_keys: ", paste(input$town_keys, collapse = ", ")
    )
  })
  
  filtered <- reactive({
    req(input$topic_key, input$year, input$town_keys)
    
    df |>
      filter(
        topic_key == input$topic_key,
        year == as.integer(input$year),
        town_key %in% input$town_keys
      )
  })
  
  plot_data <- reactive({
    dat <- filtered()
    validate(need(nrow(dat) > 0, "No rows match your selection."))
    
    if (input$metric == "pct") {
      dat <- dat |>
        group_by(topic_key, year, town_key) |>
        mutate(
          total = sum(value, na.rm = TRUE),
          value_plot = if_else(total > 0, value / total, NA_real_)
        ) |>
        ungroup()
    } else {
      dat <- dat |>
        mutate(value_plot = value)
    }
    
    dat |>
      filter(!is.na(value_plot), is.finite(value_plot))
  })
  
  output$bar_plot <- renderPlotly({
    dat <- plot_data()
    
    if (isTRUE(input$sort_bars)) {
      label_order <- dat |>
        group_by(label) |>
        summarize(total = sum(value_plot, na.rm = TRUE), .groups = "drop") |>
        arrange(desc(total)) |>
        pull(label)
      dat <- dat |> mutate(label = factor(label, levels = label_order))
    } else {
      dat <- dat |> mutate(label = factor(label))
    }
    
    title_txt <- paste0(str_to_sentence(gsub("_", " ", input$topic_key)), " (", input$year, ")")
    
    dat <- dat |>
      mutate(
        hover = if (input$metric == "pct") {
          paste0(
            "Town: ", town,
            "<br>Label: ", as.character(label),
            "<br>Percent: ", percent(value_plot, accuracy = 0.1),
            "<br>Count: ", comma(value)
          )
        } else {
          paste0(
            "Town: ", town,
            "<br>Label: ", as.character(label),
            "<br>Count: ", comma(value),
            ifelse(!is.na(moe), paste0("<br>MOE: ", comma(moe)), "")
          )
        }
      )
    
    y_title <- if (input$metric == "pct") "Percent of total" else "Estimate"
    
    plot_ly(
      data = dat,
      x = ~label,
      y = ~value_plot,
      color = ~town,
      colors = town_palette,
      type = "bar",
      hovertext = ~hover,
      hoverinfo = "text"
    ) |>
      layout(
        title = list(text = title_txt),
        barmode = "group",
        xaxis = list(title = "", tickangle = -35),
        yaxis = list(
          title = y_title,
          tickformat = if (input$metric == "pct") ".0%" else NULL
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.45,
          xanchor = "left",
          yanchor = "top"
        ),
        margin = list(b = 140)
      )
  })
  
  output$preview <- renderTable({
    dat <- plot_data()
    
    if (input$metric == "pct") {
      dat |>
        mutate(percent = value_plot) |>
        select(town, year, topic = topic_raw, label, estimate = value, percent, moe, variable, GEOID) |>
        arrange(town, desc(percent)) |>
        head(25)
    } else {
      dat |>
        select(town, year, topic = topic_raw, label, estimate = value, moe, variable, GEOID) |>
        arrange(town, desc(estimate)) |>
        head(25)
    }
  })
}

shinyApp(ui, server)

