# -------------------------------------------------
# Libraries
# -------------------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(rvest)
library(shiny)
library(janitor)
library(readxl)
library(readr)
library(rsconnect)

rsconnect::setAccountInfo(name='hoangde',
                          token='7F7D072A40D48C928369990ED611E363',
                          secret='C9JbgdkNnFLSW9EGB/lf35TZKKocDhOvoDbw4wA+')
theme_set(theme_classic())

# -------------------------------------------------
# Clear environment (optional)
# -------------------------------------------------
rm(list = ls())

# -------------------------------------------------
# Load job_postings data (from RData file)
# -------------------------------------------------
# This must exist in the same folder: job_postings.RData
load("job_postings.RData")   # should create object: job_postings

# -------------------------------------------------
# Helper functions
# -------------------------------------------------
clean_position <- function(title) {
  t <- tolower(title)
  hits <- names(position_map)[str_detect(t, names(position_map))]
  if (length(hits) == 0) "other" else position_map[[hits[1]]]
}

annual_salary <- function(x) {
  case_when(
    x < 100 ~ x * 2000,                 # hourly
    x >= 100 & x < 800 ~ x * 250,       # daily
    x >= 800 & x < 3000 ~ x * 50,       # weekly
    x >= 3000 & x < 10000 ~ x * 12,     # monthly
    x >= 10000 & x < 1000000 ~ x,       # annual
    x >= 1000000 ~ x / 2000,            # fix huge mistakes
    TRUE ~ NA_real_
  )
}

fix_zip <- function(x) {
  x <- as.character(x)
  four_digit <- str_detect(x, "^\\d{4}$")
  x[four_digit] <- paste0("0", x[four_digit])
  x
}

# -------------------------------------------------
# Load raw datasets
# -------------------------------------------------
data <- read_csv("data.csv")
fbc_data_2025 <- read_excel("fbc_data_2025.xlsx", sheet = "County")
Population_Density_Final <- read_excel("Population-Density-Final.xlsx")




# -------------------------------------------------
# Per zip-code data (population density)
# -------------------------------------------------
pop_den <- Population_Density_Final |>
  filter(!row_number() %in% c(1, 2, 3, 4, 5, 6, 7, 8)) |>
  row_to_names(row_number = 1) |>
  unite(CitySt, City, State, sep = ", ", remove = FALSE) |>
  unite(CountySt, County, State, sep = ", ", remove = FALSE) |>
  mutate(zip = fix_zip(Zip))

pop_den_zip <- pop_den |>
  group_by(CitySt) |>
  summarize(zip = first(zip), .groups = "drop")

# -------------------------------------------------
# Per-county cost-of-living (FBC)
# -------------------------------------------------
fbc <- fbc_data_2025 |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  filter(family == "1p0c") |>
  select(state_abv, county, total_2) |>
  mutate(county = str_remove(county, " County$")) |>
  unite(CountySt, county, state_abv, sep = ", ", remove = FALSE)

# -------------------------------------------------
# Per-county weather
# -------------------------------------------------
weather <- data |>
  mutate(
    county = str_remove(Name, " County$"),
    state = str_remove(ID, pattern = "-[0-9][0-9][0-9]")
  ) |>
  unite(CountySt, county, state, sep = ", ", remove = FALSE) |>
  select(CountySt, Value)

# -------------------------------------------------
# Position map (same as in your QMD)
# -------------------------------------------------
position_map <- c(
  "internship" = "entry level",
  "intern" = "entry level",
  "fellow" = "entry level",
  "fellowship" = "entry level",
  "entry level" = "entry level",
  "trainee" = "entry level",
  "junior" = "entry level",

  "sales associate" = "staff",
  "sales representative" = "staff",
  "associate" = "staff",
  "assist" = "staff",
  "assistant" = "staff",
  "clerk" = "staff",

  "representative" = "senior staff",
  "coordinator" = "senior staff",
  "planner" = "senior staff",
  "specialist" = "senior staff",
  "supervisor" = "senior staff",
  "administrator" = "senior staff",
  "account" = "senior staff",
  "controller" = "senior staff",
  "accountant" = "senior staff",
  "operator" = "senior staff",
  "technician" = "senior staff",
  "senior" = "senior staff",

  "manage" = "manager",
  "manager" = "manager",
  "general manager" = "manager",
  "lead" = "manager",
  "leader" = "manager",
  "director" = "manager",

  "executive" = "executive",
  "chief" = "executive",
  "officer" = "executive",
  "president" = "executive",

  "attorney" = "lawyer",
  "lawyer" = "lawyer",

  "analysis" = "analyst",
  "analyst" = "analyst",
  "strategy" = "analyst",
  "strategist" = "analyst",
  "paralegal" = "analyst",
  "consult" = "analyst",
  "consultant" = "analyst",
  "specialist" = "analyst",

  "research" = "researcher",
  "researcher" = "researcher",

  "engineer" = "engineer",
  "architect" = "engineer",

  "developer" = "developer",
  "develop" = "developer",
  "computer scientist" = "developer",

  "nurse" = "nurse",
  "therapy" = "therapist",
  "therapist" = "therapist",
  "counselor" = "therapist",
  "psychologist" = "therapist",
  "social worker" = "therapist",

  "instructor" = "teacher",
  "teacher" = "teacher",
  "coach" = "teacher",

  "design" = "designer",
  "designer" = "designer"
)

# -------------------------------------------------
# Build jobs dataset (same pipeline as QMD)
# -------------------------------------------------
# Clean up job_postings (NYC)
job_postings <- job_postings |>
  mutate(location = if_else(location == "Manhattan, NY",
                            "New York, NY", location))

jobs <- job_postings |>
  drop_na(normalized_salary) |>
  filter(!normalized_salary == 0) |>
  mutate(location = if_else(location == "Manhattan, NY",
                            "New York, NY", location)) |>
  left_join(pop_den_zip, join_by(location == CitySt)) |>
  mutate(zip = if_else(is.na(zip_code), zip, zip_code)) |>
  drop_na(zip) |>
  mutate(
    zip = fix_zip(zip),
    normalized_salary = as.numeric(normalized_salary),
    normalized_salary = annual_salary(normalized_salary),
    position = sapply(title, clean_position)
  ) |>
  left_join(pop_den, join_by(zip == zip)) |>
  left_join(fbc, join_by(CountySt == CountySt)) |>
  left_join(weather, join_by(CountySt == CountySt)) |>
  select(
    title,
    location,
    salary   = normalized_salary,
    position = position,
    density,
    COLwage  = total_2,
    avg_temp = Value
  ) |>
  drop_na(density) |>
  mutate(
    log_density = log(as.numeric(density)),
    log_salary  = log(as.numeric(salary)),
    avg_temp    = as.numeric(avg_temp),
    position    = as.factor(position)
  ) |>
  drop_na(log_density, log_salary, avg_temp) |>
  filter(!is.infinite(log_density))

# -------------------------------------------------
# (Optional) Diagnostics like in QMD
# -------------------------------------------------
# hist(jobs$log_salary)
# hist(jobs$log_density)
# hist(jobs$avg_temp)
# table(jobs$position)

# -------------------------------------------------
# Regression model (same as in QMD)
# -------------------------------------------------
model1 <- lm(log_salary ~ avg_temp + log_density + position, data = jobs)

# -------------------------------------------------
# Helper: Get COL for a location from jobs
# -------------------------------------------------
get_col <- function(location) {
  row <- jobs |>
    filter(location == !!location) |>
    slice(1)
  if (nrow(row) == 0) return(NA_real_)
  as.numeric(row$COLwage)
}

# -------------------------------------------------
# Helper: Build prediction newdata for a location
# -------------------------------------------------
make_newdata <- function(position, location) {

  city_row <- jobs |>
    filter(location == !!location) |>
    slice(1)

  if (nrow(city_row) == 0) return(NULL)

  tibble(
    avg_temp    = city_row$avg_temp,
    log_density = city_row$log_density,
    position    = factor(position, levels = levels(jobs$position))
  )
}

# -------------------------------------------------
# UI
# -------------------------------------------------
ui <- fluidPage(

  titlePanel("Regression-Based Salary & Cost-of-Living Calculator"),

  sidebarLayout(
    sidebarPanel(
      selectInput("position", "Select Position:",
                  choices = sort(unique(jobs$position))),

      selectInput("location1", "Select Location #1:",
                  choices = sort(unique(jobs$location))),

      selectInput("location2", "Select Location #2:",
                  choices = sort(unique(jobs$location)))
    ),

    mainPanel(
      h3("Outputs"),
      verbatimTextOutput("salary1"),
      verbatimTextOutput("salary2"),
      verbatimTextOutput("adjustedSalary"),
      verbatimTextOutput("COL"),
      hr(),
      h4("Notes:"),
      p("Salary estimates come from the log-salary regression model."),
      p("If adjusted salary is higher, it means location #1 is better.
        If adjusted salary is lower, then location #2 is better.")
    )
  )
)

# -------------------------------------------------
# SERVER
# -------------------------------------------------
server <- function(input, output, session) {

  # Salary #1
  output$salary1 <- renderText({
    nd <- make_newdata(input$position, input$location1)
    if (is.null(nd)) return("No data for that city.")
    pred <- exp(predict(model1, newdata = nd))
    paste0("Predicted Salary #1 in ", input$location1, ": $",
           round(pred, 2))
  })

  # Salary #2
  output$salary2 <- renderText({
    nd <- make_newdata(input$position, input$location2)
    if (is.null(nd)) return("No data for that city.")
    pred <- exp(predict(model1, newdata = nd))
    paste0("Predicted Salary #2 in ", input$location2, ": $",
           round(pred, 2))
  })

  # Adjusted Salary
  output$adjustedSalary <- renderText({
    nd1 <- make_newdata(input$position, input$location1)
    if (is.null(nd1)) return("Missing city data.")

    salary1 <- exp(predict(model1, newdata = nd1))

    col1 <- get_col(input$location1)
    col2 <- get_col(input$location2)

    if (is.na(col1) || is.na(col2)) {
      return("Missing COLwage data for one of the locations.")
    }

    adjusted <- salary1 * col2 / col1

    paste0("Salary #1 adjusted to cost of living in ",
           input$location2, ": $", round(adjusted, 2))
  })

  # COL outputs
  output$COL <- renderText({
    col1 <- get_col(input$location1)
    col2 <- get_col(input$location2)
    paste0("Cost of living in location #1 and location #2 for 1p0c: ",
           col1, " and ", col2)
  })
}

# -------------------------------------------------
# Run app
# -------------------------------------------------
shinyApp(ui, server)



