# Salary & Cost-of-Living Comparison App
This repository contains all source code and data required to run the app locally.
This Shiny application estimates salaries across U.S. locations using a
regression model and adjusts them for cost of living differences.

## Live App
👉 https://hoangde.shinyapps.io/salary/

## Features
- Predicts salary by position and location
- Adjusts salary for cost-of-living differences
- Uses population density and average temperature as covariates
- Built using a log-linear regression model

## Data Sources
- Job postings dataset (`job_postings.RData`)
- County-level cost-of-living data (FBC)
- Population density by ZIP code
- County-level weather data

## How to Run Locally

### Requirements
- R (≥ 4.2 recommended)
- RStudio (optional)

### Install packages
```r
install.packages(c(
  "shiny", "tidyverse", "janitor",
  "readxl", "readr", "stringr"
))
shiny::runApp()
