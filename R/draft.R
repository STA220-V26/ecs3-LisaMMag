
library(tidyverse)
library(data.table)
library(pointblank)

#download data
if (!fs::file_exists("data.zip")) {
  curl::curl_download(
    "https://github.com/eribul/cs/raw/refs/heads/main/data.zip",
    "data.zip",
    quiet = FALSE
  )
}

#unzip the data
patients <-
  readr::read_csv(unz("data.zip", "data-fixed/patients.csv")) |>
  setDT() |>
  setkey(id)

#remove rows or columns with only missing data
patients <- janitor::remove_empty(patients, quiet = FALSE)
patients <- janitor::remove_constant(patients, quiet = FALSE) # A column with only one constant value is also not very interesting

#check the data 
checks <-
  patients |>
  create_agent(label = "Checking the patient data") |>
  col_vals_between(
    where(is.Date),
    as.Date("1900-01-01"),
    as.Date(Sys.Date()),
    na_pass = TRUE,
    label = "Are all birthdays dates after 1900 and before today"
  ) |>
  col_vals_gte(
    deathdate,
    vars(birthdate),
    na_pass = TRUE,
    label = "Checks is birthdays are after death dates"
  ) |>
  col_vals_regex(
    ssn, # This variable was described in ECS1!
    "[0-9]{3}-[0-9]{2}-[0-9]{4}$",
    label = "Checks that the (social security?) numbers are in the correct format"
  ) |>
  col_is_integer(
    id,
    label = "Checks that ID's are integers, not numerics or strings" #it fails this
  ) |>
  col_vals_in_set(
    gender,
    set = c("M", "F"),
    label = "Check that the only genders are male (M) or female (F)"
  ) |>
col_vals_in_set(
    marital, 
    set = c("W", "M", "S"),
    #na_pass = TRUE,
    label = "Check that marital status only has one letter (W, M, or S)"
  ) |>
col_vals_expr(
    expr = ~ case_when(
      prefix == "Mr." ~ gender == "M",
      prefix == "Mrs." ~ gender == "F",
      TRUE ~ TRUE # Ignore other prefixes like "Dr." or "Ms."
    ),
    label = "Check that Mr. = M and Mrs. = F"
  ) |>
  interrogate()

checks


export_report(checks, "patient_validation.html")


