
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
    set = c("W", "M", "S", "D"),
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

#-----------------------------------------------------------------------------------------------------------------------------

#checking marital status types
patients[, .N, marital] #We have W, M, S, D

#changing them to labels we think are better
patients[,
  marital := factor(
    marital,
    levels = c("S", "M", "D", "W"),
    labels = c("Single", "Married", "Divorced", "Widowed")
  )
]

#changing labels on other code
fctr_candidates <-
  patients[, which(lapply(.SD, uniqueN) < 10), .SDcols = is.character] |>
  names()
#the columns in SD where there are less than 10 different options and they are caracters are collected into this variable

patients[,
  lapply(.SD, \(x) paste(unique(x), collapse = ", ")),
  .SDcols = fctr_candidates
] |>
  glimpse()
#it prints the columns which should be factors are printed alongside their categories
#Rows: 1
#Columns: 6
#$ prefix    <chr> "Mr., NA, Mrs., Ms."
#$ suffix    <chr> "NA, JD, MD, PhD"
#$ race      <chr> "white, black, native, asian, hawaiian, other"
#$ ethnicity <chr> "nonhispanic, hispanic"
#$ gender    <chr> "M, F"
#$ state     <chr> "Massachusetts, Alaska, Alabama, Arizona, Arkansas, California"

#The only one I think needs changing the names of is gender, so
#changing them to labels we think are better
patients[,
  gender := factor(
    gender,
    levels = c("M", "F"),
    labels = c("Male", "Female")
  ),
  prefix := factor(prefix)
]

#convert to factor when labels are already good
patients[,
  (c("prefix", "suffix", "race", "ethnicity", "state")) := lapply(.SD, as.factor), #SD makes it work, as := requires a data table
  .SDcols = c("prefix", "suffix", "race", "ethnicity", "state")
]

#now rerunning the earlier code returns 0 suggestions to convert to factors


#-----------------------------------------------------------------------------------------------------------------------------
