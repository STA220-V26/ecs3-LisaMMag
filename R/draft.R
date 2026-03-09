#-----------------------------------------------------------------------------------------------------------------------------
#Load packages
#-----------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(pointblank)
library(duckplyr)
library(leaflet)
library(decoder)

#download data
if (!fs::file_exists("data.zip")) {
  curl::curl_download(
    "https://github.com/eribul/cs/raw/refs/heads/main/data.zip",
    "data.zip",
    quiet = FALSE
  )
}


#-----------------------------------------------------------------------------------------------------------------------------
#Patient Data
#-----------------------------------------------------------------------------------------------------------------------------

#unzip the data
patients <-
  readr::read_csv(unz("data.zip", "data-fixed/patients.csv")) |>
  setDT() |>
  setkey(id)

#remove rows or columns with only missing data
patients <- janitor::remove_empty(patients, quiet = FALSE)
patients <- janitor::remove_constant(patients, quiet = FALSE) # A column with only one constant value is also not very interesting


#-----------------------------------------------------------------------------------------------------------------------------
#Expectations/validations
#-----------------------------------------------------------------------------------------------------------------------------

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
#Factors
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
#Unusual combinations
#-----------------------------------------------------------------------------------------------------------------------------

patients[, .N, .(gender, race, state)][order(N)]
#for about half the combinations there are over 20 people per option
#for a quarter there are over 40
#however many combinations have under 10 participants, which could be an issues

patients[, .N, .(race)][order(N)]
#1:    other   133
#2: hawaiian   138
#3:   native   254
#4:    asian   352
#5:    black   819
#6:    white  5155
#We could combine the categories into white and non-white to simplify analysis, increase category combination sizes, and
#hopefully make it harder to identify individuals

#make it so that everything not apearing at least half the time (so you only keep one level) is lumped into "other"
patients$race <- fct_lump_prop(patients$race, .5, w = NULL, other_level = "Other")
#1:  Other  1696
#2:  white  5155
#When running the three categories together now, we see that the lowest combination has 83, so this is much better!


#-----------------------------------------------------------------------------------------------------------------------------
#Derived variables
#-----------------------------------------------------------------------------------------------------------------------------

#making a new column in patients where we calculate the age
patients[, age := as.integer((as.IDate(Sys.Date()) - birthdate)) %/% 365.241]
#make a historgram
patients[, hist(age)]
#exclude dead people
patients[is.na(deathdate), hist(age)]

#double check which people are dead
#check when they last made a transaction (unzip the datafile)
unzip("data.zip", files = "data-fixed/payer_transitions.csv")

#get the last date out 
lastdate <-
  duckplyr::read_csv_duckdb("data-fixed/payer_transitions.csv") |>
  summarise(lastdate = max(start_date)) |>
  collect() |>
  pluck("lastdate") |>
  as.Date()


patients[is.na(deathdate) | deathdate > lastdate, hist(age)]


#-----------------------------------------------------------------------------------------------------------------------------
#Names
#-----------------------------------------------------------------------------------------------------------------------------

patients[,
  names(.SD) := lapply(.SD, \(x) replace_na(x, "")),
  .SDcols = c("prefix", "middle")
]
patients[,
  full_name := paste(
    prefix,
    first,
    middle,
    last,
    fifelse(!suffix %in% c("", NA), paste0(", ", suffix), "")
  )
]
patients[, full_name] #prints all the fullnames, but there are some NA's

#removes white spaces
patients[, names(.SD) := lapply(.SD, trimws), .SDcols = is.character]

#removed dublicated spaces
patients[, full_name := stringr::str_replace(full_name, "  ", " ")]

#remove unneeded columns
patients[, c("prefix", "first", "middle", "last", "suffix", "maiden") := NULL]


#-----------------------------------------------------------------------------------------------------------------------------
#Necessary Data
#-----------------------------------------------------------------------------------------------------------------------------

#do they have their licence or not
patients[, driver := !is.na(drivers)][, drivers := NULL]

leaflet::leaflet(data = patients) |>
  leaflet::addTiles() |>
  leaflet::addMarkers(~lon, ~lat, label = ~full_name)
#Super cool geographical visualization!!!

#There are clusters of data in alaska, california, massacheusets, and arizona. Since it's from different areas
#it might be representative of the US. It might even be generalizable to the "western" countries, but unlikely 
#to africa or asia. 


#-----------------------------------------------------------------------------------------------------------------------------
#Linkage
#-----------------------------------------------------------------------------------------------------------------------------

#making data into parquet files
zip::unzip("data.zip")
fs::dir_create("data-parquet")
csv2parquet <- function(file) {
  new_file <-
    file |>
    stringr::str_replace("-fixed", "-parquet") |>
    stringr::str_replace(".csv", ".parquet")

  duckplyr::read_csv_duckdb(file) |>
    duckplyr::compute_parquet(new_file)
}
fs::dir_ls("data-fixed/") |>
  purrr::walk(csv2parquet, .progress = TRUE)
fs::dir_delete("data-fixed")

#get out the procedure data
procedures <- duckplyr::read_parquet_duckdb("data-parquet/procedures.parquet")

#getting only data of the patient id, age, and produre
procedures <-
  procedures |>
  select(patient, reasoncode_icd10, start) |>
  filter(!is.na(reasoncode_icd10)) |>
  collect()

#data is now in memory, so treat it as a dt
setDT(procedures, key = "patient")

#only want year of prodecure, not time
procedures[, year := year(start)][, start := NULL]

#join the necessary patient data with this new dataset, filter out 
#the adults and tabulate their resons for their performed procedures over the years
proc_n_adults <-
  procedures[
    patients[, .(id, birthdate = as.IDate(birthdate))],
    on = c(patient = "id")
  ] |>
  _[year - year(birthdate) >= 18L, .N, .(reasoncode_icd10, year)]

#a reference table
cond_by_year <- setDT(decoder::icd10se)[
  proc_n_adults,
  on = c(key = "reasoncode_icd10")
]

#visualization of top 5 conditions
top5 <- cond_by_year[, .(N = sum(N)), .(value)][order(-N)][1:5, value]
ggplot(cond_by_year[.(top5), on = "value"], aes(year, N, color = value)) +
  geom_line() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_discrete(
    labels = function(x) str_wrap(x, width = 40)
  )


#- What happens in the end? Is it really reasonable to include tha last year (we were looking at 
# the assumed data extraction date earlier)

#It is possible that in the last year, the year had not finished yet, and so everything dropping off
#might be due to not having completed the year. It might also be a delay in reporting. In this case
#the last year should be be included (yet)

#- What happened early in history? Do we actually have all relevant data already from the start or 
# should we focus on years which are more acurately recorded?

#We only have data for gingivit (and a couple years of diabeties) before 1950. This might mean that
#some diseases were not being tracked yet. Potentially, we should only include data from after a
#version of the ICE was implimented across sweden, and after we digitalized health data across the
#country. Maybe starting from the 60's or later even would be more appropriate. 

#- Does this visualisation tell us anything? Are certain conditions more common today or do we need 
# to standardize the numbers with account to population size or health seeking behaviour etc.

#It can help us look at proportions between diseases - even if population sizes are increasing,
#overall prevailance of diseases seems to be in the same order. The suggestions are absolutely
#interesting analysis options, but there are things to be gathered from this visualization alone
#as well

#- Are patients sicker today or do they get more treatment for conditions which might have been 
# undertreated in the past?

#we cannot know which. It can also be that diagnosis critera have changed, public awareness has made
#more people get treatment, or reduced stigma makes people go to the doctor for more varied issues. 