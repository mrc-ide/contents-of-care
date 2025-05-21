library(dplyr)
library(foreign)
library(lubridate)
library(orderly2)
library(readr)
library(tidylog)
library(tidyr)

## Dataset ID 2731
## Congo, Dem. Rep. - Health Results Based Financing Impact Evaluation 2018, Health Facility Midline Survey - 2018
### Midline survey data
indir <- "resources/drc/midline-survey/COD_2018_HRBFIE-FML_v01_M_CSV"
infile <- "f3_do_anc.csv"

orderly_shared_resource(drc_midline.csv = paste(indir, infile, sep = "/"))
drc_midline_dco <- read_csv("drc_midline.csv")

drc_midline_dco <- rename(
  drc_midline_dco,
  first_anc = f3_01_01_b,
  num_prev_anc_visits = f3_01_02,
  pregnancy_in_weeks = f3_01_03,
  first_pregnancy = f3_01_04,
  hcw_sex = f3_01_06,
  hcw_qualification = f3_01_07,
  hcw_qualification_other = f3_01_07_autre,
)

drc_midline_dco$trimester <- ifelse(
  drc_midline_dco$pregnancy_in_weeks < 13,
  "First Trimester",
  ifelse(
    drc_midline_dco$pregnancy_in_weeks < 28,
    "Second Trimester", "Third Trimester"
  )
)

drc_midline_dco$hcw_sex <- case_when(
  drc_midline_dco$hcw_sex %in% "Masculin" ~ "Male",
  drc_midline_dco$hcw_sex %in% "Féminin" ~ "Female",
  TRUE ~ NA_character_
)

drc_midline_dco$hcw_qualification <- case_when(
  drc_midline_dco$hcw_qualification %in% "Médecin" ~ "Doctor",
  drc_midline_dco$hcw_qualification %in%
    c("Infirmier(ère) A2", "infirmier(ère) A1") ~ "Nurse",
  drc_midline_dco$hcw_qualification %in% 5 ~ "Lab technician",
  drc_midline_dco$hcw_qualification %in%
    "Sage-Femme / accoucheur" ~ "Midwife/Obstetrician",
  drc_midline_dco$hcw_qualification %in%
    "Autre: Specifiez" ~ "Other",
  TRUE ~ as.character(drc_midline_dco$hcw_qualification)
)

drc_midline_dco <- rename(
  drc_midline_dco,
  start_hour_of_consultation = f3_02_01_heure,
  start_minute_of_consultation = f3_02_01_minute,
  end_hour_of_consultation = f3_02_20_heure,
  end_minute_of_consultation = f3_02_20_minute,
  consult_length = f3_temps_tot_consultation
)

drc_midline_dco$start_time_of_consultation <- hm(paste(
  drc_midline_dco$start_hour_of_consultation,
  drc_midline_dco$start_minute_of_consultation
))

drc_midline_dco$start_time_of_consultation <- hm(paste(
  drc_midline_dco$start_hour_of_consultation,
  drc_midline_dco$start_minute_of_consultation
))

drc_midline_dco$end_time_of_consultation <- hm(paste(
  drc_midline_dco$end_hour_of_consultation,
  drc_midline_dco$end_minute_of_consultation
))

drc_midline_dco$consult_length_calc <-
  time_length(
    drc_midline_dco$end_time_of_consultation -
      drc_midline_dco$start_time_of_consultation,
    unit = "minute"
  )
## Looks like start and end times are swapped for one row
idx <- which(drc_midline_dco$consult_length < 0)
tmp <- drc_midline_dco$start_time_of_consultation[idx]
drc_midline_dco$start_time_of_consultation[idx] <-
  drc_midline_dco$end_time_of_consultation[idx]
drc_midline_dco$end_time_of_consultation[idx] <- tmp

## calculated and provided consultation lengths are the same except for this one
## row which we have fixed above
drc_midline_dco$consult_length_calc <-
  time_length(
    drc_midline_dco$end_time_of_consultation -
      drc_midline_dco$start_time_of_consultation,
    unit = "minute"
  )

drc_midline_dco <- rename(
  drc_midline_dco,
  consultation_language = f3_02_21,
  consultation_language_other = f3_02_21_autre
)

saveRDS(drc_midline_dco, file = "drc_midline_dco.rds")
orderly_artefact(
  files = "drc_midline_dco.rds",
  description = "DRC midline survey data (DCO)"
)




