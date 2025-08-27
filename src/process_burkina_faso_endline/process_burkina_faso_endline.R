library(cli)
library(dplyr)
library(janitor)
library(lubridate)
library(orderly2)
library(purrr)
library(readr)
library(skimr)
library(table1)
library(tidylog)

orderly_shared_resource("utils.R")
source("utils.R")


orderly_shared_resource("bfa_utils.R")
source("bfa_utils.R")

orderly_dependency(
  "process_burkina_faso", "latest", "bfa_hcw_count.rds"
)

indir <- "resources/bfa/endline/BFA_2017_HRBFIE-FEL_v01_M_CSV"
infile <- "f3_jul19.csv"
orderly_shared_resource(bfa_endline_dco.csv = paste(indir, infile, sep = "/"))

bfa_endline_dco <- read_csv("bfa_endline_dco.csv")

bfa_endline_dco <- rename(
  bfa_endline_dco,
  SE = se,
  REGION = reg,
  DISTRICT = dst,
  facility_level = nivfs,
  milieu_of_residence = milieu,
  num_prev_anc_visits = f3_102,
  pregnancy_week = f3_103,
  first_pregnancy = patpar_f3,
  hcw_sex = hwsex_i,
  hcw_age = hwage_i,
  hcw_time_in_service = hwsen_i,
  hcw_qualification = f3_107,
  consult_start = f3_201,
  consult_end = f3_219,
  consult_length = f3_220,
  consult_language = f3_221,
  ## The following are yes/no questions 1 is yes, 2 is no
  hcw_intro_name = f3_202_a,
  hcw_intro_degree = f3_202_b,
  procedure_explained = f3_203_a,
  questions_encouraged = f3_203_b,
  third_person_assistance_asked = f3_203_c,
  patient_age = f3_204_b,
  patient_residence = f3_204_c,
  patient_taken_drugs = f3_204_d,
  num_prev_anc_before_this_pregnancy = f3_204_e,
  date_last_period = f3_204_f,
  num_prev_pregnancies = f3_204_g,
  prev_interruption = f3_205_a,
  prev_delivery_preterm = f3_205_b,
  children_death_during_first_week = f3_205_c,
  heavy_bleeding_during_or_after_delivery = f3_205_d,
  prev_assisted_delivery = f3_205_e,
  prev_voluntary_interruption = f3_205_f,
  ## Questions related to the current pregnancy
  abdominal_pain = f3_206_a,
  breathing_difficulty = f3_206_b,
  severe_vomiting = f3_206_c,
  convulsions = f3_206_d,
  health_problems_before_or_during_pregnancy = f3_206_e,
  stress = f3_206_f, ## or anxiety or depression
  domestic_violence = f3_206_g,
  drug_treatment_course = f3_206_h,
  hiv_status = f3_206_i,
  vaccination_against_tetanus = f3_206_j,
  bleeding = f3_206_k,
  fever = f3_206_l,
  headache = f3_206_m,
  swelling = f3_206_n,
  tiredness = f3_206_o,
  fetus_movement = f3_206_p,
  other_symptoms = f3_206_q,
  pregnancy_related_symptoms = f3_206_r,
  projecting_delivery = f3_206_s,
  confidentiality_ensured = f3_207_a,
  hygiene_measures_before_touching_patient = f3_207_b,
  patient_weight = f3_207_c,
  patient_pulse = f3_207_d,
  patient_respiratory_rate = f3_207_e,
  patient_temperature = f3_207_f,
  patient_blood_taken = f3_207_g,
  patient_chest_auscultation = f3_207_h
  ## more questions upto f3_218
)


bfa_endline_dco <- recode_bfa_vars(bfa_endline_dco) |>
  fix_bfa_data_errors() |>
  calculate_consult_time() |>
  swap_start_end_times() |>
  calculate_consult_time() 






hcw_count <- readRDS("bfa_hcw_count.rds")

bfa_endline_dco <- left_join(bfa_endline_dco, hcw_count, by = "SE")
bfa_endline_dco <- rename(bfa_endline_dco, num_csps_in_district = "EFF")
bfa_endline_dco <- rename(bfa_endline_dco, num_personnel = "PERSO")


cols_to_scale <- c(
  "num_csps_in_district", "num_personnel"
)

scaled_col_names <- paste0(cols_to_scale, "_scaled")

bfa_endline_dco <- mutate(
  bfa_endline_dco,
  across(
    all_of(cols_to_scale),
    ~ scale(.)[, 1],
    .names = "{.col}_scaled"
  )
)

scaled_attrs <- map_dfr(
  cols_to_scale,
  function(col) {
    x <- scale(bfa_endline_dco[[col]])
    data.frame(
      variable = col,
      mean = attr(x, "scaled:center"),
      sd = attr(x, "scaled:scale")
    )
  }
)


saveRDS(bfa_endline_dco, "bfa_endline_dco.rds")
orderly_artefact(
  files = "bfa_endline_dco.rds",
  description = "DCO for Burkina Faso endline survey"
)


bfa_small <- select(
  bfa_endline_dco,
  consult_length = consult_length_calc,
  ## HF attributes
  region_name,
  milieu_of_residence = milieu_of_residence.x,  
  facility_level_mapping = facility_level_mapping.x,
  num_maternal_deaths,
  ## patient attributes
  patage_i,
  patlit_i,
  patmar_i,
  patdist_i,
  wealth_decile_i,
  first_anc,
  trimester,
  pregnancy_week,
  first_pregnancy,
  ## HCW attributes
  hcw_time_in_service,
  hcw_sex,
  hcw_qualification,
  ## Appointment attributes
  consult_language,
  time_elapsed_since_start_of_day,
  all_of(grep("scaled", colnames(bfa_endline_dco), value = TRUE))
)

bfa_small$patlit_i <- factor(
  bfa_small$patlit_i,
  levels = c(0, 1),
  labels = c("Illiterate", "Literate"),
  ordered = FALSE
)



bfa_small$patmar_i <- factor(
  bfa_small$patmar_i,
  levels = c(0, 1),
  labels = c("Not married", "Married"),
  ordered = FALSE
)




bfa_small <- filter(bfa_small, consult_length != 0)
bfa_small$log_consult_length <- log(bfa_small$consult_length)
bfa_small <- select(bfa_small, -consult_length)

bfa_small <- mutate_if(
  bfa_small, is.character, ~ ifelse(is.na(.), "Unknown", .)
)


bfa_split <- split(
  bfa_small,
  list(bfa_small$first_anc, bfa_small$trimester),
  sep = "_"
)

cli_alert_info("Data split into {length(bfa_split)} datasets")
cli_alert_info("Number of rows in each dataset: {map_int(bfa_split, nrow)}")

saveRDS(bfa_split, "bfa_endline_split.rds")
orderly_artefact(
  files = "bfa_endline_split.rds",
  description = "Data used for model fitting"
)
