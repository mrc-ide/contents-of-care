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

## Questionnaire F3 is DCO for prenatal consultation
## https://microdata.worldbank.org/index.php/catalog/2761/data-dictionary
orderly_shared_resource("utils.R")
source("utils.R")

orderly_shared_resource("bfa_utils.R")
source("bfa_utils.R")

indir <- "resources/bfa/baseline/BFA_2013_HRBFIE-FBL_v01_M_CSV"
infile <- "f3_aug19.csv"
orderly_shared_resource(bfa_dco.csv = paste(indir, infile, sep = "/"))
bfa_baseline_dco <- read_csv("bfa_dco.csv")


## 95.2% of the data is from CSPS which is primary health care in BFA.
bfa_baseline_dco <- rename(
  bfa_baseline_dco,
  ## SE looks like the ID of the health facility
  facility_level = NIVEAU_FS,
  num_csps_in_district = EFF,
  num_csps_in_sample = ECH,
  num_personnel = PERSO,
  milieu_of_residence = f3_empl,
  num_prev_anc_visits = f3_102,
  pregnancy_week = f3_103,
  first_pregnancy = f3_104,
  hcw_sex = f3_106,
  hcw_qualification = f3_107,
  consult_start = f3_201,
  consult_end = f3_219,
  consult_length = f3_220,
  consultation_language = f3_221,
  ## The following are yes/no questions 1 is yes, 2 is no
  hcw_intro_name = f3_202_a,
  hcw_intro_degree = f3_202_b,
  procedure_explained = f3_203_a,
  questions_encouraged = f3_203_b,
  third_person_assistance_asked = f3_203_c,
  patients_age_reported = f3_204_b,
  patient_residence_reported = f3_204_c,
  patient_taken_drugs = f3_204_d,
  num_prev_anc_before_this_pregnancy = f3_204_e,
  date_last_period = f3_204_f,
  number_of_previous_pregnancies_enquired = f3_204_g,
  prev_interruption_enquired = f3_205_a,
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
  patient_chest_auscultation = f3_207_h,
  patient_conjuctiva_examined = f3_207_i,
  lower_limb_edema = f3_207_j,
  fetal_presentation_exam = f3_207_k,
  uterus_size_exam = f3_207_l,
  fetal_heartbeat_listened = f3_207_m,
  breast_exam = f3_207_n,
  hemoglobin_test_requested = f3_207_o,
  blood_group_rhesus_requested = f3_207_p,
  urine_test_albumin_glucose = f3_207_q,
  syphilis_test_requested = f3_207_r,
  hiv_test_voluntary_offered = f3_207_s,
  hiv_counselling_referred = f3_207_t,
  health_card_checked = f3_207_u,
  exam_results_explained = f3_207_v,
  ifa_prescribed_or_given = f3_208_a,
  ifa_role_explained = f3_208_b,
  ifa_dosage_explained = f3_208_c,
  ifa_side_effects_explained = f3_208_d,
  tetanus_injection_prescribed_or_given = f3_208_e,
  tetanus_injection_role_explained = f3_208_f,
  antimalarial_prescribed_or_given = f3_208_g,
  ipt_role_explained = f3_208_h,
  ipt_dosage_explained = f3_208_i,
  antimalarial_side_effects_explained = f3_208_j,
  ipt_second_dose_importance_explained = f3_208_k,
  itn_use_importance_explained = f3_208_l,
  ipt_first_dose_given = f3_209_a,
  itn_given = f3_209_b,
  diet_during_pregnancy_explained = f3_210_a,
  pregnancy_progression_explained = f3_210_b,
  vaginal_bleeding = f3_211_a,
  fever_as_risk_factor_explained = f3_211_b,
  tiredness_as_risk_factor_explained = f3_211_c,
  swelling_as_risk_factor_explained = f3_211_d, 
  headache_or_blurred_vision_as_risk_factor_explained = f3_211_e,
  delivery_place_discussed = f3_212_a,
  birth_preparedness_advised = f3_212_b,
  skilled_birth_attendant_advised = f3_212_c,
  home_birth_items_discussed = f3_212_d,
  newborn_vaccination_importance_explained = f3_212_e,
  exclusive_breastfeeding_explained = f3_213,
  contraception_barrier_methods_discussed = f3_214_a,
  contraception_hormonal_methods_discussed = f3_214_b,
  contraception_surgical_methods_discussed = f3_214_c,
  contraception_traditional_methods_discussed = f3_214_d,
  contraception_methods_compared = f3_214_e,
  questions_invited_at_the_end = f3_215,
  visual_aids_used = f3_216
)




bfa_baseline_dco <- recode_bfa_vars(bfa_baseline_dco) |>
  fix_bfa_data_errors() |>
  calculate_consult_time() |>
  swap_start_end_times() |>
  calculate_consult_time()


cols_to_scale <- c(
  "num_csps_in_district", "num_personnel"
)

scaled_col_names <- paste0(cols_to_scale, "_scaled")

bfa_baseline_dco <- mutate(
  bfa_baseline_dco,
  across(
    all_of(cols_to_scale),
    ~ scale(., center = FALSE, scale = FALSE)[, 1],
    .names = "{.col}_scaled"
  )
)


## Following columns not found in any csv file:
## f1_niv, f1_reg, f1_dist;
## f1_niv is called NIVEAU_FS. That is, variable names as noted in the
## translation are not the same as in the data dictionary or the csv files.

indir <- "resources/bfa/baseline/BFA_2013_HRBFIE-FBL_v01_M_CSV"
infile <- "f1_main_aug19.csv"
orderly_shared_resource(bfa_hf_survey.csv = paste(indir, infile, sep = "/"))
bfa_hf_survey <- read_csv("bfa_hf_survey.csv")

bfa_hf_survey <- rename(
  bfa_hf_survey,
  facility_level = NIVEAU_FS,
  facility_type = f1_105,
  milieu_of_residence = f1_empl,
  ## The following are the number of *filled* positions for each type
  ## I am not confident these numbers are correct.
  ## For example, for the number of doctors, 90% are NA; and an
  ## additional 4% are 0
  doctors = f1_305_2,
  pharmacists = f1_305_3,
  dental_surgeon = f1_305_4,
  health_attache = f1_305_5,
  nursing_graduate = f1_305_6,
  nurse = f1_305_7,
  midwife_state = f1_305_8,
  midwife_patented = f1_305_13,
  ## In last one year
  maternal_deaths_last_year = f1_631,
  ## catchment population:
  ## Can you estimate the size of the population that uses this health facility?
  estimate_catchment_population = f1_701, ## 15 people said no
  catchment_pop = f1_702_a,
  catchment_pop_female_15_49 = f1_702_b,
  ## last month;
  ## total attendance is not always the sum of the attendance in subpopulations
  ## Additionally, total attendance and that of pregnant women are poorly correlated
  ## But the other three subpopulations are well correlated with total attendance
  total_attendance = f1_703,
  attendance_pregnant_women = f1_704,
  attendance_under_5_years = f1_705,
  attendance_under_1_year = f1_706,
  attendance_over_5_years = f1_707
)

bfa_hf_survey$facility_level_name <- case_when(
  bfa_hf_survey$facility_level == 1 ~ "Regional hospital",
  bfa_hf_survey$facility_level == 2 ~ "District hospital",
  bfa_hf_survey$facility_level == 3 ~ "Medical center",
  bfa_hf_survey$facility_level == 4 ~ "CSPS",
  bfa_hf_survey$facility_level == 5 ~ "Dispensary maternity Unit",
  bfa_hf_survey$facility_level == 6 ~ "Private clinic",
  bfa_hf_survey$facility_level == 7 ~ "Private religious health facility",
  TRUE ~ NA
)

bfa_hf_survey$facility_level_mapping <- case_when(
  bfa_hf_survey$facility_level_name %in% c("CSPS") ~ "Primary",
  bfa_hf_survey$facility_level %in% c("Medical center", "District hospital") ~
    "Secondary",
  TRUE ~ "Other"
)


bfa_hf_survey$total_attendance <- case_when(
  bfa_hf_survey$total_attendance %in% c(9998, 9999) ~ NA,
  TRUE ~ bfa_hf_survey$total_attendance
)

bfa_hf_survey$total_attendance_last_year <-
  bfa_hf_survey$total_attendance * 12



bfa_hf_survey$attendance_pregnant_women <- case_when(
  bfa_hf_survey$attendance_pregnant_women %in% c(9998, 9999) ~ NA,
  TRUE ~ bfa_hf_survey$attendance_pregnant_women
)

bfa_hf_survey$pregnant_women_last_year <-
  bfa_hf_survey$attendance_pregnant_women * 12


## Question is: who owns this health facility?
bfa_hf_survey$facility_type <- case_when(
  bfa_hf_survey$facility_type == 1 ~ "government facility",
  bfa_hf_survey$facility_type == 2 ~ "private-for-profit facility",
  bfa_hf_survey$facility_type == 3 ~ "ngo",
  bfa_hf_survey$facility_type == 4 ~ "mission/religious facility",
  bfa_hf_survey$facility_type == 5 ~ "special",
  bfa_hf_survey$facility_type == 6 ~ "military",
  bfa_hf_survey$facility_type == 7 ~ "other",
  TRUE ~ NA
)

## I don't have acccess to the questionnaire but these values feel like code
## for NA
bfa_hf_survey$maternal_deaths_last_year <- case_when(
  bfa_hf_survey$maternal_deaths_last_year %in% c(998, 999, 98, 9994, 860) ~ NA,
  TRUE ~ bfa_hf_survey$maternal_deaths_last_year
)
## Note: 860 does't look like an error BUT
## the value of total_Attendance for this facility is 2
## and the value of other subpopulation attendances are all NA

bfa_hf_survey$catchment_pop <- case_when(
  bfa_hf_survey$catchment_pop %in% c(99111998, 99999998) ~ NA,
  TRUE ~ bfa_hf_survey$catchment_pop
)

## Scale
cols_to_scale <-  "pregnant_women_last_year"


scaled_col_names <- c(
  scaled_col_names,
  paste0(cols_to_scale, "_scaled")
)

bfa_hf_survey <- mutate(
  bfa_hf_survey,
  across(
    all_of(cols_to_scale),
    ~ scale(., center = FALSE, scale = FALSE)[, 1],
    .names = "{.col}_scaled"
  )
)




## Don't use this column; use the column from the HW roster below.
## bfa_hf_survey$doctor_or_nursing_and_midwifery <- rowSums(
##   cbind(
##     bfa_hf_survey$doctors, bfa_hf_survey$midwife_state, bfa_hf_survey$midwife_patented
##   ), na.rm = TRUE
## )


## https://microdata.worldbank.org/index.php/catalog/2761/data-dictionary/F1?file_name=f1_healthworkers_aug19
infile <- "f1_healthworkers_aug19.csv"
orderly_shared_resource(bfa_hf_workers.csv = paste(indir, infile, sep = "/"))
bfa_hf_workers <- read_csv("bfa_hf_workers.csv")

bfa_hf_workers$hcw_role <- case_when(
  bfa_hf_workers$f1_405 == 1 ~ "health center chief",
  bfa_hf_workers$f1_405 == 2 ~ "doctor",
  bfa_hf_workers$f1_405 == 3 ~ "pharmacist",
  bfa_hf_workers$f1_405 == 4 ~ "dental surgeon",
  bfa_hf_workers$f1_405 == 5 ~ "ophthalmology health technician",
  bfa_hf_workers$f1_405 == 6 ~ "state-certified nurse",
  bfa_hf_workers$f1_405 == 7 ~ "certified nurse",
  bfa_hf_workers$f1_405 == 8 ~ "state-certified midwife",
  bfa_hf_workers$f1_405 == 9 ~ "biomedical technologist",
  bfa_hf_workers$f1_405 == 10 ~ "radiologic technician",
  bfa_hf_workers$f1_405 == 13 ~ "certified midwife (non-degree)",
  bfa_hf_workers$f1_405 == 14 ~ "chcw",
  bfa_hf_workers$f1_405 == 16 ~ "auxilliary midwife",
  ## No pediatric nurse in the data dictionary, but it is in the questionnaire 
  ##bfa_hf_workers$f1_405 == 19 ~ "pediatric nurse",
  TRUE ~ as.character(bfa_hf_workers$f1_405)
)

hcw_count <-
  count(bfa_hf_workers, SE, hcw_role) |> spread(hcw_role, n)

hcw_count$doctor_or_nursing_and_midwifery <- rowSums(
  cbind(
    hcw_count$doctor,
    hcw_count$`certified nurse`,
    hcw_count$`state-certified nurse`,
    hcw_count$`state-certified midwife`,
    hcw_count$`certified midwife (non-degree)`,
    hcw_count$`auxilliary midwife`
  ),
  na.rm = TRUE
)

hcw_count <- left_join(hcw_count, bfa_hf_survey, by = "SE")
hcw_count$doctor_or_nursing_and_midwifery_per_10000 <- (
  hcw_count$doctor_or_nursing_and_midwifery / hcw_count$catchment_pop
) * 10000


scaled_col_names <-
  c(scaled_col_names, "doctor_or_nursing_and_midwifery_scaled")

tmp <-
  scale(hcw_count$doctor_or_nursing_and_midwifery_per_10000, center = FALSE, scale = FALSE)
hcw_count$doctor_or_nursing_and_midwifery_scaled <- tmp[, 1]

saveRDS(hcw_count, "bfa_hcw_count.rds")
orderly_artefact(
  files = "bfa_hcw_count.rds",
  description = "HCW count for Burkina Faso baseline survey"
)


bfa_baseline_dco <- left_join(bfa_baseline_dco, hcw_count, by = "SE")

## Patients seen per clinical staff per year
bfa_baseline_dco$patients_per_staff_per_year <-
  bfa_baseline_dco$total_attendance_last_year /
  bfa_baseline_dco$doctor_or_nursing_and_midwifery

bfa_baseline_dco <- filter(
  bfa_baseline_dco,
  is.finite(patients_per_staff_per_year)
)

## Scale
tmp <-
  scale(
    bfa_baseline_dco$patients_per_staff_per_year,
    center = FALSE,
    scale = FALSE
  )

bfa_baseline_dco$patients_per_staff_per_year_scaled <- tmp[, 1]
scaled_col_names <-
  c(scaled_col_names, "patients_per_staff_per_year_scaled")

saveRDS(bfa_baseline_dco, "bfa_baseline_dco.rds")
orderly_artefact(
  files = "bfa_baseline_dco.rds",
  description = "DCO for Burkina Faso baseline survey"
)

## Processing for LM
bfa_small <- select(
  bfa_baseline_dco,
  consult_length = consult_length_calc,
  ## HF attributes
  region_name,
  milieu_of_residence = milieu_of_residence.x,
  facility_level_mapping = facility_level_mapping.x,
  maternal_deaths_last_year,
  ## patient attributes
  first_anc,
  trimester,
  pregnancy_week,
  first_pregnancy,
  ## HCW attributes
  hcw_sex,
  hcw_qualification,
  ## Appointment attributes
  consultation_language,
  time_elapsed_since_start_of_day,
  ## scaled attributes
  all_of(scaled_col_names)
)



bfa_small <- filter(bfa_small, consult_length != 0)

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


saveRDS(bfa_split, "bfa_baseline_split.rds")
orderly_artefact(
  files = "bfa_baseline_split.rds",
  description = "Data used for model fitting"
)








## orderly_resource("process_exit_survey.R")
## source("process_exit_survey.R")
