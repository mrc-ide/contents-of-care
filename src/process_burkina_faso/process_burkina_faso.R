library(orderly2)
library(dplyr)
library(janitor)
library(readr)
library(skimr)
library(tidylog)
## Questionnaire F3 is DCO for prenatal consultation

indir <- "resources/bfa/baseline/BFA_2013_HRBFIE-FBL_v01_M_CSV"
infile <- "f3_aug19.csv"
orderly_shared_resource(bfa_dco.csv = paste(indir, infile, sep = "/"))
bfa_baseline_dco <- read_csv("bfa_dco.csv")


bfa_baseline_dco <- rename(
  bfa_baseline_dco,
  num_prev_anc_visits = f3_102,
  pregnancy_week = f3_103,
  first_pregnancy = f3_104,
  hcw_sex = f3_106,
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

bfa_baseline_dco$first_anc <- ifelse(bfa_baseline_dco$num_prev_anc_visits == 0, 1, 0)
bfa_baseline_dco$pregnancy_week <- ifelse(
  bfa_baseline_dco$pregnancy_week %in% 98, NA, bfa_baseline_dco$pregnancy_week
)

bfa_baseline_dco$trimester <- ifelse(
  bfa_baseline_dco$pregnancy_week < 13, "First Trimester",
  ifelse(bfa_baseline_dco$pregnancy_week < 28, "Second Trimester", "Third Trimester")
)
## Following columns not found in any csv file:
## f1_niv, f1_reg, f1_dist


infile <- "f1_main_aug19.csv"
orderly_shared_resource(bfa_hf_survey.csv = paste(indir, infile, sep = "/"))
bfa_hf_survey <- read_csv("bfa_hf_survey.csv")

bfa_hf_survey <- rename(
  bfa_hf_survey,
  facility_type = f1_105,
  ## The following are the number of *filled* positions for each type
  doctors = f1_305_2,
  pharmacists = f1_305_3,
  dental_surgeon = f1_305_4,
  health_attache = f1_305_5,
  nursing_graduate = f1_305_6,
  nurse = f1_305_7,
  midwife_state = f1_305_8,
  midwife_patented = f1_305_13,
  ## In last one year
  num_maternal_deaths = f1_631,
  ## catchment population:
  ## Can you estimate the size of the population that uses this health facility?
  estimate_catchment_population = f1_701, ## 15 people said no
  catchment_pop = f1_702_a,
  catchment_pop_female_15_49 = f1_702_b,
  total_attendance = f1_703 ## duration unclear
)

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
bfa_hf_survey$num_maternal_deaths <- case_when(
  bfa_hf_survey$num_maternal_deaths %in% c(998, 999, 98) ~ NA,
  TRUE ~ bfa_hf_survey$num_maternal_deaths
)

bfa_hf_survey$catchment_pop <- case_when(
  bfa_hf_survey$catchment_pop %in% c(99111998, 99999998) ~ NA,
)

bfa_baseline_dco <- left_join(bfa_baseline_dco, bfa_hf_survey, by = "SE")
saveRDS(bfa_baseline_dco, "bfa_baseline_dco.rds")
orderly_artefact(
  files = "bfa_baseline_dco.rds",
  description = "DCO for Burkina Faso baseline survey"
)
