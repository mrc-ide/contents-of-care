library(orderly2)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)
library(skimr)
library(table1)
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

bfa_baseline_dco$consult_start <- case_when(
  bfa_baseline_dco$consult_start %in% 99 ~ NA,
  TRUE ~ bfa_baseline_dco$consult_start
)
time_vec <- bfa_baseline_dco$consult_start
time_str <- sprintf("%04d", time_vec)
bfa_baseline_dco$consult_start_formatted <- hm(
  paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
)

time_vec <- bfa_baseline_dco$consult_end
time_str <- sprintf("%04d", time_vec)
bfa_baseline_dco$consult_end_formatted <- hm(
  paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
)
start_of_day <- hm("07:00") 
bfa_baseline_dco$time_elapsed_since_start_of_day <-
  time_length(bfa_baseline_dco$consult_start_formatted - start_of_day, unit = "minute")



bfa_baseline_dco$first_anc <- ifelse(bfa_baseline_dco$num_prev_anc_visits == 0, 1, 0)
bfa_baseline_dco$pregnancy_week <- ifelse(
  bfa_baseline_dco$pregnancy_week %in% 98, NA, bfa_baseline_dco$pregnancy_week
)

bfa_baseline_dco$trimester <- ifelse(
  bfa_baseline_dco$pregnancy_week < 13, "First Trimester",
  ifelse(bfa_baseline_dco$pregnancy_week < 28, "Second Trimester", "Third Trimester")
)
## Following columns not found in any csv file:
## f1_niv, f1_reg, f1_dist;
## f1_niv is called NIVEAU_FS. That is, variable names as noted in the
## translation are not the same as in the data dictionary or the csv files.


infile <- "f1_main_aug19.csv"
orderly_shared_resource(bfa_hf_survey.csv = paste(indir, infile, sep = "/"))
bfa_hf_survey <- read_csv("bfa_hf_survey.csv")

bfa_hf_survey <- rename(
  bfa_hf_survey,
  facility_level = NIVEAU_FS,
  facility_type = f1_105,
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
  num_maternal_deaths = f1_631,
  ## catchment population:
  ## Can you estimate the size of the population that uses this health facility?
  estimate_catchment_population = f1_701, ## 15 people said no
  catchment_pop = f1_702_a,
  catchment_pop_female_15_49 = f1_702_b,
  total_attendance = f1_703 ## duration unclear
)

bfa_hf_survey$facility_level <- case_when(
  bfa_hf_survey$facility_level == 1 ~ "Regional hospital",
  bfa_hf_survey$facility_level == 2 ~ "District hospital",  
  bfa_hf_survey$facility_level == 3 ~ "Medical center",
  bfa_hf_survey$facility_level == 4 ~ "CSPS",
  bfa_hf_survey$facility_level == 5 ~ "Dispensary maternity Unit",
  bfa_hf_survey$facility_level == 6 ~ "Private clinic",
  bfa_hf_survey$facility_level == 7 ~ "Private religious health facility",
  TRUE ~ NA
)


bfa_hf_survey$total_attendance <- case_when(
  bfa_hf_survey$total_attendance %in% 9998 ~ NA,
  TRUE ~ bfa_hf_survey$total_attendance)

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
  TRUE ~ bfa_hf_survey$catchment_pop
)

## Don't use this column; use the column from the HW roster below.
## bfa_hf_survey$doctor_or_nursing_and_midwifery <- rowSums(
##   cbind(
##     bfa_hf_survey$doctors, bfa_hf_survey$midwife_state, bfa_hf_survey$midwife_patented
##   ), na.rm = TRUE
## )



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
  TRUE ~ as.character(bfa_hf_workers$f1_405)
)
## Still lots of NAs.
## Further, there are codes that are not explained in the questionnaire
## So I dont know what they mean

hcw_count <- count(bfa_hf_workers, SE, hcw_role) |> spread(hcw_role, n)

hcw_count$doctor_or_nursing_and_midwifery <- rowSums(
  cbind(
    hcw_count$doctor,
    hcw_count$`certified nurse`,
    hcw_count$`state-certified nurse`,
    hcw_count$`state-certified midwife`
  ), na.rm = TRUE
)
hcw_count <- left_join(hcw_count, bfa_hf_survey, by = "SE")
hcw_count$doctor_or_nursing_and_midwifery_per_10000 <- (
  hcw_count$doctor_or_nursing_and_midwifery / hcw_count$catchment_pop
) * 10000

bfa_baseline_dco <- left_join(bfa_baseline_dco, hcw_count, by = "SE")
saveRDS(bfa_baseline_dco, "bfa_baseline_dco.rds")
orderly_artefact(
  files = "bfa_baseline_dco.rds",
  description = "DCO for Burkina Faso baseline survey"
)

## ANC Exit interview; tranlsation from page 670 of accompanying PDF
## Questionnaire F5 is exit survey; however data dictionary says it is F6!
## In the tranlsation, it appears to be f5 rather than f6.
## SO I am going with F5.
orderly_shared_resource(
  bfa_baseline_exit.csv = paste(indir, "f5_aug19.csv", sep = "/")
) 

bfa_baseline_exit <- read_csv("bfa_baseline_exit.csv")

bfa_baseline_exit <- rename(
  bfa_baseline_exit,
  facility_level = NIVEAU_FS,
  patient_age = f5_101,
  patient_can_read_write = f5_102,
  patient_highest_education = f5_103,
  ## 104 has more details about education
  patient_marital_status = f5_105,
  patient_partner_highest_education = f5_106,
  num_of_hcws_seen = f5_201,
  num_of_weeks_pregnant_an_book = f5_212,
  num_of_weeks_pregnant_self_report = f5_215,
  ## These two variables are *very* different for 41% rows
  first_pregnancy = f5_216,
  first_anc_visit_at_this_hf = f5_217,
  num_prev_anc_visits = f5_218, ## including this one
  num_prev_anc_visits_other_hf = f5_219,
  ## f5_220:f5_251_m talk the patient through the various steps
  patient_residence_distance = f5_301,
  patient_residence_travel_time = f5_302, ## in minutes
  patient_mode_of_transport = f5_303,
  patient_travel_cost = f5_304,
  patient_wait_time = f5_305,
  consult_length = f5_306,
  waiting_time_too_long = f5_307,
  patient_paid_consult_fee = f5_308,
  patient_consult_fee = f5_309,
  patient_paid_extra_fee = f5_310, ## 99.6% said no
  lab_test_done = f5_312,
  lab_test_fee = f5_313,
  us_done = f5_314,
  us_fee = f5_315,
  medicine_dispensed = f5_316,
  medicine_fee = f5_317,
  total_hf_fees = f5_318,
  ## f5_401 to f5:428 record answers about the HF
  ## f5_601 onwards are about patient's household
  patient_seen_chw_at_health_center = f5_703, ## in the last 1 month
  patient_seen_chw_at_home = f5_704, ## in the last 1 month
  patient_seen_chw_elsewhere = f5_705, ## in the last 1 month
  ## tba is traditional birth attendant
  patient_seen_tba = f5_801 ## in the last 1 month
)



cols <- c(
  "patient_seen_chw_at_health_center",
  "patient_seen_chw_at_home",
  "patient_seen_chw_elsewhere"
)

bfa_baseline_exit$patient_seen_chw <- apply(
  bfa_baseline_exit[, cols], 1, function(row) {
    if (any(row == 1, na.rm = TRUE)) {
      return(1)
    } else if (all(is.na(row))) {
      return(NA)
    } else {
      return(2)
    }
  }
)

bfa_baseline_exit <- mutate_at(
  bfa_baseline_exit, vars(contains("patient_seen_chw")), function(x) {
    x <- ifelse(x %in% 9994, NA, x)
  }
)




bfa_baseline_exit$trimester <- case_when(
  bfa_baseline_exit$num_of_weeks_pregnant_an_book < 13 ~ "First Trimester",
  bfa_baseline_exit$num_of_weeks_pregnant_an_book > 13 &
    bfa_baseline_exit$num_of_weeks_pregnant_an_book < 28 ~ "Second Trimester",
  bfa_baseline_exit$num_of_weeks_pregnant_an_book >= 28 ~ "Third Trimester",
  TRUE ~ NA)



bfa_baseline_exit$total_hf_fees <- case_when(
  bfa_baseline_exit$total_hf_fees %in% c(9998, 99999) ~ NA,
  TRUE ~ bfa_baseline_exit$total_hf_fees
)

bfa_baseline_exit$num_prev_anc_visits <- case_when(
  bfa_baseline_exit$num_prev_anc_visits %in% c(9994, 9999) ~ NA,
  TRUE ~ bfa_baseline_exit$num_prev_anc_visits
)

bfa_baseline_exit$patient_seen_tba <- case_when(
  bfa_baseline_exit$patient_seen_tba %in% 1:7 ~ 1,
  bfa_baseline_exit$patient_seen_tba == 8 ~ 2,
  TRUE ~ NA
)


bfa_baseline_exit$total_hf_fees <- case_when(
  bfa_baseline_exit$total_hf_fees %in% c(9998, 9999) ~ NA,
  TRUE ~ bfa_baseline_exit$total_hf_fees
)

## I suspect these are NAs
bfa_baseline_exit$patient_residence_distance <- case_when(
  bfa_baseline_exit$patient_residence_distance %in% c(996, 998, 999) ~ NA,
  TRUE ~ bfa_baseline_exit$patient_residence_distance
)

bfa_baseline_exit$patient_residence_travel_time <- case_when(
  bfa_baseline_exit$patient_residence_travel_time %in% c(996, 998, 999) ~ NA,
  TRUE ~ bfa_baseline_exit$patient_residence_travel_time
)

bfa_baseline_exit$patient_wait_time <- case_when(
  bfa_baseline_exit$patient_wait_time %in% c(996, 998, 999) ~ NA,
  TRUE ~ bfa_baseline_exit$patient_wait_time
)


bfa_baseline_exit$patient_highest_education <- case_when(
  bfa_baseline_exit$patient_highest_education == 1 ~ "No education",
  bfa_baseline_exit$patient_highest_education == 2 ~ "Primary",
  bfa_baseline_exit$patient_highest_education == 3 ~ "Secondary 1 cycle",
  bfa_baseline_exit$patient_highest_education == 4 ~ "Secondary 2 cycle",
  bfa_baseline_exit$patient_highest_education == 5 ~ "Higher",
  TRUE ~ NA
)
bfa_baseline_exit$facility_level <- case_when(
  bfa_baseline_exit$facility_level == 1 ~ "Regional hospital",
  bfa_baseline_exit$facility_level == 2 ~ "District hospital",
  bfa_baseline_exit$facility_level == 3 ~ "Medical center",
  bfa_baseline_exit$facility_level == 4 ~ "CSPS",
  bfa_baseline_exit$facility_level == 5 ~ "Dispensary maternity Unit",
  bfa_baseline_exit$facility_level == 6 ~ "Private clinic",
  bfa_baseline_exit$facility_level == 7 ~ "Private religious health facility",
  TRUE ~ NA
)
bfa_baseline_exit$patient_marital_status <- case_when(
  bfa_baseline_exit$patient_marital_status == 1 ~ "Single",
  bfa_baseline_exit$patient_marital_status == 2 ~ "Married/Common-law",
  bfa_baseline_exit$patient_marital_status == 3 ~ "Widowed",
  bfa_baseline_exit$patient_marital_status == 4 ~ "Divorced",
  TRUE ~ NA
)

bfa_baseline_exit$patient_partner_highest_education <- case_when(
  bfa_baseline_exit$patient_partner_highest_education == 1 ~ "No education",
  bfa_baseline_exit$patient_partner_highest_education == 2 ~ "Primary",
  bfa_baseline_exit$patient_partner_highest_education == 3 ~ "Secondary 1 cycle",
  bfa_baseline_exit$patient_partner_highest_education == 4 ~ "High school",
  bfa_baseline_exit$patient_partner_highest_education == 5 ~ "University",
  bfa_baseline_exit$patient_partner_highest_education == 6 ~ "Do not know",
  TRUE ~ NA
)

bfa_baseline_exit$num_of_weeks_pregnant_an_book <- case_when(
  bfa_baseline_exit$num_of_weeks_pregnant_an_book %in% 9994 ~ NA,
  TRUE ~ bfa_baseline_exit$num_of_weeks_pregnant_an_book
)

bfa_baseline_exit$num_of_weeks_pregnant_self_report <- case_when(
  bfa_baseline_exit$num_of_weeks_pregnant_self_report %in% 9994 ~ NA,
  TRUE ~ bfa_baseline_exit$num_of_weeks_pregnant_self_report
)
## 12 is the minimum age in the dataset
 bfa_baseline_exit$patient_age_centered <- bfa_baseline_exit$patient_age - 12


## Collect all yes/no columns so that we can recode in one go
bfa_baseline_exit <- relocate(
  bfa_baseline_exit,
  patient_can_read_write, first_pregnancy, first_anc_visit_at_this_hf,
  waiting_time_too_long, patient_paid_consult_fee,
  patient_paid_extra_fee, lab_test_done, us_done, medicine_dispensed,
  patient_seen_chw_at_health_center, patient_seen_chw_at_home,
  patient_seen_chw_elsewhere, patient_seen_tba
)


bfa_baseline_exit <- mutate(
  bfa_baseline_exit, across(patient_can_read_write:patient_seen_tba, function(x) {
    x <- case_when(x %in% 1 ~ "yes", x %in% 2 ~ "no", TRUE ~ NA)
    x
  }))

## num_prev_anc_visits includes this one; so 0 is a mistake and the smallest
## possible value is 1.
bfa_baseline_exit$first_anc <- case_when(
  bfa_baseline_exit$num_prev_anc_visits > 0 ~ "no",
  bfa_baseline_exit$num_prev_anc_visits %in% c(0, 1) ~ "yes",

  ## If first ANC visit to this HF and no previous visits to any other HF, then
  ## this is the first ANC visit; this fills out 459 NAs
  is.na(bfa_baseline_exit$num_prev_anc_visits) &
    bfa_baseline_exit$first_anc_visit_at_this_hf %in% "yes" &
  bfa_baseline_exit$num_prev_anc_visits_other_hf == 0 ~ "yes",
  
  ## If num_prev_anc_visits is NA, but num_prev_anc_visits_other_hf is not, and
  ## is greater than 0, we know at least that this is not the patient's first ANC.
  is.na(bfa_baseline_exit$num_prev_anc_visits) &
    !is.na(bfa_baseline_exit$num_prev_anc_visits_other_hf) &
    bfa_baseline_exit$num_prev_anc_visits_other_hf > 0 ~ "no",
  TRUE ~ NA_character_
)



saveRDS(bfa_baseline_exit, "bfa_baseline_exit.rds")
orderly_artefact(
  files = "bfa_baseline_exit.rds",
  description = "Exit survey for Burkina Faso baseline survey"
)
