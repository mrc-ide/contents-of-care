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

bfa_baseline_dco <- rename(
  bfa_baseline_dco,
  facility_level = NIVEAU_FS,
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

common_cols <- intersect(names(bfa_baseline_dco), names(bfa_endline_dco))

bfa_dco <- bind_rows(
  list(
    baseline = bfa_baseline_dco[, common_cols],
    endline = bfa_endline_dco[, common_cols]
  ),
  .id = "survey"
)

bfa_dco$consult_start <- case_when(
  bfa_dco$consult_start %in% 99 ~ NA,
  TRUE ~ bfa_dco$consult_start
)

## bfa_dco$consult_start <- as.character(bfa_dco$consult_start)
## bfa_dco$consult_end <- as.integer(bfa_dco$consult_end)

## Fix obvious data entry errors
## Not sure what this is meant to be; end time is 1100
idx <- bfa_dco$consult_start %in% 3
bfa_dco$consult_start[idx] <- NA

## end time is 1143; so 1105?
idx <- bfa_dco$consult_start %in% 115
bfa_dco$consult_start[idx] <- 1105


## end time is 1809; so 1758 instead of 758?
idx <- bfa_dco$consult_start %in% 758
bfa_dco$consult_start[idx] <- 1758

idx <- bfa_dco$consult_start %in% 12
bfa_dco$consult_start[idx] <- 1200

idx <- bfa_dco$consult_start %in% 8
bfa_dco$consult_start[idx] <- 0800

idx <- bfa_dco$consult_start %in% 9
bfa_dco$consult_start[idx] <- 0900

idx <- bfa_dco$consult_start %in% 10
bfa_dco$consult_start[idx] <- 1000

idx <- bfa_dco$consult_end %in% 10
bfa_dco$consult_end[idx] <- 1000


idx <- bfa_dco$consult_end %in% 100
bfa_dco$consult_end[idx] <- 1000

## This could be 1002 or 1020.
idx <- bfa_dco$consult_end %in% 102
bfa_dco$consult_end[idx] <- NA

idx <- bfa_dco$consult_end %in% 115
bfa_dco$consult_end[idx] <- 1315 ## start time is 1143




idx <- bfa_dco$consult_start %in% 11
bfa_dco$consult_start[idx] <- 1100

idx <- bfa_dco$consult_end %in% 11
bfa_dco$consult_end[idx] <- 1100



idx <- bfa_dco$consult_end %in% 12
bfa_dco$consult_end[idx] <- 1200

idx <- bfa_dco$consult_start %in% 13
bfa_dco$consult_start[idx] <- 1300

idx <- bfa_dco$consult_end %in% 13
bfa_dco$consult_end[idx] <- 1300


idx <- bfa_dco$consult_start %in% 14
bfa_dco$consult_start[idx] <- 1400

idx <- bfa_dco$consult_end %in% 14
bfa_dco$consult_end[idx] <- 1400


## End time of this one is 1025; so probably start time is 1015
## But we cant be sure
idx <- bfa_dco$consult_start %in% 15
bfa_dco$consult_start[idx] <- NA


idx <- bfa_dco$consult_end %in% 15
bfa_dco$consult_end[idx] <- NA

idx <- bfa_dco$consult_start %in% c(3, 4, 32, 45, 98, 246, 295)
bfa_dco$consult_start[idx] <- NA


idx <- bfa_dco$consult_end %in% c(3, 4, 32, 45, 98)
bfa_dco$consult_end[idx] <- NA

## End times are 956 and 950; so unsure what 102 could be
idx <- bfa_dco$consult_start %in% 102
bfa_dco$consult_start[idx] <- NA

idx <- bfa_dco$consult_start %in% 830
bfa_dco$consult_start[idx] <- 0830

idx <- bfa_dco$consult_start %in% 8030
bfa_dco$consult_start[idx] <- 0830
bfa_dco$consult_end[idx] <- 0945

idx <- bfa_dco$consult_start %in% 9025
bfa_dco$consult_start[idx] <- 0925
bfa_dco$consult_end[idx] <- 0940

idx <- bfa_dco$consult_start %in% 9048
bfa_dco$consult_start[idx] <- 0948
bfa_dco$consult_end[idx] <- 0958


idx <- bfa_dco$consult_end %in% 9000
bfa_dco$consult_start[idx] <- 0853
bfa_dco$consult_end[idx] <- 0900

## Start time here is 710, but unclear what the end time is from 7320
idx <- bfa_dco$consult_end %in% 7320
bfa_dco$consult_start[idx] <- 0710
bfa_dco$consult_end[idx] <- NA

idx <- bfa_dco$consult_end %in% 9052
bfa_dco$consult_start[idx] <- 0941
bfa_dco$consult_end[idx] <- 0952

idx <- bfa_dco$consult_end %in% 9172
bfa_dco$consult_start[idx] <- 0905
bfa_dco$consult_end[idx] <- NA

idx <- bfa_dco$consult_end %in% 9998
bfa_dco$consult_start[idx] <- NA
bfa_dco$consult_end[idx] <- NA






time_vec <- bfa_dco$consult_start
time_str <- sprintf("%04d", time_vec)
bfa_dco$consult_start_formatted <- hm(
  paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
)

time_vec <- bfa_dco$consult_end
time_str <- sprintf("%04d", time_vec)
bfa_dco$consult_end_formatted <- hm(
  paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
)
start_of_day <- hm("07:00") 
bfa_dco$time_elapsed_since_start_of_day <-
  time_length(bfa_dco$consult_start_formatted - start_of_day, unit = "minute")

bfa_dco$consult_length_calc <-
  time_length(
    bfa_dco$consult_end_formatted - bfa_dco$consult_start_formatted,
    unit = "minute"
  )

## This reveals some more data entry errors
idx <- which(bfa_dco$consult_length_calc < 0)
tmp <- bfa_dco$consult_start[idx]
bfa_dco$consult_start[idx] <- bfa_dco$consult_end[idx]
bfa_dco$consult_end[idx] <- tmp

## Recalculate the consult length
time_vec <- bfa_dco$consult_start
time_str <- sprintf("%04d", time_vec)
bfa_dco$consult_start_formatted <- hm(
  paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
)

time_vec <- bfa_dco$consult_end
time_str <- sprintf("%04d", time_vec)
bfa_dco$consult_end_formatted <- hm(
  paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
)
start_of_day <- hm("07:00")
bfa_dco$time_elapsed_since_start_of_day <-
  time_length(bfa_dco$consult_start_formatted - start_of_day, unit = "minute")

bfa_dco$consult_length_calc <-
  time_length(
    bfa_dco$consult_end_formatted - bfa_dco$consult_start_formatted,
    unit = "minute"
  )

## Now we are left with 10 rows where start and times are the same
## One consult_start at 813, ends ar 1758;  not sure if this is mistake?
## For now I'll set it to NA
idx <- bfa_dco$consult_start %in% 813 & bfa_dco$consult_end %in% 1758
bfa_dco$consult_start[idx] <- NA
bfa_dco$consult_end[idx] <- NA
bfa_dco$consult_length_calc[idx] <- NA



bfa_dco$first_anc <- ifelse(bfa_dco$num_prev_anc_visits == 0, "Yes", "No")
bfa_dco$pregnancy_week <- ifelse(
  bfa_dco$pregnancy_week %in% 98, NA, bfa_dco$pregnancy_week
)

bfa_dco$trimester <- ifelse(
  bfa_dco$pregnancy_week < 13, "First Trimester",
  ifelse(bfa_dco$pregnancy_week < 28, "Second Trimester", "Third Trimester")
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

bfa_dco <- left_join(bfa_dco, hcw_count, by = "SE")
saveRDS(bfa_dco, "bfa_dco.rds")
orderly_artefact(
  files = "bfa_dco.rds",
  description = "DCO for Burkina Faso baseline and endline surveys"
)

