library(cli)
library(dplyr)
library(foreign)
library(janitor)
library(lubridate)
library(orderly2)
library(purrr)
library(readr)
library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")


## Dataset ID: 2711
## Data dictionary
## https://microdata.worldbank.org/index.php/catalog/5832/data-dictionary
indir <- "resources/drc/follow-up-survey/COD_2021-2022_HRBFIE-HFFU_v01_M_CSV"


orderly_shared_resource(
  drc_endline_dco.csv =
    paste(indir, "f3_f5_anc_observation_exit_anon.csv", sep = "/")
)



drc_endline_dco <- read_csv("drc_endline_dco.csv")


drc_endline_dco <- rename(
  drc_endline_dco,
  province = f3_06
)

drc_endline_dco$province <- case_when(
  drc_endline_dco$province == 11 ~ "Kwango",
  drc_endline_dco$province == 12 ~ "Kwilu",
  drc_endline_dco$province == 13 ~ "Mai-ndombe",
  drc_endline_dco$province == 31 ~ "Haut-Katanga",
  drc_endline_dco$province == 32 ~ "Haut-Lomami",
  drc_endline_dco$province == 33 ~ "Lualaba",
  TRUE ~ as.character(drc_endline_dco$province)
)


drc_endline_dco$facility_level_mapping <- case_when(
  drc_endline_dco$f3_12 %in% 1 ~ "Tertiary",
  drc_endline_dco$f3_12 %in% 2 ~ "Secondary",
  drc_endline_dco$f3_12 %in% 3 ~ "Primary",
  TRUE ~ as.character(drc_endline_dco$f3_12)
)

drc_endline_dco$facility_status_mapping <- case_when(
  drc_endline_dco$f3_14 %in% 1 ~ "Public",
  TRUE ~ "Not Public"
)

drc_endline_dco$milieu_of_residence <- case_when(
  drc_endline_dco$f3_15 %in% 1 ~ "Urban",
  TRUE ~ "Rural"
)

drc_endline_dco$date_of_visit <- mdy(drc_endline_dco$f3_16)
drc_endline_dco$day_of_visit <- weekdays(drc_endline_dco$date_of_visit)

## Is the language of the respondent (f3_17) same as the language
## is which consultation was conducted (f3_221)
drc_endline_dco$languages_aligned <- case_when(
  drc_endline_dco$f3_221 == drc_endline_dco$f3_17 ~ "Yes",
  TRUE ~ "No"
)

drc_endline_dco$consultation_language <- case_when(
  drc_endline_dco$f3_221 == 1 ~ "French",
  drc_endline_dco$f3_221 == 2 ~ "Lingala",
  drc_endline_dco$f3_221 == 3 ~ "Kikongo",
  drc_endline_dco$f3_221 == 4 ~ "Swahili",
  drc_endline_dco$f3_221 == 5 ~ "Other",
  TRUE ~ NA_character_
)

drc_endline_dco$first_anc <- case_when(
  drc_endline_dco$f3_101b %in% 1 ~ "First ANC",
  drc_endline_dco$f3_101b %in% 2 ~ "Follow-up ANC",
  TRUE ~ NA_character_
)

## 1785 NAs, so best to not use this variable
drc_endline_dco <- rename(
  drc_endline_dco,
  num_prev_anc_visits = f3_102
  ## excluding this one; 0 if this is the first ANC
)


drc_endline_dco <- rename(
  drc_endline_dco, pregnancy_in_weeks = f3_103
)

drc_endline_dco$trimester <- ifelse(
  drc_endline_dco$pregnancy_in_weeks < 13,
  "First Trimester",
   ifelse(
     drc_endline_dco$pregnancy_in_weeks < 28,
     "Second Trimester", "Third Trimester"
   )
)
  
drc_endline_dco <- rename(
  drc_endline_dco,
  first_pregnancy = f3_104
)

drc_endline_dco$first_pregnancy <- factor(
  drc_endline_dco$first_pregnancy,
  levels = c(1, 2), labels = c("Yes", "No")
)


drc_endline_dco <- rename(
  drc_endline_dco,
  hcw_sex = f3_106,
  hcw_qualification = f3_107,
  hcw_qualification_other = f3_107_autre
)

drc_endline_dco$hcw_sex <- case_when(
  drc_endline_dco$hcw_sex %in% 1 ~ "Male",
  drc_endline_dco$hcw_sex %in% 2 ~ "Female",
  TRUE ~ NA_character_
)

drc_endline_dco$hcw_qualification <- case_when(
  drc_endline_dco$hcw_qualification == 1 ~ "Doctor",
  drc_endline_dco$hcw_qualification %in% c(2, 3, 4) ~ "Nurse",
  drc_endline_dco$hcw_qualification == 6 ~ "Midwife",
  TRUE ~ "Other"
)

drc_endline_dco <- rename(
  drc_endline_dco,
  start_time_of_consultation = f3_201,
  end_time_of_consultation = f3_220,
  consult_length = f3_220a
)

drc_endline_dco$consult_length_calc <-
  difftime(
    drc_endline_dco$end_time_of_consultation,
    drc_endline_dco$start_time_of_consultation,
    units = "mins"
  ) |> as.numeric() |> round(0)


## Three rows where consult length is less than 0; looks like
## start and end times are swapped
idx <- which(drc_endline_dco$consult_length_calc < 0)
tmp <- drc_endline_dco$start_time_of_consultation[idx]
drc_endline_dco$start_time_of_consultation[idx] <-
  drc_endline_dco$end_time_of_consultation[idx]
drc_endline_dco$end_time_of_consultation[idx] <- tmp

drc_endline_dco$consult_length_calc <-
  difftime(
    drc_endline_dco$end_time_of_consultation,
    drc_endline_dco$start_time_of_consultation,
    units = "mins"
  ) |>
  as.numeric() |>
  round(0)


 drc_endline_dco$time_elapsed_since_start_of_day <-
  as.numeric(difftime(
    drc_endline_dco$start_time_of_consultation,
    start_of_day, 
    units = "mins"
  )) |> round(0)

drc_endline_dco <- rename(
  drc_endline_dco,

  hcw_introduced_professional_grade = f3_202b,
  hcw_explained_procedure. = f3_203a,
  hcw_encouraged_questions = f3_203b,
  hcw_offered_third_party_support = f3_203c,
  hcw_asked_patient_age = f3_204b,
  hcw_asked_patient_residence = f3_204c,
  hcw_asked_patient_medications = f3_204d,
  hcw_asked_number_previous_anc_visits = f3_204e,
  hcw_asked_last_menstrual_period_date = f3_204f,
  hcw_asked_number_previous_pregnancies = f3_204g,

  is_this_first_pregnancy = f3_205a,
  ## if yes; questions f3_205_a to f3_205_g are applicable
  hcw_asked_about_prev_medical_termination = f3_205_a,
  hcw_asked_about_preterm_deliveries = f3_205_b,
  hcw_asked_about_early_neonatal_deaths = f3_205_c,
  hcw_asked_about_excessive_bleeding = f3_205_d,
  hcw_asked_about_assisted_deliveries = f3_205_e,
  hcw_asked_about_induced_abortions = f3_205_f,
  hcw_asked_about_normal_deliveries = f3_205_g,
  
  hcw_asked_about_severe_abdominal_pain = f3_206a,
  hcw_asked_about_severe_breathing_difficulty = f3_206b,
  hcw_asked_about_severe_vomiting = f3_206c,
  hcw_asked_about_seizures_or_epilepsy = f3_206d,
  hcw_asked_about_hypertension = f3_206e,
  hcw_asked_about_health_problems_during_pregnancy = f3_206f,
  hcw_asked_about_stress_or_depression = f3_206g,
  hcw_asked_about_domestic_or_partner_violence = f3_206h,
  hcw_asked_about_current_medications = f3_206i,
  hcw_asked_about_hiv_status = f3_206j,
  hcw_asked_about_tetanus_vaccination_status = f3_206k,
  hcw_asked_about_bleeding = f3_206l,
  hcw_asked_about_fever = f3_206m,
  hcw_asked_about_headache_or_blurred_vision = f3_206n,
  hcw_asked_about_swelling_face_hands_feet = f3_206o,
  hcw_asked_about_fatigue_or_shortness_of_breath = f3_206p,
  hcw_asked_if_fetus_moved = f3_206q,
  hcw_asked_about_other_symptoms_or_problems = f3_206r,
  hcw_asked_about_symptoms_related_to_pregnancy = f3_206s,
  hcw_asked_about_birth_plan = f3_206t,


  hcw_ensured_exam_confidentiality = f3_207a,
  hcw_ensured_hygiene_before_exam = f3_207b,
  hcw_measured_patient_weight = f3_207c,
  hcw_measured_patient_pulse = f3_207d,
  hcw_measured_respiratory_rate = f3_207e,
  hcw_measured_temperature = f3_207f,
  hcw_measured_blood_pressure = f3_207g,
  hcw_performed_chest_auscultation = f3_207h,
  hcw_examined_conjunctiva_or_palms_for_anemia = f3_207i,
  hcw_examined_legs_or_feet_for_edema = f3_207j,
  hcw_palpated_abdomen_for_fetal_position_or_used_ultrasound = f3_207k,
  hcw_measured_fundal_height = f3_207l,
  hcw_heard_fetal_heartbeat = f3_207m,
  hcw_examined_breasts = f3_207n,
  hcw_asked_for_blood_test_for_hemoglobin = f3_207o,
  hcw_asked_for_blood_test_for_bloodtype = f3_207p,
  hcw_asked_for_blood_test_for_rhesus_factor = f3_207q,
  hcw_did_urine_test_for_glucose_and_albumin = f3_207r,
  hcw_tested_or_referred_for_syphillis_test = f3_207s,
  hcw_tested_or_referred_for_hiv_test = f3_207t,
  hcw_looked_at_patient_records = f3_207u,
  hcw_explained_difficult_results = f3_207v,
  
  
  hcw_prescribed_or_administered_iron_suppl = f3_208a,
  hcw_explained_ifa_role = f3_208b,
  hcw_explained_ifa_dosage = f3_208c,
  hcw_explained_ifa_side_effects = f3_208d,
  hcw_provided_tetanus_toxoid_injection = f3_208e,
  hcw_explained_tetanus_vaccine_role = f3_208f,
  hcw_provided_ipt_treatment = f3_208g,
  hcw_explained_ipt_role = f3_208h,
  hcw_explained_ipt_dosage = f3_208i,
  hcw_explained_ipt_side_effects = f3_208j,
  ##hcw_explained_importance_second_ipt_dose = f3_208k,
  hcw_provided_quinine_treatment = f3_208l,
  hcw_provided_act_treatment = f3_208m,
  
  patient_received_first_dose_ipt_at_facility = f3_209a,
  patient_received_insecticide_treated_net = f3_209b,
  hcw_advised_on_nutrition_during_pregnancy = f3_210a,
  patient_informed_about_pregnancy_progress = f3_210b,
  
  hcw_mentioned_vaginal_bleeding = f3_211a,
  hcw_mentioned_convulsions = f3_211a1,
  hcw_mentioned_severe_headache_or_blurred_vision = f3_211a2,  
  hcw_mentioned_fever_and_lethargy = f3_211a3,
  hcw_mentioned_severe_abdominal_pain = f3_211a4,
  hcw_mentioned_breathlessness = f3_211a5,
  hcw_mentioned_fever_only = f3_211b,
  hcw_mentioned_abdominal_pain = f3_211b1,
  hcw_mentioned_feeling_sick = f3_211b2,
  hcw_mentioned_hand_or_face_swelling = f3_211d,


  hcw_asked_delivery_location = f3_212a,
  hcw_advised_on_birth_preparedness = f3_212b,
  hcw_advised_use_of_skilled_birth_attendant = f3_212c,
  hcw_discussed_items_needed_for_home_birth = f3_212d,
  hcw_discussed_importance_of_newborn_vaccination = f3_212e,

  hcw_talked_about_exclusive_breastfeeding = f3_213a,
  hcw_talked_about_duration_breastfeeding = f3_213b,
  hcw_advised_initiation_breastfeeding = f3_213c,
  
  hcw_asked_about_awareness_of_contraceptive_methods = f3_214a,
  hcw_talked_about_hormonal_contraceptive_methods = f3_214b,
  hcw_asked_about_past_use_of_contraceptive_methods = f3_214b,
  hcw_asked_about_intent_to_use_contraceptives_postpartum = f3_214c,
  hcw_asked_if_patient_knows_where_to_access_fp_services = f3_214d,

  hcw_discussed_barrier_methods = f3_215a,
  hcw_discussed_hormonal_methods = f3_215b,
  hcw_discussed_surgical_methods = f3_215c,
  hcw_discussed_traditional_methods = f3_215d,
  hcw_compared_contraceptive_methods_effectiveness_and_cost = f3_215e,

  hcw_asked_if_patient_had_questions = f3_216,
  did_patient_ask_questions = f3_216a,
  hcw_used_visual_aids = f3_217,
  hcw_recorded_in_health_booklet = f3_218
)



drc_endline_dco <- mutate(
  drc_endline_dco,
  across(
    hcw_introduced_professional_grade:hcw_recorded_in_health_booklet,
    ~ factor(., levels = c(1, 2), labels = c("yes", "no"))
  )
)

  

saveRDS(drc_endline_dco, "drc_dco_2015.rds")
orderly_artefact(
  files = c("drc_dco_2015.rds"),
  description = "DRC DCO 2015"
)


orderly_shared_resource(
  drc_endline_hf.csv = paste(indir, "f1_facility_anon.csv", sep = "/")
)

drc_endline_hf <- read_csv("drc_endline_hf.csv")


drc_endline_hf$catchment_pop <- drc_endline_hf$f1_702a
drc_endline_hf$catchment_pop <- ifelse(
  drc_endline_hf$catchment_pop < 0, NA_integer_, drc_endline_hf$catchment_pop
)


drc_endline_hf$catchment_pop_pregnant_women <- drc_endline_hf$f1_702b
drc_endline_hf$catchment_pop_pregnant_women <- ifelse(
  drc_endline_hf$catchment_pop_pregnant_women < 0, NA_integer_,
  drc_endline_hf$catchment_pop_pregnant_women
)

drc_endline_hf$catchment_pop_female_15_49 <- drc_endline_hf$f1_702c
drc_endline_hf$catchment_pop_female_15_49 <- ifelse(
  drc_endline_hf$catchment_pop_female_15_49 < 0, NA_integer_,
  drc_endline_hf$catchment_pop_female_15_49
)


drc_endline_hf$catchment_pop_under_5 <- drc_endline_hf$f1_702d
drc_endline_hf$catchment_pop_under_5 <- ifelse(
  drc_endline_hf$catchment_pop_under_5 < 0, NA_integer_,
  drc_endline_hf$catchment_pop_under_5
)


## Questions 7.20 to 7.27 are about whether the relevant
## reports exist in the facility

## Section 9 is about whether patients pay for services
drc_endline_hf$patients_pay_for_consultation <- case_when(
  drc_endline_hf$f1_902 == 1 ~ "Yes",
  drc_endline_hf$f1_902 == 2 ~ "No",
  TRUE ~ NA_character_
)

## lab fees
drc_endline_hf$patients_pay_for_lab_tests <- case_when(
  drc_endline_hf$f1_903 == 1 ~ "Yes",
  drc_endline_hf$f1_903 == 2 ~ "No",
  drc_endline_hf$f1_903 == 9 ~ "Not applicable",
  TRUE ~ NA_character_
)

## medical imaging
drc_endline_hf$patients_pay_for_medical_imaging <- case_when(
  drc_endline_hf$f1_904 == 1 ~ "Yes",
  drc_endline_hf$f1_904 == 2 ~ "No",
  drc_endline_hf$f1_904 == 9 ~ "Not applicable",
  TRUE ~ NA_character_
)

## hospitalization cost
drc_endline_hf$patients_pay_for_hospitalization <- case_when(
  drc_endline_hf$f1_905 == 1 ~ "Yes",
  drc_endline_hf$f1_905 == 2 ~ "No",
  TRUE ~ NA_character_
)

## consumables
drc_endline_hf$patients_pay_for_consumables <- case_when(
  drc_endline_hf$f1_906 == 1 ~ "Yes",
  drc_endline_hf$f1_906 == 2 ~ "No",
  TRUE ~ NA_character_
)

## medicines
drc_endline_hf$patients_pay_for_medicines <- case_when(
  drc_endline_hf$f1_907 == 1 ~ "Yes",
  drc_endline_hf$f1_907 == 2 ~ "No",
  TRUE ~ NA_character_
)

## Section 13 is a set of questions on availability of equipments
## 13.08 is specific to ANC
## f1_13_08b_c: operational; f1_13_08a_c
drc_endline_hf$hf_has_fetoscope <- case_when(
  drc_endline_hf$f1_1308c > 0 ~ "Yes",
  drc_endline_hf$f1_1308c == 0 ~ "No",
  TRUE ~ NA_character_
)

## Filled posts; these are in sheet 4 but have been made available in a
## separate file
orderly_shared_resource(
  drc_endline_hf_roster.csv =
    paste(indir, "f1_facility_roster_anon.csv", sep = "/")
)

drc_endline_hf_roster <- read_csv("drc_endline_hf_roster.csv")


drc_endline_hf_roster$hcw_role <- case_when(
  drc_endline_hf_roster$f1_405b %in% c(1, 4, 5) ~ "Doctor",
  drc_endline_hf_roster$f1_405b %in%
    c(2, 10, 11, 12, 17, 18, 19, 20) ~ "Nurse/Midwife",
  drc_endline_hf_roster$f1_405b %in% c(3, 8, 97) ~ "Other",
  TRUE ~ "Other"
)

hcw_count <-
  count(drc_endline_hf_roster, f1_10, hcw_role) |> spread(hcw_role, n)

hcw_count$doctor_or_nursing_and_midwifery <- rowSums(
  cbind(
    hcw_count$Doctor,
    hcw_count$`Nurse/Midwife`
  ),
  na.rm = TRUE
)

## First calculate doctors and nurses per 10,000 population
## and scale; then combine with DCO
tmp <- left_join(
  drc_endline_hf, hcw_count,
  by = c("f1_10" = "f1_10")
)
tmp$doctor_or_nursing_and_midwifery_per_10000 <-
  (tmp$doctor_or_nursing_and_midwifery / tmp$catchment_pop) * 10000

x <- scale(tmp$doctor_or_nursing_and_midwifery_per_10000)
tmp$doctor_or_nursing_and_midwifery_scaled <- x[, 1]
  


scaled_attrs <- 
  data.frame(
    variable = "doctor_or_nursing_and_midwifery_per_10000",
    mean = attr(x, "scaled:center"),
    sd = attr(x, "scaled:scale")
  )
  
saveRDS(scaled_attrs, "drc_endline_hf_scaled_attrs.rds")
orderly_artefact(
  files = c("drc_endline_hf_scaled_attrs.rds"),
  description = "DRC health facility data scaled attributes"
)


## Put eveything together
## The dictionary doesn't say which column is the "id" of the facility
## but lookin at the values, i think f3_10 in the ANC observation is
## facility id, and f1_10 is the facility id in the facility dataset.
## There is only one id in DCO which is missing from HF,
## and that leads to the exclusion of 8 rows.

drc_endline_dco <- left_join(
  drc_endline_dco, drc_endline_hf, by = c("f3_10" = "f1_10")
)

drc_endline_dco <- left_join(
  drc_endline_dco, tmp[, c("f1_10", "doctor_or_nursing_and_midwifery_scaled")],
  by = c("f3_10" = "f1_10")
)



saveRDS(drc_endline_hf, "drc_endline_hf.rds")


saveRDS(drc_endline_dco, "drc_dco_2021.rds")
orderly_artefact(
  files = c("drc_dco_2021.rds"),
  description = "DRC DCO 2021 augmented with health facility data"
)

drc_endline_small <- select(
  drc_endline_dco,
  consult_length_calc,
  province,
  facility_level_mapping,
  facility_type = facility_status_mapping,
  milieu_of_residence,
  languages_aligned,
  patients_pay_for_consumables,
  hf_has_fetoscope,
  doctor_or_nursing_and_midwifery_scaled,
  ## Patient characteristics
  pregnancy_in_weeks, first_pregnancy, first_anc,
  trimester,
  ## HCW characteristics
  hcw_sex, hcw_qualification,
  ## Appointment characteristics
  consultation_language,
  time_elapsed_since_start_of_day,
  day_of_visit
)

## drc_endline_small$first_anc <- factor(
##   drc_endline_small$first_anc,
##   levels = c("yes", "no"),
##   labels = c("First ANC: Yes", "First ANC: No"),
##   ordered = TRUE
## )


drc_endline_small <- mutate_if(
  drc_endline_small, is.character, ~ ifelse(is.na(.), "Unknown", .)
)

drc_endline_split <- split(
  drc_endline_small,
  list(drc_endline_small$first_anc, drc_endline_small$trimester),
  sep = "_"
)


map(drc_endline_split, nrow)
map(drc_endline_split, function(x) {
  map(x, ~ sum(is.na(.))) |> keep(~ . > 0)
})

saveRDS(drc_endline_split, "drc_endline_split.rds")
orderly_artefact(
  files = "drc_endline_split.rds",
  description = "DRC data used for model fitting"
)
  

## orderly_cleanup()


