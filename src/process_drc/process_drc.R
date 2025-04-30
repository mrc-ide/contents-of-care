library(dplyr)
library(foreign)
library(lubridate)
library(orderly2)
library(readr)
library(tidylog)
library(tidyr)
## Data dictionary
## https://microdata.worldbank.org/index.php/catalog/2825/data-dictionary/F1?file_name=do_anc_f3_data
indir <- "resources/drc/baseline/COD_2015_HRBFIE-FBL_v01_M_CSV"
infile <- "do_anc_f3_data.csv"
orderly_shared_resource(drc_baseline_dco.csv = paste(indir, infile, sep = "/"))

drc_baseline_dco <- read_csv("drc_baseline_dco.csv")

drc_baseline_dco <- rename(
  drc_baseline_dco,
  province = f3_id1, ## provice; 12 of these in DRC
  district = f3_id2,
  health_zone = zs_id, 
  ## Cannot use the other id columns because apart from the fact that the
  ## breakdown is inconsistent with the online data dictionary (which is true for  ## all the columns), they contain codes are that not present in any mapping.
  ## f1_00_01, f1_00_04 are not present in the data dictionary
  ## So I don't know what they mean.
  ## f3_00_05 is supposed to be health facility type
  ## however, it has 875 1s, 223 2s and 325 NAs; so I don't think it is
  ## health facility type.
  health_facility_type = f3_00_05,
  milieu_of_residence = f3_00_07,
  code_enqueteur = f3_00_08,
  date_of_visit = f3_00_09,
  hcw_qualification = f3_00_11,
  ## excluding this one; 0 if this is the first ANC
  num_prev_anc_for_this_pregnancy = f3_01_02,
  
  pregnancy_in_weeks = f3_01_03,
  first_pregnancy = f3_01_04,
  hcw_sex = f3_01_06,
  hcw_qualification = f3_01_07,
  preciser_si_autre_qualification = f3_01_07_other,
  start_time_of_consultation = f3_02_01,
  hcw_introduced_name = f3_02_02a,
  hcw_introduced_professional_grade = f3_02_02b,
  hcw_explained_procedure. = f3_02_03a,
  hcw_encouraged_questions = f3_02_03b,
  hcw_offered_third_party_support = f3_02_03c,
  hcw_asked_patient_name = f3_02_04a,
  hcw_asked_patient_age = f3_02_04b,
  hcw_asked_patient_residence = f3_02_04c,
  hcw_asked_patient_medications = f3_02_04d,
  hcw_asked_number_previous_anc_visits = f3_02_04e,
  hcw_asked_last_menstrual_period_date = f3_02_04f,
  hcw_asked_number_previous_pregnancies = f3_02_04g,
  hcw_asked_about_therapeutic_abortions = f3_02_05a,
  hcw_asked_about_preterm_deliveries = f3_02_05b,
  hcw_asked_about_early_neonatal_deaths = f3_02_05c,
  hcw_asked_about_excessive_bleeding = f3_02_05d,
  hcw_asked_about_assisted_deliveries = f3_02_05e,
  hcw_asked_about_induced_abortions = f3_02_05f,
  hcw_asked_about_normal_deliveries = f3_02_05g,
  
  hcw_asked_about_severe_abdominal_pain = f3_02_06a,
  hcw_asked_about_severe_breathing_difficulty = f3_02_06b,
  hcw_asked_about_severe_vomiting = f3_02_06c,
  hcw_asked_about_seizures_or_epilepsy = f3_02_06d,
  hcw_asked_about_health_problems_during_pregnancy = f3_02_06e,
  hcw_asked_about_stress_or_depression = f3_02_06f,
  hcw_asked_about_domestic_or_partner_violence = f3_02_06g,
  hcw_asked_about_current_medications = f3_02_06h,
  hcw_asked_about_hiv_status = f3_02_06i,
  hcw_asked_about_tetanus_vaccination_status = f3_02_06j,
  hcw_asked_about_bleeding = f3_02_06k,
  hcw_asked_about_fever = f3_02_06l,
  hcw_asked_about_headache_or_blurred_vision = f3_02_06m,
  hcw_asked_about_swelling_face_hands_feet = f3_02_06n,
  hcw_asked_about_fatigue_or_shortness_of_breath = f3_02_06o,
  hcw_asked_if_fetus_moved = f3_02_06p,
  hcw_asked_about_other_symptoms_or_problems = f3_02_06q,
  hcw_asked_about_symptoms_related_to_pregnancy = f3_02_06r,
  hcw_asked_about_birth_plan = f3_02_06s,


  hcw_ensured_exam_confidentiality = f3_02_07a,
  hcw_ensured_hygiene_before_exam = f3_02_07b,
  hcw_measured_patient_weight = f3_02_07c,
  hcw_measured_patient_pulse = f3_02_07d,
  hcw_measured_respiratory_rate = f3_02_07e,
  hcw_measured_temperature = f3_02_07f,
  hcw_measured_blood_pressure = f3_02_07g,
  hcw_performed_chest_auscultation = f3_02_07h,
  hcw_examined_conjunctiva_or_palms_for_anemia = f3_02_07i,
  hcw_examined_legs_or_feet_for_edema = f3_02_07j,
  hcw_palpated_abdomen_for_fetal_position_or_used_ultrasound = f3_02_07k,
  
  
  hcw_provided_ifa_from_six_months = f3_02_08a,
  hcw_explained_ifa_role = f3_02_08b,
  hcw_explained_ifa_dosage = f3_02_08c,
  hcw_explained_ifa_side_effects = f3_02_08d,
  hcw_provided_tetanus_toxoid_injection = f3_02_08e,
  hcw_explained_tetanus_vaccine_role = f3_02_08f,
  hcw_provided_ipt_treatment = f3_02_08g,
  hcw_explained_ipt_role = f3_02_08h,
  hcw_explained_ipt_dosage = f3_02_08i,
  hcw_explained_ipt_side_effects = f3_02_08j,
  hcw_explained_importance_second_ipt_dose = f3_02_08k,
  hcw_provided_quinine_treatment = f3_02_08l,
  hcw_provided_act_treatment = f3_02_08m,
  
  patient_received_first_dose_ipt_at_facility = f3_02_09a,
  patient_received_insecticide_treated_net = f3_02_09b,
  hcw_advised_on_nutrition_during_pregnancy = f3_02_10a,
  patient_informed_about_pregnancy_progress = f3_02_10b,
  
  hcw_mentioned_vaginal_bleeding = f3_02_11a,
  hcw_mentioned_fever = f3_02_11b,
  hcw_mentioned_excessive_fatigue_or_breathlessness = f3_02_11c,
  hcw_mentioned_hand_or_face_swelling = f3_02_11d,
  hcw_mentioned_severe_headache_or_blurred_vision = f3_02_11e,

  hcw_asked_delivery_location = f3_02_12a,
  hcw_advised_on_birth_preparedness = f3_02_12b,
  hcw_advised_use_of_skilled_birth_attendant = f3_02_12c,
  hcw_discussed_items_needed_for_home_birth = f3_02_12d,
  hcw_discussed_importance_of_newborn_vaccination = f3_02_12e,
  
  hcw_advised_exclusive_breastfeeding_until_6_months = f3_02_13,
  hcw_asked_about_awareness_of_contraceptive_methods = f3_02_14a,
  hcw_asked_about_past_use_of_contraceptive_methods = f3_02_14b,
  hcw_asked_about_intent_to_use_contraceptives_postpartum = f3_02_14c,
  hcw_asked_if_patient_knows_where_to_access_fp_services = f3_02_14d,

  hcw_discussed_barrier_methods = f3_02_15a,
  hcw_discussed_hormonal_methods = f3_02_15b,
  hcw_discussed_surgical_methods = f3_02_15c,
  hcw_discussed_traditional_methods = f3_02_15d,
  hcw_compared_contraceptive_methods_effectiveness_and_cost = f3_02_15e,

  hcw_asked_if_patient_had_questions = f3_02_16,
  hcw_used_visual_aids = f3_02_17,
  hcw_recorded_in_health_booklet = f3_02_18,
  consultation_outcome = f3_02_19,

  end_time_of_consultation = f3_02_20,
  consult_length = f3_02_20m,
  consultation_language = f3_02_21,
  consultation_language_other = f3_02_21_other
)

drc_baseline_dco$department_val  <- case_when(
  drc_baseline_dco$department == 1 ~ "Bouenza",
  drc_baseline_dco$department == 2 ~ "Brazzaville",
  drc_baseline_dco$department == 3 ~ "Cuvette",
  drc_baseline_dco$department == 4 ~ "Cuvette-Ouest",
  drc_baseline_dco$department == 5 ~ "Kouilou",
  drc_baseline_dco$department == 6 ~ "Lékoumou",
  drc_baseline_dco$department == 7 ~ "Likouala",
  drc_baseline_dco$department == 8 ~ "Niari",
  drc_baseline_dco$department == 9 ~ "Plateaux",
  drc_baseline_dco$department == 10 ~ "Pointe-Noire",
  drc_baseline_dco$department == 11 ~ "Pool",
  drc_baseline_dco$department == 12 ~ "Sangha",
  TRUE ~ as.character(drc_baseline_dco$department) 
)

drc_baseline_dco$hcw_qualification <- case_when(
  drc_baseline_dco$hcw_qualification == 1 ~ "Doctor",
  drc_baseline_dco$hcw_qualification == 2 ~ "State Registered Nurse",
  drc_baseline_dco$hcw_qualification == 3 ~ "Health Assistant",
  drc_baseline_dco$hcw_qualification == 4 ~ "Health Technician",
  drc_baseline_dco$hcw_qualification == 5 ~ "Midwife / Birth Attendant",
  drc_baseline_dco$hcw_qualification == 7 ~ "Other",
  TRUE ~ as.character(drc_baseline_dco$hcw_qualification)
)

# Save cleaned dataset
saveRDS(data, "cleaned_dataset.rds")

### Midline survey data
indir <- "resources/drc/midline-survey/COD_2018_HRBFIE-FML_v01_M_CSV" 
infile <- "f3_do_anc.csv"

orderly_shared_resource(drc_midline.csv = paste(indir, infile, sep = "/"))
drc_midline <- read_csv("drc_midline.csv")

### Endline survey data
