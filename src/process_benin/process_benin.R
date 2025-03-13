library(dplyr)
library(foreign)
library(lubridate)
library(orderly2)
## Data dictionary available at
## https://microdata.worldbank.org/index.php/catalog/2176/data-dictionary

## Lemière, C., & de Walque, D. (2014). Health Results-Based Financing Impact
## Evaluation Survey 2010-2011, Baseline [Data set]. World Bank, Development Data
## Group. https://doi.org/10.48529/QKMG-R734

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")
indir <- "resources/benin/BEN_2010_HRBF_v01_M_v01_A_PUF_Stata8/"
infile <- "benhrbf2_m.dta" 
benin <- orderly_shared_resource(benin.dta = paste0(indir, infile))
benin <- read.dta("benin.dta")


## m0_03 to m3_6 describe the individual steps in the examnination
## Recode
benin <- rename(
  benin,
  hour_start = m0_h1d,
  min_start = m0_m1d,
  hour_end = m0_h1f,
  min_end = m0_m1f,
  trimester = m0_01,
  first_anc = m0_02,
  weight = m0_03,
  temperature = m0_04,
  inspection_of_mucous_membranes = m0_05,
  abdominal_palpation = m0_06,
  lower_limb_edema = m0_07,
  vaginal_exam = m0_08,
  speculum_exam = m0_09,
  blood_pressure = m0_10,
  albuminuria = m0_11,
  glucoserie = m0_12,
  us_requested = m0_13,
  info_complications = m0_14,
  info_placenta_previa = m0_14_1a,
  info_premature_rupture = m0_14_1b,
  info_postpartum_hemorrhage = m0_14_1c,
  info_retained_placenta = m0_14_1d,
  info_uterus_inverted = m0_14_1e,
  info_ectopic_pregnancy = m0_14_1f,
  info_hemorrhage_antepartum = m0_14_1g,
  info_prolonged_labor_latent = m0_14_1h,
  info_prolonged_labor_second = m0_14_1i,
  info_cpd = m0_14_1j,
  info_uterine_rupture = m0_14_1k,
  info_abnormal_presentation = m0_14_1l,
  info_infection = m0_14_1m,
  info_uterine_perforation = m0_14_1n,
  info_hypertensive_crises = m0_14_1o,
  info_ecclampsia = m0_14_1p,
  info_pre_eclampsia = m0_14_1q,
  info_severe_anemia = m0_14_1r,
  info_multiple_pregnancy = m0_14_1s,
  info_embolism = m0_14_1t,
  ## Section 1 questions only for patients iin first trimester
  patients_age_reported = m1_1,
  patients_height_reported = m1_2,
  diet_suggested = m1_3,
  vaccination_suggested = m1_4,
  previous_pregnancy_enquired = m1_5,
  number_of_previous_pregnancies_enquired = m1_5_1,
  toxoplasmosis_test_requested = m1_6,
  additional_bw_tests_requested = m1_7,
  blood_type_enquired = m1_8,
  blood_type_proof_enquired = m1_8_1,
  blood_type_test_requested = m1_8_2,
  mosq_kit_prescribed = m1_9,
  ## Section 2 questions only for patients in second trimester
  uterus_measured2 = m2_1,
  fetal_heartbeat_checked2 = m2_2,
  vaccination_record_checked = m2_3,
  fansidar_given2 = m2_4,
  fansidar_prescribed = m2_5,
  ## Section 3 questions only for patients in third trimester
  ## There is a mismatch between the questions listed in the data
  ## dictionary and the actual questionnaire.
  ## In the data dictionary, m3_4 is stageof pregnancy, m3_5 is incomplete and
  ## m3_6 is about ensuring SP.
  ## In the questionnaire, m3_5 is stage of pregnancy and m3_6 is
  ## about ensuring SP.
  ## In the data file m3_4 encodes pregnancy stage. I think m3_4 and m3_5 are
  ## swapped in the data; that is:
  ## m3_5 is sp_prescribed
  ## m3_4 is pregnancy_stage
  ## m3_6 is
  ## "Did the provider make sure that the patient had already (or had not yet)
  ## taken SP?"
  uterus_measured3 = m3_1,
  fetal_heartbeat_checked3 = m3_2,
  sp_given3 = m3_3,
  pregnancy_stage = m3_4,
  ## If m3_3 and m3_ are both no, then m3_6 is not applicable
  sp_prescribed3 = m3_5,
  sp_ensured3 = m3_6
)



## Answers to pregnancy_stage are at most 36 weeks, and more than 36 weeks
benin <- relocate(benin, pregnancy_stage, .after = sp_ensured3)

start <- hm(paste(benin$hour_start, benin$min_start, sep = ":"))
end <- hm(paste(benin$hour_end, benin$min_end, sep = ":"))

benin$consult_length <- time_length(end - start, unit = "minute")


benin <- mutate(benin, across(first_anc:sp_ensured3, recode_oui_non))
## benin <- rowwise(benin) |> mutate(nsteps = sum(c_across(m0_03:m3_6), na.rm = TRUE))

saveRDS(benin, "benin_dco.rds")
orderly_artefact(
  files = "benin_dco.rds",
  description = "Benin direct clinical observations survey"
)

## Process Health facility survey (part 2: clinical aspects)
## https://microdata.worldbank.org/index.php/catalog/2176/data-dictionary/F29?file_name=benhrbf2_f
infile <- "benhrbf2_f.dta"
benin_hfds <- orderly_shared_resource(benin_hfds.dta = paste0(indir, infile))
facility_survey_cl <- read.dta("benin_hfds.dta")

## These codes are used when information is not available
facility_survey_cl <- mutate(
  facility_survey_cl,
  across(
    everything(),
    ~ ifelse(. %in% c(888, 999, 88888, 88888888, 99999999, 88889999), NA, .)
  )
)

facility_survey_cl <- rename(
  facility_survey_cl,
  catchment_pop = f1_1,
  catchment_pop_female_15_49 = f1_2,
  catchment_pop_under_1 = f1_3,
  catchment_pop_under_5 = f1_4,
  total_attendance_2009 = f1_5,
  new_patients_2009 = f1_6,
  new_female_patients_2009 = f1_7,
  new_pregrant_patients_2009 = f1_8,
  new_patients_under_5_2009 = f1_9,
  vaccinations_provided = f2_1,
  vaccinations_room = f2_2,
  vaccinations_children_regulary = f2_3,
  vaccinations_workplan_2009 = f2_4,
  ## Go to reproductive health section now
  anc_offered = f3_1,
  anc_visits_2009 = f3_2,
  number_first_anc_before_first_trimester = f3_3,
  number_at_least_4_anc = f3_4,
  ## Procedures offered at every visit
  info_complications = f3_5a,
  patients_weight = f3_5b,
  blood_pressure = f3_5c,
  blood_test = f3_5d,
  urine_test = f3_5e,
  patients_height = f3_5f,
  abdominal_palpation = f3_5g,
  nutritional_advice = f3_5h,
  sp_administration = f3_5j,
  ultrasound = f3_5k,
  ## Procedures performed during the first trimester or first prenatal visit
  patients_age = f3_6a,
  patients_height2 = f3_6b,
  patients_weight2 = f3_6c,
  inspection2 = f3_6d,
  abdominal_palpation2 = f3_6e,
  vaginal_exam2 = f3_6f,
  speculum_exam2 = f3_6g,
  blood_pressure2 = f3_6h,
  blood_test2 = f3_6i,
  urine_test2 = f3_6j,
  vaccination_advice2 = f3_6k,
  nutritional_advice2 = f3_6l,
  number_previous_pregnancies2 = f3_6m,
  ultrasound2 = f3_6n,
  ## MII most likely means mosquito net
  mii = f3_6o,
  other2 = f3_6p,
  ## Procedures performed during the second trimester
  patients_weight3 = f3_7a,
  inspection3 = f3_7b,
  abdominal_palpation3 = f3_7c,
  vaginal_exam3 = f3_7d,
  speculum_exam3 = f3_7e,
  blood_pressure3 = f3_7f,
  blood_test3 = f3_7g,
  urine_test3 = f3_7h,
  uterus_measured3 = f3_7i,
  ultrasound3 = f3_7j,
  vaccination_verification3 = f3_7k,
  ## Procedures performed during the third trimester
  patients_weight4 = f3_8a,
  inspection4 = f3_8b,
  abdominal_palpation4 = f3_8c,
  vaginal_exam4 = f3_8d,
  speculum_exam4 = f3_8e,
  blood_pressure4 = f3_8f,
  blood_test4 = f3_8g,
  urine_test4 = f3_8h,
  uterus_measured4 = f3_8i,
  ultrasound4 = f3_8j,
  pelvic_exam4 = f3_8k,
  ## skipping over some less relevant questions
  number_women_vaccinated_tetanus = f3_12, ## last 6 months
  number_women_vaccinated_tetanus_2 = f3_13, ## 2009
  number_women_sp_second_dose = f3_14, ## in the required timeframe
  number_women_itn = f3_15, ## in 2009
  hiv_test_offered = f3_16, 
  pregnant_women_private_space = f3_17, ##
  ## Are the following equipment and materials available and functional
  consultation_table = f3_18a,
  blood_pressure_monitor = f3_18b,
  stethoscope = f3_18c,
  measuring_tape = f3_18d,
  weighing_scale_with_height = f3_18e,
  fetoscope = f3_18f,
  gloves_not_torn = f3_18g,
  births_assisted_24_7 = f3_19,
  number_of_days_stay_afetr_birth = f3_20,
  number_of_days_premature_no_complications = f3_21,
  number_of_women_referred_by_hf = f3_22,
  number_of_women_referred_to_hf = f3_25,
  avg_length_of_stay_if_complication = f3_29,
  number_of_maternal_deaths = f3_30, ## last 12 months
  number_of_births_2009 = f3_32,
  csec_offered = f3_39,
  csec_trained_personnel = f3_40,
  number_csec_trained_personnel = f3_40a,
  number_of_beds_for_delivery = f3_48,
  women_in_labour_seen_by_patients = f3_49,
  women_in_labour_heard_by_patients = f3_50,
  women_in_labour_pay = f3_51
)


cols_to_bin <- c(
  "catchment_pop", "catchment_pop_female_15_49", "catchment_pop_under_1",
  "catchment_pop_under_5"
)

bins <- unique(
  c(
    seq(0, 10000, by = 500), seq(10000, 15000, by = 1000),
    seq(15000, 50000, by = 5000),
    Inf
  )
)

facility_survey_cl <- mutate(
  facility_survey_cl,
  across(
    all_of(cols_to_bin),
    list(binned = ~ cut(., breaks = bins, dig.lab = 5, ordered_result = TRUE)),
    .names = "{.col}_binned"
  )
)


cols_to_bin <- c(
  "total_attendance_2009", "new_patients_2009",
  "new_female_patients_2009", "new_pregrant_patients_2009",
  "new_patients_under_5_2009", "anc_visits_2009"
)

bins <- unique(
  c(
    seq(0, 1000, by = 100), seq(1000, 10000, by = 2000), Inf
  )
)

facility_survey_cl <- mutate(
  facility_survey_cl,
  across(
    all_of(cols_to_bin),
    list(binned = ~ cut(., breaks = bins, dig.lab = 5, ordered_result = TRUE)),
    .names = "{.col}_binned"
  )
)


saveRDS(facility_survey_cl, "benin_facility_survey_clinical.rds")
orderly_artefact(
  files = "benin_facility_survey_clinical.rds",
  description = "Benin facility survey clinical survey"
)




