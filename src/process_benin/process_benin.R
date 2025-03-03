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
  ## In the data file m3_4 encodes pregnancy stage
  uterus_measured3 = m3_1,
  fetal_heartbeat_checked3 = m3_2,
  fansidar_given3 = m3_3,
  pregnancy_stage = m3_4,
  sp_ensured = m3_6
)


## Answers to pregnancy_stage are at most 36 weeks, and more than 36 weeks
benin <- relocate(benin, pregnancy_stage, .after = sp_ensured)

start <- hm(paste(benin$hour_start, benin$min_start, sep = ":"))
end <- hm(paste(benin$hour_end, benin$min_end, sep = ":"))

benin$consult_length <- time_length(end - start, unit = "minute")


benin <- mutate(benin, across(first_anc:sp_ensured, recode_oui_non))
##benin <- rowwise(benin) |> mutate(nsteps = sum(c_across(m0_03:m3_6), na.rm = TRUE))

saveRDS(benin, "benin.rds")
