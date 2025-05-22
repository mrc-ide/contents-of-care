## ANC Exit interview; tranlsation from page 670 of accompanying PDF
## Questionnaire F5 is exit survey; however data dictionary says it is F6!
## In the tranlsation, it appears to be f5 rather than f6.
## SO I am going with F5.
indir <- "resources/bfa/baseline/BFA_2013_HRBFIE-FBL_v01_M_CSV"

orderly_shared_resource(
  bfa_baseline_exit.csv = paste(indir, "f5_aug19.csv", sep = "/")
) 
bfa_baseline_exit <- read_csv("bfa_baseline_exit.csv")

bfa_baseline_exit <- mutate(bfa_baseline_exit, across(
  c(
    f5_216, f5_217, f5_220, f5_221, f5_222, f5_223, f5_224,
    f5_225, f5_226, f5_227, f5_228, f5_229, f5_230,
    f5_231_a, f5_231_b, f5_231_c, f5_231_d, f5_231_e, f5_231_spe,
    f5_232, f5_234, f5_235_a, f5_235_b, f5_235_c, f5_235_d,
    f5_236, f5_238, f5_239:f5_243, f5_249, f5_250, f5_246_a:f5_246_m,
    f5_248_a:f5_248_d, f5_251_a:f5_251_m,
    ## From sheet Temps et depense
    f5_307, f5_308, f5_310, f5_312, f5_314, f5_316, f5_320,
    f5_319_a:f5_319_g
  ),
  function(x) {
    x <- ifelse(x %in% 2, "no", ifelse(x %in% 1, "yes", NA))
  }
))



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
  ## f5_703:f5_705 and f5_801 are NA across all rows in the endline dataset
)


cols <- c(
  "patient_seen_chw_at_health_center", "patient_seen_chw_at_home",
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

bfa_baseline_exit$patient_mode_of_transport <- case_when(
  bfa_baseline_exit$patient_mode_of_transport == 1 ~ "Walk",
  bfa_baseline_exit$patient_mode_of_transport == 2 ~ "Bicycle",
  bfa_baseline_exit$patient_mode_of_transport == 3 ~ "Donkey or horsecart",
  bfa_baseline_exit$patient_mode_of_transport == 4 ~ "Car",
  bfa_baseline_exit$patient_mode_of_transport == 5 ~ "Motorcycle",
  bfa_baseline_exit$patient_mode_of_transport == 6 ~ "Public transport",
  bfa_baseline_exit$patient_mode_of_transport == 7 ~ "Other",
  TRUE ~ NA
)

bfa_baseline_exit$patient_highest_education <- case_when(
  bfa_baseline_exit$patient_highest_education == 1 ~ "No education",
  bfa_baseline_exit$patient_highest_education == 2 ~ "Primary",
  bfa_baseline_exit$patient_highest_education %in% c(3, 4) ~ "Secondary",
  bfa_baseline_exit$patient_highest_education == 5 ~ "Higher",
  bfa_baseline_exit$patient_highest_education == 6 ~ "Do not know",
  TRUE ~ NA
)

bfa_baseline_exit$patient_partner_highest_education <- case_when(
  bfa_baseline_exit$patient_partner_highest_education == 1 ~ "No education",
  bfa_baseline_exit$patient_partner_highest_education == 2 ~ "Primary",
  bfa_baseline_exit$patient_partner_highest_education %in% c(3, 4) ~ "Secondary",
  bfa_baseline_exit$patient_partner_highest_education == 5 ~ "Higher",
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

bfa_baseline_exit$REGION <- case_when(
  bfa_baseline_exit$REGION %in% 1 ~ "BOUCLE DU MOUHOUN",
  bfa_baseline_exit$REGION %in% 2 ~ "CENTRE-EST",
  bfa_baseline_exit$REGION %in% 3 ~ "CENTRE-NORD",
  bfa_baseline_exit$REGION %in% 4 ~ "CENTRE-OUEST",
  bfa_baseline_exit$REGION %in% 5 ~ "NORD",
  bfa_baseline_exit$REGION %in% 6 ~ "SUD-OUEST",
  TRUE ~ NA
 )

bfa_baseline_exit$patient_highest_education <- case_when(
  bfa_baseline_exit$patient_highest_education == 1 ~ "No education",
  bfa_baseline_exit$patient_highest_education == 2 ~ "Primary",
  bfa_baseline_exit$patient_highest_education == 3 ~ "Secondary 1 cycle",
  bfa_baseline_exit$patient_highest_education == 4 ~ "Secondary 2 cycle",
  bfa_baseline_exit$patient_highest_education == 5 ~ "Higher",
  TRUE ~ NA
)


## Collect all yes/no columns so that we can recode in one go
bfa_baseline_exit <- relocate(
  bfa_baseline_exit,
  patient_can_read_write, first_pregnancy, first_anc_visit_at_this_hf,
  waiting_time_too_long, patient_paid_consult_fee,
  patient_paid_extra_fee, lab_test_done, us_done, medicine_dispensed,
  patient_seen_chw_at_health_center, patient_seen_chw_at_home,
  patient_seen_chw_elsewhere, patient_seen_tba
)


####### Endline exit survey

indir <- "resources/bfa/endline/BFA_2017_HRBFIE-FEL_v01_M_CSV"
orderly_shared_resource(
  bfa_endline_exit.csv = paste(indir, "f5_jul19.csv", sep = "/")
)

bfa_endline_exit <- read_csv("bfa_endline_exit.csv")

bfa_endline_exit <- mutate(bfa_endline_exit, across(
  c(
    f5_216, f5_217, f5_220, f5_221, f5_222, f5_223, f5_224,
    f5_225, f5_226, f5_227, f5_228, f5_229, f5_230,
    f5_231_a, f5_231_b, f5_231_c, f5_231_d, f5_231_e, f5_231_spe,
    f5_232, f5_234, f5_235_a, f5_235_b, f5_235_c, f5_235_d,
    f5_236, f5_238, f5_239:f5_243, f5_249, f5_250, f5_246_a:f5_246_m,
    f5_248_a:f5_248_d, f5_251_a:f5_251_m
  ),
  function(x) {
    x <- ifelse(x %in% "NON", "no", ifelse(x %in% "OUI", "yes", NA))
  }
))


bfa_endline_exit <- rename(
  bfa_endline_exit, SE = se,
  REGION = reg,
  ## Exclude district because in baseline it is available as numeric code and
  ## I don';t have the mapping
  ## DISTRICT = dst,
  facility_level = nivfs,
  patient_age = f5_101,
  patient_can_read_write = f5_102,
  patient_highest_education = f5_103,
  patient_marital_status = f5_105,
  patient_partner_highest_education = f5_106,
  num_of_hcws_seen = f5_201,
  num_of_weeks_pregnant_an_book = f5_212,
  num_of_weeks_pregnant_self_report = f5_215,
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

bfa_endline_exit$patient_highest_education <- case_when(
  bfa_endline_exit$patient_highest_education %in% "Aucun" ~ "No education",
  bfa_endline_exit$patient_highest_education %in% "Primaire" ~ "Primary",
  bfa_endline_exit$patient_highest_education %in%
    c("Secondaire 1er cycle", "Secondaire 2ème cycle") ~ "Secondary",
  bfa_endline_exit$patient_highest_education %in% "Supérieur" ~ "Higher",
  TRUE ~ NA
)

bfa_endline_exit$patient_partner_highest_education <- case_when(
  bfa_endline_exit$patient_partner_highest_education %in% "Aucun" ~ "No education",
  bfa_endline_exit$patient_partner_highest_education %in% "Primaire" ~ "Primary",
  bfa_endline_exit$patient_partner_highest_education %in%
    c("Secondaire 1er cycle", "Secondaire 2ème cycle") ~ "Secondary",
  bfa_endline_exit$patient_partner_highest_education %in% "Supérieur" ~ "Higher",
  bfa_endline_exit$patient_partner_highest_education %in% "Ne sais pas" ~ "Do not know",
  TRUE ~ NA
)


bfa_endline_exit$patient_marital_status <- case_when(
  bfa_endline_exit$patient_marital_status %in% "Célibataire" ~ "Single",
  bfa_endline_exit$patient_marital_status %in% "Mariée / union libre" ~ "Married/Common-law",
  bfa_endline_exit$patient_marital_status %in% "Veuve" ~ "Widowed",
  TRUE ~ NA
)


bfa_baseline_exit <- mutate(
  bfa_baseline_exit, across(patient_can_read_write:patient_seen_tba, function(x) {
    x <- case_when(x %in% 1 ~ "yes", x %in% 2 ~ "no", TRUE ~ NA)
    x
  }))


bfa_endline_exit <- relocate(
  bfa_endline_exit,
  patient_can_read_write, first_pregnancy, first_anc_visit_at_this_hf,
  waiting_time_too_long, patient_paid_consult_fee,
  patient_paid_extra_fee, lab_test_done, us_done, medicine_dispensed,
  patient_seen_chw_at_health_center, patient_seen_chw_at_home,
  patient_seen_chw_elsewhere, patient_seen_tba
)


bfa_endline_exit <- mutate(
  bfa_endline_exit, across(patient_can_read_write:patient_seen_tba, function(x) {
    x <- case_when(x %in% "OUI" ~ "yes", x %in% "NON" ~ "no", TRUE ~ NA)
    x
  })
)

## Remove columns we don't want
## f5_104 is about the highest class attended;
## f5_210: Did you bring a prenatal record book? Might be useful 98% answer yes
## hence exlcuding
bfa_baseline_exit <- select(
  bfa_baseline_exit, -f5_104, -f5_210, -f5_214, -f5_233, -f5_237,
  -starts_with("f5_321"),
  ## Sheet 4 has questions about patients reason for choosing this HF and several
  ## other questions that I think might not be relevant for our analysis. Hence
  ## excluding for now.
  -starts_with("f5_4"),
  ## Sheet 5 similarly has questions about the security situration etc
  -starts_with("f5_5"),
  ## Sheet 6 has questions about the patients household
  -starts_with("f5_6"),
  ## Sheet 7 has questions about community health workers; questions I thought
  ## might be relevant have been renamed above
  -starts_with("f5_7"),
  ## Sheet 8 has quesions about traditional birth attendants; questions I thought
  ## might be relevant have been renamed above
  -starts_with("f5_8")
  )

bfa_endline_exit <- select(
  bfa_endline_exit, -f5_104, -f5_210, -f5_214, -f5_233, -f5_237,
  -starts_with("f5_321"),
  ## Sheet 4 has questions about patients reason for choosing this HF and several
  ## other questions that I think might not be relevant for our analysis. Hence
  ## excluding for now.
  -starts_with("f5_4"),
  ## Sheet 5 similarly has questions about the security situration etc
  -starts_with("f5_5"),
  ## Sheet 6 has questions about the patients household
  -starts_with("f5_6"),
  ## Sheet 7 has questions about community health workers; questions I thought
  ## might be relevant have been renamed above
  -starts_with("f5_7"),
  ## Sheet 8 has quesions about traditional birth attendants; questions I thought
  ## might be relevant have been renamed above
  -starts_with("f5_8")
)

bfa_baseline_exit <- rename(bfa_baseline_exit, patient_receivied_tat = f5_211)
bfa_endline_exit <- rename(bfa_endline_exit, patient_receivied_tat = f5_211)

bfa_endline_exit$patient_receivied_tat <- case_when(
  bfa_endline_exit$patient_receivied_tat %in%
    c("OUI, 1 FOIS", "OUI, 2 FOIS OU PLUS") ~ "yes",
  bfa_endline_exit$patient_receivied_tat %in% "NON" ~ "no",
  TRUE ~ NA
)

bfa_baseline_exit$patient_receivied_tat <- case_when(
  bfa_baseline_exit$patient_receivied_tat %in% c(1, 2) ~ "yes",
  bfa_baseline_exit$patient_receivied_tat %in% 3 ~ "no",
  TRUE ~ NA
)

bfa_endline_exit$num_of_weeks_pregnant_an_book <- case_when(
  bfa_endline_exit$num_of_weeks_pregnant_an_book %in% "NON DECLARE" ~ NA,
  TRUE ~ bfa_endline_exit$num_of_weeks_pregnant_an_book
)

bfa_endline_exit$num_of_weeks_pregnant_an_book <- as.integer(
  bfa_endline_exit$num_of_weeks_pregnant_an_book
)


bfa_baseline_exit <- rename(bfa_baseline_exit, patient_receivied_sp = f5_213)
bfa_endline_exit <- rename(bfa_endline_exit, patient_receivied_sp = f5_213)

bfa_baseline_exit$patient_receivied_sp <- case_when(
  bfa_baseline_exit$patient_receivied_sp %in% c(1, 2) ~ "yes",
  bfa_baseline_exit$patient_receivied_sp %in% 3 ~ "no",
  TRUE ~ NA
)

bfa_endline_exit$patient_receivied_sp <- case_when(
  bfa_endline_exit$patient_receivied_sp %in%
    c("OUI, 1 DOSE", "OUI, 2 DOSES") ~ "yes",
  bfa_endline_exit$patient_receivied_sp %in% "NON" ~ "no",
  TRUE ~ NA
)
bfa_baseline_exit$f5_245 <- case_when(
  bfa_baseline_exit$f5_245 %in% c(1, 2) ~ "yes",
  bfa_baseline_exit$f5_245 %in% 3 ~ "no",
  TRUE ~ NA
)
bfa_endline_exit$f5_245 <- case_when(
  bfa_endline_exit$f5_245 %in%
    c("Oui, pendant cette visite", "Oui, pendant les précédentes visites") ~ "yes",
  bfa_endline_exit$f5_245 %in% "NON" ~ "no",
  TRUE ~ NA
)

bfa_baseline_exit$patient_mode_of_transport <- case_when(
  bfa_baseline_exit$patient_mode_of_transport %in% "À pieds" ~ "Walk",
  bfa_baseline_exit$patient_mode_of_transport %in% "Vélo personnel" ~ "Bicycle",
  bfa_baseline_exit$patient_mode_of_transport %in%
    "Charrette avec âne ou cheval" ~ "Donkey or horsecart",
  bfa_baseline_exit$patient_mode_of_transport %in%
    "Voiture personnelle"~ "Car",
  bfa_baseline_exit$patient_mode_of_transport %in%
    "Moto personnelle"~ "Motorcycle",
  bfa_baseline_exit$patient_mode_of_transport %in%
    "Transport collectif (bus,vélo, moto, taxi)" ~ "Public transport",
  bfa_baseline_exit$patient_mode_of_transport %in% "Autre" ~ "Other",
  TRUE ~ NA
)

bfa_exit <- bind_rows(
  list(baseline = bfa_baseline_exit, endline = bfa_endline_exit), .id = "survey"
)


## Now create new variables for the baseline and endline data

bfa_exit$trimester <- case_when(
  bfa_exit$num_of_weeks_pregnant_an_book < 13 ~ "First Trimester",
  bfa_exit$num_of_weeks_pregnant_an_book > 13 &
    bfa_exit$num_of_weeks_pregnant_an_book < 28 ~ "Second Trimester",
  bfa_exit$num_of_weeks_pregnant_an_book >= 28 ~ "Third Trimester",
  TRUE ~ NA)



bfa_exit$total_hf_fees <- case_when(
  bfa_exit$total_hf_fees %in% c(9998, 99999) ~ NA,
  TRUE ~ bfa_exit$total_hf_fees
)

bfa_exit$num_prev_anc_visits <- case_when(
  bfa_exit$num_prev_anc_visits %in% c(9994, 9999) ~ NA,
  TRUE ~ bfa_exit$num_prev_anc_visits
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






## 12 is the minimum age in the dataset
 bfa_exit$patient_age_centered <- bfa_exit$patient_age - 12




## num_prev_anc_visits includes this one; so 0 is a mistake and the smallest
## possible value is 1.
bfa_exit$first_anc <- case_when(
  bfa_exit$num_prev_anc_visits > 0 ~ "no",
  bfa_exit$num_prev_anc_visits %in% c(0, 1) ~ "yes",

  ## If first ANC visit to this HF and no previous visits to any other HF, then
  ## this is the first ANC visit; this fills out 459 NAs
  is.na(bfa_exit$num_prev_anc_visits) &
    bfa_exit$first_anc_visit_at_this_hf %in% "yes" &
  bfa_exit$num_prev_anc_visits_other_hf == 0 ~ "yes",
  
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




