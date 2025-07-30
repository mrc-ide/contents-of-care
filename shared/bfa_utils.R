calculate_consult_time <- function(x, start_col = "consult_start",
                                   end_col = "consult_end") {
  time_vec <- x[[start_col]]
  time_str <- sprintf("%04d", time_vec)
  x$consult_start_formatted <- hm(
    paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
  )

  time_vec <- x[[end_col]]
  time_str <- sprintf("%04d", time_vec)
  x$consult_end_formatted <- hm(
    paste0(substr(time_str, 1, 2), ":", substr(time_str, 3, 4))
  )

  ## start_of_day is defined in utils.R
  x$time_elapsed_since_start_of_day <-
    time_length(x$consult_start_formatted - start_of_day, unit = "hour")

  x$consult_length_calc <-
    time_length(
      x$consult_end_formatted - x$consult_start_formatted,
      unit = "minute"
    )

  x
}

swap_start_end_times <- function(x, start_col = "consult_start",
                                 end_col = "consult_end") {
  idx <- which(x$consult_length_calc < 0)
  cli_warn(
    "Swapping start and end times for {length(idx)} consultations with negative duration."
  )
  tmp <- x$consult_start[idx]
  x$consult_start[idx] <- x$consult_end[idx]
  x$consult_end[idx] <- tmp
  x
}

fix_bfa_data_errors <- function(x) {

  ## 99 indicates missing data
  x$consult_start[x$consult_start %in% 99] <- NA
  x$consult_end[x$consult_end %in% 99] <- NA
  
  ## Fix obvious data entry errors
  ## Not sure what this is meant to be; end time is 1100
  idx <- x$consult_start %in% 3
  cli_warn("Consult start time is 3 for {sum(idx)} enteries, setting to NA")
  x$consult_start[idx] <- NA

  ## end time is 1143; so 1105?
  idx <- x$consult_start %in% 115
  x$consult_start[idx] <- 1105


  ## end time is 1809; so 1758 instead of 758?
  idx <- x$consult_start %in% 758
  x$consult_start[idx] <- 1758

  idx <- x$consult_start %in% 12
  x$consult_start[idx] <- 1200

  idx <- x$consult_start %in% 8
  x$consult_start[idx] <- 0800

  idx <- x$consult_start %in% 9
  x$consult_start[idx] <- 0900

  idx <- x$consult_start %in% 10
  x$consult_start[idx] <- 1000

  idx <- x$consult_end %in% 10
  x$consult_end[idx] <- 1000


  idx <- x$consult_end %in% 100
  x$consult_end[idx] <- 1000

  ## This could be 1002 or 1020.
  idx <- x$consult_end %in% 102
  x$consult_end[idx] <- NA

  idx <- x$consult_end %in% 115
  x$consult_end[idx] <- 1315 ## start time is 1143

  idx <- x$consult_start %in% 11
  x$consult_start[idx] <- 1100

  idx <- x$consult_end %in% 11
  x$consult_end[idx] <- 1100

  idx <- x$consult_end %in% 12
  x$consult_end[idx] <- 1200

  idx <- x$consult_start %in% 13
  x$consult_start[idx] <- 1300

  idx <- x$consult_end %in% 13
  x$consult_end[idx] <- 1300


  idx <- x$consult_start %in% 14
  x$consult_start[idx] <- 1400

  idx <- x$consult_end %in% 14
  x$consult_end[idx] <- 1400


  ## End time of this one is 1025; so probably start time is 1015
  ## But we cant be sure
  idx <- x$consult_start %in% 15
  x$consult_start[idx] <- NA


  idx <- x$consult_end %in% 15
  x$consult_end[idx] <- NA

  idx <- x$consult_start %in% c(3, 4, 32, 45, 98, 246, 295)
  x$consult_start[idx] <- NA


  idx <- x$consult_end %in% c(3, 4, 32, 45, 98)
  x$consult_end[idx] <- NA

  ## End times are 956 and 950; so unsure what 102 could be
  idx <- x$consult_start %in% 102
  x$consult_start[idx] <- NA

  idx <- x$consult_start %in% 830
  x$consult_start[idx] <- 0830

  idx <- x$consult_start %in% 8030
  x$consult_start[idx] <- 0830
  x$consult_end[idx] <- 0945

  idx <- x$consult_start %in% 9025
  x$consult_start[idx] <- 0925
  x$consult_end[idx] <- 0940

  idx <- x$consult_start %in% 9048
  x$consult_start[idx] <- 0948
  x$consult_end[idx] <- 0958

  idx <- x$consult_end %in% 9000
  x$consult_start[idx] <- 0853
  x$consult_end[idx] <- 0900

  ## Start time here is 710, but unclear what the end time is from 7320
  idx <- x$consult_end %in% 7320
  x$consult_start[idx] <- 0710
  x$consult_end[idx] <- NA

  idx <- x$consult_end %in% 9052
  x$consult_start[idx] <- 0941
  x$consult_end[idx] <- 0952

  idx <- x$consult_end %in% 9172
  x$consult_start[idx] <- 0905
  x$consult_end[idx] <- NA

  idx <- x$consult_end %in% 9998
  x$consult_start[idx] <- NA
  x$consult_end[idx] <- NA

  idx <- x$consult_end %in% 813 & x$consult_start %in% 1758
  cli_warn("Consult start time is 813 for {sum(idx)} enteries, setting to NA")
  x$consult_start[idx] <- NA
  x$consult_end[idx] <- NA

  x
}

recode_bfa_vars <- function(x) {
  x$region_name <- case_when(
    x$REGION == 1 ~ "Boucle du Mouhoun",
    x$REGION == 2 ~ "Centre-est",
    x$REGION == 3 ~ "Centre-nord",
    x$REGION == 4 ~ "Centre-ouest",
    x$REGION == 5 ~ "Nord",
    x$REGION == 6 ~ "Sud-ouest",
    TRUE ~ NA_character_
  )

  x$facility_level_name <- case_when(
    x$facility_level == 1 ~ "Regional hospital",
    x$facility_level == 2 ~ "District hospital",
    x$facility_level == 3 ~ "Medical center",
    x$facility_level == 4 ~ "CSPS",
    x$facility_level == 5 ~ "Dispensary or maternity Unit",
    x$facility_level == 6 ~ "Private clinic",
    x$facility_level == 7 ~ "Private religious health facility",
    TRUE ~ NA_character_
  )

  x$facility_level_mapping <- case_when(
    x$facility_level_name %in% c("CSPS") ~ "Primary",
    x$facility_level %in% c("Medical center", "District hospital") ~
      "Secondary",
    TRUE ~ "Other"
  )

  x$milieu_of_residence <- case_when(
    x$milieu_of_residence == 1 ~ "Urban",
    x$milieu_of_residence == 2 ~ "Rural",
    TRUE ~ NA_character_
  )

  x$first_anc <- ifelse(
    x$num_prev_anc_visits %in% 0, "First ANC", "Follow-up ANC"
  )

  x$pregnancy_week <- ifelse(
    x$pregnancy_week %in% c(98, 99), NA, x$pregnancy_week
  )

  x$hcw_sex <- case_when(
    x$hcw_sex %in% 1 ~ "Male",
    x$hcw_sex %in% 2 ~ "Female",
    TRUE ~ NA_character_
  )

  x$trimester <- case_when(
    x$pregnancy_week < 13 ~ "First Trimester",
    x$pregnancy_week >= 13 & x$pregnancy_week < 28 ~ "Second Trimester",
    TRUE ~ "Third Trimester"
  )

  x$hcw_qualification <- case_when(
    x$hcw_qualification %in% 1 ~ "Doctor",
    x$hcw_qualification %in% c(2, 4) ~ "Nurse",
    x$hcw_qualification %in% c(5, 6, 7) ~ "Midwife",
    x$hcw_qualification %in% 8 ~ "CHCW",
    TRUE ~ "Other"
  )

  x$consult_language <- case_when(
    x$consult_language %in% 1 ~ "French",
    x$consult_language %in% 2 ~ "Moore",
    x$consult_language %in% 3 ~ "Dioula",
    x$consult_language %in% c(4:10) ~ "Other",
    x$consult_language %in% 97 ~ "Unknown",
    is.na(x$consult_language) ~"Unknown"
  )

  x$first_pregnancy <- case_when(
    x$first_pregnancy %in% 1 ~ "Yes",
    x$first_pregnancy %in% 2 ~ "No",
    TRUE ~ NA_character_
  )

  x
}

