library(broom)
library(dplyr)
library(gghalves)
library(ggpmisc)
library(glue)
library(orderly2)
library(purrr)
library(tidyr)
library(zip)

dir.create("figures")

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency("process_benin", "latest", "benin_dco.rds")
benin_dco <- readRDS("benin_dco.rds")

## Relation between patient to staff ratio and D&N/M per 10k
benin_dco$patient_to_staff_ratio <-
  benin_dco$total_attendance_last_year /
    benin_dco$doctor_or_nursing_and_midwifery 


p <- ggplot(
  benin_dco,
  aes(doctor_or_nursing_and_midwifery_per_10000,
    patient_to_staff_ratio,
    col = facility_level_mapping
  )) +
  geom_point() +
  xlab("D&N/M per 10,000 population") +
  ylab("Patients per staff per year") +
  theme_manuscript()

ggsave_manuscript(
  "figures/benin_dco_patient_to_staff_ratio_vs_dn_per_10k",
  p,
  width = 12,
  height = 8
)

p <- ggplot(
  benin_dco,
  aes(doctor_or_nursing_and_midwifery_per_10000,
    catchment_pop,
    col = facility_level_mapping
  )
) +
  geom_point() +
  scale_x_log10(guide = "axis_logticks") +
  scale_y_log10(guide = "axis_logticks") +
  xlab("D&N/M per 10,000 population") +
  ylab("Catchment population") +
  theme_manuscript()


ggsave_manuscript(
  "figures/benin_dco_patient_to_staff_ratio_vs_dn_per_10k_vs_catchment_pop",
  p,
  width = 12,
  height = 8
)


orderly_dependency("process_drc", "latest", "drc_dco_2015_augmented.rds")
drc_baseline_dco <- readRDS("drc_dco_2015_augmented.rds")
