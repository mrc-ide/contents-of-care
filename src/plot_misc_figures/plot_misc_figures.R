library(broom)
library(dplyr)
library(ggforce)
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
  facet_zoom(xlim = c(0, 20)) +
  ggtitle("Benin: Patient to staff ratio vs D&N/M per 10k") +
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
  aes(
    catchment_pop,
    doctor_or_nursing_and_midwifery_per_10000,
    col = facility_level_mapping
  )
) +
  geom_point() +
  scale_x_log10(guide = "axis_logticks") +
  scale_y_log10(guide = "axis_logticks") +
  ggtitle("Benin: D&N/M per 10k vs Catchment population") +
  ylab("D&N/M per 10,000 population") +
  xlab("Catchment population") +
  theme_manuscript()


ggsave_manuscript(
  "figures/benin_dco_patient_to_staff_ratio_vs_dn_per_10k_vs_catchment_pop",
  p,
  width = 12,
  height = 8
)


orderly_dependency("process_drc", "latest", "drc_dco_2015_augmented.rds")
drc_baseline_dco <- readRDS("drc_dco_2015_augmented.rds")
drc_baseline_dco <- filter(drc_baseline_dco, total_attendance_last_year > 0)

drc_baseline_dco$patient_to_staff_ratio <-
  drc_baseline_dco$total_attendance_last_year /
  (drc_baseline_dco$doctor_or_nursing_and_midwifery)

p <- ggplot(
  drc_baseline_dco,
  aes(doctor_or_nursing_and_midwifery_scaled,
    patient_to_staff_ratio,
    col = facility_level_mapping
  )
) +
  geom_point() +
  ggtitle("DRC: Patient to staff ratio vs D&N/M per 10k") +
  xlab("D&N/M per 10,000 population") +
  ylab("Patients per staff per year") +
  facet_zoom(xlim = c(0, 20)) +
  theme_manuscript()


ggsave_manuscript(
  "figures/drc_dco_patient_to_staff_ratio_vs_dn_per_10k",
  p,
  width = 12,
  height = 8
)



orderly_dependency("process_burkina_faso", "latest", "bfa_baseline_dco.rds")
bfa_baseline_dco <- readRDS("bfa_baseline_dco.rds")
bfa_baseline_dco <- filter(bfa_baseline_dco, total_attendance_last_year > 0)
bfa_baseline_dco$patient_to_staff_ratio <-
  bfa_baseline_dco$total_attendance_last_year /
    (bfa_baseline_dco$doctor_or_nursing_and_midwifery)

p <- ggplot(
  bfa_baseline_dco,
  aes(doctor_or_nursing_and_midwifery_per_10000,
    patient_to_staff_ratio,
    col = `facility_level_mapping.x`
  )
) +
  geom_point() +
  facet_zoom(xlim = c(0, 20)) +
  ggtitle("BFA Baseline: Patient to staff ratio vs D&N/M per 10k") +
  xlab("D&N/M per 10,000 population") +
  ylab("Patients per staff per year") +
  theme_manuscript()


ggsave_manuscript(
  "figures/bfa_dco_patient_to_staff_ratio_vs_dn_per_10k",
  p,
  width = 12,
  height = 8
)


p <- ggplot(
  bfa_baseline_dco,
  aes(
    catchment_pop,
    doctor_or_nursing_and_midwifery_per_10000,
    col = `facility_level_mapping.x`
  )
) +
  geom_point() +
  scale_x_log10(guide = "axis_logticks") +
  scale_y_log10(guide = "axis_logticks") +
  ggtitle("BFA Baseline: D&N/M per 10k vs Catchment population") +
  ylab("D&N/M per 10,000 population") +
  xlab("Catchment population") +
  theme_manuscript()

ggsave_manuscript(
  "figures/bfa_dco_patient_to_staff_ratio_vs_dn_per_10k_vs_catchment_pop",
  p,
  width = 12,
  height = 8
)


orderly_dependency(
  "process_burkina_faso_endline", "latest", "bfa_endline_dco.rds"
)

bfa_endline_dco <- readRDS("bfa_endline_dco.rds")
bfa_endline_dco <- filter(bfa_endline_dco, total_attendance_last_year > 0)
bfa_endline_dco$patient_to_staff_ratio <-
  bfa_endline_dco$total_attendance_last_year /
    (bfa_endline_dco$doctor_or_nursing_and_midwifery)

p <- ggplot(
  bfa_endline_dco,
  aes(doctor_or_nursing_and_midwifery_per_10000,
    patient_to_staff_ratio,
    col = `facility_level_mapping.x`
  )) +
  geom_point() +
  facet_zoom(xlim = c(0, 20)) +
  ggtitle("BFA: Patient to staff ratio vs D&N/M per 10k") +
  xlab("D&N/M per 10,000 population") +
  ylab("Patients per staff per year") +
  theme_manuscript()

ggsave_manuscript(
  "figures/bfa_endline_dco_patient_to_staff_ratio_vs_dn_per_10k",
  p,
  width = 12,
  height = 8
)

p <- ggplot(
  bfa_endline_dco,
  aes(
    catchment_pop,
    doctor_or_nursing_and_midwifery_per_10000,
    col = `facility_level_mapping.x`
  )
) +
  geom_point() +
  scale_x_log10(guide = "axis_logticks") +
  scale_y_log10(guide = "axis_logticks") +
  ggtitle("BFA: D&N/M per 10k vs Catchment population") +
  ylab("D&N/M per 10,000 population") +
  xlab("Catchment population") +
  theme_manuscript()

ggsave_manuscript(
  "figures/bfa_endline_dco_patient_to_staff_ratio_vs_dn_per_10k_vs_catchment_pop",
  p,
  width = 12,
  height = 8
)
zip::zip(
  "figures_patient_to_staff_ratio_vs_dn_per_10k.zip",
  list.files("figures", full.names = TRUE)
)

orderly_artefact(
  files = "figures_patient_to_staff_ratio_vs_dn_per_10k.zip",
  description = "Figures for patient to staff ratio vs D&N/M per 10k"
)
