library(dplyr)
library(ggplot2)
library(orderly2)
library(snakecase)

orderly_shared_resource("utils.R")
source("utils.R")

deps1 <- orderly_dependency(
  "plot_lr_bfa",
  "latest(parameter:survey == 'baseline')",
  files = c(
    "bfa_baseline_hdi.rds" = "bfa_dco_bayes_hdi.rds",
    "bfa_baseline_map.rds" = "bfa_dco_bayes_map.rds",
    "bfa_baseline_pd.rds" = "bfa_dco_bayes_pd.rds"
  )
)


deps2 <- orderly_dependency(
  "plot_lr_bfa",
  "latest(parameter:survey == 'endline')",
  files = c(
    "bfa_endline_hdi.rds" = "bfa_dco_bayes_hdi.rds",
    "bfa_endline_map.rds" = "bfa_dco_bayes_map.rds",
    "bfa_endline_pd.rds" = "bfa_dco_bayes_pd.rds"
  )
)


deps3 <- orderly_dependency(
  "plot_lr_drc",
  "latest(parameter:survey == 'baseline')",
  files = c(
    "drc_baseline_hdi.rds" = "drc_dco_bayes_hdi.rds",
    "drc_baseline_map.rds" = "drc_dco_bayes_map.rds",
    "drc_baseline_pd.rds" = "drc_dco_bayes_pd.rds"
  )
)


deps4 <- orderly_dependency(
  "plot_lr_drc",
  "latest(parameter:survey == 'endline')",
  files = c(
    "drc_endline_hdi.rds" = "drc_dco_bayes_hdi.rds",
    "drc_endline_map.rds" = "drc_dco_bayes_map.rds",
    "drc_endline_pd.rds" = "drc_dco_bayes_pd.rds"
  )
)

deps5 <- orderly_dependency(
  "plot_lr_benin",
  "latest",
  files = c(
    "benin_hdi.rds" = "benin_dco_bayes_hdi.rds",
    "benin_map.rds" = "benin_dco_bayes_map.rds",
    "benin_pd.rds" = "benin_dco_bayes_pd.rds"
  )
)

deps <- list(deps1, deps2, deps3, deps4, deps5)
lapply(
  deps,
  function(dep) {
    lapply(
      dep$files$here,
      function(f) {
        assign(tools::file_path_sans_ext(basename(f)), readRDS(f), envir = .GlobalEnv)
      }
    )
  }
)

all_hdi <- bind_rows(
  `BFA (2013)` = bfa_baseline_hdi,
  `BFA (2017)` = bfa_endline_hdi,
  `DRC (2015)` = drc_baseline_hdi,
  `DRC (2021)` = drc_endline_hdi,
  `Benin (2010)` = benin_hdi,
  .id = "country"
)
all_map <- bind_rows(
  `BFA (2013)` = bfa_baseline_map,
  `BFA (2017)` = bfa_endline_map,
  `DRC (2015)` = drc_baseline_map,
  `DRC (2021)` = drc_endline_map,
  `Benin (2010)` = benin_map,
  .id = "country"
)

all_pd <- bind_rows(
  `BFA (2013)` = bfa_baseline_pd,
  `BFA (2017)` = bfa_endline_pd,
  `DRC (2015)` = drc_baseline_pd,
  `DRC (2021)` = drc_endline_pd,
  `Benin (2010)` = benin_pd,
  .id = "country"
)

vars <- c(
  "consult_length_scaled",
  "patients_per_staff_per_year_scaled",
  "anc_visits_last_year_scaled",
  "total_births_last_year_scaled",
  "anc_contacts_last_week_scaled",
  "anc_contacts_last_month_scaled",
  "day_of_visitWeekend",
  "total_attendance_2009_scaled",
  "pregnant_women_last_year_scaled",
  "num_personnel_scaled",
  "num_csps_in_district_scaled",
  "doctor_or_nursing_and_midwifery_scaled",
  "time_elapsed_since_start_of_day"
)

zpd <-
  filter(all_pd, rowname %in% vars)

zpd$first_anc <- case_when(
  zpd$first_anc == "first_anc" ~ "First ANC",
  zpd$first_anc == "follow_up_anc" ~ "Follow-up ANC"
)

zpd$trimester <- case_when(
  zpd$trimester == "first_trimester" ~ "First Trimester",
  zpd$trimester == "second_trimester" ~ "Second Trimester",
  zpd$trimester == "third_trimester" ~ "Third Trimester"
)

zpd$datacut <- paste0(
  zpd$first_anc, "/", zpd$trimester
)

p <- ggplot(zpd, aes(datacut, rowname, fill = `Post.Prob`)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "firebrick", mid = "white", high = "steelblue",
    midpoint = 0.5, limits = c(0, 1), name = "Pr(β > 0)"
  )  +
  facet_wrap(~country, nrow = 1) +
  coord_fixed(ratio = 1) +
  ggtitle("Posterior probabilities that coefficients > 0") +
  theme_manuscript() +
  theme(axis.title = element_blank()) 


## Fix the x-axis labels
p <- p +
  scale_x_discrete(
    limits = c(
      "First ANC/First Trimester",
      "Follow-up ANC/First Trimester",
      "First ANC/Second Trimester",
      "Follow-up ANC/Second Trimester",
      "First ANC/Third Trimester",
      "Follow-up ANC/Third Trimester"
  )) +
  theme(
    axis.text.x = element_text(
      angle = 90, hjust = 1, vjust = 0.5
    )
  ) 

## Fix y-axis
p <- p +
  scale_y_discrete(
    labels = c(
      "anc_visits_last_year_scaled" = "ANC visits last year",
      "day_of_visitWeekend" = "Day of visit: Weekend",
      "num_csps_in_district_scaled" = "CSPS in district",
      "num_personnel_scaled" = "Number of personnel",
      "pregnant_women_last_year_scaled" = "Pregnant women last year",
      "patients_per_staff_per_year_scaled" = "Patients per staff per year",
      "total_births_last_year_scaled" = "Total births last year",
      "doctor_or_nursing_and_midwifery_scaled" = "Doctors/N&M per 10k",
      "time_elapsed_since_start_of_day" = "Time elapsed since 6AM"
    )
  ) 

## Fix legend
p <- p +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

ggsave_manuscript(
  "figures/p_beta_positive_other_busyness_vars.png",
  p,
  width = 12,
  height = 6
)
