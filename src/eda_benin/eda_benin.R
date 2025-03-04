library(dplyr)
library(ggplot2)
library(orderly2)
library(purrr)

theme_manuscript <- function() {
  theme_classic() +
    theme(
      ## text = element_text(family = "Arial", ),
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(size = 14),
    )
}

ggsave_manuscript <- function(
    filename,
    plot,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300) {
  ggsave(
    filename = paste0(filename, ".svg"), plot = plot,
    width = width, height = height, units = units
  )
  ggsave(
    filename = paste0(filename, ".png"), plot = plot, width = width,
    height = height, units = units, dpi = dpi
  )

  ggsave(
    filename = paste0(filename, ".pdf"), plot = plot, width = width,
    height = height, units = units, dpi = dpi
  )
}


orderly_dependency("process_benin", "latest","benin.rds")
benin <- readRDS("benin.rds")

p1 <- gghistogram(
  benin, x = "consult_length", add = "mean", rug = TRUE, add_density = TRUE,
  binwidth = 5) +
  labs(title = "Consultation length (minutes) in Benin") +
  theme_manuscript() +
  xlab("Consultation length (minutes)") 

ggsave_manuscript("consult_length", p1)

benin$first_anc <- factor(benin$first_anc)

p2 <- gghistogram(
  benin,
  x = "consult_length", add = "mean", rug = TRUE, add_density = TRUE,
  binwidth = 5, fill = "first_anc", palette = c("#00AFBB", "#E7B800"),
  color = NA
) +
  labs(title = "Consultation length (minutes) in Benin") +
  theme_manuscript() +
  xlab("Consultation length (minutes)")

ggsave_manuscript("consult_length_by_first_anc", p2)

p3 <- gghistogram(
  benin,
  x = "consult_length", add = "mean", rug = TRUE, add_density = TRUE,
  binwidth = 5, fill = "trimester",
  palette = c("#000000", "#E69F00", "#56B4E9"),
  color = NA
) +
  labs(title = "Consultation length (minutes) in Benin") +
  theme_manuscript() +
  xlab("Consultation length (minutes)")

ggsave_manuscript("consult_length_by_trimester", p3)

## Now number of steps in the consultation
## weight:info_complications are common for all consultations
## If info_complications is yes, then info_placenta_previa: info_embolism are relevant
## patients_age_reported:mosq_kit_prescribed for first trimester ANC
## uterus_measured2:fansidar_prescribed for second trimester ANC
## uterus_measured3:sp_ensured for third trimester ANC

first_trimester <- filter(benin, trimester %in% "premier trimestre") |>
  select(
    consult_length, hour_start:info_embolism,
    patients_age_reported:mosq_kit_prescribed
  )

second_trimester <- filter(benin, trimester %in% "deuxieme trimestre") |>
  select(
    consult_length, hour_start:info_embolism,
    uterus_measured2:fansidar_prescribed
  )

second_trimester_steps <- select(
  second_trimester, weight:fansidar_prescribed) |>
  colSums(na.rm = TRUE) |>
  broom::tidy()

second_trimester_steps$prop <- second_trimester_steps$x /
  nrow(second_trimester)

second_trimester_steps$names <- factor(
  second_trimester_steps$names,
  levels = second_trimester_steps$names, ordered = TRUE
)

p <- ggplot(second_trimester_steps) +
  geom_col(aes(x = names, y = prop)) +
  xlab("") +
  ylab("Proportion of consultations with step recorded") +
  labs(title = "Proportion of steps taken in ANC") +
  coord_flip() +
  theme_manuscript()

ggsave_manuscript("steps_proportion_second", p)


third_trimester <- filter(benin, trimester %in% "troisieme trimestre") |>
  select(
    consult_length, hour_start:info_embolism, uterus_measured3:sp_ensured
  )

third_trimester_steps <- select(
  third_trimester, weight:sp_ensured) |>
  colSums(na.rm = TRUE) |>
  broom::tidy()

third_trimester_steps$prop <- third_trimester_steps$x /
  nrow(third_trimester)

third_trimester_steps$names <- factor(
  third_trimester_steps$names,
  levels = third_trimester_steps$names, ordered = TRUE
)

p <- ggplot(third_trimester_steps) +
  geom_col(aes(x = names, y = prop)) +
  xlab("") +
  ylab("Proportion of consultations with step recorded") +
  labs(title = "Proportion of steps taken in ANC") +
  coord_flip() +
  theme_manuscript()

ggsave_manuscript("steps_proportion_third", p)






## Now we count the number of steps recorded as having been taken
first_trimester <- rowwise(first_trimester) |>
  mutate(nsteps = sum(c_across(weight:info_complications), na.rm = TRUE))


nsteps <- map_dfr(
  list(first = first_trimester, second = second_trimester, third = third_trimester),
  function(x) {
  out <- select(x, weight:info_complications) |>
    colSums(na.rm = TRUE) |>
    broom::tidy()
  out$prop <- out$x / nrow(x)
  out
  }, .id = "trimester")
## Order the steps chronologically
nsteps$names <- factor(
  nsteps$names,
  levels = c(
    "weight", "temperature", "inspection_of_mucous_membranes",
    "abdominal_palpation", "lower_limb_edema", "vaginal_exam", "speculum_exam",
    "blood_pressure", "albuminuria", "glucoserie", "us_requested",
    "info_complications", "info_placenta_previa", "info_premature_rupture",
    "info_postpartum_hemorrhage", "info_retained_placenta", "info_uterus_inverted",
    "info_ectopic_pregnancy", "info_hemorrhage_antepartum", "info_prolonged_labor_latent",
    "info_prolonged_labor_second", "info_cpd", "info_uterine_rupture",
    "info_abnormal_presentation", "info_infection", "info_uterine_perforation",
    "info_hypertensive_crises", "info_ecclampsia", "info_pre_eclampsia",
    "info_severe_anemia", "info_multiple_pregnancy", "info_embolism"
  ), ordered = TRUE
)

psteps <- ggplot(nsteps) +
  geom_col(aes(x = names, y = prop)) +
  facet_wrap(~trimester, ncol = 1) +
  scale_x_discrete(limits = rev) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), alpha = 0.2) +
  xlab("") + ylab("Proportion of consultations with step recorded") +
  labs(title = "Proportion of steps taken in ANC") +
  coord_flip() +
  theme_manuscript()

ggsave_manuscript("steps_proportion", psteps)


