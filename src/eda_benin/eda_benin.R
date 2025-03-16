library(broom)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(glue)
library(orderly2)
library(purrr)

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")
orderly_dependency("process_benin", "latest", "benin_dco.rds")
orderly_dependency("process_benin", "latest", "benin_facility_survey_clinical.rds")

benin_dco <- readRDS("benin_dco.rds")
benin_hf <- readRDS("benin_facility_survey_clinical.rds")

#### EDA DCO

outfile_prefix <- "benin_consult_length_by"


out <- summary(benin_dco$consult_length) |> round(2) |> tidy()

p1 <- gghistogram(
    benin_dco, x = "consult_length", add = "mean", rug = TRUE, add_density = TRUE,
    binwidth = 5
  ) + geom_table_npc(data = out, label = list(out), npcx = 1, npcy = 0.7) +
  labs(title = "Consultation length (minutes) in Benin") +
  theme_manuscript() +
  xlab("Consultation length (minutes)") 

ggsave_manuscript("consult_length", p1)

########### by urban/rural
benin_dco$m0_milieu <- case_when(
  benin_dco$m0_milieu == 1 ~ "Urban",
  benin_dco$m0_milieu == 2 ~ "Rural",
  TRUE ~ NA_character_
)

p2 <- ggsummarystats(
  data = benin_dco[! is.na(benin_dco$m0_milieu),], x = "m0_milieu", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter") 
  

p2$main.plot <- p2$main.plot +
  stat_compare_means(label = "p.signif", comparisons = list(c("Urban", "Rural"))) +
  ylab("Consultation length (minutes)") +
  theme_manuscript()

ggsave_manuscript(glue("{outfile_prefix}_milieu"), p2, 9, 6)


########### by health zones
benin_dco$m_id2 <- factor(benin_dco$m_id2)

p3 <- ggsummarystats(
  data = benin_dco[!is.na(benin_dco$m_id2), ], x = "m_id2", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_manuscript()

ggsave_manuscript(glue("{outfile_prefix}_healthzone"), p3, 9, 6)

########### by first ANC
benin_dco$first_anc <- factor(benin_dco$first_anc)

p4 <- ggsummarystats(
  data = benin_dco, x = "first_anc", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p4$main.plot <- p4$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_manuscript() +
  xlab("First ANC visit (yes/no)")

ggsave_manuscript(glue("{outfile_prefix}_first_anc"), p4, 9, 6)

############# by trimester

p5 <- ggsummarystats(
  data = benin_dco, x = "trimester", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p5$main.plot <- p5$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_manuscript() 

ggsave_manuscript(glue("{outfile_prefix}_first_anc"), p5, 9, 6)

########### by time of day

benin_dco$hour_start <- factor(benin_dco$hour_start)

p6 <- ggsummarystats(
  data = benin_dco, x = "hour_start", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p6$main.plot <- p6$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_manuscript() +
  xlab("Consultation start hour")

ggsave_manuscript(glue("{outfile_prefix}_hour_start"), p6, 9, 6)

############
## Now number of steps in the consultation
## weight:info_complications are common for all consultations
## If info_complications is yes, then info_placenta_previa: info_embolism are rel## evant
## patients_age_reported:mosq_kit_prescribed for first trimester ANC
## uterus_measured2:fansidar_prescribed for second trimester ANC
## uterus_measured3:sp_ensured for third trimester ANC

first_trimester <- filter(benin_dco, trimester %in% "premier trimestre") |>
  select(
    consult_length, hour_start:info_embolism,
    patients_age_reported:mosq_kit_prescribed
  )

first_trimester_steps <- select(first_trimester, weight:mosq_kit_prescribed) |>
  colSums(na.rm = TRUE) |>
  tidy()
first_trimester_steps$prop <- first_trimester_steps$x / nrow(first_trimester)

first_trimester_steps$names <- factor(
  first_trimester_steps$names,
  levels = first_trimester_steps$names, ordered = TRUE
)


second_trimester <- filter(benin_dco, trimester %in% "deuxieme trimestre") |>
  select(
    consult_length, hour_start:info_embolism,
    uterus_measured2:fansidar_prescribed
  )

second_trimester_steps <- select(
  second_trimester, weight:fansidar_prescribed
) |>
  colSums(na.rm = TRUE) |>
  tidy()

second_trimester_steps$prop <- second_trimester_steps$x /
  nrow(second_trimester)

second_trimester_steps$names <- factor(
  second_trimester_steps$names,
  levels = second_trimester_steps$names, ordered = TRUE
)


third_trimester <- filter(benin_dco, trimester %in% "troisieme trimestre") |>
  select(
    consult_length, hour_start:info_embolism, uterus_measured3:sp_ensured
  )


third_trimester_steps <- select(
  third_trimester, weight:sp_ensured
) |>
  colSums(na.rm = TRUE) |>
  tidy()

third_trimester_steps$prop <- third_trimester_steps$x /
  nrow(third_trimester)

third_trimester_steps$names <- factor(
  third_trimester_steps$names,
  levels = third_trimester_steps$names, ordered = TRUE
)

all_semesters <- rbind(
  mutate(first_trimester_steps, trimester = "First"),
  mutate(second_trimester_steps, trimester = "Second"),
  mutate(third_trimester_steps, trimester = "Third")
)

p <- ggplot(all_semesters) +
  geom_col(aes(x = names, y = prop)) +
  xlab("") +
  scale_x_discrete(limits = rev) +
  facet_wrap(~trimester, ncol = 3) +
  ylab("Proportion of consultations with step recorded") +
  labs(title = "Proportion of steps taken in ANC") +
  coord_flip() +
  theme_manuscript()

ggsave_manuscript("steps_proportion_second", p)



p <- ggplot(third_trimester_steps) +
  geom_col(aes(x = names, y = prop)) +
  xlab("") +
  ylab("Proportion of consultations with step recorded") +
  labs(title = "Proportion of steps taken in ANC") +
  coord_flip() +
  theme_manuscript()

ggsave_manuscript("steps_proportion_third", p)






## Now we count the number of steps recorded as having been taken


nsteps <- map_dfr(
  list(first = first_trimester, second = second_trimester, third = third_trimester),
  function(x) {
  out <- select(x, weight:info_complications) |>
    colSums(na.rm = TRUE) |>
    tidy()
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
    "info_postpartum_hemorrhage", "info_retained_placenta",
    "info_uterus_inverted",
    "info_ectopic_pregnancy", "info_hemorrhage_antepartum",
    "info_prolonged_labor_latent",
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

########### by facility
benin_dco$m_id1 <- factor(benin_dco$m_id1)
## Retain facilities with at least 30 consultations
facility_counts <- count(benin_dco, m_id1)
morethan10 <- facility_counts$n >= 30
benin_subset <- benin_dco[benin_dco$m_id1 %in% facility_counts$m_id1[morethan10], ]

p3 <- ggsummarystats(
  data = benin_subset, x = "m_id1", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_manuscript() +
  xlab("Facility Id")

ggsave_manuscript(glue("{outfile_prefix}_facility"), p3, 9, 6)

########### by facility
benin_dco$m_id1 <- factor(benin_dco$m_id1)
## Retain facilities with at least 30 consultations
facility_counts <- count(benin_dco, m_id1)

p3 <- ggsummarystats(
  data = benin_dco, x = "m_id1", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

ggsave_manuscript(glue("{outfile_prefix}_facility_all"), p3, 9, 6)

morethanx <- facility_counts$n >= 10
benin_hf$f_id1 <- factor(benin_hf$f_id1)
benin_subset <- left_join(benin_dco, benin_hf, by = c("m_id1" = "f_id1"))
x <- benin_subset[benin_subset$m_id1 %in% facility_counts$m_id1[morethanx], ]
y <- group_by(x, m_id1) |>
  summarise(med = median(consult_length)) |>
  arrange(med)

x$m_id1 <- factor(x$m_id1, levels = y$m_id1, ordered = TRUE)

p3 <- ggsummarystats(
  data = x, x = "m_id1", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter", color = "catchment_pop_binned"
)

p3$main.plot <- p3$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_manuscript() +
  xlab("Facility Id")

ggsave_manuscript(glue("{outfile_prefix}_facility"), p3, 9, 6)


p3 <- ggsummarystats(
  data = x, x = "catchment_pop_binned", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  ylab("Consultation length (minutes)") +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  


## Explore the characteristics of the facilities
##benin_hf <- benin_hf[benin_hf$f_id1 %in% facility_counts$m_id1[morethan10], ]

p3 <- ggsummarystats(
  data = benin_subset, x = "number_csec_trained_personnel", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  stat_compare_means(
    label = "p.signif", comparisons = list(c(1, 2), c(1, 3), c(2, 3)), label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_manuscript() +
  xlab("Do women in labour pay for equipment?")
