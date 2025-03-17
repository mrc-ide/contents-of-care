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
benin_dco <- readRDS("benin_dco.rds")

outfile_prefix <- "benin_consult_length_by"

#### Overall
out <- summary(benin_dco$consult_length) |> round(2) |> tidy()

p1 <- gghistogram(
    benin_dco, x = "consult_length", add = "mean", rug = TRUE, add_density = TRUE,
    binwidth = 5
  ) + geom_table_npc(data = out, label = list(out), npcx = 1, npcy = 0.7) +
  labs(title = "Consultation length (minutes) in Benin") +
  theme_pubr() +
  xlab("Consultation length (minutes)") 

outfile <- glue("{outfile_prefix}_overall.pdf")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(description = "Consultation length in Benin", files = outfile)

## Atrributes of the
### health facility:
###### m_id1 (facility id)
###### m0_milieu (urban/rural)
###### m_id2 (health zone)
###### facility_type
###### facility_status
###### catchment_pop_binned
###### pregnant_women_private_space
###### number_of_maternal_deaths
###### number_of_births_2009
###### number_of_beds_for_delivery
###### women_in_labour_pay
###### n_hcw
### patient: first_anc, trimester
### hcw: m0_id8 (qualification)
### consultation: hour_start


########### by health facility
benin_dco$m_id1 <- factor(benin_dco$m_id1)
out <- group_by(benin_dco, m_id1) |>
  summarise(med = median(consult_length)) |>
  arrange(med)
benin_dco$m_id1 <- factor(benin_dco$m_id1, levels = out$m_id1, ordered = TRUE)

p1 <- ggsummarystats(
  data = benin_dco, x = "m_id1", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p1$main.plot <- p1$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("Facility Id")

outfile <- glue("{outfile_prefix}_facility.pdf")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(
  description = "Consultation length by facility", files = outfile
)

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
  theme_pubr() 

outfile <- glue("{outfile_prefix}_milieu.pdf")
ggexport(
  plotlist = list(p2), filename = glue("{outfile_prefix}_milieu.pdf")
)
orderly_artefact(
  description = "Consultation length by urban/rural", files = outfile
)


########### by health zone
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
  xlab("Health zone") +
  theme_pubr()

outfile <- glue("{outfile_prefix}_healthzone.pdf")
ggexport(plotlist = list(p3), filename = outfile)
orderly_artefact(
  description = "Consultation length by health zone", files = outfile
)

########### by facility type
p3 <- ggsummarystats(
  data = benin_dco, x = "facility_type", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75,
    comparisons = list(c(1, 2), c(1, 3), c(1, 4), c(2, 3), c(2, 4), c(3, 4))
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  theme(axis.text.x = element_text(hjust = c(0.9, 0.7, 0.5, 0.4), size = 8))

outfile <- glue("{outfile_prefix}_facility_type.pdf")
ggexport(plotlist = list(p3), filename = outfile)
orderly_artefact(
  description = "Consultation length by facility type", files = outfile
)

p3 <- ggsummarystats(
  data = benin_dco, x = "facility_status", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75, comparisons = list(c(1, 2))
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("Facility status")

outfile <- glue("{outfile_prefix}_facility_status.pdf")
ggexport(plotlist = list(p3), filename = outfile)
orderly_artefact(
  description = "Consultation length by facility status", files = outfile
)
########### by catchment population

p3 <- ggsummarystats(
  data = benin_dco, x = "catchment_pop_binned", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

outfile <- glue("{outfile_prefix}_catchment_pop.pdf")
ggexport(plotlist = list(p3), filename = outfile)
orderly_artefact(
  description = "Consultation length by catchment population", files = outfile
)

########### by total attendance in 2009
p3 <- ggsummarystats(
  data = benin_dco, x = "total_attendance_2009_binned", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("Total attendance in 2009")

outfile <- glue("{outfile_prefix}_attendance.pdf")
ggexport(plotlist = list(p3), filename = outfile)
orderly_artefact(
  description = "Consultation length by total attendane in 2009", files = outfile
)
##### pregnant_women_private_space
benin_dco$pregnant_women_private_space <- factor(benin_dco$pregnant_women_private_space)

p3 <- ggsummarystats(
  data = benin_dco, x = "pregnant_women_private_space", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75, comparisons = list(c(1, 2))
  ) +
  scale_x_discrete(labels = c("No", "Yes"), breaks = c(2, 1)) +
  theme_pubr() +
  ylab("Consultation length (minutes)") +
  xlab("Do pregnant women have private space?")

########### by first ANC
benin_dco$first_anc <- factor(benin_dco$first_anc)

p4 <- ggsummarystats(
  data = benin_dco, x = "first_anc", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p4$main.plot <- p4$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75, comparisons = list(c(1, 2))
  ) +
  ylab("Consultation length (minutes)") +
  scale_x_discrete(labels = c("No", "Yes"), breaks = c(0, 1)) +
  theme_pubr() +
  xlab("First ANC visit (yes/no)")

outfile <- glue("{outfile_prefix}_first_anc.pdf")
ggexport(plotlist = list(p4), filename = outfile)
orderly_artefact(
  description = "Consultation length by first ANC visit", files = outfile
)

############# by trimester

p5 <- ggsummarystats(
  data = benin_dco, x = "trimester", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p5$main.plot <- p5$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75,
    comparisons = list(c("premier trimestre", "deuxieme trimestre"),
                       c("premier trimestre", "troisieme trimestre"),
                       c("deuxieme trimestre", "troisieme trimestre"))
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() 

outfile <- glue("{outfile_prefix}_trimester.pdf")
ggexport(plotlist = list(p5), filename = outfile)
orderly_artefact(
  description = "Consultation length by trimester", files = outfile
)

########### by time of day

benin_dco$hour_start <- factor(benin_dco$hour_start)

p6 <- ggsummarystats(
  data = benin_dco, x = "hour_start", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p6$main.plot <- p6$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75, ref.group = ".all."
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("Consultation start hour")

outfile <- glue("{outfile_prefix}_hour_start.pdf")
ggexport(plotlist = list(p6), filename = outfile)
orderly_artefact(
  description = "Consultation length by hour of day", files = outfile
)

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
  theme_pubr()

ggsave_manuscript("steps_proportion_second", p)



p <- ggplot(third_trimester_steps) +
  geom_col(aes(x = names, y = prop)) +
  xlab("") +
  ylab("Proportion of consultations with step recorded") +
  labs(title = "Proportion of steps taken in ANC") +
  coord_flip() +
  theme_pubr()

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
  theme_pubr()

ggsave_manuscript("steps_proportion", psteps)



p3 <- ggsummarystats(
  data = x, x = "catchment_pop_binned", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p3$main.plot <- p3$main.plot +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  


## Explore the characteristics of the facilities
##benin_hf <- benin_hf[benin_hf$f_id1 %in% facility_counts$m_id1[morethan10], ]

