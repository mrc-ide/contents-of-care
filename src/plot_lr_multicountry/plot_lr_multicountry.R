library(brms)
library(cli)
library(dplyr)
library(emmeans)
library(ggplot2)
library(orderly2)
library(purrr)
library(snakecase)
library(stringr)

dir.create("figures", showWarnings = FALSE)

orderly_shared_resource("utils.R")
source("utils.R")


orderly_dependency("lr_multicountry", "latest", "fits/")
infiles <- list.files("fits/")
fits <- map(infiles, ~ readRDS(file.path("fits", .x)))
names(fits) <- infiles

trimesters <- paste0(c("first", "second", "third"), "_trimester")
anc_types <- c("first_anc", "follow_up_anc")

##### 1. Consult length
marginal_preds <- imap_dfr(fits, function(fit, infile) {
  cli_inform("Processing {infile}")
  preds <- emmeans(
    fit, ~consult_length_scaled,
    at = list(consult_length_scaled = seq(-5, 5, 1)),
    type = "response"
  ) |> confint()

  preds$trimester <- str_extract(infile, trimesters, group = NULL) |>
    keep(~ !is.na(.))

  preds$anc <- str_extract(infile, anc_types, group = NULL) |>
    keep(~ !is.na(.)) 


  preds$intervention <-
    gsub(x = infile, pattern = "multicountry_lr_fit_", replacement = "") |>
    gsub(pattern = preds$trimester[1], replacement = "") |>
    gsub(pattern = preds$anc[1], replacement = "") |>
    gsub(pattern = "_rds", replacement = "") |>
    to_sentence_case()

  preds
  
})

marginal_preds$anc <- recode_factor(
  marginal_preds$anc,
  first_anc = "First ANC",
  follow_up_anc = "Follow-up ANC"
)

p <- ggplot(marginal_preds) +
  geom_line(aes(x = consult_length_scaled, y = prob, col = anc)) +
  geom_ribbon(
    aes(x = consult_length_scaled, ymin = `lower.HPD`, ymax = `upper.HPD`, fill = anc),
    alpha = 0.2
  ) +
  facet_grid(
    intervention ~ trimester,
    labeller = labeller(
      trimester = to_title_case,
      intervention = c(
        "Continuity of care" = "Continuity",
        "Informational interventions" = "Information",
        "Maternal and fetal assessment" = "Assessment",
        "Patient hcw interaction" = "Interaction",
        "Preventive measures" = "Prevention"
      )
    )
  ) +
  theme_manuscript()
p <- p +
  ylab("Proportion of completed steps") +
  xlab("Consultation length (scaled)") 

outfile <- "figures/marginal_consult_length"

ggsave_manuscript(outfile, p, width = 12, height = 8)
  


### 2. Doctor and N&M

marginal_preds <- imap_dfr(fits, function(fit, infile) {
  cli_inform("Processing {infile}")
  preds <- emmeans(
    fit, ~doctor_or_nursing_and_midwifery_scaled,
    at = list(doctor_or_nursing_and_midwifery_scaled = seq(-0.5, 5, 0.5)),
    type = "response"
  ) |> confint()

  preds$trimester <- str_extract(infile, trimesters, group = NULL) |>
    keep(~ !is.na(.))

  preds$anc <- str_extract(infile, anc_types, group = NULL) |>
    keep(~ !is.na(.))

  preds$intervention <-
    gsub(x = infile, pattern = "multicountry_lr_fit_", replacement = "") |>
    gsub(pattern = preds$trimester[1], replacement = "") |>
    gsub(pattern = preds$anc[1], replacement = "") |>
    gsub(pattern = "_rds", replacement = "") |>
    to_sentence_case()

  preds
  
})


marginal_preds$anc <- recode_factor(
  marginal_preds$anc,
  first_anc = "First ANC",
  follow_up_anc = "Follow-up ANC"
)

p <- ggplot(marginal_preds) +
  geom_line(aes(x = doctor_or_nursing_and_midwifery_scaled, y = prob, col = anc)) +
  geom_ribbon(
    aes(x = doctor_or_nursing_and_midwifery_scaled,
        ymin = `lower.HPD`, ymax = `upper.HPD`, fill = anc),
    alpha = 0.2
  ) +
  scale_x_continuous(
    breaks = seq(-0.5, 5, 1)
  ) +
  facet_grid(
    intervention ~ trimester,
    labeller = labeller(
      trimester = to_title_case,
      intervention = c(
        "Continuity of care" = "Continuity",
        "Informational interventions" = "Information",
        "Maternal and fetal assessment" = "Assessment",
        "Patient hcw interaction" = "Interaction",
        "Preventive measures" = "Prevention"
      )
    )
  ) +
  theme_manuscript()
p <- p +
  ylab("Proportion of completed steps") +
  xlab("Doctor/N&M per 10,000 population (scaled)") 

outfile <- "figures/marginal_doctor_nursing_midwifery"

ggsave_manuscript(outfile, p, width = 12, height = 8)


### 3. Time elapsed
breaks <- seq(-6, 6, 1)


marginal_preds <- imap_dfr(fits, function(fit, infile) {
  cli_inform("Processing {infile}")
  preds <- emmeans(
    fit, ~time_elapsed_since_start_of_day,
    at = list(time_elapsed_since_start_of_day = breaks),
    type = "response"
  ) |> confint()

  preds$trimester <- str_extract(infile, trimesters, group = NULL) |>
    keep(~ !is.na(.))

  preds$anc <- str_extract(infile, anc_types, group = NULL) |>
    keep(~ !is.na(.))

  preds$intervention <-
    gsub(x = infile, pattern = "multicountry_lr_fit_", replacement = "") |>
    gsub(pattern = preds$trimester[1], replacement = "") |>
    gsub(pattern = preds$anc[1], replacement = "") |>
    gsub(pattern = "_rds", replacement = "") |>
    to_sentence_case()

  preds
  
})


marginal_preds$anc <- recode_factor(
  marginal_preds$anc,
  first_anc = "First ANC",
  follow_up_anc = "Follow-up ANC"
)

p <- ggplot(marginal_preds) +
  geom_line(aes(x = time_elapsed_since_start_of_day, y = prob, col = anc)) +
  geom_ribbon(
    aes(
      x = time_elapsed_since_start_of_day,
      ymin = `lower.HPD`, ymax = `upper.HPD`, fill = anc
    ),
    alpha = 0.2
  ) +
  facet_grid(
    intervention ~ trimester,
    labeller = labeller(
      trimester = to_title_case,
      intervention = c(
        "Continuity of care" = "Continuity",
        "Informational interventions" = "Information",
        "Maternal and fetal assessment" = "Assessment",
        "Patient hcw interaction" = "Interaction",
        "Preventive measures" = "Prevention"
      )
    )
  ) +
  theme_manuscript()
p <- p +
  ylab("Proportion of completed steps") +
  xlab("Hours since 6AM") 

outfile <- "figures/marginal_time_elapsed_since_start_of_day"

ggsave_manuscript(outfile, p, width = 12, height = 8)

## 4. HCW Qualification

marginal_preds <- imap_dfr(fits, function(fit, infile) {
  cli_inform("Processing {infile}")
  preds <- emmeans(
    fit, ~hcw_qualification,
    type = "response"
  ) |> confint()

  preds$trimester <- str_extract(infile, trimesters, group = NULL) |>
    keep(~ !is.na(.))

  preds$anc <- str_extract(infile, anc_types, group = NULL) |>
    keep(~ !is.na(.))

  preds$intervention <-
    gsub(x = infile, pattern = "multicountry_lr_fit_", replacement = "") |>
    gsub(pattern = preds$trimester[1], replacement = "") |>
    gsub(pattern = preds$anc[1], replacement = "") |>
    gsub(pattern = "_rds", replacement = "") |>
    to_sentence_case()

  preds
})

marginal_preds$anc <- recode_factor(
  marginal_preds$anc,
  first_anc = "First ANC",
  follow_up_anc = "Follow-up ANC"
)

dodge_width <- 0.2

p <- ggplot(marginal_preds, aes(x = hcw_qualification, y = prob, col = anc)) +
  geom_errorbar(
    aes(ymin = `lower.HPD`, ymax = `upper.HPD`, col = anc),
    alpha = 0.5, position = position_dodge(width = dodge_width)
  ) +
  geom_point(position = position_dodge(width = dodge_width)) +
  labs(
    y = "Predicted number of steps"
  ) +
  facet_grid(
    intervention ~ trimester,
    labeller = labeller(
      trimester = to_title_case,
      intervention = c(
        "Continuity of care" = "Continuity",
        "Informational interventions" = "Information",
        "Maternal and fetal assessment" = "Assessment",
        "Patient hcw interaction" = "Interaction",
        "Preventive measures" = "Prevention"
      )
    )
  ) +
  theme_manuscript() +
  theme(axis.text.x = element_text(size = 8))

p <- p +
  ylab("Proportion of completed steps") +
  xlab("")
  


outfile <-
  paste0("figures/marginal_hcw_qualification")
cli_inform("Saving to {outfile}")
ggsave_manuscript(outfile, p, width = 12, height = 8)




## Urba/Rural

marginal_preds <- imap_dfr(fits, function(fit, infile) {
  cli_inform("Processing {infile}")
  preds <- emmeans(
    fit, ~milieu_of_residence,
    type = "response"
  ) |> confint()

  preds$trimester <- str_extract(infile, trimesters, group = NULL) |>
    keep(~ !is.na(.))

  preds$anc <- str_extract(infile, anc_types, group = NULL) |>
    keep(~ !is.na(.))

  preds$intervention <-
    gsub(x = infile, pattern = "multicountry_lr_fit_", replacement = "") |>
    gsub(pattern = preds$trimester[1], replacement = "") |>
    gsub(pattern = preds$anc[1], replacement = "") |>
    gsub(pattern = "_rds", replacement = "") |>
    to_sentence_case()

  preds
})

marginal_preds$anc <- recode_factor(
  marginal_preds$anc,
  first_anc = "First ANC",
  follow_up_anc = "Follow-up ANC"
)

p <- ggplot(marginal_preds, aes(x = milieu_of_residence, y = prob, col = anc)) +
  geom_errorbar(
    aes(ymin = `lower.HPD`, ymax = `upper.HPD`, col= anc), alpha = 0.5,
    position = position_dodge(width = dodge_width)
  ) +
  geom_point(position = position_dodge(width = dodge_width)) +
  facet_grid(
    intervention ~ trimester,
    labeller = labeller(
      trimester = to_title_case,
      intervention = c(
        "Continuity of care" = "Continuity",
        "Informational interventions" = "Information",
        "Maternal and fetal assessment" = "Assessment",
        "Patient hcw interaction" = "Interaction",
        "Preventive measures" = "Prevention"
      )
    )
  ) +
  labs(y = "Predicted number of steps") + 
  theme_manuscript() +
  theme(axis.title.x = element_blank())




outfile <-
  paste0("figures/marginal_milieu_of_residence")
cli_inform("Saving to {outfile}")
ggsave_manuscript(outfile, p, width = 12, height = 8)





zip::zip(zipfile = "figures.zip", "figures")
orderly_artefact(
  description = "Figures for consultation length and other predictors",
  files = "figures.zip"
)
