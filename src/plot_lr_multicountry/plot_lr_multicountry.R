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
    fit, ~consult_length, at = list(consult_length = seq(1, 150, 10)),
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


x <- split(marginal_preds, marginal_preds$intervention)

iwalk(x, function(tmp, name) {

 p <- ggplot(tmp, aes(x = consult_length, y = prob)) +
   geom_ribbon(aes(ymin = `lower.HPD`, ymax = `upper.HPD`), alpha = 0.2) +
    geom_line() +
    labs(
      x = "Consultation length (minutes)",
      y = "Predicted number of steps"
    ) +
    theme_manuscript() 

  p <- my_facets(p)
  p <- p + ylab("Proportion of completed steps")
  
  outfile <-
    paste0("figures/", to_snake_case(name), "_consult_length")
  ggsave_manuscript(outfile, p, width = 12, height = 8)
})


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


x <- split(marginal_preds, marginal_preds$intervention)

iwalk(x, function(tmp, name) {

  p <- ggplot(tmp, aes(x = doctor_or_nursing_and_midwifery_scaled, y = prob)) +
   geom_ribbon(aes(ymin = `lower.HPD`, ymax = `upper.HPD`), alpha = 0.2) +
    geom_line() +
    labs(
      x = "Doctor and Nursing & Midwifery",
      y = "Predicted number of steps"
    ) +
    theme_manuscript() 

  p <- my_facets(p)
  p <- p + ylab("Proportion of completed steps")
  
  outfile <-
    paste0("figures/", to_snake_case(name), "_doctor_nursing_midwifery")
  cli_inform("Saving to {outfile}")
  ggsave_manuscript(outfile, p, width = 12, height = 8)
})


### 3. Time elapsed

marginal_preds <- imap_dfr(fits, function(fit, infile) {
  cli_inform("Processing {infile}")
  preds <- emmeans(
    fit, ~time_elapsed_since_start_of_day,
    at = list(time_elapsed_since_start_of_day = seq(-60, 1200, 60)),
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


x <- split(marginal_preds, marginal_preds$intervention)

iwalk(x, function(tmp, name) {

  p <- ggplot(tmp, aes(x = time_elapsed_since_start_of_day, y = prob)) +
   geom_ribbon(aes(ymin = `lower.HPD`, ymax = `upper.HPD`), alpha = 0.2) +
    geom_line() +
    scale_x_continuous(
      breaks = seq(-60, 1200, 120), labels = seq(-60, 1200, 120)/60
    ) +
    labs(
      x = "Time elapsed since 6AM (hours)",
      y = "Predicted number of steps"
    ) +
    theme_manuscript() 

  p <- my_facets(p)
  p <- p + ylab("Proportion of completed steps")
  
  outfile <-
    paste0("figures/", to_snake_case(name), "_time_elapsed")
  cli_inform("Saving to {outfile}")
  ggsave_manuscript(outfile, p, width = 12, height = 8)
})


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

x <- split(marginal_preds, marginal_preds$intervention)

iwalk(x, function(tmp, name) {
  p <- ggplot(tmp, aes(x = hcw_qualification, y = prob)) +
    geom_errorbar(aes(ymin = `lower.HPD`, ymax = `upper.HPD`), alpha = 0.2) +
    geom_point() +
    labs(
      y = "Predicted number of steps"
    ) +
    theme_manuscript() +
    theme(axis.title.x = element_blank())

  p <- my_facets(p)


  outfile <-
    paste0("figures/", to_snake_case(name), "_hcw_qualification")
  cli_inform("Saving to {outfile}")
  ggsave_manuscript(outfile, p, width = 12, height = 8)
})



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

x <- split(marginal_preds, marginal_preds$intervention)

iwalk(x, function(tmp, name) {
  p <- ggplot(tmp, aes(x = milieu_of_residence, y = prob)) +
    geom_errorbar(aes(ymin = `lower.HPD`, ymax = `upper.HPD`), alpha = 0.2) +
    geom_point() +
    labs(
      y = "Predicted number of steps"
    ) + 
    theme_manuscript() +
    theme(axis.title.x = element_blank())

  p <- my_facets(p)


  outfile <-
    paste0("figures/", to_snake_case(name), "_milieu_of_residence")
  cli_inform("Saving to {outfile}")
  ggsave_manuscript(outfile, p, width = 12, height = 8)
})




zip::zip(zipfile = "figures.zip", "figures")
orderly_artefact(
  description = "Figures for consultation length and other predictors",
  files = "figures.zip"
)
