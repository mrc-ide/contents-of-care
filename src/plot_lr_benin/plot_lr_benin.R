library(brms)
library(cli)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(glue)
library(orderly2)
library(performance)
library(posterior)
library(purrr)
library(snakecase)
library(stringr)
library(tibble)
library(tidyr)


orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

fit_files <- list.files(path = ".", pattern = "*.rds")
fits <- map(fit_files, readRDS)
names(fits) <- gsub("\\.rds$", "", fit_files)

## intervention_names
intv_names <- to_snake_case(names(intervention_types))
trimester_names <- paste(c("first", "second", "third"), "trimester", sep = "_")
anc_names <- c("first_anc", "follow_up_anc")



fixed_effects <- imap_dfr(fits, function(fit, fname) {
  x <- as.data.frame(fixef(fit, probs = c(0.025, 0.5, 0.975)))
  out <- rownames_to_column(x)
  out$intervention <- str_match(fname, intv_names)[, 1] |> keep(~ !is.na(.))
  out$trimester <- str_match(fname, trimester_names)[, 1] |> keep(~ !is.na(.))
  out$anc <- str_match(fname, anc_names)[, 1] |> keep(~ !is.na(.))
  out
})

saveRDS(fixed_effects, file = "benin_dco_bayes_fixed_effects.rds")
orderly_artefact(
  files = "benin_dco_bayes_fixed_effects.rds",
  description = "Fixed effects for DRC 2015 DCO model fits"
)

x <- filter(fixed_effects, !rowname %in% "Intercept")

breaks <- c(
  "consult_length",
  "milieu_of_residenceUrban",
  "milieu_of_residenceUnknown",
  "facility_level_mappingSecondary",
  "facility_status_mappingPublic",
  "women_in_labour_payYes",
  "pregnant_women_private_spaceYes",
  "fetoscopeYes",
  "fetoscopeUnknown",
  "number_of_births_2009",
  "doctor_or_nursing_and_midwifery_per_10000_scaled",
  "hcw_qualificationNurse",
  "hcw_qualificationMidwife",
  "hcw_qualificationOther",
  "time_elapsed_since_start_of_day"
)

x$rowname <- factor(x$rowname, levels = breaks, ordered = TRUE)

labels <- c(
  "Consult length",
  "Urban",
  "Unknown",
  "Secondary facility",
  "Public facility",
  "Women in labour pay",
  "Pregnant women private space",
  "Fetoscope: yes",
  "Fetoscope: unknown",
  "Number of births in 2009",
  "Doctor/N&M per 10000",
  "HCW:Nurse",
  "HCW:Midwife",
  "HCW:Other",
  "Hours since 6AM"
)

walk(intv_names, function(intv) {
  y <- filter(x, intervention %in% intv)
  p <- ggplot(y) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(aes(y = rowname, x = Q50)) +
    geom_errorbarh(
      aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
      height = 0
    ) +    
    theme_manuscript() +
    theme(axis.title.y = element_blank())
  

  p <- p + scale_y_discrete(breaks = breaks, labels = labels)

  p <- my_facets(p)

  cli_inform("Saving {intv}")
  ggsave_manuscript(glue("{intv}_fixed_effects"), p, width = 12, height = 8)
})




## Random effects
ran_effects <- imap_dfr(fits, function(fit, fname) {
  x <- ranef(fit, probs = c(0.025, 0.5, 0.975))
  x <- as.data.frame(x$health_zone[, , "Intercept"])
  out <- rownames_to_column(x)
  out$intervention <- str_match(fname, intv_names)[, 1] |> keep(~ !is.na(.))
  out$trimester <- str_match(fname, trimester_names)[, 1] |> keep(~ !is.na(.))
  out$anc <- str_match(fname, anc_names)[, 1] |> keep(~ !is.na(.))
  out
})

saveRDS(ran_effects, file = "benin_dco_bayes_random_effects.rds")
orderly_artefact(
  files = "benin_dco_bayes_random_effects.rds",
  description = "Random effects for DRC 2015 DCO model fits"
)

walk(intv_names, function(intv) {
  y <- filter(ran_effects, intervention %in% intv)
  p <- ggplot(y) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(aes(y = rowname, x = Q50)) +
    geom_errorbarh(
      aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
      height = 0
    ) +
    theme_manuscript() +
    theme(axis.title.y = element_blank())

  p <- my_facets(p)
  cli_inform("Saving {intv}")
  ggsave_manuscript(glue("{intv}_random_effects"), p, width = 12, height = 8)
})



bayes_r2 <- map(fits, bayes_R2, probs = c(0.025, 0.5, 0.975))

saveRDS(bayes_r2, file = "benin_dco_bayes_r2.rds")

orderly_artefact(
  files = "benin_dco_bayes_r2.rds",
  description = "Bayes R2 for DRC 2015 DCO model fits"
)

coeffs_gt_0 <- imap_dfr(
  fits, function(fit, fname) {
    out <- probability_of_direction(fit)[[1]]
    out$intervention <- str_match(fname, intv_names)[, 1] |> keep(~ !is.na(.))
    out$trimester <- str_match(fname, trimester_names)[, 1] |> keep(~ !is.na(.))
    out$anc <- str_match(fname, anc_names)[, 1] |> keep(~ !is.na(.))
    out
  }
)

saveRDS(coeffs_gt_0, file = "benin_dco_bayes_coeffs_gt_0.rds")
orderly_artefact(
  files = "benin_dco_bayes_coeffs_gt_0.rds",
  description = "Coefficients greater than 0 for DRC 2015 DCO model fits"
)

coeffs_gt_0 <- filter(coeffs_gt_0, !rowname %in% "Intercept")
coeffs_gt_0$rowname <- factor(coeffs_gt_0$rowname, levels = breaks, ordered = TRUE)

walk(intv_names, function(intv) {
  x <- filter(coeffs_gt_0, intervention %in% intv)
  p <- ggplot(x) +
    geom_tile(
      aes(x = 0.5, y = rowname, width = 1, height = 0.25),
      fill = "gray"
    ) +
    geom_tile(
      aes(x = `Post.Prob` / 2, y = rowname, width = `Post.Prob`, height = 0.25),
      fill = "red"
    ) +
    geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    scale_y_discrete(breaks = breaks, labels = labels) +
    xlim(0, 1) +
    xlab("Probability(coefficient > 0)") +
    theme_manuscript() +
    theme(
      axis.title.y = element_blank(), axis.title.x = element_text(size = 12)
    )

  p <- my_facets(p)
  
  cli_inform("Saving {intv}")
  ggsave_manuscript(glue("{intv}"), p, width = 12, height = 8)
})
    
