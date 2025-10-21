library(brms)
library(cli)
library(dplyr)
library(emmeans)
library(ggplot2)
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

orderly_dependency(
  "lm_multicountry", "latest", "fits/"
)

infiles <- list.files("fits", full.names = TRUE)

fits <- map(infiles, readRDS)
names(fits) <- str_remove(infiles, "fits/") |>
  str_remove("_multicountry_fit.rds") 


## Marginal means
mm_doctor_and_nm <- map_dfr(fits, function(fit) {

  z_grid <- seq(-2, 2, by = 0.5)
  emm <- emmeans(
    fit,
    ~doctor_or_nursing_and_midwifery_scaled, 
    at = list(doctor_or_nursing_and_midwifery_scaled = z_grid), 
    re_formula = NULL, type = "link", frequentist = FALSE,
    weights = "proportional" ,
    epred = TRUE
   )
   summary(emm, link = "log", predict.type = "response")
   
}, .id = "datacut")

out <- separate(
  mm_doctor_and_nm, datacut,
  into = c("anc", "trimester"), sep = "_"
)


## Now we need to translate the scaled covariate to its original scale
orderly_dependency("process_benin", "latest", "benin_scaled_vars_attrs.rds")
benin_scaled_vars_attrs <- readRDS("benin_scaled_vars_attrs.rds")
benin_mu <-
  filter(benin_scaled_vars_attrs, variable == "doctor_or_nursing_and_midwifery") |>
  pull(mean)

benin_sd <-
  filter(benin_scaled_vars_attrs, variable == "doctor_or_nursing_and_midwifery") |>
  pull(sd)

orderly_dependency("process_drc", "latest", "drc_hf_scaled_attrs.rds")
drc_scaled_vars_attrs <- readRDS("drc_hf_scaled_attrs.rds")

drc_mu <-
  filter(drc_scaled_vars_attrs, variable == "doctor_or_nursing_and_midwifery") |>
  pull(mean)

drc_sd <-
  filter(drc_scaled_vars_attrs, variable == "doctor_or_nursing_and_midwifery") |>
  pull(sd)

orderly_dependency("process_burkina_faso", "latest", "bfa_hf_scaled_attrs.rds")
bfa_scaled_vars_attrs <- readRDS("bfa_hf_scaled_attrs.rds")

bfa_mu <-
  filter(bfa_scaled_vars_attrs, variable == "doctor_or_nursing_and_midwifery") |>
  pull(mean)

bfa_sd <-
  filter(bfa_scaled_vars_attrs, variable == "doctor_or_nursing_and_midwifery") |>
  pull(sd)


out$original_var <- case_when(
  out$country %in% "Benin (2010)" ~
    out$doctor_or_nursing_and_midwifery_scaled * benin_sd + benin_mu,
  out$country %in% c("Burkina Faso (2013)", "Burkina Faso (2017)") ~
    out$doctor_or_nursing_and_midwifery_scaled * bfa_sd + bfa_mu,
  out$country %in% c("DRC (2015)", "DRC (2021)") ~
    out$doctor_or_nursing_and_midwifery_scaled * drc_sd + drc_mu
)


p <- ggplot(out) +
  geom_line(
    aes(x = doctor_or_nursing_and_midwifery_scaled, y = emmean)
  ) + geom_ribbon(
    aes(
      x = doctor_or_nursing_and_midwifery_scaled, ymin = lower.HPD,
      ymax = upper.HPD
    ), width = 0, alpha = 0.2
  ) +
  facet_grid(anc ~ trimester) +
  theme_manuscript() +
  ylab("ANC contact length") +
  xlab("Doctors and N&M per 10,000 population (scaled)")


ggsave_manuscript(
  p,
  file = "figures/mm_doctor_and_nm",
  width = 12, height = 8
)



mm_time_elapsed <- map_dfr(fits, function(fit) {
  
  z_grid <- seq(-5, 17, by = 1)
  emm_ctry <- emmeans(
    fit,
    ~time_elapsed_since_start_of_day,
    at = list(time_elapsed_since_start_of_day = z_grid),
    re_formula = NULL,
    weights = "proportional", epred = TRUE
  )

  summary(emm_ctry, link = "log", predict.type = "response")
}, .id = "datacut")

out <- separate(
  mm_time_elapsed, datacut,
  into = c("anc", "trimester"), sep = "_"
)



p <- ggplot(out) +
  geom_line(
    aes(
      x = time_elapsed_since_start_of_day,
      y = emmean
    )
  ) +
  geom_ribbon(
    aes(
      x = time_elapsed_since_start_of_day,
      ymin = lower.HPD, ymax = upper.HPD
    ),
    alpha = 0.2
  ) +
  facet_grid(anc ~ trimester) +
  scale_x_continuous(
    breaks = c(-5, 0, 6, 12, 17),
    labels = c("1AM", "6AM", "12PM", "6PM", "11PM")
  ) +
  theme_manuscript() +
  ylab("ANC contact length") +
  xlab("")


ggsave_manuscript(
  p, file = "figures/mm_time_elapsed", width = 12, height = 8
)


fixed_effects <- map_dfr(fits, function(fit) {
  x <- as.data.frame(fixef(fit, probs = c(0.025, 0.5, 0.975)))
  rownames_to_column(x)
}, .id = "datacut")

fixed_effects <- separate(
  fixed_effects, datacut,
  into = c("anc", "trimester"), sep = "_"
)

saveRDS(fixed_effects, file = "multicountry_dco_bayes_fixed_effects.rds")
orderly_artefact(
  files = "multicountry_dco_bayes_fixed_effects.rds",
  description = "Fixed effects for DRC 2015 DCO model fits"
)

x <- filter(fixed_effects, !rowname %in% "Intercept")

dodge_width <- 0.2

dir.create("figures", showWarnings = FALSE)

split(x, x$rowname) |>
  iwalk(function(y, name) {
    p <- ggplot(y) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(
        aes(y = trimester, x = Q50, col = anc),
        position = position_dodge(width = dodge_width)
      ) +
      geom_errorbarh(
        aes(y = trimester, xmin = `Q2.5`, xmax = `Q97.5`, col = anc),
        height = 0, 
        position = position_dodge(width = dodge_width)
      ) +
      theme_manuscript() +
      ylab("") +
      xlab("Coefficient estimate (log scale)") +
      ggtitle(to_title_case(name))

    ggsave_manuscript(
      p,
      file = paste0("figures/", name),
      width = 12, height = 8
    )
    
  })
 



breaks <- c(
  "milieu_of_residenceUrban",
  "milieu_of_residenceUnknown",
  "facility_level_mappingSecondary",
  "facility_level_mappingTertiary",
  "facility_level_mappingOther",
  "doctor_or_nursing_and_midwifery_scaled",
  "hcw_sexMale",
  "hcw_sexUnknown",
  "hcw_qualificationDoctor",
  "hcw_qualificationNurse",
  "hcw_qualificationOther",
  "hcw_qualificationUnknown",
  "time_elapsed_since_start_of_day"
)


x$rowname <- factor(x$rowname, levels = breaks, ordered = TRUE)

labels <- c(    
  "Urban",  
  "Unknown",
  "Secondary HF",
  "Tertiary HF",
  "Other facility type",
  "Doctor/N&M per 10,000",
  "HCW:Male",
  "HCW sex:Unknown",
  "Doctor",
  "Nurse",
  "Qualification:Other",
  "Qualification:Unknown",
  "Hours since 6AM"
)


p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = x, aes(y = rowname, x = Q50)) +
  geom_errorbarh(
    data = x,
    aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
    height = 0
  ) +
  theme_manuscript() 

p <- p + scale_y_discrete(breaks = breaks, labels = labels)

p <- my_facets(p)

ggsave_manuscript(
  p,
  file = "multicountry_dco_bayes_fixed_effects",
  width = 12, height = 8
)

## How far have we moved from the priors?
pquantiles <- qnorm(p = c(0.025, 0.975), mean = 0, sd = 0.5)
df <- data.frame(
  anc = rep(c("First ANC", "Follow-up ANC"), each = 3),
  trimester = rep(
    c("First Trimester", "Second Trimester", "Third Trimester"), times = 2
  ),
  xmin = pquantiles[1],
  xmax = pquantiles[2]
)
## Random effects
ran_effects <- map_dfr(fits, function(fit) {
  x <- ranef(fit, probs = c(0.025, 0.5, 0.975))
  x <- as.data.frame(x$country[, , "Intercept"])
  rownames_to_column(x)
}, .id = "datacut")


ran_effects <- separate(
  ran_effects, datacut,
  into = c("anc", "trimester"), sep = "_"
)



saveRDS(ran_effects, file = "multicountry_dco_bayes_random_effects.rds")
orderly_artefact(
  files = "multicountry_dco_bayes_random_effects.rds",
  description = "Random effects for DRC 2015 DCO model fits"
)

p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = ran_effects, aes(y = rowname, x = Q50)) +
  geom_errorbarh(
    data = ran_effects,
    aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
    height = 0
  ) +
  geom_rect(
    data = df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey80", alpha = 0.2
  ) +
  theme_manuscript() 

p <- my_facets(p)

ggsave_manuscript(
  p,
  file = "multicountry_dco_bayes_random_effects",
  width = 12, height = 8
)

icc <- map(fits, compute_icc)


saveRDS(icc, file = "multicountry_dco_bayes_icc.rds")

orderly_artefact(
  files = "multicountry_dco_bayes_icc.rds",
  description = "ICC for DRC 2015 DCO model fits"
)


bayes_r2 <- map(fits, bayes_R2, probs = c(0.025, 0.5, 0.975))

saveRDS(bayes_r2, file = "multicountry_dco_bayes_r2.rds")

orderly_artefact(
  files = "multicountry_dco_bayes_r2.rds",
  description = "Bayes R2 for DRC 2015 DCO model fits"
)


coeffs_gt_0 <- map_dfr(
  fits, function(fit) probability_of_direction(fit)[[1]],
  .id = "datacut"
)

coeffs_gt_0 <- separate(
  coeffs_gt_0, datacut, into = c("anc", "trimester"), sep = "_"
)

saveRDS(coeffs_gt_0, file = "multicountry_dco_bayes_coeffs_gt_0.rds")
orderly_artefact(
  files = "multicountry_dco_bayes_coeffs_gt_0.rds",
  description = "Coefficients greater than 0"
)


coeffs_gt_0 <- filter(coeffs_gt_0, !rowname %in% "Intercept")
coeffs_gt_0$rowname <- factor(
  coeffs_gt_0$rowname,
  levels = breaks, ordered = TRUE
)

p <- ggplot(coeffs_gt_0) +
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
  xlab("Posterior probability of coefficient > 0") +
  theme_manuscript() +
  theme(axis.title.x = element_text(size = 12))

p <- my_facets(p)
  

ggsave_manuscript(
  plot = p, filename = "multicountry_dco_coeffs_gt_0", width = 12, height = 8
)
orderly_artefact(
  files = "multicountry_dco_coeffs_gt_0.png",
  description = "Posterior probability of coefficients being greater than 0"
)
