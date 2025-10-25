library(brms)
library(bayesplot)
library(bayestestR)
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

  ##z_grid <- pretty(fit$data$doctor_or_nursing_and_midwifery_scaled, n = 10)
  ## 95th percentile is less than 1 in all cases
  z_grid <- seq(-0.5, 1, length.out = 10)
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

saveRDS(out, file = "multicountry_dco_bayes_mm_doctor_and_nm.rds")
orderly_artefact(
  files = "multicountry_dco_bayes_mm_doctor_and_nm.rds",
  description = "Marginal means for doctor and nursing and midwifery covariate"
)


## Now we need to translate the scaled covariate to its original scale
orderly_dependency("lm_multicountry", "latest", "scaled_attributes.rds")
scaled_vars_attrs <- readRDS("scaled_attributes.rds")
center_used <-
  filter(scaled_vars_attrs, variable == "doctor_or_nursing_and_midwifery_scaled") |>
  pull(center)

scale_used <-
  filter(scaled_vars_attrs, variable == "doctor_or_nursing_and_midwifery_scaled") |>
  pull(scale)

out$original_var <- 
    out$doctor_or_nursing_and_midwifery_scaled * scale_used + center_used

p <- ggplot(out) +
  geom_line(
    aes(x = original_var, y = emmean)
  ) + geom_ribbon(
    aes(
      x = original_var, ymin = lower.HPD,
      ymax = upper.HPD
    ), alpha = 0.2
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
  ## Choosing the 95th quantile 
  z_grid <- seq(-2, 9, by = 1)
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

saveRDS(out, file = "multicountry_dco_bayes_mm_time_elapsed.rds")
orderly_artefact(
  files = "multicountry_dco_bayes_mm_time_elapsed.rds",
  description = "Marginal means for time elapsed since start of day"
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
    breaks = c(-2, 0, 3, 6, 9),
    labels = c("4AM", "6AM", "9AM", "12PM", "3PM")
  ) +
  theme_manuscript() +
  ylab("ANC contact length") +
  xlab("")


ggsave_manuscript(
  p, file = "figures/mm_time_elapsed", width = 12, height = 8
)

out <- map_dfr(fits, function(fit) hdi(fit), .id = "datacut")

xhdi <- filter(
  out,
  Parameter %in%
    c(
      "b_doctor_or_nursing_and_midwifery_scaled",
      "b_time_elapsed_since_start_of_day"
    )
)

out <- map_dfr(fits, point_estimate, .id = "datacut")
xmap <- filter(
  out,
  Parameter %in%
    c(
      "b_doctor_or_nursing_and_midwifery_scaled",
      "b_time_elapsed_since_start_of_day"
    )
)

p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(
    data = xmap, aes(y = datacut, x = `MAP`),
    size = 4
  ) +  
  geom_linerange(
    data = xhdi,
    aes(y = datacut, xmin = `CI_low`, xmax = `CI_high`),
    linewidth = 0.8
  ) +
  facet_wrap(
    ~Parameter,
    scales = "free_x",
    ncol = 2,
    labeller = as_labeller(
      c(
        b_doctor_or_nursing_and_midwifery_scaled =
           "Doctors and N&M per 10,000 population (scaled)",
        b_time_elapsed_since_start_of_day =
          "Time elapsed since 6AM"
      )
    )
  ) +
  theme_manuscript() 

p <- p +
  scale_y_discrete(
    limits = c(
      "First ANC_First Trimester",
      "Follow-up ANC_First Trimester",
      "First ANC_Second Trimester",
      "Follow-up ANC_Second Trimester",
      "First ANC_Third Trimester", 
      "Follow-up ANC_Third Trimester"
    ),
    labels = c(
      "First ANC/First Trimester",
      "Follow-up ANC/First Trimester",
      "First ANC/Second Trimester",
      "Follow-up ANC/Second Trimester",
      "First ANC/Third Trimester",
      "Follow-up ANC/Third Trimester"
    )
   )

p <- p +
  xlab("MAP estimate and 95% Highest Density Interval") +
  ylab("")

ggsave_manuscript(
  p,
  file = "figures/multicountry_dco_bayes_fixed_effects",
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
  fits, function(fit) p_direction(fit),
  .id = "datacut"
)



saveRDS(coeffs_gt_0, file = "multicountry_dco_bayes_coeffs_gt_0.rds")
orderly_artefact(
  files = "multicountry_dco_bayes_coeffs_gt_0.rds",
  description = "Coefficients greater than 0"
)


xpd <- filter(
  coeffs_gt_0,
  Parameter %in%
    c(
      "b_doctor_or_nursing_and_midwifery_scaled",
      "b_time_elapsed_since_start_of_day"
    )
)

p <- ggplot(xpd) +
  geom_tile(
    aes(x = 0.5, y = datacut, width = 1, height = 0.25),
    fill = "gray"
  ) +
  geom_tile(
    aes(x = pd / 2, y = datacut, width = pd, height = 0.25),
    fill = "red"
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  xlim(0, 1) +
  xlab("Posterior probability of coefficient > 0") +
  ylab("") +
  theme_manuscript() +
  theme(axis.title.x = element_text(size = 12))

p <- p +
  facet_wrap(
    ~Parameter,
    scales = "free_x",
    ncol = 2,
    labeller = as_labeller(
      c(
        b_doctor_or_nursing_and_midwifery_scaled =
          "Doctors and N&M per 10,000 population (scaled)",
        b_time_elapsed_since_start_of_day =
          "Time elapsed since 6AM"
      )
    )
  ) 

p <- p +
  scale_y_discrete(
    limits = c(
      "First ANC_First Trimester",
      "Follow-up ANC_First Trimester",
      "First ANC_Second Trimester",
      "Follow-up ANC_Second Trimester",
      "First ANC_Third Trimester",
      "Follow-up ANC_Third Trimester"
    ),
    labels = c(
      "First ANC/First Trimester",
      "Follow-up ANC/First Trimester",
      "First ANC/Second Trimester",
      "Follow-up ANC/Second Trimester",
      "First ANC/Third Trimester",
      "Follow-up ANC/Third Trimester"
    )
  )

ggsave_manuscript(
  plot = p,
  filename = "figures/multicountry_dco_coeffs_gt_0", width = 12, height = 8
)



