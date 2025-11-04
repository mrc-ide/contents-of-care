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

dir.create("figures", showWarnings = FALSE)

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

pars <- orderly_parameters(all_countries = FALSE)

orderly_dependency(
  "lm_multicountry",
  "latest(parameter:all_countries == this:all_countries)",
  files = c("scaled_attributes.rds", "fits/")
)

scaled_vars_attrs <- readRDS("scaled_attributes.rds")

infiles <- list.files("fits", full.names = TRUE)

fits <- map(infiles, readRDS)
names(fits) <- str_remove(infiles, "fits/") |>
  str_remove("_multicountry_fit.rds") 

obs <- map_dfr(fits, function(fit) fit$data, .id = "datacut")

obs$patients_per_staff_per_year_rounded <- round(
  obs$patients_per_staff_per_year_scaled, 1
)

obs_mm_patients_per_staff <- obs |>
  group_by(
    datacut,
    patients_per_staff_per_year_rounded
  ) |>
  summarise(
    mean_length = mean(consult_length),
    n = n(),
    .groups = "drop"
  ) |>
  separate(
    datacut,
    into = c("anc", "trimester"),
    sep = "_"
  )

z_grid <- seq(-1, 2, length.out = 10)
mm_patients_per_staff <- map_dfr(fits, function(fit) {
  
  emm <- emmeans(
    fit,
    ~patients_per_staff_per_year_scaled,
    at = list(patients_per_staff_per_year_scaled = z_grid),
    re_formula = NULL, type = "link", frequentist = FALSE,
    weights = "proportional",
    epred = TRUE
  )
  summary(emm, link = "log", predict.type = "response")
}, .id = "datacut")

out <- separate(
  mm_patients_per_staff, datacut,
  into = c("anc", "trimester"), sep = "_"
)

var <- "patients_per_staff_per_year_scaled"
center_used <-
  filter(scaled_vars_attrs, variable == var) |>
  pull(center)

scale_used <-
  filter(scaled_vars_attrs, variable == var) |>
  pull(scale)


out$patients_per_staff_per_year_scaled_original <-
  out$patients_per_staff_per_year_scaled * scale_used +
  center_used

obs_mm_patients_per_staff <- filter(
  obs_mm_patients_per_staff,
  between(patients_per_staff_per_year_rounded,min(z_grid), max(z_grid))
)

obs_mm_patients_per_staff$patients_per_staff_per_year_original <-
  obs_mm_patients_per_staff$patients_per_staff_per_year_rounded * scale_used +
  center_used

p <- ggplot(out) +
  geom_line(
    aes(x = patients_per_staff_per_year_scaled_original, y = emmean)
  ) +
  geom_ribbon(
    aes(
      x = patients_per_staff_per_year_scaled_original, ymin = lower.HPD,
      ymax = upper.HPD
    ),
    alpha = 0.2
  ) +
  facet_grid(anc ~ trimester, scales = "free_y") +
  theme_manuscript() +
  ylab("ANC contact length") +
  xlab("Patients per health staff per year")


p <- p + geom_point(
  data = obs_mm_patients_per_staff,
  aes(
    x = patients_per_staff_per_year_original,
    y = mean_length,
    size = n
  ),
  fill = "red",
  col = "red",
  alpha = 0.2
)

ggsave_manuscript(
  p,
  file = "figures/mm_patients_per_staff",
  width = 12, height = 8
)

## Marginal means Doctor and Nursing and Midwifery per 10,000 population
obs$doctor_or_nursing_and_midwifery_scaled_rounded <- round(
  obs$doctor_or_nursing_and_midwifery_scaled, 1
)
obs_mm_doctor_and_nm <- obs |>
  group_by(
    datacut,
    doctor_or_nursing_and_midwifery_scaled_rounded
  ) |>
  summarise(
    mean_length = mean(consult_length),
    n = n(),
    .groups = "drop"
  ) |>
  separate(
    datacut,
    into = c("anc", "trimester"),
    sep = "_"
  )

z_grid <- seq(-0.5, 1, length.out = 10)
mm_doctor_and_nm <- map_dfr(fits, function(fit) {
 
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

var <- "doctor_or_nursing_and_midwifery_scaled"
center_used <-
  filter(scaled_vars_attrs, variable == var) |>
  pull(center)

scale_used <-
  filter(scaled_vars_attrs, variable == var) |>
  pull(scale)

out$original_var <- 
    out[[var]] * scale_used + center_used

p <- ggplot(out) +
  geom_line(
    aes(x = original_var, y = emmean)
  ) + geom_ribbon(
    aes(
      x = original_var, ymin = lower.HPD,
      ymax = upper.HPD
    ), alpha = 0.2
  ) +
  facet_grid(anc ~ trimester, scales = "free_y") +
  theme_manuscript() +
  ylab("ANC contact length") +
  xlab("Doctors and N&M per 10,000 population (scaled)")

obs_mm_doctor_and_nm <- filter(
  obs_mm_doctor_and_nm,
  between(doctor_or_nursing_and_midwifery_scaled_rounded, min(z_grid), max(z_grid))
)

p <- p + geom_point(
  data = obs_mm_doctor_and_nm,
  aes(
    x = doctor_or_nursing_and_midwifery_scaled_rounded * scale_used + center_used,
    y = mean_length,
    size = n
  ),
  fill = "red",
  col = "red",
  alpha = 0.2
)

ggsave_manuscript(
  p,
  file = "figures/mm_doctor_and_nm",
  width = 12, height = 8
)

## Marginal means time elapsed since start of day
## Choosing the 2.5th to 95th quantile

obs$time_elapsed_since_start_of_day_rounded <- round(
  obs$time_elapsed_since_start_of_day, 1
)
obs_mm_time_elapsed <- obs |>
  group_by(
    datacut,
    time_elapsed_since_start_of_day_rounded
  ) |>
  summarise(
    mean_length = mean(consult_length),
    n = n(),
    .groups = "drop"
  ) |>
  separate(
    datacut,
    into = c("anc", "trimester"),
    sep = "_"
  )



z_grid <- seq(2, 9, by = 1)
mm_time_elapsed <- map_dfr(fits, function(fit) {
  
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
  facet_grid(anc ~ trimester, scales = "free_y") +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8),
    labels = c("8AM", "10AM", "12PM", "2PM")
  ) +
  theme_manuscript() +
  ylab("ANC contact length") +
  xlab("")

obs_mm_time_elapsed <- filter(
  obs_mm_time_elapsed,
  between(time_elapsed_since_start_of_day_rounded, min(z_grid), max(z_grid))
)

p <- p + geom_point(
  data = obs_mm_time_elapsed,
  aes(
    x = time_elapsed_since_start_of_day_rounded,
    y = mean_length,
    size = n
  ),
  fill = "red",
  col = "red",
  alpha = 0.2
)


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



