library(broom)
library(brms)
library(cli)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(glmnet)
library(glue)
library(orderly2)
library(purrr)
library(rsample)
library(scales)
library(snakecase)
library(tibble)
library(tidylog)
library(tidyr)




orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency("process_benin", "latest", files = c("benin_split.rds"))
benin_split <- readRDS("benin_split.rds")

## Stepwise regression
models <- map(benin_split, function(x) {
  
  full_model <- lm(log_consult_length ~ (.), data = x)
  scope_terms <- terms(log_consult_length ~ (.), data = x)

  final <- step(
    full_model,
    direction = "backward", scope = scope_terms, trace = 1
  )
  list(initial = full_model, final = final)
})


## Visualise the models
coeffs <- map_dfr(
  models, function(x) tidy(x$final, conf.int = TRUE),
  .id = "datacut"
)
coeffs <- separate(
  coeffs, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)
coeffs$significant <- coeffs$p.value < 0.05

## Extract R2 and nobs
r2_nobs <- map_dfr(models, function(x) {
  data.frame(
    r2 = summary(x$final)$r.squared,
    nobs = nobs(x$final)
  )
}, .id = "datacut") |>
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

r2_nobs$nobs_label <- glue::glue("n = {r2_nobs$nobs}")
r2_nobs$r2_label <- glue::glue("R² = {percent(r2_nobs$r2)}")
r2_nobs$label <- glue("{r2_nobs$nobs_label}; {r2_nobs$r2_label}")


## Check improvement in AIC
delta_aic <- map_dfr(models, function(x) {
  aic_initial <- AIC(x$initial)
  aic_final <- AIC(x$final)
  data.frame(delta_aic = aic_initial - aic_final)
}, .id = "datacut") |>
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

delta_aic$delta_aic <- glue("Diff AIC = {round(delta_aic$delta_aic, 2)}")



p <- ggplot(coeffs[coeffs$term != "(Intercept)", ]) +
  geom_point(aes(estimate, term, col = significant)) +
  geom_errorbarh(
    aes(y = term, xmin = conf.low, xmax = conf.high, col = significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey") + 
  facet_grid(
    first_anc ~ trimester, scales = "free"
    ##labeller = labeller(.rows = first_anc_labels)
  ) +
  scale_y_discrete(
    breaks = c(
      "milieu_of_residenceUrban",
      "health_zoneBanikoara",
      "health_zoneCovè/Ouinhi/Zangnanado",
      "health_zoneKouandé/ Pehunco/Kerou",
      "health_zoneLokossa/Athiémé",
      "health_zoneOuidah/Kpomassè/Tori",
      "health_zonePorto-Novo/ Sèmè-Kpodji/ Aguégués",
      "health_zoneZogbodomey/Bohicon/Zakpota",
      "number_of_births_2009",
      "women_in_labour_payYes",
      "pregnant_women_private_spaceYes",
      "doctor_or_nursing_and_midwifery_per_10000_scaled",
      "fetoscopeYes",
      "fetoscopeUnknown",
      "facility_status_mappingPublic",
      "facility_level_mappingSecondary",
      "hcw_qualificationNurse",
      "hcw_qualificationMidwife",
      "hcw_qualificationOther", 
      "time_elapsed_since_start_of_day"
    ),
    labels = c(
      "Urban", 
      "Banikoara",
      "Covè/Ouinhi/Zangnanado",
      "Kouandé/ Pehunco/Kerou",
      "Lokossa/Athiémé",
      "Ouidah/Kpomassè/Tori",
      "Porto-Novo/ Sèmè-Kpodji/ Aguégués",
      "Zogbodomey/Bohicon/Zakpota",
      "Number of births in 2009",
      "Women in labour pay: Yes",
      "Pregnant women private space: Yes",
      "Doctors/N&M per 10000",
      "Fetoscope: Yes",
      "Fetoscope: Unknown",
      "Public Facility",
      "Secondary Facility",
      "Nurse",
      "Midwife",
      "Other", 
      "Hours elapsed since 6AM" 
      ),
  ) +
  theme_manuscript() +
  theme(
    axis.text.y = element_text(size = 10), legend.title = element_text(size = 12)
  ) +
  labs(x = "Estimate", color = "Significant") 


p1 <- p +
  geom_text_npc(data = r2_nobs, aes(npcx = 0.1, npcy = 0.1, label = label)) +
  geom_text_npc(
    data = delta_aic, aes(npcx = 0.1, npcy = 0.15, label = delta_aic)
  ) 

check <- map(models, function(x) anova(x[[1]], x[[2]], test = "LRT"))

ggsave_manuscript("benin_stepwise", p1, width = 12, height = 8)


## Model diagnostics

orderly_resource("lm_benin_multilevel.R")
source("lm_benin_multilevel.R")

## orderly_resource("lm_benin_lasso.R")
## source("lm_benin_lasso.R")


