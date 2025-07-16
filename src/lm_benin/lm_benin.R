library(broom)
library(brms)
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




orderly_dependency("process_benin", "latest", files = c("benin_dco.rds"))
benin_dco <- readRDS("benin_dco.rds")

benin_dco$time_elapsed_since_start_of_day <- round(benin_dco$time_elapsed_since_start_of_day)
## Some questions use 1 for yes and 2 for no; recode as 0 for no and 1 for yes
benin_dco$fetoscope <- case_when(
  benin_dco$fetoscope %in% 2L ~ "non",
  benin_dco$fetoscope %in% 1L ~ "oui",
  TRUE ~ NA_character_
)

benin_dco$women_in_labour_pay <- case_when(
  benin_dco$women_in_labour_pay %in% 2L ~ "non",
  benin_dco$women_in_labour_pay %in% 1L ~ "oui",
  TRUE ~ NA_character_
)

benin_dco$pregnant_women_private_space <- case_when(
  benin_dco$pregnant_women_private_space %in% 2L ~ "non",
  benin_dco$pregnant_women_private_space %in% 1L ~ "oui",
  TRUE ~ NA_character_
)

##first_anc_labels <- c(oui = "First ANC: Yes", non = "First ANC: No")

## benin_dco$first_anc <- factor(
##   benin_dco$first_anc,
##   levels = c("oui", "non")
##   ##labels = first_anc_labels
## )


benin_small <- select(
  benin_dco, consult_length,
  m0_milieu, health_zone, facility_type, facility_status,
  pregnant_women_private_space, doctor_or_nursing_and_midwifery_per_10000,
  fetoscope, number_of_births_2009, women_in_labour_pay,
  hcw_qualification, first_anc, trimester, time_elapsed_since_start_of_day,
  )

## Make this snake case to avoid problems when we put the model matrix and
## the data together
benin_small$facility_type <- to_snake_case(benin_small$facility_type)

benin_small$hcw_qualification <- case_when(
  !benin_small$hcw_qualification %in% c("Doctor", "Midwife", "Nurese") ~ "Other",
  TRUE ~ benin_small$hcw_qualification
)

benin_small$log_consult_length <- log(benin_small$consult_length)
## Drop consult_length to avoid it being included as a covariate
benin_small <- select(benin_small, -consult_length)
factor_vars <- c(
  "m0_milieu", "health_zone", "facility_type",
  "facility_status", "pregnant_women_private_space",
  "fetoscope", "women_in_labour_pay",
  "hcw_qualification", "first_anc", "trimester"
)

## Make NAs into "Unknown"
benin_small <- mutate(
  benin_small, across(all_of(factor_vars), function(x) {
    x[is.na(x)] <- "Unknown"
    x
  }
))


set.seed(42)

## Build contrasts before splitting the data, to avoid insufficient
## levels in the factors
benin_small <- mutate(
  benin_small, across(all_of(factor_vars), function(x) {
    x <- factor(x)
    contrasts(x) <- contr.treatment(levels(x), contrasts = TRUE)
    x
  })
)

contrasts_list <- map(
  benin_small[factor_vars], function(x) contrasts(x)
)

## Scale continuous variables before splitting
benin_small <- mutate(
  benin_small,
  across(
    c(doctor_or_nursing_and_midwifery_per_10000,
      number_of_births_2009),
    scale
  ))


benin_split <- split(
  benin_small, list(benin_dco$first_anc, benin_dco$trimester),
  sep = "_"
)


benin_split <- map(benin_split, function(x) {
  insuff_levels <- map_int(factor_vars, function(var) length(unique(x[[var]])))
  insuff_levels <- factor_vars[which(insuff_levels == 1)]
  ## Drop invariant variables
  cli::cli_alert(
    "Dropping {length(insuff_levels)} invariant variables: {insuff_levels}"
  )
  x <- x[, !names(x) %in% insuff_levels]
  x
})

saveRDS(benin_split, "benin_split.rds")
orderly_artefact(
  files = "benin_split.rds",
  description = "Data split by first ANC and trimester"
)

## Experimental: remove all covariates related to facility, and only use
## facility_id






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
      "m0_milieuUrban", 
      "health_zoneBanikoara",
      "health_zoneCovè/Ouinhi/Zangnanado",
      "health_zoneKouandé/ Pehunco/Kerou",
      "health_zoneLokossa/Athiémé",
      "health_zoneOuidah/Kpomassè/Tori",
      "health_zonePorto-Novo/ Sèmè-Kpodji/ Aguégués",
      "health_zoneZogbodomey/Bohicon/Zakpota",
      "number_of_births_2009",
      "women_in_labour_payoui",
      "pregnant_women_private_spaceoui",
      "doctor_or_nursing_and_midwifery_per_10000",
      "fetoscopeoui",
      "fetoscopeUnknown",
      "facility_typeFormer Communal Health Center",
      "facility_typeFormer District Health Center",
      "facility_typeZone Hospital",
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
      "Former Communal Health Center",
      "Former District Health Center",
      "Zone Hospital",
      "Midwife",
      "Other", 
      "Hours elapsed since 6AM" 
      ),
  ) +
  theme_manuscript() +
  theme(axis.text.y = element_text(size = 10), legend.title = element_text(size = 12)) +
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


