library(broom)
library(dplyr)
library(ggforce)
library(ggplot2)
library(glue)
library(ggpmisc)
library(glmnet)
library(glue)
library(janitor)
library(lubridate)
library(orderly2)
library(purrr)
library(rsample)
library(scales)
library(tibble)
library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")



orderly_dependency("process_drc", "latest", "drc_baseline_split.rds")


set.seed(42)
orderly_resource("lm_drc_multilevel.R")
source("lm_drc_multilevel.R")


## ## Experiment: remove all covariates related to the facility
## ## and regress on facility_id only
## x <- drc_baseline_split$`yes_Third Trimester`
## model <- lm(
##   log_consult_length ~ as.factor(facility_id),
##   data = x
## )
## coeffs <- tidy(model, conf.int = TRUE)
## coeffs$significant <- coeffs$p.value < 0.05
## coeffs$facility_id <- as.numeric(gsub("as.factor\\(facility_id\\)", "", coeffs$term))
## ## Bring back the covariates_nice_names
## coeffs <- left_join(coeffs, x, by = "facility_id")
## coeffs <- arrange(coeffs, desc(estimate))
## coeffs$facility_id <- factor(
##   coeffs$facility_id,
##   levels = unique(coeffs$facility_id),
##   ordered = TRUE
## )
## coeffs <- filter(coeffs, !term %in% "(Intercept)")

## ggplot(coeffs, aes(as.factor(facility_id), estimate, col = doctor_or_nursing_and_midwifery_per_10000)) +
##   geom_point() +
##   geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
##   geom_hline(yintercept = 0, linetype = "dashed", col = "grey") +
##   labs(x = "Facility ID", y = "Estimate", color = "Significant") +
##   theme_manuscript() 

## Exclude the very small dataset as we get -Inf AIC
drc_baseline_split <-
  drc_baseline_split[names(drc_baseline_split) != "First ANC_First Trimester"]

models <- map(drc_baseline_split, function(x) {
  full_model <- lm(log_consult_length ~ (.), data = x)
  scope_terms <- terms(log_consult_length ~ (.), data = x)

  final <- step(
    full_model,
    direction = "backward", scope = scope_terms, trace = 1
  )
  list(initial = full_model, final = final)
})

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

delta_aic <- map_dfr(models, function(x) {
  aic_initial <- AIC(x$initial)
  aic_final <- AIC(x$final)
  data.frame(delta_aic = aic_initial - aic_final)
}, .id = "datacut") |>
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

delta_aic$delta_aic <- glue("Diff AIC = {round(delta_aic$delta_aic, 2)}")

referece_categories <- map_dfr(
  models, function(x) {
    xlevels <- x$final$xlevels
    data.frame(
      term = names(xlevels),
      reference = map_chr(xlevels, ~ .[1]),
      estimate = 0
    )
  }, .id = "datacut"
) |> 
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

 

p <- ggplot(coeffs[coeffs$term != "(Intercept)", ]) +
  geom_point(aes(estimate, term, col = significant)) +
  geom_errorbarh(
    aes(y = term, xmin = conf.low, xmax = conf.high, col = significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  facet_grid(
    first_anc ~ trimester,
    scales = "free"
  ) +
  theme_manuscript() +
  theme(axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  labs(x = "Estimate", color = "Significant") 


p1 <- p +
  geom_text_npc(data = r2_nobs, aes(npcx = 0.1, npcy = 0.1, label = label)) +
  geom_text_npc(
    data = delta_aic, aes(npcx = 0.1, npcy = 0.15, label = delta_aic)
  ) 

ggsave_manuscript("drc_2015_stepwise", p1, width = 12, height = 8)

