library(broom)
library(dplyr)
library(ggforce)
library(ggplot2)
library(ggpmisc)
library(glmnet)
library(glue)
library(lubridate)
library(orderly2)
library(purrr)
library(rsample)
library(scales)
library(tibble)
library(tidylog)
library(tidyr)



full_model <- function(df) {
  x_matrix <- model.matrix(log(consult_length_calc) ~ (
    province +
    consultation_language +
    ## Pregnancy characteristics
    pregnancy_in_weeks +
    factor(first_pregnancy) +
    ## HCW characteristics
    factor(hcw_qualification) +
    factor(hcw_sex) + 
    ## Appointment characteristics
    time_elapsed_since_start_of_day), data = df)[, -1]

  y_vec <- log(df$consult_length_calc)

  list(x_matrix = x_matrix, y_vec = y_vec)
}



orderly_shared_resource("utils.R")
source("utils.R")


orderly_dependency(
  "process_drc_midline", "latest",
  files = c("drc_midline_dco.rds")
)

drc_dco_midline <- readRDS("drc_midline_dco.rds")

drc_dco_midline$time_elapsed_since_start_of_day <- as.numeric(
  drc_dco_midline$start_time_of_consultation - start_of_day,
  units = "hour"
) |> round()

drc_midline_small <- select(
  drc_dco_midline, consult_length_calc,
  province,
  pregnancy_in_weeks, first_pregnancy, first_anc,
  trimester,
  hcw_sex, hcw_qualification,
  consultation_language,
  time_elapsed_since_start_of_day
)

drc_midline_small <- mutate_if(
  drc_midline_small, is.character, ~ ifelse(is.na(.), "Unknown", .)
)

## There is only one observation with language French
## So we will club it with "Other"
drc_midline_small$consultation_language <- recode(
  drc_midline_small$consultation_language,
  "Français" = "Other"
)

drc_midline_small$consultation_language <- recode(
  drc_midline_small$consultation_language,
  "Lingala" = "Other"
)

drc_midline_small$consultation_language <- recode(
  drc_midline_small$consultation_language,
  "Autre, à préciser" = "Other"
)


drc_midline_small$log_consult_length <- log(
  drc_midline_small$consult_length_calc)

set.seed(42)

drc_midline_split <- split(
  drc_midline_small,
  list(drc_dco_midline$first_anc, drc_dco_midline$trimester),
  sep = "_"
)

## Remove strata with less than 30 observations
map(drc_midline_split, nrow)
map(drc_midline_split, function(x) {
  map(x, ~ sum(is.na(.))) |> keep(~ . > 0)
})

##drc_midline_split <- keep(drc_midline_split, function(x) nrow(x) >= 30)
##drc_midline_split <- map(drc_midline_split, na.omit)

## Exclude because 5 observations
drc_midline_split <- drc_midline_split[names(drc_midline_split) != "Non_First Trimester"]

models <- map(drc_midline_split, function(x) {
  x <- select(x, -first_anc, -trimester, -consult_length_calc)
  insuff_levels <- map(x, ~ length(unique(.))) |> keep(~ . < 2)
  x <- select(x, -names(insuff_levels))
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

r2_nobs$nobs_label <- glue("n = {r2_nobs$nobs}")
r2_nobs$r2_label <- glue("R² = {percent(r2_nobs$r2)}")
r2_nobs$label <- glue("{r2_nobs$nobs_label}; {r2_nobs$r2_label}")

delta_aic <- map_dfr(models, function(x) {
  aic_initial <- AIC(x$initial)
  aic_final <- AIC(x$final)
  data.frame(delta_aic = aic_initial - aic_final)
}, .id = "datacut") |>
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

delta_aic$delta_aic <- glue("Diff AIC = {round(delta_aic$delta_aic, 2)}")

first_anc_labels <- c(
  "Oui" = "First ANC: Yes",
  "Non" = "First ANC: No"
)

coeffs$term <- factor(
  coeffs$term,
  levels = c("(Intercept)",
             "provincekl Kwilu Province",
             "provincemd Maindombe Province",             
             "hcw_qualificationNurse",
             "hcw_qualificationOther", 
             "consultation_languageOther",
             "time_elapsed_since_start_of_day"),
      labels = c("(Intercept)",
             "Kwilu Province",
             "Maindombe Province",             
             "Nurse",
             "HCW Qualification: Other", 
             "Consultation language: Other", 
             "Hours elapsed since 6AM"),
  ordered = TRUE
)
      
p <- ggplot(coeffs[coeffs$term != "(Intercept)", ]) +
  geom_point(aes(estimate, term, col = significant)) +
  geom_errorbarh(
    aes(y = term, xmin = conf.low, xmax = conf.high, col = significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  facet_grid(
    first_anc ~ trimester,
    labeller = labeller(.rows = c('Oui' = "First ANC: Yes", 'Non' = "First ANC: No"))
  ) +
  ##scale_y_reverse() +
  theme_manuscript() +
  theme(axis.text.y = element_text(size = 10), legend.title = element_text(size = 12)) +
  labs(x = "Estimate", color = "Significant") 


p1 <- p +
  geom_text_npc(data = r2_nobs, aes(npcx = 0.1, npcy = 0.1, label = label)) +
  geom_text_npc(
    data = delta_aic, aes(npcx = 0.1, npcy = 0.15, label = delta_aic)
  ) 

ggsave_manuscript("drc_2018_stepwise", p1, width = 12, height = 8)


##############################################
### 1. LASSO model with cross-validated lambda
##############################################
best_lambda <- map(drc_midline_split, function(x) {
  out <- full_model(x)
  cv_fit <- cv.glmnet(out$x_matrix, out$y_vec, alpha = 1, standardize = TRUE)
  bestl <- cv_fit$lambda.min
  bestl
})

lasso_fit <- map2(drc_midline_split, best_lambda, function(x, bestl) {
  if (nrow(x) == 0) return(NULL)
  cli::cli_alert("Retained rows {nrow(x)}")
  out <- full_model(x)
  glmnet(out$x_matrix, out$y_vec, alpha = 1, lambda = bestl)
})

saveRDS(lasso_fit, "drc_midline_dco_lasso_fits.rds")
orderly_artefact(
  files = "drc_midline_dco_lasso_fits.rds",
  description = "LASSO fits"
)

coefficients <- map_dfr(lasso_fit, function(x) {
  out <- as.data.frame(as.matrix(coef(x)))
  out <- tibble::rownames_to_column(out, "term")
  out
}, .id = "datacut")

coefficients <- separate(
  coefficients, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)


n_boot <- 1000

boot_samples_extra <- map(
  drc_midline_split, function(x) bootstraps(x, times = n_boot)
)

n_boot_to_keep <- 100

boot_samples <- imap(boot_samples_extra, function(boot, datacut) {
  cli::cli_alert("######### Bootstrap sample {datacut} ######################")
  thinned_samples <- map(1:n_boot, function(index) {
    split <- boot$splits[[index]]
    id <- boot$id[[index]]
    x <- analysis(split)
    nlevels <- sapply(
      c("province", 
        "consultation_language", "first_pregnancy", "pregnancy_in_weeks",
        "hcw_qualification", "hcw_sex"
      ), function(var) length(unique(x[[var]])) > 1
    )
    if (all(nlevels)) {
      return(list(split = split, id = id))
    } else {
      cli::cli_alert("Discarding bootstrap sample {index}")
      return(NULL)
    }
  })
  idx <- which(!map_lgl(thinned_samples, is.null))
  thinned_samples <- thinned_samples[idx]
  thinned_samples <- list(
    splits = map(thinned_samples, "split"), id = map(thinned_samples, "id")
  )
  cli::cli_alert("Retained {length(thinned_samples[[1]])} bootstrap samples")
  thinned_samples
})

boot_samples <- map(boot_samples, function(boot) {
  n <- length(boot$splits)
  if (n > n_boot_to_keep) {
    return(list(
      splits = boot$splits[1:n_boot_to_keep], id = boot$id[1:n_boot_to_keep]
    ))
  }
})

boot_fits <- map2(boot_samples, best_lambda, function(boot, bestl) {
  map(boot$splits, function(split) {
    x <- analysis(split)
    model <- full_model(x)
    glmnet(model$x_matrix, model$y_vec, alpha = 1, lambda = bestl)
  })
})

boot_coefs <- map_dfr(boot_fits, function(fits) {
  map_dfr(fits, function(fit) {
    out <- as.data.frame(as.matrix(coef(fit)))
    tibble::rownames_to_column(out, "term")
  })
} , .id = "datacut")   

boot_dev_ratio <- map_dfr(boot_fits, function(fits) {
  map_dfr(fits, function(fit) {
    data.frame(dev_ratio = fit$dev.ratio,df = fit$df)
  })
} , .id = "datacut")

dev_ratio_summary <- boot_dev_ratio |>
  group_by(datacut) |>
  summarise(
    mid = quantile(dev_ratio, 0.5),
    lower = quantile(dev_ratio, 0.025),
    upper = quantile(dev_ratio, 0.975)   
)

dev_ratio_summary <- separate(
  dev_ratio_summary, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

saveRDS(
  dev_ratio_summary, "drc_midline_dco_lasso_dev_ratio.rds"
)
orderly_artefact(
  files = "drc_midline_dco_lasso_dev_ratio.rds",
  description = "LASSO deviance ratio"
)

##############################################
# 3. Summarise bootstrapped estimates
##############################################
coeffs_summary <- boot_coefs |>
  group_by(datacut, term) |>
  summarise(
    mean = mean(s0),
    se = sd(s0) / sqrt(n() - 1),
    mid = quantile(s0, 0.5),
    lower = quantile(s0, 0.025),
    upper = quantile(s0, 0.975)   
  ) 
  

coeffs_summary <- separate(
  coeffs_summary, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

print(coeffs_summary)


orderly_shared_resource("utils.R")
source("utils.R")

x <- filter(coeffs_summary, term != "(Intercept)")

x$first_anc <- factor(
  x$first_anc,
  levels = c("Oui", "Non"),
  labels = c("First ANC", "Follow-up ANC"),
  ordered = TRUE
)

y <- filter(coefficients, term != "(Intercept)")
y$first_anc <- factor(
  y$first_anc,
  levels = c("Oui", "Non"),
  labels = c("First ANC", "Follow-up ANC"),
  ordered = TRUE
)



p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = x, aes(y = term, x = mid)) +
  geom_point(data = y, aes(y = term, x = s0), shape = 4) +
  geom_errorbarh(data = x, 
    aes(y = term, xmin = lower, xmax = upper),
    height = 0
  ) +
  facet_grid(trimester ~ first_anc) +
  scale_y_discrete(labels = covariates_nice_names) +
  theme_manuscript() +
  theme(
    axis.title.y = element_blank(), axis.title.x = element_text(size = 12)
  ) + xlab("Median and 95% CrI") 

outfile <- "drc_midline_dco_lasso_coefficients"
ggsave_manuscript(outfile, p, width = 9, height = 6)

orderly_artefact(
  files = glue("{outfile}.png"), description = "LASSO coefficients"
)
