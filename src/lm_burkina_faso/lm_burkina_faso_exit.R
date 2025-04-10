library(broom)
library(dplyr)
library(ggplot2)
library(glmnet)
library(orderly2)
library(purrr)
library(rsample)
library(tibble)
library(tidylog)
library(tidyr)



full_model <- function(df) {
  x_matrix <- model.matrix(log(consult_length) ~ (
    patient_age_centered + 
    factor(patient_can_read_write) + patient_highest_education + 
    patient_partner_highest_education +
    num_of_hcws_seen + num_of_weeks_pregnant_an_book +
    first_pregnancy + patient_residence_travel_time +
    patient_wait_time + total_hf_fees + patient_seen_chw
  ), data = df)[, -1]

  y_vec <- log(df$consult_length)

  list(x_matrix = x_matrix, y_vec = y_vec)
}





orderly_dependency(
  "process_burkina_faso", "latest", files = c("bfa_baseline_exit.rds")
)

bfa_baseline_exit <- readRDS("bfa_baseline_exit.rds")
## num_of_prev_anc_visits is NA for 535 patients; 
## Of these, 65 are in First trimester, which has only 71 observations for CSPS.
## So we end up removing most of First trimester patients from CSPS.


bfa_baseline_small <- select(
  bfa_baseline_exit, consult_length, facility_level, trimester, first_anc,
  patient_age_centered,
  patient_can_read_write, patient_highest_education,
  patient_partner_highest_education,
  num_of_hcws_seen, num_of_weeks_pregnant_an_book,
  first_pregnancy, patient_residence_travel_time,
  patient_wait_time, total_hf_fees, patient_seen_chw
)

bfa_baseline_small$log_consult_length <- log(bfa_baseline_small$consult_length)

set.seed(42)

## Stratify by facility type and first ANC; 97% of the observations are from government facilities
bfa_baseline_split <- split(
  bfa_baseline_small,
  list(
    bfa_baseline_exit$facility_level, bfa_baseline_exit$trimester,
    bfa_baseline_exit$first_anc
  ),
  sep = "_"
)

## Remove strata with less than 30 observations
map(bfa_baseline_split, nrow)


bfa_baseline_split <- map(bfa_baseline_split, na.omit)
bfa_baseline_split <- keep(bfa_baseline_split, function(x) nrow(x) >= 30)

##############################################
### 1. LASSO model with cross-validated lambda
##############################################
best_lambda <- map(bfa_baseline_split, function(x) {
  out <- full_model(x)
  cv_fit <- cv.glmnet(out$x_matrix, out$y_vec, alpha = 1, standardize = TRUE)
  bestl <- cv_fit$lambda.min
  bestl
})

lasso_fit <- map2(bfa_baseline_split, best_lambda, function(x, bestl) {
  if (nrow(x) == 0) return(NULL)
  cli::cli_alert("Retained rows {nrow(x)}")
  out <- full_model(x)
  glmnet(out$x_matrix, out$y_vec, alpha = 1, lambda = bestl)
})

coefficients <- map_dfr(lasso_fit, function(x) {
  out <- as.data.frame(as.matrix(coef(x)))
  out <- tibble::rownames_to_column(out, "term")
  out
}, .id = "datacut")

##############################################
# 2. Bootstrapping with rsample
##############################################
## Take a lot more samples and keep some; discarding those where not all
## categorical variables have at least 2 levels present
n_boot <- 10000

boot_samples_extra <- map(
  bfa_baseline_split, function(x) bootstraps(x, times = n_boot)
)

n_boot_to_keep <- 100

boot_samples <- imap(boot_samples_extra, function(boot, datacut) {
  cli::cli_alert("######### Bootstrap sample {datacut} ######################")
  thinned_samples <- map(1:n_boot, function(index) {
    split <- boot$splits[[index]]
    id <- boot$id[[index]]
    x <- analysis(split)
    nlevels <- sapply(
      c("patient_can_read_write", "patient_highest_education",
        "patient_partner_highest_education", "first_pregnancy",
        "patient_seen_chw"
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
  }})
  


boot_coefs <- map2_dfr(boot_samples, best_lambda, function(boot, bestl) {
  imap_dfr(boot$splits, function(split, index) {
    cli::cli_alert("Bootstrap sample {index}")
    x <- analysis(split)
    skimr::skim(x)
    model <- full_model(x)
    fit <- glmnet(model$x_matrix, model$y_vec, alpha = 1, lambda = bestl)
    out <- as.data.frame(as.matrix(coef(fit)))
    out <- tibble::rownames_to_column(out, "term")
    out
  })}, .id = "datacut")

##############################################
# 3. Summarise bootstrapped estimates
##############################################
summary_table <- boot_coefs %>%
  group_by(datacut, term) %>%
  summarise(
    mean = mean(s0),
    se = sd(s0) / sqrt(n() - 1),
    lower = quantile(s0, 0.025),
    upper = quantile(s0, 0.975)   
  ) %>%
  arrange(desc(abs(mean)))

print(summary_table)

##############################################
### 4. RMSE and R-squared
##############################################
predicted_y <- map2(lasso_fit, bfa_baseline_split, function(fit, x) {
  if (nrow(x) == 0) return(NULL)
  out <- full_model(x)
  predict(fit, newx = out$x_matrix)
})

rmse <- map2_dbl(predicted_y, bfa_baseline_split, function(pred, x) {
  if (nrow(x) == 0) {
    return(NULL)
  }
  sqrt(mean((x$log_consult_length - pred)^2))
})

r_squared <- map2_dbl(predicted_y, bfa_baseline_split, function(pred, x) {
  if (nrow(x) == 0) {
    return(NULL)
  }
  cor(x$log_consult_length, pred)^2
})

summary_table <- separate(
  summary_table, datacut,
  into = c("facility_type", "first_anc", "trimester"), sep = "_"
)

r_squared <- tidy(r_squared)

r_squared <- separate(
  r_squared, names,
  into = c("facility_type", "first_anc", "trimester"), sep = "_"
)
r_squared$x <- scales::percent(r_squared$x)
##############################################
# 5. Visualise bootstrapped intervals
##############################################
x <- filter(summary_table, term != "(Intercept)")

p <- ggplot(x, aes(y = term, x = mean)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = mean - se, xmax = mean + se), height = 0) +
  geom_point() +
  facet_grid(trimester ~ first_anc) +
  theme_minimal() +
  labs(
    title = "Bootstrapped LASSO Coefficients",
    y = "Predictor",
    x = "Mean +/- SE"
  )

ggsave_manuscript("bfa_lasso_bootstrapped", p, width = 10, height = 6)
