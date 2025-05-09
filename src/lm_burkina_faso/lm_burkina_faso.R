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
    factor(region) +
    doctor_or_nursing_and_midwifery_per_10000 + 
      ## HCW characteristics
    factor(hcw_qualification) +
    factor(hcw_sex) + 
      ## Patient characteristics
      ## Appointment characteristics
      time_elapsed_since_start_of_day), data = df)[, -1]

  y_vec <- log(df$consult_length)

  list(x_matrix = x_matrix, y_vec = y_vec)
}





orderly_dependency("process_burkina_faso", "latest", files = c("bfa_dco.rds"))
bfa_baseline_dco <- readRDS("bfa_dco.rds")


bfa_baseline_small <- select(
  bfa_baseline_dco, consult_length, facility_type, hcw_sex, hcw_qualification,
  doctor_or_nursing_and_midwifery_per_10000,
  first_anc, trimester, time_elapsed_since_start_of_day, region = `REGION.x`
)

bfa_baseline_small$log_consult_length <- log(bfa_baseline_small$consult_length)

set.seed(42)

## Stratify by facility type and first ANC; 97% of the observations are from government facilities
bfa_baseline_split <- split(
  bfa_baseline_small,
  list(bfa_baseline_dco$facility_type, bfa_baseline_dco$first_anc,
       bfa_baseline_dco$trimester),
  sep = "_"
)

## Remove strata with less than 30 observations
map(bfa_baseline_split, nrow)
bfa_baseline_split <- keep(bfa_baseline_split, function(x) nrow(x) >= 30)
bfa_baseline_split <- map(bfa_baseline_split, na.omit)
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
n_boot <- 100

boot_samples <- map(bfa_baseline_split, function(x) bootstraps(x, times = n_boot))

boot_coefs <- map2_dfr(boot_samples, best_lambda, function(boot, bestl) {
  map_dfr(boot$splits, function(split) {
    x <- analysis(split)
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
p <- ggplot(summary_table, aes(y = term, x = mean)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = mean - se, xmax = mean + se), height = 0) +
  geom_point() +
  facet_grid(trimester ~ first_anc) +
  theme_manuscript() +
  labs(
    title = "Bootstrapped LASSO Coefficients",
    y = "Predictor",
    x = "Mean +/- SE"
  )

ggsave_manuscript("bfa_lasso_bootstrapped", p, width = 10, height = 6)
