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
  x_matrix <- model.matrix(consult_length ~ (
    doctor_or_nursing_and_midwifery_per_10000 + m0_milieu + m_id2 +
      women_in_labour_pay +
      ## HCW characteristics
      factor(m0_id8) +
      ## Patient characteristics
      trimester +
      ## Appointment characteristics
      time_elapsed_since_start_of_day), data = df)[, -1]

  y_vec <- df$consult_length

  list(x_matrix = x_matrix, y_vec = y_vec)
}



orderly_dependency("process_benin", "latest", files = c("benin_dco.rds"))
benin_dco <- readRDS("benin_dco.rds")

benin_small <- select(
  benin_dco, consult_length, m0_milieu, m_id2, facility_type, facility_status,
  pregnant_women_private_space, doctor_or_nursing_and_midwifery_per_10000,
  women_in_labour_pay, m0_id8, first_anc, trimester, time_elapsed_since_start_of_day
)

benin_small$log_consult_length <- log(benin_small$consult_length)

set.seed(42)

## Stratify by facility type and first ANC
benin_split <- split(
  benin_small, list(benin_small$facility_type, benin_small$first_anc),
  sep = "_"
)

## Remove strata with less than 30 observations
map(benin_split, nrow)
benin_split <- keep(benin_split, function(x) nrow(x) >= 30)
## Remove NAs
benin_split <- map(benin_split, na.omit)

##############################################
### 1. LASSO model with cross-validated lambda
##############################################
best_lambda <- map(benin_split, function(x) {
  out <- full_model(x)
  cv_fit <- cv.glmnet(out$x_matrix, out$y_vec, alpha = 1, standardize = TRUE)
  bestl <- cv_fit$lambda.min
  bestl
})

lasso_fit <- map2(benin_split, best_lambda, function(x, bestl) {

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

boot_samples <- map(benin_split, function(x) bootstraps(x, times = n_boot))

boot_coefs <- map2_dfr(boot_samples, best_lambda, function(boot, bestl) {
  map_dfr(boot$splits, function(split) {
    x <- analysis(split)
    model <- full_model(x)
    fit <- glmnet(model$x_matrix, model$y_vec, alpha = 1, lambda = bestl)
    out <- as.data.frame(as.matrix(coef(fit)))
    out <- tibble::rownames_to_column(out, "term")
    out
  })
}, .id = "datacut")

##############################################
# 3. Summarise bootstrapped estimates
##############################################
summary_table <- boot_coefs %>%
  group_by(datacut, term) %>%
  summarise(
    mean = mean(s0),
    sd = sd(s0),
    lower = quantile(s0, 0.025),
    upper = quantile(s0, 0.975)   
  ) %>%
  arrange(desc(abs(mean)))

print(summary_table)

##############################################
### 4. RMSE and R-squared
##############################################
predicted_y <- map2(lasso_fit, benin_split, function(fit, x) {
  if (nrow(x) == 0) return(NULL)
  out <- full_model(x)
  predict(fit, newx = out$x_matrix)
})

rmse <- map2_dbl(predicted_y, benin_split, function(pred, x) {
  if (nrow(x) == 0) {
    return(NULL)
  }
  sqrt(mean((x$consult_length - pred)^2))
})

r_squared <- map2_dbl(predicted_y, benin_split, function(pred, x) {
  if (nrow(x) == 0) {
    return(NULL)
  }
  cor(x$consult_length, pred)^2
})

##############################################
# 5. Visualise bootstrapped intervals
##############################################
ggplot(summary_table, aes(y = term, x = mean)) +
  geom_point() +
  facet_wrap(~datacut) +
  labs(
    title = "Bootstrapped LASSO Coefficients",
    y = "Predictor",
    x = "Estimate (with 95% CI)"
  )
