library(broom)
library(dplyr)
library(ggplot2)
library(glmnet)
library(orderly2)
library(purrr)
library(rsample)
library(tibble)
library(tidyr)

boot_lasso <- function(df, best_lambda) {
  
  x_boot <- model.matrix(log_consult_length ~ (
    n_staff + catchment_pop_centered + m0_milieu + m_id2 +
      total_attendance_2009 +
      pregnant_women_private_space + number_of_maternal_deaths +
      women_in_labour_pay +
      ## HCW characteristics
      m0_id8 +
      ## Patient characteristics
      trimester +
      ## Appointment characteristics
      hour_start), data = df)[, -1]
  y_boot <- df_boot$log_consult_length

  # Fit LASSO using fixed lambda from original CV
  fit <- glmnet(x_boot, y_boot, alpha = 1, lambda = best_lambda)
  fit
}


orderly_dependency("process_benin", "latest", files = c("benin_dco.rds"))
benin_dco <- readRDS("benin_dco.rds")
benin_small <- select(
  benin_dco, consult_length, m0_milieu, m_id2, facility_type, facility_status,
  n_staff, catchment_pop, pregnant_women_private_space, number_of_maternal_deaths,
  total_attendance_2009,
  women_in_labour_pay, m0_id8, first_anc, trimester, hour_start
)
benin_small$log_consult_length <- log(benin_small$consult_length)
## We will center some variables to enhance
## interpretability of coefficients.

benin_small$catchment_pop_centered <- scale(
  benin_small$catchment_pop, center = TRUE, scale = FALSE
)

set.seed(42)

## Stratify by facility type and first ANC
benin_split <- split(
  benin_small, list(benin_small$facility_type, benin_small$first_anc),
  sep = "_"
)

## Remove strata with less than 30 observations
benin_split <- keep(benin_split, function(x) nrow(x) >= 30)


##############################################
### 1. LASSO model with cross-validated lambda
##############################################
lasso_fit <- map(benin_split, function(x) {
  x <- x[complete.cases(x), ]
  cli::cli_alert("Retained rows {nrow(x)}")
  x_matrix <- model.matrix(log_consult_length ~ (
    n_staff + catchment_pop_centered + m0_milieu + m_id2 +
      total_attendance_2009 +
      pregnant_women_private_space + number_of_maternal_deaths +
      women_in_labour_pay +
      ## HCW characteristics
      m0_id8 +
      ## Patient characteristics
      trimester +
      ## Appointment characteristics
      hour_start), data = x)[, -1]

  y_vec <- x$log_consult_length

  cv_fit <- cv.glmnet(x_matrix, y_vec, alpha = 1, standardize = TRUE)
  bestl <- cv_fit$lambda.min

  out <- glmnet(x_matrix, y_vec, alpha = 1, lambda = bestl)
  out
})

##############################################
# 2. Bootstrapping with rsample
##############################################
n_boot <- 100

boot_samples <- bootstraps(benin_small, times = n_boot)


boot_coefs <- map_dfr(boot_samples$splits, boot_lasso)

# ================================
# 5. Summarise bootstrapped estimates
# ================================
summary_table <- boot_coefs %>%
  pivot_longer(cols = everything(), names_to = "term", values_to = "estimate") %>%
  group_by(term) %>%
  summarise(
    mean = mean(estimate),
    sd = sd(estimate),
    lower = quantile(estimate, 0.025),
    upper = quantile(estimate, 0.975),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(mean)))

print(summary_table)

# ================================
# 6. Visualise bootstrapped intervals
# ================================
ggplot(summary_table, aes(x = reorder(term, abs(mean)), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(
    title = "Bootstrapped LASSO Coefficients",
    x = "Predictor",
    y = "Estimate (with 95% CI)"
  )


# Predict on the original dataset
log_predicted_y <- predict(lasso_fit, newx = x_matrix)
predicted_y <- exp(log_predicted_y)
# Compare to actual values

benin_small$predicted_consult_length <- predicted_y
  
library(ggplot2)

ggplot(benin_small, aes(x = log_consult_length, y = log(predicted_consult_length))) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Values from LASSO",
    x = "Actual y",
    y = "Predicted y"
  ) +
  theme_minimal() + coord_fixed()

rmse <- sqrt(mean((benin_small$log_consult_length - log_predicted_y)^2))
r_squared <- cor(benin_small$log_consult_length, log_predicted_y)^2

cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")
