full_model <- function(df) {
  x_matrix <- model.matrix(log(consult_length_calc) ~ (
    province +
      milieu_of_residence +
      facility_status +
      consultation_language +
      ## Pregnancy characteristics
      pregnancy_in_weeks +
      first_pregnancy +
      ## HCW characteristics
      hcw_qualification +
      hcw_sex +
      ## Appointment characteristics
      time_elapsed_since_start_of_day
  ), data = df)[, -1]

  y_vec <- log(df$consult_length_calc)

  list(x_matrix = x_matrix, y_vec = y_vec)
}

##############################################
### 1. LASSO model with cross-validated lambda
##############################################
best_lambda <- map(drc_baseline_split, function(x) {
  out <- full_model(x)
  cv_fit <- cv.glmnet(out$x_matrix, out$y_vec, alpha = 1, standardize = TRUE)
  bestl <- cv_fit$lambda.min
  bestl
})

lasso_fit <- map2(drc_baseline_split, best_lambda, function(x, bestl) {
  if (nrow(x) == 0) {
    return(NULL)
  }
  cli::cli_alert("Retained rows {nrow(x)}")
  out <- full_model(x)
  glmnet(out$x_matrix, out$y_vec, alpha = 1, lambda = bestl)
})

saveRDS(lasso_fit, "drc_2015_dco_lasso_fits.rds")
orderly_artefact(
  files = "drc_2015_dco_lasso_fits.rds",
  description = "LASSO fits"
)


coefficients <- map_dfr(lasso_fit, function(x) {
  out <- as.data.frame(as.matrix(coef(x)))
  out <- tibble::rownames_to_column(out, "term")
  out
}, .id = "datacut")

##############################################
# 2. Bootstrapping with rsample
##############################################
n_boot <- 10000

boot_samples_extra <- map(
  drc_baseline_split, function(x) bootstraps(x, times = n_boot)
)

n_boot_to_keep <- 100

boot_samples <- imap(boot_samples_extra, function(boot, datacut) {
  cli::cli_alert("######### Bootstrap sample {datacut} ######################")
  thinned_samples <- map(1:n_boot, function(index) {
    split <- boot$splits[[index]]
    id <- boot$id[[index]]
    x <- analysis(split)
    nlevels <- sapply(
      c("province", "milieu_of_residence",
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
summary_table <- boot_coefs |>
  group_by(datacut, term) |>
  summarise(
    mean = mean(s0),
    se = sd(s0) / sqrt(n() - 1),
    mid = quantile(s0, 0.5),
    lower = quantile(s0, 0.025),
    upper = quantile(s0, 0.975)   
  ) 


summary_table <- separate(
  summary_table, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

coefficients <- separate(
  coefficients, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)


x <- filter(summary_table, term != "(Intercept)")
x$first_anc <- factor(
  x$first_anc,
  levels = c("yes", "no"),
  labels = c("First ANC", "Follow-up ANC"),
  ordered = TRUE
)

y <- filter(coefficients, term != "(Intercept)")
y$first_anc <- factor(
  y$first_anc,
  levels = c("yes", "no"),
  labels = c("First ANC", "Follow-up ANC"),
  ordered = TRUE
)

p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = x, aes(y = term, x = mid)) +
  geom_errorbarh(data = x, 
    aes(y = term, xmin = lower, xmax = upper),
    height = 0
    ) +
  geom_point(data = y, aes(y = term, x = s0), shape = 4) +
  scale_y_discrete(labels = covariates_nice_names) +
  facet_grid(
     trimester ~ first_anc
    ##labeller = labeller(.rows = label_both, .cols = label_value)
  ) +
  theme_manuscript() +
  theme(
    axis.title.y = element_blank(),
  ) +
  labs(
    title = "Bootstrapped LASSO Coefficients"
  ) 

outfile <- "drc_2015_dco_lasso_coefficients"
ggsave_manuscript(outfile, p, width = 12, height = 8)

orderly_artefact(
  files = glue("{outfile}.png"), description = "LASSO coefficients"
)
