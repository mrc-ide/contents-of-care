library(broom)
library(dplyr)
library(ggforce)
library(ggplot2)
library(glmnet)
library(glue)
library(lubridate)
library(orderly2)
library(purrr)
library(rsample)
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





orderly_dependency(
  "process_drc_midline", "latest",
  files = c("drc_midline_dco.rds")
)

drc_dco_midline <- readRDS("drc_midline_dco.rds")
start_of_day <- hm("06:00")
drc_dco_midline$time_elapsed_since_start_of_day <- as.numeric(
  drc_dco_midline$start_time_of_consultation - start_of_day,
  units = "mins"
)

drc_midline_small <- select(
  drc_dco_midline, consult_length_calc,
  province,
  pregnancy_in_weeks, first_pregnancy, first_anc,
  trimester,
  hcw_sex, hcw_qualification,
  consultation_language,
  time_elapsed_since_start_of_day
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
drc_midline_split <- keep(drc_midline_split, function(x) nrow(x) >= 30)
drc_midline_split <- map(drc_midline_split, na.omit)
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
