full_model <- function(df) {
  x_matrix <- model.matrix(log(consult_length) ~ (
    doctor_or_nursing_and_midwifery_per_10000 + m0_milieu + health_zone +
    facility_type + 
      women_in_labour_pay +
      pregnant_women_private_space +
      number_of_births_2009 +
      fetoscope +
      ## HCW characteristics
      hcw_qualification +
      ## Appointment characteristics
      time_elapsed_since_start_of_day), data = df)[, -1]

  y_vec <- df$log_consult_length

  list(x_matrix = x_matrix, y_vec = y_vec)
}



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

saveRDS(lasso_fit, file = "benin_dco_lasso_fit.rds")
orderly_artefact(
  files = "benin_dco_lasso_fit.rds",
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
  benin_split, function(x) bootstraps(x, times = n_boot)
)

n_boot_to_keep <- 100

boot_samples <- imap(boot_samples_extra, function(boot, datacut) {
  cli::cli_alert("######### Bootstrap sample {datacut} ######################")
  thinned_samples <- map(1:n_boot, function(index) {
    split <- boot$splits[[index]]
    id <- boot$id[[index]]
    x <- analysis(split)
    nlevels <- sapply(
      c("health_zone", "m0_milieu", "women_in_labour_pay",
        "pregnant_women_private_space", "fetoscope",
        "hcw_qualification", "facility_type"
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
  })
}, .id = "datacut")

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


x <- filter(summary_table, term != "(Intercept)")

x$trimester <- factor(
  x$trimester,
  levels = c("First Trimester", "Second Trimester", "Third Trimester"),
  ordered = TRUE
)

x$first_anc <- factor(
  x$first_anc,
  levels = c("oui", "non"),
  labels = c("First ANC", "Follow-up ANC"),
  ordered = TRUE
)

p <- ggplot(x, aes(y = term, x = mid)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0
  ) +
  geom_point() +
  facet_grid(trimester ~ first_anc) +
  scale_y_discrete(
    breaks = c(
      "doctor_or_nursing_and_midwifery_per_10000", "fetoscopeoui",
      "facility_typeFormer District Health Center", "facility_typeZone Hospital",
      "hcw_qualificationMidwife", "hcw_qualificationOther",
      "health_zoneBanikoara", 
      "health_zoneCovè/Ouinhi/Zangnanado", "health_zoneKouandé/ Pehunco/Kerou", 
      "health_zoneLokossa/Athiémé", "health_zoneOuidah/Kpomassè/Tori", 
      "health_zonePorto-Novo/ Sèmè-Kpodji/ Aguégués", "health_zoneZogbodomey/Bohicon/Zakpota", 
      "m0_milieuUrban", "number_of_births_2009", "pregnant_women_private_spaceoui", 
      "time_elapsed_since_start_of_day", "women_in_labour_payoui"),
    labels = c(
      "Doctors/_N&M per 10000",
      "Facility has fetoscope",
      "Former District Health Center", "Zone Hospital",
      "HCW qualification: Midwife",
      "HCW qualification: Other",
      "Banikoara",
      "Covè/Ouinhi/Zangnanado", "Kouandé/ Pehunco/Kerou",
      "Lokossa/Athiémé", "Ouidah/Kpomassè/Tori",
      "Porto-Novo/ Sèmè-Kpodji/ Aguégués", "Zogbodomey/Bohicon/Zakpota",
      "Urban", "Number of births in HF",
      "Pregnant women have private space",
      "Time elapsed since 8AM",
      "Women in labour pay"
    )
  ) +
  theme_manuscript() +
  labs(
    title = "Bootstrapped LASSO Coefficients (Benin)",
  )

outfile <- "benin_dco_lasso_coefficients"
ggsave_manuscript(outfile, p, width = 12, height = 9)

orderly_artefact(
  files = glue("{outfile}.png"), description = "LASSO coefficients"
)
