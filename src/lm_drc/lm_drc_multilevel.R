library(brms)

formula <- bf(
  log(consult_length_calc) ~
    ## milieu_of_residence + ## Removed because of class imbalance
    facility_status +
    ## consultation_language + ## Removed because of weak global signal
    pregnancy_in_weeks +
    first_pregnancy +
    hcw_qualification +
    hcw_sex +
    time_elapsed_since_start_of_day +
    (1 + consultation_language + facility_status | province)
)

drc_baseline_split <- split(
  drc_baseline_small,
  list(
    drc_dco_2015$first_anc, drc_dco_2015$trimester
  ),
  sep = "_"
)


fits <- map(drc_baseline_split, function(x) {
  brm(
  formula = formula,
  data = x,
  family = gaussian(), 
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.95)
)})




coefs <- map_dfr(fits, function(fit) {
  x <- as.data.frame(posterior_summary(fit, probs = c(0.025, 0.5, 0.975)))
  tibble::rownames_to_column(x)
}, .id = "datacut")


coefs <- separate(
  coefs, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

orderly_shared_resource("utils.R")
source("utils.R")


idx <- grepl(unique(coefs$rowname), pattern = "^b_")
x <- filter(
  coefs, rowname %in% unique(coefs$rowname)[idx]
)
x <- filter(x, rowname != "b_Intercept")
x$first_anc <- factor(
  x$first_anc,
  levels = c("yes", "no"),
  labels = c("First ANC", "Follow-up ANC"),
  ordered = TRUE
)




p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = x, aes(y = rowname, x = Q50)) +
  geom_errorbarh(
    data = x,
    aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
    height = 0
  ) +
  facet_grid(
    trimester ~ first_anc, scales = "free",
    ## labeller = labeller(.rows = label_both, .cols = label_value)
  ) +
  scale_y_discrete(
    breaks = c(
      "b_facility_statusPublic",
      "b_consultation_languageLingala",
      "b_consultation_languageOther",
      "b_consultation_languageSwahili",
      "b_pregnancy_in_weeks",
      "b_first_pregnancyno",
      "b_hcw_qualificationMidwifeDObstetrician",
      "b_hcw_qualificationNurse",
      "b_hcw_qualificationOther",
      "b_hcw_sexMale",
      "b_time_elapsed_since_start_of_day"
    ),
    labels = c(
      "HF Public",
      "Consultation Language: Lingala",
      "Consultation Language: Other",
      "Consultation Language: Swahili",
      "Pregnancy in weeks",
      "First Pregnancy: No",
      "HCW Qualification: Midwife/Obstetrician",
      "HCW Qualification: Nurse",
      "HCW Qualification: Other",
      "HCW Sex: Male",
      "Time Elapsed Since Start of Day"
    )
  ) +
  theme_manuscript() +
  theme(
    axis.title.y = element_blank(),
  ) 

map(fits, bayes_R2)





