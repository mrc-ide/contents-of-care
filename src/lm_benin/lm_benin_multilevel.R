library(brms)

formula <- bf(
  log(consult_length) ~
    m0_milieu +
    doctor_or_nursing_and_midwifery_per_10000 +
    women_in_labour_pay +
    pregnant_women_private_space +
    number_of_births_2009 +
    fetoscope +
    facility_type +
    hcw_qualification +
    time_elapsed_since_start_of_day +
    (1 + facility_type | health_zone)
)

fits <- map(benin_split, function(x) {
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


coefs <- separate(coefs, datacut, into = c("first_anc", "trimester"), sep = "_")



idx <- grepl(unique(coefs$rowname), pattern = "^b_")
x <- filter(
  coefs, rowname %in% unique(coefs$rowname)[idx]
)
x <- filter(x, rowname != "b_Intercept")

x$first_anc <- factor(
  x$first_anc,
  levels = c("oui", "non"),
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
  theme_manuscript() +
  theme(axis.title.y = element_blank())

p <- p +
      scale_y_discrete(
    breaks = paste0("b_", c(
      "doctor_or_nursing_and_midwifery_per_10000", "fetoscopeoui",
      "facility_typeFormerDistrictHealthCenter", "facility_typeZoneHospital",
      "hcw_qualificationMidwife", "hcw_qualificationOther",
      "health_zoneBanikoara", 
      "health_zoneCovè/Ouinhi/Zangnanado",
      "health_zoneKouandé/ Pehunco/Kerou", 
      "health_zoneLokossa/Athiémé",
      "health_zoneOuidah/Kpomassè/Tori", 
      "health_zonePorto-Novo/ Sèmè-Kpodji/ Aguégués",
      "health_zoneZogbodomey/Bohicon/Zakpota", 
      "m0_milieuUrban", "number_of_births_2009",
      "pregnant_women_private_spaceoui", 
      "time_elapsed_since_start_of_day", "women_in_labour_payoui")),
    labels = c(
      "Doctors/N&M per 10000",
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
  )

ggsave_manuscript(
  p,
  filename = "benin_dco_bayes_coefs",
  width = 12, height = 8
)
orderly_artefact(
  files = "benin_dco_bayes_coefs.png",
  description = "Bayes coefficients for Benin 2010 DCO model fits"
)


bayes_r2 <- map(fits, bayes_R2)
saveRDS(bayes_r2, file = "benin_dco_bayes_r2.rds")

orderly_artefact(
  files = "benin_dco_bayes_r2.rds",
  description = "Bayes R2 for DRC 2015 DCO model fits"
)







