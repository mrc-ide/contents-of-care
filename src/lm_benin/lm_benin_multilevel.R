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
    (1 | health_zone)
)

fits <- map(benin_split, function(x) {
  X <- model.matrix(
    ~ m0_milieu +
      doctor_or_nursing_and_midwifery_per_10000 +
      women_in_labour_pay +
      pregnant_women_private_space +
      number_of_births_2009 +
      fetoscope +
      facility_type +
      hcw_qualification +
      time_elapsed_since_start_of_day,
    data = x,
    contrasts.arg = contrasts_list
  )

  df_brms <- data.frame(
    y = log(x$consult_length),
    facility_type = x$facility_type,
    health_zone = x$health_zone
  )
  df_brms <- cbind(df_brms, as.data.frame(X[, -1]))

  brm(
    formula = bf(y ~ . + (1| health_zone)),
    data = df_brms,
    family = gaussian(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 4,
    iter = 4000,
    priors <- prior(normal(0, 1), class = "b"),
    control = list(adapt_delta = 0.99)
  )
})


fixed_effects <- map_dfr(fits, function(fit) {
  x <- as.data.frame(fixef(fit, probs = c(0.025, 0.5, 0.975)))
  tibble::rownames_to_column(x)
}, .id = "datacut")

fixed_effects <- separate(
  fixed_effects, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

x <- filter(fixed_effects, ! rowname %in% "Intercept")


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
        breaks = c(
          "facility_typeFormerCommunalHealthCenter",
          "facility_typeFormerDistrictHealthCenter",
          "facility_typeZoneHospital",
          "health_zoneBanikoara",
          "health_zoneCovèDOuinhiDZangnanado",
          "health_zoneKouandéDPehuncoDKerou",
          "health_zoneLokossaDAthiémé",
          "health_zoneOuidahDKpomassèDTori",
          "health_zonePortoMNovoDSèmèMKpodjiDAguégués",
          "health_zoneZogbodomeyDBohiconDZakpota",
          "m0_milieu_unknown",
          "m0_milieu_urban",
          "doctor_or_nursing_and_midwifery_per_10000",
          "women_in_labour_payoui",
          "pregnant_women_private_spaceoui",
          "number_of_births_2009",
          "fetoscopeoui",
          "fetoscope_unknown",
          "facility_type_former_communal_health_center", "facility_type_former_district_health_center",
          "facility_type_zone_hospital", "hcw_qualification_midwife", "hcw_qualification_other",
          "time_elapsed_since_start_of_day"
        ),
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







