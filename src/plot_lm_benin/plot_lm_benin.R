library(brms)
library(dplyr)
library(ggplot2)
library(orderly2)
library(performance)
library(posterior)
library(purrr)

library(tidyr)

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency("lm_benin", "latest", files = c("benin_dco_fits.rds"))
fits <- readRDS("benin_dco_fits.rds")

fixed_effects <- map_dfr(fits, function(fit) {
  x <- as.data.frame(fixef(fit, probs = c(0.025, 0.5, 0.975)))
  tibble::rownames_to_column(x)
}, .id = "datacut")

fixed_effects <- separate(
  fixed_effects, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

saveRDS(fixed_effects, file = "benin_dco_bayes_fixed_effects.rds")
orderly_artefact(
  files = "benin_dco_bayes_fixed_effects.rds",
  description = "Fixed effects for DRC 2015 DCO model fits"
)

x <- filter(fixed_effects, !rowname %in% "Intercept")

x$first_anc <- factor(
  x$first_anc,
  levels = c("oui", "non"),
  labels = c("First ANC", "Follow-up ANC"),
  ordered = TRUE
)

breaks <- c(
  "health_zoneBanikoara",
  "health_zoneCovèDOuinhiDZangnanado",
  "health_zoneKouandéDPehuncoDKerou",
  "health_zoneLokossaDAthiémé",
  "health_zoneOuidahDKpomassèDTori",
  "health_zonePortoMNovoDSèmèMKpodjiDAguégués",
  "health_zoneZogbodomeyDBohiconDZakpota",
  "m0_milieuUrban",
  "m0_milieuUnknown",
  "facility_typeformer_communal_health_center",
  "facility_typeformer_district_health_center",
  "facility_typezone_hospital",
  "facility_statusSemiMpublic",
  "women_in_labour_payoui",
  "pregnant_women_private_spaceoui",
  "fetoscopeoui",
  "fetoscopeUnknown",
  "number_of_births_2009",
  "doctor_or_nursing_and_midwifery_per_10000",
  "hcw_qualificationMidwife",
  "hcw_qualificationOther",
  "time_elapsed_since_start_of_day"
)
x$rowname <- factor(x$rowname, levels = breaks, ordered = TRUE)

labels <- c(
  "Banikoara",
  "CovèDOuinhiDZangnanado",
  "KouandéDPehuncoDKerou",
  "LokossaDAthiémé",
  "OuidahDKpomassèDTori",
  "PortoMNovoDSèmèMKpodjiDAguégués",
  "ZogbodomeyDBohiconDZakpota",
  "Urban",
  "Unknown",
  "Former communal health center",
  "Former district health_center",
  "Zone hospital",
  "Semi-public",
  "Women in labour pay",
  "Pregnant women private space",
  "Fetoscope: yes",
  "Fetoscope: unknown",
  "Number of births in 2009",
  "Doctor/N&M per 10000",
  "HCW:Midwife",
  "HCW:Other",
  "Hours since 6AM"
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
    trimester ~ first_anc,
    scales = "free",
    ## labeller = labeller(.rows = label_both, .cols = label_value)
  ) +
  theme_manuscript() +
  theme(axis.title.y = element_blank())

p <- p + scale_y_discrete(breaks = breaks, labels = labels)


ggsave_manuscript(
  p,
  file = "benin_dco_bayes_fixed_effects",
  width = 8, height = 10
)

## Random effects
ran_effects <- map_dfr(fits, function(fit) {
  x <- ranef(fit, probs = c(0.025, 0.5, 0.975))
  x <- as.data.frame(x$health_zone[, , "Intercept"])
  tibble::rownames_to_column(x)
}, .id = "datacut")

ran_effects <- separate(
  ran_effects, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

ran_effects$first_anc <- factor(
  ran_effects$first_anc,
  levels = c("oui", "non"),
  labels = c("First ANC", "Follow-up ANC"),
  ordered = TRUE
)

saveRDS(ran_effects, file = "benin_dco_bayes_random_effects.rds")
orderly_artefact(
  files = "benin_dco_bayes_random_effects.rds",
  description = "Random effects for DRC 2015 DCO model fits"
)

p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = ran_effects, aes(y = rowname, x = Q50)) +
  geom_errorbarh(
    data = ran_effects,
    aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
    height = 0
  ) +
  facet_grid(
    trimester ~ first_anc,
    scales = "free",
    ## labeller = labeller(.rows = label_both, .cols = label_value)
  ) +
  theme_manuscript() +
  theme(axis.title.y = element_blank())

ggsave_manuscript(
  p,
  file = "benin_dco_bayes_random_effects",
  width = 8, height = 10
)

icc <- map(fits, performance::icc)

saveRDS(icc, file = "benin_dco_bayes_icc.rds")

orderly_artefact(
  files = "benin_dco_bayes_icc.rds",
  description = "ICC for DRC 2015 DCO model fits"
)


bayes_r2 <- map(fits, bayes_R2, probs = c(0.025, 0.5, 0.975))

saveRDS(bayes_r2, file = "benin_dco_bayes_r2.rds")

orderly_artefact(
  files = "benin_dco_bayes_r2.rds",
  description = "Bayes R2 for DRC 2015 DCO model fits"
)
