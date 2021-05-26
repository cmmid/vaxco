suppressPackageStartupMessages({
  require(qs)
  require(data.table)
})

.debug <- "~/Dropbox/Covid-WHO-vax/inputs"
.args <- if (interactive()) sprintf(c(
  "%s/config.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

baseepi <- list(
  setId = 0,
  nat_imm_dur_days = round(c(1, 2.5, 5, Inf)*365),
  seasonality = c("none"),
  R0 = "fitted",
  contact_matrix = "prem et al",
  npis = "google mobility",
  susceptibility = "nat med fit",
  clin_frac = "nat med fit",
  subclin_inf = 0.5,
  horizon = 10, #' in years
  birthdeath = "yes",
  hosp_model = "current", icu_model = "current", death_model = "current",
  start_timing = as.Date(c("2021-04-01","2021-07-01")) # start timing forms reference measurement dates
)

#' want:
#'  - infection + allornothing (ve = efficacy, uv = 0)
#'  - infection + leaky (ve = 1, uv = u*(1-eff))
#'  - disease-only + allornothing (ve = efficacy, yv = 0)
#'  - disease-only + leaky (ve = 1, yv = y*(1-eff))
vaxepi <- c(baseepi, list(
  strategy = "campaign",
  vax_mech = c("infection", "disease"), #' as in, vs infection & vs disease
  eff_mech = c("allornothing", "leaky"), #' what does efficacy mean? later consider "leaky"
  vax_eff = seq(30, 90, by=20)/100,
  vax_imm_dur_days = round(c(1, 2.5, 5, Inf)*365),
  vax_delay = c(0, 30), # immunity onset delay from first dose - manages 1 vs 2 dose courses
  repeat_period = 0,
  repeat_number = 0,
  doses_per_day = c(4, 8, 12)*1e3,
  increasing = T,
  strategy_str = c(0, 1, 5)*365,
  #' days (for 'campaign' - other interpretations for other strategies; 0 == continuous)
  from_age = c(4, 14), # 15+ vs 65+
  to_age = 16
))

scen.dt <- rbind(
  data.table(do.call(expand.grid, vaxepi)),
  data.table(do.call(expand.grid, c(baseepi, list(strategy = "none")))),
  fill = TRUE
)

final.dt <- rbind(
  scen.dt,
  copy(scen.dt)[doses_per_day == 4000 & strategy_str == 0 & from_age == 14][, increasing := FALSE ],
  copy(scen.dt)[doses_per_day == 4000 & strategy_str == 365 & from_age == 4][, increasing := FALSE ][, doses_per_day := 184000 ][, strategy_str := 180 ]
)[, id := 1:.N ]

saveRDS(final.dt, tail(.args, 1))
