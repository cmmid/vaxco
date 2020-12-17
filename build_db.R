suppressPackageStartupMessages({
  require(RSQLite)
  require(qs)
  require(data.table)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "fit_sindh.qs", "../covidm", "%s/config.sqlite"
), .debug) else commandArgs(trailingOnly = TRUE)

fitS = qread(.args[1])

cm_path = .args[2]
cm_force_rebuild = F;
cm_build_verbose = F;
cm_force_shared = T;
cm_version = 2;
source(file.path(cm_path, "R", "covidm.R"))

n_samples <- 100
rng_seed <- 0

pars.dt <- as.data.table(fitS$post)[cm_temp_which_rows(
  cm_translate_parameters(fitS$par),
  fitS$post, n_samples, seed = rng_seed
)][,c(
  .(setId=0L, particleId = 1L:.N),
  .SD
), .SDcols = -c("trial", "lp", "chain", "ll")]

drv <- RSQLite::SQLite()
conn <- dbConnect(drv, tail(.args, 1))

#' for sampling fitted parameters generally
#' but in this approach, parameters are all specified
#' in the fit_sindh.qs
#' TODO: add columns here for documentation purposes
dbSendStatement(
    conn,
    "CREATE TABLE parameter (
        setId INTEGER NOT NULL,
        particleId INTEGER NOT NULL,
        param TEXT NOT NULL,
        value REAL NOT NULL
    )"
)

# load fitted model for Sindh

dbWriteTable(
    conn,
    "parameter",
    melt(pars.dt, id.vars = c("setId", "particleId"), variable.name = "param", value.name = "value")[order(setId, particleId, param)],
    append = TRUE
)

dbSendStatement(
    conn,
    "CREATE TABLE scenario (
        id INTEGER PRIMARY KEY,
        setId INTEGER NOT NULL,
        strategy TEXT DEFAULT 'campaign',
        vax_mech TEXT DEFAULT 'infection',
        eff_mech TEXT DEFAULT 'allornothing',
        vax_eff REAL,
        nat_imm_dur_days INTEGER,
        vax_imm_dur_days INTEGER,
        start_timing INTEGER,
        vax_delay INTEGER,
        repeat_period INTEGER DEFAULT 0,
        repeat_number INTEGER DEFAULT 0,
        seasonality TEXT DEFAULT 'none',
        doses_per_day REAL,
        strategy_str INTEGER,
        from_age INTEGER,
        to_age INTEGER,
        R0 TEXT, contact_matrix TEXT, npis TEXT,
        susceptibility TEXT, clin_frac TEXT,
        subclin_inf REAL,
        horizon INTEGER,
        birthdeath TEXT, hosp_model TEXT, icu_model TEXT, death_model TEXT
    )"
)

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
  birthdeath = "no",
  hosp_model = "current", icu_model = "current", death_model = "current",
  start_timing = as.Date(c("2021-01-01", "2021-04-01")) # start timing forms reference measurement dates
)

vaxepi <- c(baseepi, list(
  strategy = "campaign",
  vax_mech = "infection", #' as in, vs infection rather than vs disease
  eff_mech = "allornothing", #' what does efficacy mean? later consider "leaky"
  vax_eff = seq(30,90,by=20)/100,
  vax_imm_dur_days = round(c(1, 2.5, 5, Inf)*365),
  vax_delay = c(0, 30), # immunity onset delay from first dose - manages 1 vs 2 dose courses
  repeat_period = 0,
  repeat_number = 0,
  doses_per_day = c(8, 12)*1e3,
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

res <- dbWriteTable(
    conn,
    "scenario",
    scen.dt,
    append = TRUE
)

print(res)

#' max_scen_id <- dbGetQuery(conn, "SELECT max(id) FROM scenario;")[1,1]
#' 
#' dbSendStatement(
#'     conn,
#'     "CREATE TABLE sample (
#'       id INTEGER PRIMARY KEY,
#'       setId INTEGER NOT NULL,
#'       particleId INTEGER NOT NULL,
#'       scenarioId INTEGER NOT NULL DEFAULT 0,
#'       UNIQUE (setId, particleId, scenarioId)
#'     );"
#' )
#' 
#' samp.dt <- data.table(expand.grid(
#'     setId = 0,
#'     particleId = 1:smpsize,
#'     scenarioId = 1:max_scen_id
#' ))
#' 
#' dbWriteTable(
#'     conn,
#'     "sample",
#'     samp.dt,
#'     append = TRUE
#' )
#' 
#' max_samples_id <- dbGetQuery(conn, "SELECT max(id) FROM sample;")[1,1]
#' 
#' #' statuses:
#' #'  - S, staged (planned)
#' #'  - Q, queued (to be run)
#' #'  - R, running (some process running)
#' #'  - D, done (some process finished and recorded data)
#' #'  - E, error
#' dbSendStatement(
#'     conn,
#'     "CREATE TABLE job (
#'       id INTEGER,
#'       status CHAR(1) CHECK( status IN ('S', 'Q', 'R', 'D', 'E') ) NOT NULL DEFAULT 'S',
#'       startTime INTEGER,
#'       duration INTEGER,
#'       posterior INTEGER,
#'       attempts INTEGER NOT NULL DEFAULT 0
#'     );"
#' )
#' 
#' job.dt <- data.table(id = 1:max_samples_id)
#' 
#' dbWriteTable(
#'     conn,
#'     "job",
#'     job.dt,
#'     append = TRUE
#' )

dbDisconnect(conn)
