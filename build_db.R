suppressPackageStartupMessages({
  require(RSQLite)
  require(qs)
#' require(jsonlite)
  require(data.table)
})

.args <- if (interactive()) c(
    "fit_sindh.qs", "../covidm", "config.sqlite"
) else commandArgs(trailingOnly = TRUE)

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
        repeat_period INTEGER,
        repeat_number INTEGER,
        seasonality TEXT DEFAULT 'none',
        doses_per_day REAL,
        strategy_str INTEGER,
        from_age INTEGER,
        to_age INTEGER
    )"
)

scen.dt <- data.table(expand.grid(
    setId = 0,
    strategy = "campaign",
    vax_mech = "infection", #' as in, vs infection rather than vs disease
    eff_mech = "allornothing", #' what does efficacy mean? later consider "leaky"
    vax_eff = seq(10,90,by=20)/100,
    nat_imm_dur_days = round(2.5*365),
    vax_imm_dur_days = round(c(2.5, 5)*365),
    start_timing = as.Date(c("2020-10-01","2021-01-01")),
    repeat_period = 0,
    repeat_number = 0,
    seasonality = c("none"),
    doses_per_day = 12000,
    strategy_str = c(90, 365),
    #' days (for campaign - other interpretations for other strategies)
    from_age = c(4, 14), # 16+ vs 65+
    to_age = 16
))

scen_more.dt <- data.table(expand.grid(
  setId = 0,
  strategy = "campaign",
  vax_mech = "infection", #' as in, vs infection rather than vs disease
  eff_mech = "allornothing", #' what does efficacy mean? later consider "leaky"
  vax_eff = seq(10,90,by=20)/100,
  nat_imm_dur_days = round(2.5*365),
  vax_imm_dur_days = round(c(2.5, 5)*365),
  start_timing = as.Date(c("2020-10-01","2021-01-01")),
  repeat_period = 0,
  repeat_number = 0,
  seasonality = c("none"),
  doses_per_day = 12000,
  strategy_str = 0, # do indefinitely
  #' days (for campaign - other interpretations for other strategies)
  from_age = c(4, 14), # 16+ vs 65+
  to_age = 16
))

scen.dt <- rbind(
    data.table(expand.grid(
        setId = 0,
        strategy = "none",
        nat_imm_dur_days = scen.dt[, unique(nat_imm_dur_days)],
        seasonality = scen.dt[, unique(seasonality)],
        start_timing = scen.dt[, unique(start_timing)]
    )),
    scen.dt,
    scen_more.dt,
    fill = TRUE
)

dbWriteTable(
    conn,
    "scenario",
    scen.dt,
    append = TRUE
)

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
