suppressPackageStartupMessages({
    require(RSQLite)
#' TODO needed for construction from other sources
#' require(qs)
#' require(jsonlite)
    require(data.table)
})

.args <- if (interactive()) c(
    "scenarios.json", "fit_sindh.qs", "results.sqlite"
) else commandArgs(trailingOnly = TRUE)

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
        particleId INTEGER NOT NULL
    )"
)

pars.dt <- data.table(expand.grid(
    setId = 0, particleId = 1:1000
))

dbWriteTable(
    conn,
    "parameter",
    pars.dt,
    append = TRUE
)

smpsize <- dbGetQuery(conn, "SELECT max(particleId) FROM parameter;")[1,1]

dbSendStatement(
    conn,
    "CREATE TABLE scenario (
        id INTEGER PRIMARY KEY,
        setId INTEGER NOT NULL,
        vaxEff REAL NOT NULL,
        immuneDurDays REAL NOT NULL
    )"
)

scen.dt <- data.table(expand.grid(
    setId = 0,
    vaxEff = seq(10,90,by=20)/100,
    immuneDurDays = round(c(0, 1, 2.5, 5, 10)*365)
))

dbWriteTable(
    conn,
    "scenario",
    scen.dt,
    append = TRUE
)

max_scen_id <- dbGetQuery(conn, "SELECT max(id) FROM scenario;")[1,1]

dbSendStatement(
    conn,
    "CREATE TABLE sample (
      id INTEGER PRIMARY KEY,
      setId INTEGER NOT NULL,
      particleId INTEGER NOT NULL,
      scenarioId INTEGER NOT NULL DEFAULT 0,
      UNIQUE (setId, particleId, scenarioId)
    );"
)

samp.dt <- data.table(expand.grid(
    setId = 0,
    particleId = 1:smpsize,
    scenarioId = 1:max_scen_id
))

dbWriteTable(
    conn,
    "sample",
    samp.dt,
    append = TRUE
)

max_samples_id <- dbGetQuery(conn, "SELECT max(id) FROM sample;")[1,1]

#' statuses:
#'  - S, staged (planned)
#'  - Q, queued (to be run)
#'  - R, running (some process running)
#'  - D, done (some process finished and recorded data)
#'  - E, error
dbSendStatement(
    conn,
    "CREATE TABLE job (
      id INTEGER,
      status CHAR(1) CHECK( status IN ('S', 'Q', 'R', 'D', 'E') ) NOT NULL DEFAULT 'S',
      startTime INTEGER,
      duration INTEGER,
      posterior INTEGER,
      attempts INTEGER NOT NULL DEFAULT 0
    );"
)

job.dt <- data.table(id = 1:max_samples_id)

dbWriteTable(
    conn,
    "job",
    job.dt,
    append = TRUE
)

dbSendStatement(
    conn,
    "CREATE TABLE metrics (
      id INTEGER,
      sampleId INTEGER,
      name TEXT NOT NULL,
      value_numeric REAL DEFAULT NULL,
      value_label TEXT DEFAULT NULL
    );"
)

dbDisconnect(conn)
