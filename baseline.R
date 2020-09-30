suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/config.sqlite", "%s/outputs/low/metrics_",
    "%s/outputs/low/baseline.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

if (!interactive()) warning(sprintf("invoked with args %s", paste(.args, collapse = " ")))

readDBtable <- function(fl, tbl = "metrics", drv = RSQLite::SQLite(), flags = SQLITE_RO) {
    conn <- dbConnect(drv, fl, flags = flags)
    res <- data.table(dbReadTable(conn, tbl))
    dbDisconnect(conn)
    res
}

scn <- readDBtable(.args[1], tbl = "scenario")

#' TODO: currently specifying what's relevant by hand
baselinescns <- scn[strategy == "none", .(
  id, nat_imm_dur_days, start_timing, seasonality
)]

keepids <- baselinescns[,
  sprintf("_(%s)\\.sqlite", paste(sprintf("%03g",id), collapse = "|"))
]

fls <- grep(keepids,
    list.files(dirname(.args[2]), basename(.args[2]), full.names = TRUE),
    value = TRUE
)
all <- rbindlist(lapply(fls, readDBtable))

all[order(simday),
  rebase := value_numeric - value_numeric[1],
  by = .(scenarioId, sampleId, age, outcome)
]

manip <- rbind(
  all[!(outcome %in% c("cases_reported","foi","obs0")),.(
    scenarioId, sampleId, simday, outcome,
    age, rebase
  )],
  all[!(outcome %in% c("cases_reported","foi","obs0")),.(
    age = "all", rebase = sum(rebase)
  ),by=.(scenarioId, sampleId, simday, outcome)]
)

manip[order(simday),
  inc := c(rebase[1], diff(rebase)),
  by = .(scenarioId, sampleId, age, outcome)
]

saveRDS(manip, tail(.args, 1))
