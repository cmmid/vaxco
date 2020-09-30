suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/config.sqlite", "%s/outputs/metrics_",
    "%s/outputs/diffs.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

if (!interactive()) warning(sprintf("invoked with args %s", paste(.args, collapse = " ")))

readDBtable <- function(fl, tbl = "metrics", drv = RSQLite::SQLite(), flags = SQLITE_RO) {
    conn <- dbConnect(drv, fl, flags = flags)
    res <- data.table(dbReadTable(conn, tbl))
    dbDisconnect(conn)
    res
}

scn <- readDBtable(.args[1], tbl = "scenario")

fls <- grep("_\\d+\\.sqlite",
    list.files(dirname(.args[2]), basename(.args[2]), full.names = TRUE),
    value = TRUE
)
all <- rbindlist(lapply(fls, readDBtable))

#' TODO: currently specifying what's relevant by hand
baselinescns <- scn[strategy == "none", .(
    id, nat_imm_dur_days, start_timing, seasonality
)]

responsescns <- scn[strategy != "none"]
responsescns[
    baselinescns,
    on = setdiff(colnames(baselinescns), "id"),
    refid := i.id
]

baselineres <- all[scenarioId %in% unique(baselinescns$id)]
responseres <- all[!(scenarioId %in% unique(baselinescns$id))]
responseres[responsescns, on=.(scenarioId == id), refScnId := refid ]

responseres[
    baselineres,
    on=.(sampleId, age, simday, outcome, refScnId == scenarioId),
    refval := i.value_numeric
]

responseres[order(simday),
  rebase := value_numeric - value_numeric[1],
  by = .(scenarioId, sampleId, age, outcome)
]

responseres[order(simday),
  ref := refval - refval[1],
  by = .(scenarioId, sampleId, age, outcome)
]

manip <- rbind(
  responseres[!(outcome %in% c("cases_reported","foi","obs0")),.(
    scenarioId, sampleId, simday, outcome,
    age, ref, rebase
  )],
  responseres[!(outcome %in% c("cases_reported","foi","obs0")),.(
    age = "all", ref = sum(ref), rebase = sum(rebase)
  ),by=.(scenarioId, sampleId, simday, outcome)]
)

manip[order(simday),
  inc := c(rebase[1], diff(rebase)),
  by = .(scenarioId, sampleId, age, outcome)
]

manip[,
  cum.averted := ref - rebase
]

manip[,
  cum.eff := fifelse(ref == rebase, 0, cum.averted/ref)
]

manip$ref <- manip$rebase <- NULL

saveRDS(manip, tail(.args, 1))
