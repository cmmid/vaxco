suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/config.sqlite", "%s/outputs/other_",
    "%s/outputs/validation.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

if (!interactive()) warning(sprintf("invoked with args %s", paste(.args, collapse = " ")))

readDBtable <- function(fl, tbl = "other", drv = RSQLite::SQLite(), flags = SQLITE_RO) {
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
all <- rbindlist(lapply(fls, function(fl) readDBtable(fl)[outcome %in% c("cases", "death_o")]))

comb <- rbind(
  all,
  all[, .(
    value_numeric = sum(value_numeric),
    age = "all"
  ), by=.(scenarioId, sampleId, simday, outcome)]
)

comb[order(simday), inc := c(value_numeric[1], diff(value_numeric)), by = .(scenarioId, sampleId, age, outcome)]

ps <- c(lo95=0.025,lo50=0.25,md=.5,hi50=0.75,hi95=0.975)
qtile <- function(v, probs, fmt = "%s") {
  ret <- quantile(v, probs = probs)
  names(ret) <- sprintf(fmt, names(probs))
  ret
}

res <- comb[,{
  qv <- qtile(value_numeric, ps, "cum.%s")
  qi <- qtile(inc, ps, "inc.%s")
  c(as.list(qi), as.list(qv))
}, by=.(scenarioId, age, simday, outcome)]

saveRDS(res, tail(.args, 1))
