suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/config.sqlite", "%s/inputs/scenarios.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

drv <- RSQLite::SQLite()
conn <- dbConnect(drv, .args[1], flags = SQLITE_RO)
res <- data.table(dbReadTable(conn, "scenario"))
dbDisconnect(conn)

ret <- res[strategy != "none", .(
    id,
    vax_eff, vax_imm_dur_yrs = round(vax_imm_dur_days/365, 1),
    campaign_dur_days = strategy_str,
    start_date = as.Date(start_timing, origin = "1970-01-01"),
    tar_age = fifelse(from_age == 4, "15+","65+")
)]

saveRDS(ret, tail(.args, 1))