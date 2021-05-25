
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/outputs/sim", "%s/inputs/config.rds", "%s/outputs/epi_baseline.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

scn <- readRDS(.args[2])[strategy == "none"]
bfiles <- sprintf("^(%s)\\.rds", paste(scn$id, collapse = "|"))

fllist <- list.files(.args[1], bfiles, full.names = TRUE)

stopifnot(dim(scn)[1] == length(fllist))

intcols <- names(scn)
basecols <- setdiff(intcols, c("vax_mech","eff_mech","vax_eff","vax_imm_dur_days","vax_delay","repeat_period","repeat_number","doses_per_day","strategy_str","from_age","to_age", "strategy", "increasing"))

flt <- expression(!(outcome %in% c("cases_reported","foi","obs0","non_icu_critical2_p","non_icu_critical2_i")))
ref.dt <- rbindlist(lapply(fllist, function(pth) {
  readRDS(pth)[eval(flt)][, id := as.integer(gsub("^.*/([^/]+)\\.rds$","\\1", pth)) ]
}))[scn[,.SD,.SDcols=basecols], on=.(id)]

saveRDS(ref.dt, tail(.args, 1))
