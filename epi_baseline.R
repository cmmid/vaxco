
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "%s/sim", "%s/config.rds", "%s/epi_baseline.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

scn <- readRDS(.args[2])

fllist <- list.files(.args[1], "^\\d+\\.rds$", full.names = TRUE)

file.dt <- data.table(
    path = fllist
)[,
  id := as.integer(gsub("^.*/([^/]+)\\.rds$","\\1", path))
][scn, on=.(id=id)][!is.na(path)]
flt <- expression(!(outcome %in% c("cases_reported","foi","obs0","non_icu_critical2_p","non_icu_critical2_i")))

intcols <- names(scn)
basecols <- setdiff(intcols, c("vax_mech","eff_mech","vax_eff","vax_imm_dur_days","vax_delay","repeat_period","repeat_number","doses_per_day","strategy_str","from_age","to_age", "strategy", "increasing"))

ref.dt <- scn[,.SD,.SDcols=basecols][rbindlist(mapply(function(p, filt, id) {
  readRDS(p)[eval(filt)][, id := id ]
},
  p=file.dt[strategy == "none", path],
  filt = flt,
  id = file.dt[strategy == "none", id],
  SIMPLIFY = FALSE
)), on=.(id)]

saveRDS(ref.dt, tail(.args, 1))
