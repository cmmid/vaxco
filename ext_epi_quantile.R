
suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "%s", "%s/config_ext.sqlite", "%s/epi_quantile.rds", "%s/epi_quantile_ext.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

readDBtable <- function(fl, tbl = "metrics", drv = RSQLite::SQLite(), flags = SQLITE_RO) {
    conn <- dbConnect(drv, fl, flags = flags)
    res <- data.table(dbReadTable(conn, tbl))
    dbDisconnect(conn)
    res
}

scn <- readDBtable(.args[2], tbl = "scenario")
scnbase <- readDBtable(gsub("_ext","",.args[2]), tbl = "scenario")

file.dt <- data.table(
    path = list.files(.args[1], "^\\d+_ext\\.rds$", full.names = TRUE)
)[,
  id := as.integer(gsub("^.*/([^/]+)_ext\\.rds$","\\1", path))
][scn, on=.(id=id)][!is.na(path)]

basefile.dt <- data.table(
  path = list.files(.args[1], "^\\d+\\.rds$", full.names = TRUE)
)[,
  id := as.integer(gsub("^.*/([^/]+)\\.rds$","\\1", path))
][scnbase, on=.(id=id)][!is.na(path)]

intcols <- names(scn)
basecols <- setdiff(intcols, c("vax_mech","eff_mech","vax_eff","vax_imm_dur_days","vax_delay","repeat_period","repeat_number","doses_per_day","strategy_str","from_age","to_age", "strategy"))

flt <- expression(!(outcome %in% c("cases_reported","foi","obs0","non_icu_critical2_p","non_icu_critical2_i")))

qtile <- function(
  v, ps = c(lo95=0.025, lo50=0.25, md=0.5, hi50=0.75, hi95=0.975),
  withMean = c("mn", NA),
  fmt = "%s",
  na.rm = TRUE
) {
  qs <- quantile(v, probs = ps, na.rm = na.rm)
  names(qs) <- sprintf(fmt, names(ps))
  if (!is.na(withMean[1])) {
    mn <- mean(v)
    names(mn) <- sprintf(fmt, withMean[1])
    qs <- c(qs, mn)
  }
  as.list(qs)
}

reshaper <- function(dt) dcast(
  melt(dt, id.vars = c("id","outcome","age","anni_year"), variable.name = "qtile"),
  id + age + anni_year + qtile ~ outcome
)[
  order(anni_year), {
    ret <- lapply(.SD[,-"anni_year"], diff)
    c(list(anni_year = anni_year[-1]), ret)
  },
  by=.(id, age, qtile)
]

join_and_q.dt <- function(fl.dt, filt, cols, base) {
  res <- rbindlist(with(
    fl.dt,
    mapply(function(p,id) readRDS(p)[eval(filt)][, id := id ], p=path, id=id, SIMPLIFY = FALSE)
  ))[fl.dt[, .SD, .SDcols = cols], on = .(id)]

  res[base[, -"id" ], del := i.value - value, on=c("sampleId", "outcome", "age", "anni_year", basecols[-1]) ]

  qs.del.dt <- res[, qtile(del), by=.(id, outcome, age, anni_year)]
  qs.val.dt <- res[, qtile(value), by=.(id, outcome, age, anni_year)]
  
  del.dt <- reshaper(qs.del.dt[!is.na(outcome)])
  val.dt <- reshaper(qs.val.dt[!is.na(outcome)])
  
  return(merge(val.dt, del.dt, by = c("id","age","qtile","anni_year"), suffixes = c("",".del")))
}

res.dt <- join_and_q.dt(file.dt[1:12], flt, intcols, ref.dt)

for (offset in seq(13, dim(file.dt)[1], by=12)) {
  res.dt <- rbind(
    res.dt,
    join_and_q.dt(file.dt[(0:11)+offset], flt, intcols, ref.dt)
  )
}

saveRDS(res.dt, tail(.args, 1))
