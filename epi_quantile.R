
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c("~/Dropbox/Covid-WHO-vax/outputs", "00001")
.args <- if (interactive()) sprintf(c(
    "%s/sim/%s.rds", "%s/config.rds", "%s/epi_baseline.rds", .debug[2], "%s/epiq/%s.rds"
), .debug[1], .debug[2]) else commandArgs(trailingOnly = TRUE)

scn <- readRDS(.args[2])
intcols <- names(scn)
basecols <- setdiff(intcols, c("vax_mech","eff_mech","vax_eff","vax_imm_dur_days","vax_delay","repeat_period","repeat_number","doses_per_day","strategy_str","from_age","to_age", "strategy", "increasing"))

ref.dt <- readRDS(.args[3])

tarid <- as.integer(tail(.args, 2)[1])

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

if (ref.dt[, !any(id == tarid)]) {
  tar.dt <- scn[,.SD,.SDcols=intcols][
    readRDS(.args[1])[, id := as.integer(tail(.args, 2)[1]) ], on = .(id)
  ][
    ref.dt[,-"id"], del := i.value - value, on=setdiff(names(ref.dt), c("value", "id"))
  ][,.(id, sampleId, age, outcome, anni_year, value, del)]
} else {
  tar.dt <- ref.dt[id == tarid, .(id, sampleId, age, outcome, anni_year, value, del = NA) ]
}

#' we want outcomes-wide, quantiles+other keys long, and
#' incidence values instead of cumulative values
#'
#' first get the quantiles

qs.del.dt <- tar.dt[, qtile(del), by=.(id, outcome, age, anni_year)]
qs.val.dt <- tar.dt[, qtile(value), by=.(id, outcome, age, anni_year)]

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

del.dt <- reshaper(qs.del.dt[!is.na(outcome)])
val.dt <- reshaper(qs.val.dt[!is.na(outcome)])

epi.dt <- merge(val.dt, del.dt, by = c("id","age","qtile","anni_year"), suffixes = c("",".del"))

saveRDS(epi.dt, tail(.args, 1))
