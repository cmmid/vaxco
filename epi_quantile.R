
suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "%s", "%s/config.sqlite", "%s/epi_quantile.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

readDBtable <- function(fl, tbl = "metrics", drv = RSQLite::SQLite(), flags = SQLITE_RO) {
    conn <- dbConnect(drv, fl, flags = flags)
    res <- data.table(dbReadTable(conn, tbl))
    dbDisconnect(conn)
    res
}

scn <- readDBtable(.args[2], tbl = "scenario")

file.dt <- data.table(
    path = list.files(.args[1], "^\\d+\\.rds$", full.names = TRUE)
)[,
  id := as.integer(gsub("^.*/([^/]+)\\.rds$","\\1", path))
][scn, on=.(id=id)][!is.na(path)]

intcols <- names(scn)
basecols <- setdiff(intcols, c("vax_mech","eff_mech","vax_eff","vax_imm_dur_days","vax_delay","repeat_period","repeat_number","doses_per_day","strategy_str","from_age","to_age", "strategy"))

merge.dt <- function(fl.dt, filt, cols) {
    res <- rbindlist(with(
        fl.dt,
        mapply(function(p,id) readRDS(p)[eval(filt)][, id := id ], p=path, id=id, SIMPLIFY = FALSE)
    ))
    res[fl.dt[, .SD, .SDcols = cols], on = .(id)]
}

flt <- expression(!(outcome %in% c("cases_reported","foi","obs0","non_icu_critical2_p","non_icu_critical2_i")))
int.dt <- merge.dt(file.dt[strategy != "none"], flt, intcols)
ref.dt <- merge.dt(file.dt[strategy == "none"], flt, basecols)

int.dt[ref.dt[, -"id" ], del := i.value - value, on=setdiff(names(ref.dt), c("value", "id"))]

res.dt <- rbind(
    int.dt[,.(id, sampleId, age, outcome, anni_year, value, del)],
    ref.dt[,.(id, sampleId, age, outcome, anni_year, value, del=NA)]
)

#' we want outcomes-wide, quantiles+other keys long, and
#' incidence values instead of cumulative values
#'
#' first get the quantiles

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

qs.del.dt <- res.dt[, qtile(del), by=.(id, outcome, age, anni_year)]
qs.val.dt <- res.dt[, qtile(value), by=.(id, outcome, age, anni_year)]

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
