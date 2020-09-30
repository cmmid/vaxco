suppressPackageStartupMessages({
    require(data.table)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/outputs/diffs.rds", "%s/outputs/quantiles.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

raw.dt <- readRDS(.args[1])

ps <- c(lo95=0.025,lo50=0.25,md=.5,hi50=0.75,hi95=0.975)

qtile <- function(v, probs, fmt = "%s") {
  ret <- quantile(v, probs = probs)
  names(ret) <- sprintf(fmt, names(probs))
  ret
}

stat.dt <- raw.dt[,{
  qs.i <- qtile(inc, ps, "inc.%s")
  qs.a <- qtile(cum.averted, ps, "cum.averted.%s")
  qs.e <- qtile(cum.eff, ps, "cum.eff.%s")
  c(as.list(qs.i), as.list(qs.a), as.list(qs.e))
}, keyby=.(scenarioId, outcome, age, simday)]

saveRDS(stat.dt, tail(.args, 1))