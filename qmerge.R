
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c("~/Dropbox/Covid-WHO-vax/outputs", "epi")
.args <- if (interactive()) sprintf(c(
    "%s/epiq",
    "%s/%s_quantile.rds"
), .debug[1], .debug[2]) else commandArgs(trailingOnly = TRUE)

fls <- list.files(.args[1], "\\d+\\.rds", full.names = TRUE)

saveRDS(rbindlist(lapply(fls, readRDS)), tail(.args, 1))
