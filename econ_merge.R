
suppressPackageStartupMessages({
    require(data.table)
})

.debug <- c("~/Dropbox/Covid-WHO-vax/outputs/econ")

.args <- if (interactive()) sprintf(c(
    "%s",
    "%s/merge.rds"
),.debug) else commandArgs(trailingOnly = TRUE)

fls <- list.files(.args[1], "\\d+\\.rds", full.names = TRUE)

saveRDS(rbindlist(lapply(fls, readRDS)), tail(.args, 1))