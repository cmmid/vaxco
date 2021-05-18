
suppressPackageStartupMessages({
    require(data.table)
})

.args <- commandArgs(trailingOnly = TRUE)

saveRDS(
    rbindlist(lapply(list.files(.args[1], "^\\d+\\.rds$", full.names = TRUE), readRDS)),
    tail(.args, 1)
)