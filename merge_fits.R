suppressPackageStartupMessages({
    require(qs)
})

.args <- if (interactive()) c(
    "fit_sindh.qs", 
    list.files(pattern = "waning_.+\\.qs$"),
    "fit_combined.qs"
) else commandArgs(trailingOnly = TRUE)

fls <- head(.args, -1)

nms <- gsub("^.*waning_(.+)\\.qs$","\\1",fls)
nms[1] <- "Inf"

all_fits <- lapply(fls, qread)

names(all_fits) <- nms

qsave(all_fits, tail(.args, 1))