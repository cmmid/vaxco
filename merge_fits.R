suppressPackageStartupMessages({
    require(qs)
})

.args <- if (interactive()) c(
    "fitd_sindh.qs", 
    list.files(pattern = "fitd_.+waning_.+\\.qs$"),
    "fitd_combined.qs"
) else commandArgs(trailingOnly = TRUE)

fls <- head(.args, -1)

nms <- gsub("^.*waning_(.+)\\.qs$","\\1",fls)
nms[1] <- "Inf"

all_fits <- lapply(fls, function(fl) {
    res <- qread(fl)
    res$sample_dynamics <- NULL
    res
})

names(all_fits) <- nms

qsave(all_fits, tail(.args, 1))