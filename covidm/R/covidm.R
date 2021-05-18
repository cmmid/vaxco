# covidm.R
# main file to source for users of the covidm API.

# TODO
#  - recover better from user halting of execution of cm_fit
#  - recover better from crashes during cm_fit
# TODO
#  - make sure seq_len and seq_along are used instead of 1:length() for 0-length things

packageStartupMessage("Loading covidm.")

# Required libraries
suppressPackageStartupMessages({
    library(data.table)   # for data.table, an enhanced (and faster) data.frame
    library(ggplot2)      # for plotting
    library(Rcpp)         # for running the C++ model backend
    library(BH)           # for linking with Boost
    library(qs)           # for qsave and qread, faster equivalents of saveRDS and readRDS
    library(lubridate)    # for manipulating dates and times. NB requires stringr
    library(HDInterval)   # for summarizing results
    library(cowplot)      # for plotting grids
})

# Settings for covidm
cm_option_ = function(name, default_value) {
    if (!exists(name)) {
        packageStartupMessage("Using default value for ", name, ": ", default_value)
    }
    return (get0(name, ifnotfound = default_value))
}

cm_path_           = normalizePath(cm_option_("cm_path", "~/Dropbox/nCoV/covidm/"))
cm_version_        = cm_option_("cm_version", 2)
cm_build_          = cm_option_("cm_build", T)
cm_force_rebuild_  = cm_option_("cm_force_rebuild", F)
cm_build_dir_      = normalizePath(
        cm_option_("cm_build_dir", file.path(cm_path_, "build"))
    )
cm_build_verbose_  = cm_option_("cm_build_verbose", T)
cm_force_shared_   = cm_option_("cm_force_shared", F)

# Attach code

sapply(
    file.path(cm_path_, "R", "shared", c("cmS_misc.R", "cmS_plot.R")),
    source
)

if (cm_version_ %in% c(1, 2)) {
    sapply(
        file.path(cm_path_, sprintf(file.path(
            "R", "v%i",
            c("cm%i_backend.R", "cm%i_run.R", "cm%i_params.R", "cm%i_interventions.R", "cm%i_fit.R")
        ), cm_version_, cm_version_)),
        source
    )
} else {
    stop(paste("Requested covidm version", cm_version_, "but there are only versions 1 and 2."));
}

if (cm_build_) {
    packageStartupMessage("Building backend code...");
    cm_source_backend();
}

packageStartupMessage("Loading data...");
invisible(mapply(
    assign,
    x=sprintf("cm_%s", c("matrices","populations","structure_UK","highrisk")),
    value=lapply(file.path(
        cm_path_,"data",c("all_matrices.rds","wpp2019_pop2020.rds","structure_UK.rds","prevalence_morbidities.rds")),
        readRDS
    ),
    MoreArgs = list(envir = environment())
))
