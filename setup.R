
suppressPackageStartupMessages({
    require(data.table)
    require(qs)
})

.args <- if (interactive()) c(
    "fit_sindh.qs", "../covidm"
) else commandArgs(trailingOnly = TRUE)

# load fitted model for Sindh
fitS = qread(.args[1])

cm_path <- tail(.args, 1)

# load covidm
cm_force_rebuild = T;
cm_build_verbose = T;
cm_force_shared = F;
cm_version = 2;
source(file.path(cm_path, "R", "covidm.R"))

#' TODO - this will misbehave on HPC - fix along with parameter checking
# This will recompile covidm with certain components required to run 
# for this setting and model setup -- this step may need to be done 
# prior to batch execution of runs to avoid multiple threads trying to 
# recompile covidm at once.
cm_source_backend(user_defined = fitS$user_defined)
