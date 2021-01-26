
suppressPackageStartupMessages({
    require(data.table)
    require(qs)
})

.args <- if (interactive()) c(
    "fitd_sindh.qs", "../covidm-vaxco"
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

#' address fitting model ascertainment being sensitive to end time
#' instead fit to fitting end time (258 - but only for start time equals 1 Jan 2020)
fitS$user_defined$model_v2$cpp_observer <- c(
"auto odds = [&](double x, double odds_ratio) {"                                                                                 
,"    double a = x / (1 - x);"                                                                                                    
,"    return a * odds_ratio / (a * odds_ratio + 1);"                                                                              
,"};"                                                                                                                             
,"auto asc = [&](double x, double y0, double y_lo, double s0, double s1) {"                                                       
,"    double y1 = odds(y0, exp(y_lo));"                                                                                           
,"    double xx = s0 + x * (s1 - s0);"                                                                                            
,"    double h0 = exp(s0) / (1 + exp(s0));"                                                                                       
,"    double h1 = exp(s1) / (1 + exp(s1));"                                                                                       
,"    double h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0);"                                                                     
,"    return y0 + (y1 - y0) * h;"                                                                                                 
,"};"                                                                                                                             
,"dyn.Obs(t, 0, 0, 0) = estimate_Rt(P, dyn, t, 0, 50);"                                                                           
,"dyn.Obs(t, 0, 1, 0) = dyn(\"death_o\", t, {}, {}) * asc((t - P.time0) / (258 - P.time0), x[3], x[4], -x[5], x[6]);"         
,"dyn.Obs(t, 0, 2, 0) = dyn(\"cases_reported\", t, {}, {}) * asc((t - P.time0) / (258 - P.time0), x[8], x[9], -x[10], x[11]);"
)

#' TODO - this will misbehave on HPC - fix along with parameter checking
# This will recompile covidm with certain components required to run 
# for this setting and model setup -- this step may need to be done 
# prior to batch execution of runs to avoid multiple threads trying to 
# recompile covidm at once.
cm_source_backend(user_defined = fitS$user_defined)
