# shared/cmS_misc.R
# various useful functions

# Wrappers around qs to save/load an R object.
cm_save = function(x, file) {
    qsave(x, file, "balanced");
}

cm_load = function(file) {
    qread(file);
}

# Safe sampling (unlike sample, works even if x is only one number)
cm_resample = function(x, ...) x[sample.int(length(x), ...)]

# Return the mean and estimated highest density interval from a set of samples x.
cm_mean_hdi = function(x, credMass = 0.95)
{
    h = hdi(x, credMass);
    m = mean(x);
    return (list(mean = m, lower = h[[1]], upper = h[[2]]))
}

# construct a delay distribution following a gamma distribution with mean mu and shape parameter shape.
cm_delay_gamma = function(mu, shape, t_max, t_step)
{
    scale = mu / shape;
    t_points = seq(0, t_max, by = t_step);
    heights = pgamma(t_points + t_step/2, shape, scale = scale) - 
        pgamma(pmax(0, t_points - t_step/2), shape, scale = scale);
    return (data.table(t = t_points, p = heights / sum(heights)))
}

# construct a delay distribution that effectively skips the compartment
cm_delay_skip = function(t_max, t_step)
{
    t_points = seq(0, t_max, by = t_step);
    return (data.table(t = t_points, p = c(1, rep(0, t_max / t_step))))
}

# smoothly interpolate between points (x0, y0) and (x1, y1) using cosine interpolation.
# for x < x0, returns y0; for x > x1, returns y1; for x0 < x < x1, returns the cosine interpolation between y0 and y1
cm_interpolate_cos = function(x, x0, y0, x1, y1)
{
    ifelse(x < x0, y0, ifelse(x > x1, y1, y0 + (y1 - y0) * (0.5 - 0.5 * cos(pi * (x - x0) / (x1 - x0)))))
}

# for a set of age group bounds age_bounds (including the lower and upper bound as well as intermediate bounds),
# return a vector giving length(age_bounds) - 1 numerical entries between 0 and 1 inclusive, depending on how much
# each age group lies within the range age_min to age_max.
cm_age_coefficients = function(age_min, age_max, age_bounds)
{
    x = rep(0, length(age_bounds) - 1);
    for (ag in 1:(length(age_bounds) - 1)) {
        ag_lower = age_bounds[ag];
        ag_upper = age_bounds[ag + 1];
        ag_0 = max(age_min, ag_lower);
        ag_1 = min(age_max, ag_upper);
        x[ag] = max(0, (ag_1 - ag_0) / (ag_upper - ag_lower));
    }
    return (x);
}


# look up pop_region and return vector with the number of people in different age bands (5 year size)
cm_make_population = function(pop_region, n_age_groups)
{
    pop_size = cm_populations[name == pop_region, .(pop = round(sum(f, m) * 1000)), by = age]$pop;
    
    if (length(pop_size) == 0) {
        stop(paste("Could not find region", pop_region, "in cm_populations."));
    } else if (anyDuplicated(cm_populations[name == pop_region, age])) {
        stop(paste("Duplicate entries for region", pop_region, "in cm_populations."));
    }
    
    if (n_age_groups == 1) {
        pop_size = sum(pop_size);
    } else {
        pop_size = c(pop_size[1:(n_age_groups - 1)], sum(pop_size[n_age_groups:length(pop_size)]));
    }
    return (pop_size);
}


# expects dynamic results from a single run; the start date of the simulation; 
# the date upon which measurement starts; dates marking the end of each measurement period (i.e. n bounds for n measurements);
# and age group outer bounds (n+1 bounds for n measurements)
cm_case_distribution = function(z, date_simulation_start, date_measurement_start, dates_measurement_end, age_bounds, compart = "cases")
{
    # do date magic
    dss = ymd(date_simulation_start);
    t_ref = as.numeric(dss);
    t_bounds = c(as.numeric(ymd(date_measurement_start)) - t_ref,
                 as.numeric(ymd(dates_measurement_end)) - t_ref);

    # do age magic
    n_age_groups = z[, max(group)];
    age_dists = list();
    age_names = c();
    for (a in 1:(length(age_bounds) - 1)) {
        age_dists[[a]] = cm_age_coefficients(age_bounds[a], age_bounds[a + 1], 5 * (0:n_age_groups));
        age_names = c(age_names, paste(age_bounds[a], "-", age_bounds[a + 1]));
    }

    # cast incidence into a usable form
    incidence = data.table(dcast(z[compartment == compart], t ~ group, fun.aggregate = sum, value.var = "value"))

    # slice up cases by date
    dist = NULL;
    for (slice in 1:length(dates_measurement_end)) {
        inc = unname(incidence[t >= t_bounds[slice] & t < t_bounds[slice + 1], colSums(.SD), .SDcols = -1]);
        t_name = paste(dss + t_bounds[slice] + min(1, slice - 1), "-", dss + t_bounds[slice + 1]);
        dist = rbind(dist, 
            data.table(
                date = rep(t_name, length(age_dists)),
                age = age_names,
                cases = sapply(age_dists, function(age_dist) sum(inc * age_dist))
            )
        )
    }
    
    # also report fraction of cases by age group
    dist[, fcases := cases / sum(cases), by = date];
    
    return (dist)
}

mean_duration <- function(death_rate, dX, time_step) {
    ts <- seq(0, by=time_step, length.out = length(dX))
    sum(dX * ts * exp(-death_rate*ts))
}

#' compute the next-generation-matrix for a particular covidm population
#' 
#' @param parameters a covidm parameters object
#' @param population which population within `parameters` to calculate
#' @param u_multipler multiplicative modifier to susceptibility;
#'   should be length 1 or same length as age categories
#' @param contact_reductions modifier to contact matrices;
#'   between 0 (no reduction) and 1 (complete reduction)
#' @param fIs_reductions modifer to symptomatic transmission;
#'   should be length 1 or same length as age categories
#' @param uval numeric vector, when performing the calculation with an apply with various u values
#' @param yval numeric vector, when performing the calculation with an apply with various y values
#' 
#' @return a next generation matrix
cm_ngm <- function(
    parameters,
    population = 1,
    u_multiplier = 1,
    contact_reductions = rep(0, 4),
    fIs_reductions = rep(0, 16),
    fIa_reductions = rep(0, 16),
    fIp_reductions = rep(0, 16),
    uval = parameters$pop[[population]]$u,
    yval = parameters$pop[[population]]$y
) {
    po = parameters$pop[[population]];
    dIp = sapply(po$D, mean_duration, dX = po$dIp, time_step = parameters$time_step)
    dIs = sapply(po$D, mean_duration, dX = po$dIs, time_step = parameters$time_step)
    dIa = sapply(po$D, mean_duration, dX = po$dIa, time_step = parameters$time_step)

    cm = Reduce('+', mapply(function(c, m, red) c * m * (1-red), po$contact, po$matrices, contact_reductions, SIMPLIFY = F));
    
    ngm = uval * u_multiplier * t(t(cm) * (
        yval * (po$fIp * (1-fIp_reductions) * dIp + po$fIs * (1-fIs_reductions) * dIs) + 
        (1 - yval) * po$fIa * (1-fIa_reductions) * dIa))
    ngm
}

cm_eigen_ngm <- function(
   ..., ngm = cm_ngm(...)
) {
    res <- eigen(ngm)
    ss <- abs(Re(res$vector[,1]))
    list(R0 = Re(res$values[1]), ss = ss)
}

cm_generation_interval <- function(
    innerpop,
    fIs_reductions = rep(0, innerpop$n_groups),
    fIa_reductions = rep(0, innerpop$n_groups),
    fIp_reductions = rep(0, innerpop$n_groups),
    yval,
    eigen_vector,
    time_step
) {
    dE  = sapply(innerpop$D, mean_duration, dX = innerpop$dE,  time_step = time_step)
    dIp = sapply(innerpop$D, mean_duration, dX = innerpop$dIp, time_step = time_step)
    dIs = sapply(innerpop$D, mean_duration, dX = innerpop$dIs, time_step = time_step)
    dIa = sapply(innerpop$D, mean_duration, dX = innerpop$dIa, time_step = time_step)
  
    ave_gen <- 0
    normev <- eigen_vector/(sum(eigen_vector))

    #' f*duration propto infections associated that interval
    #' so e.g. dIa * fIa = infection weight
    #' (1-y) probability of having Ia
    #' another dIa for the average time those infections occur at
    
    for (i in seq_along(eigen_vector)) {
      age_i_ave <- (dE[i] + c(dIa[i]^2*(1-yval[i])*innerpop$fIa[i]*(1-fIa_reductions[i]), yval[i]*dIp[i]^2*innerpop$fIp[i]*(1-fIp_reductions[i]), yval[i]*(dIp[i]+dIs[i])*dIs[i]*innerpop$fIs[i]*(1-fIs_reductions[i])))/(
        (1-yval[i])*innerpop$fIa[i]*(1-fIa_reductions[i])*dIa[i] + yval[i]*(innerpop$fIp[i]*(1-fIp_reductions[i])*dIp[i] + innerpop$fIs[i]*(1-fIs_reductions[i])*dIs[i])
      )
        ave_gen <- ave_gen +
          normev[i] * age_i_ave
    }
      
   sum(ave_gen)
}

# Calculate R0 for a given population
cm_calc_R0 = function(parameters, population, ...) cm_eigen_ngm(parameters, population, ...)$R0

