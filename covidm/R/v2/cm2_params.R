# v2/cm2_params.R
# parameters for the model

#' normalizes time inputs to model time, i.e. days since day 0
#' 
#' @param t a vector, numeric or translate-able to Date via `lubridate::ymd`
#' @param date0 a scalar, translate-able to Date via `lubridate::ymd`
#' @seealso lubridate::ymd
#' @return a numeric vector, same length as t
translate_time = function(t, date0) return(
    if (is.numeric(t)) {
        t
    } else {
        as.numeric(lubridate::ymd(t) - lubridate::ymd(date0));
    }
)

# return translated parameters to work with the backend,
# i.e. fix any times expressed in dates to be expressed in days since date0.
cm_translate_parameters = function(p) # TODO: use `within`?
{

    p$time0 = translate_time(p$time0, p$date0);
    p$time1 = translate_time(p$time1, p$date0);
    
    for (pi in seq_along(p$pop)) {
        p$pop[[pi]]$seed_times = translate_time(p$pop[[pi]]$seed_times, p$date0);
    }

    for (si in seq_along(p$schedule)) {
        p$schedule[[si]]$times = translate_time(p$schedule[[si]]$times, p$date0);
    }
    
    if (is.null(p$processes)) p$processes <- NULL
    
    return (p);
}

is_positive      <- function(x)    is.numeric(x) & all(x > 0)
non_empty_list   <- function(x)    is.list(x) & length(x) > 0
non_negative     <- function(x)    is.numeric(x) & all(x >= 0)
some_positive    <- function(x)    non_negative(x) & any(x > 0)
is_proportion    <- function(x)    is.numeric(x) & all((0 <= x) & (x <= 1))
same_length      <- function(x, y) length(x) == length(y)
is_square_matrix <- function(x)    is.matrix(x) & nrow(x) == ncol(x)
in_order         <- function(x)    is.numeric(x) & !is.unsorted(x)

requirements <- lapply(list(
    "model" = "model == 'SEI3R' || model == 'household'",
    "time_step" = "is_positive(time_step)",
    "date0" = "lubridate::is.Date(lubridate::ymd(date0))",
    "time0" = "is.numeric(time0)",
    "time1" = "is.numeric(time1)",
    "report_every" = "is.numeric(report_every) & report_every == 1. / time_step",
    "fast_multinomial" = "is.logical(fast_multinomial)",
    "deterministic" = "is.logical(deterministic)",
    "pop" = "non_empty_list(pop)",
    "travel" = "non_negative(travel) & is_square_matrix(travel) & nrow(travel) == length(parameters$pop)"
), function(exp) parse(text = exp))

population_requirements <- lapply(list(
    "dE" = "some_positive(dE)",
    "dIp" = "some_positive(dIp)",
    "dIs" = "some_positive(dIs)",
    "dIa" = "some_positive(dIa)",
    "dC" = "some_positive(dC)",
    "size" = "some_positive(size)",
    "imm0" = "is_proportion(imm0) & same_length(imm0, size)",
    "matrices" = "non_empty_list(matrices) & all(sapply(matrices, function(m) non_negative(m) & is_square_matrix(m) & nrow(m) == length(size)))",
    "contact" = "non_negative(contact) & same_length(contact, matrices)",
    "contact_mult" = "(length(contact_mult) == 0) | (non_negative(contact_mult) & same_length(contact_mult, matrices))",
    "contact_lowerto" = "(length(contact_lowerto) == 0) | (non_negative(contact_lowerto) & same_length(contact_lowerto, matrices))",
    "u" = "non_negative(u) & same_length(u, size)",
    "y" = "non_negative(y) & same_length(y, size)",
    "fIp" = "non_negative(fIp) & same_length(fIp, size)",
    "fIa" = "non_negative(fIa) & same_length(fIa, size)",
    "fIs" = "non_negative(fIs) & same_length(fIs, size)",
    "omega" = "non_negative(omega) & same_length(omega, size)",
    "rho" = "non_negative(rho) & same_length(rho, size)",
    "tau" = "non_negative(tau) & same_length(tau, size)",
    "v" = "non_negative(v) & same_length(v, size)",
    "ev" = "is_proportion(ev) & same_length(ev, size)",
    "wn" = "non_negative(wn) & same_length(wn, size)",
    "wv" = "non_negative(wv) & same_length(wv, size)",
    "A" = "non_negative(A) & same_length(A, size)",
    "B" = "non_negative(B) & same_length(B, size)",
    "D" = "non_negative(D) & same_length(D, size)",
    "season_A" = "is.numeric(season_A) & length(season_A) == 1 & all(abs(season_A <= 1))",
    "season_T" = "is.numeric(season_T) & length(season_T) == 1 & all(season_T > 0)",
    "season_phi" = "is.numeric(season_phi) & length(season_phi) == 1",
    "seed_times" = "in_order(seed_times)",
    "dist_seed_ages" = "is.numeric(dist_seed_ages) & same_length(dist_seed_ages, size)",
    "schedule" = "is.list(schedule)"
), function(exp) parse(text = exp))

population_optionals <- lapply(list(
    "dEv" = "some_positive(dEv)",
    "uv" = "non_negative(uv) & same_length(uv, size)",
    "yv" = "non_negative(yv) & same_length(yv, size)"
), function(exp) parse(text = exp))

optional_substitutes <- c(
    "dEv" = "dE",
    "uv" = "u",
    "yv" = "y"
)

req = function(
    requirements, parameters,
    existfmt = "Parameter '%s' required, but not found.",
    testfmt = "Parameter check '%s' failed."
)
{
    missed <- setdiff(names(requirements), names(parameters))
    if (length(missed)) stop(paste0(c("", sprintf(existfmt, missed)), collapse = "\n"))
    checks <- requirements[sapply(requirements, function(req, parameters) !eval(req, parameters), parameters = parameters)]
    if (length(checks)) stop(paste0(c("", sprintf(testfmt, checks)), collapse = "\n"))
}

opts <- function(
    requirements, parameters,
    substitutes,
    existfmt = "Optional parameter '%s' not found; using default substitute parameter '%s'.",
    testfmt = "Parameter check '%s' failed."
)
{
    missed <- setdiff(names(requirements), names(parameters))
    if (length(missed)) warning(
        paste0(c("", sprintf(existfmt, missed, substitutes[missed])), collapse = "\n")
    ) 
    for (par in missed) parameters[[par]] <- parameters[[substitutes[par]]]
    offered <- setdiff(names(requirements), missed)
    if (length(offered)) {
        checks <- requirements[sapply(requirements[offered], function(req, parameters) !eval(req, parameters), parameters = parameters)]
        if (length(checks)) stop(paste0(c("", sprintf(testfmt, checks)), collapse = "\n"))
    }
    return(parameters)
}

# check parameters for validity.
cm_check_parameters = function(parameters)
{
    req(requirements, parameters)
    for (popi in seq_along(parameters$pop)) {
        req(
            population_requirements, parameters$pop[[popi]],
            sprintf("Population parameter '%%s' required, but not found in population %i.", popi),
            sprintf("Parameters check '%%s' failed in population %i.", popi)
        )
        parameters$pop[[popi]] <- opts(
            population_optionals, parameters$pop[[popi]],
            optional_substitutes,
            sprintf("Optional parameter '%%s' not found in population %i; using default substitute parameter '%%s'.", popi),
            sprintf("Parameters check '%%s' failed in population %i.", popi)
        )
        if (!is.null(parameters$pop[[popi]]$observer)) {
            if (!(is.function(parameters$pop[[popi]]$observer) & length(formals(parameters$pop[[popi]]$observer) == 4))) {
                stop(paste0("observer has to be either NULL or a function taking 4 arguments, but is not in population", popi));
            }
        }
        
        schedule_times = sapply(parameters$pop[[popi]]$schedule, function(x) x$t);
        if (is.unsorted(schedule_times)) {
            stop(paste0("elements t of schedule need to be ordered, but are not in population ", popi));
        }
    }
    return(parameters)
}

# Get demographics for a given location, with error checking.
cm_get_demographics = function(dem_location, n_groups = NULL)
{
    # Get demographics.
    demographics = cm_populations[name == dem_location];
    if (nrow(demographics) == 0) {
        message(paste0("Could not find demographics for dem_location ", dem_location, "."));
        answer = readline(prompt = "View options? (y/n) ");
        if (answer != "n") {
            print(cm_populations[, unique(name)]);
        }
        stop();
    }
    if (nrow(demographics) != demographics[, uniqueN(age)]) {
        stop(paste0("Age not unique in cm_population[name == \"", dem_location, "\"]. This means cm_populations is misspecified for this location."));
    }
    
    # Adjust number of age groups if needed.
    if (!is.null(n_groups)) {
        if (n_groups > nrow(demographics)) {
            stop(sprintf("Requested %d age groups for model (up to %d+), but demographic data only goes up to %s.",
                         n_groups, (n_groups - 1) * 5, demographics[.N, age]));
        } else if (n_groups < nrow(demographics)) {
            demographics[n_groups]$f = demographics[n_groups:.N, sum(f)];
            demographics[n_groups]$m = demographics[n_groups:.N, sum(m)];
            demographics[n_groups]$age = demographics[n_groups, sub("-[0-9]+", "\\+", age)];
            demographics = demographics[1:n_groups];
        }
    }
    return (demographics);
}

# Get matrices for a given location, with error checking.
cm_get_matrices = function(mat_location, dem_location = NULL)
{
    # If requested, guess mat_location from dem_location.
    guess = F;
    if (mat_location == "guess") {
        if (is.null(dem_location)) {
            stop("cm_get_matrices needs a dem_location to guess the matrices location from.");
        }
        mat_location = dem_location;
        guess = T;
    }
    
    # Try to find matrices for mat_location.
    if (!mat_location %in% names(cm_matrices)) {
        message(paste0("Could not find matrices for mat_location ", mat_location, "."));
        answer = readline(prompt = "View options? (y/n) ");
        if (answer != "n") {
            print(names(cm_matrices));
        }
        stop();
    }
    if (sum(mat_location %in% names(cm_matrices) > 1)) {
        stop(paste0("Duplicate entries for ", mat_location, " in cm_matrices. This means cm_matrices is misspecified."));
    }
    mat = cm_matrices[[mat_location]];
    if (is.list(mat) & length(mat) > 0 & all(sapply(mat, is.matrix))) {
        return (mat);
    } else {
        if (guess) {
            stop(paste0("Could not guess mat_location for dem_location ", dem_location, "."));
        }
        stop(paste0("No valid entry in cm_matrices for matrix location ", mat_location, "."));
    }
}

# Split matrices
# ex_in: bounds separate into contacts *ex*clusively between lower groups, and *in*clusively between higher groups.
cm_split_matrices_ex_in = function(parameters, bounds)
{
    for (pi in seq_along(parameters$pop))
    {
        ng = nrow(parameters$pop[[pi]]$matrices[[1]]);
        if (any(bounds < 1 | bounds > ng)) {
            stop("Bounds must lie within [1, nrow(mat)] for splitting contact matrices.");
        }
        nmat0 = length(parameters$pop[[pi]]$matrices);
        parameters$pop[[pi]]$matrices = rep(parameters$pop[[pi]]$matrices, length(bounds) + 1);
        
        for (b in seq_along(bounds))
        {
            lb = floor(bounds[b]);
            fb = bounds[b] %% 1;
            mask1 = matrix(1, nrow = ng, ncol = ng);
            if (lb > 1) {
                mask1[1:(lb - 1), 1:(lb - 1)] = 0;
            }
            mask1[lb, 1:(lb-1)] = 1 - fb;
            mask1[1:(lb-1), lb] = 1 - fb;
            mask1[lb, lb] = (1 - fb)^2;
            mask0 = 1 - mask1;
            
            for (m in seq_len(nmat0)) {
                names(parameters$pop[[pi]]$matrices)[m + b * nmat0] = paste0(names(parameters$pop[[pi]]$matrices)[m + b * nmat0], b + 1);
                parameters$pop[[pi]]$matrices[[m + (b - 1) * nmat0]] = mask0 * parameters$pop[[pi]]$matrices[[m + (b - 1) * nmat0]];
                parameters$pop[[pi]]$matrices[[m +       b * nmat0]] = mask1 * parameters$pop[[pi]]$matrices[[m +       b * nmat0]];
            }
        }
        parameters$pop[[pi]]$contact = rep_len(parameters$pop[[pi]]$contact, nmat0 * (length(bounds) + 1));
    }
    
    return (parameters)
}

# TODO
# cm_split_matrices_in_ex
# cm_split_matrices_custom

def_matrix <- function(n_groups) {
    m <- diag(n_groups) * 0.5 + 0.5/n_groups
    dimnames(m) <- list(1:n_groups, 1:n_groups)
    return(m)
}

# Get default population parameters, SEI3R model
cm_base_pop_SEI3R = function(
    n_groups = 16,
    dE  = cm_delay_gamma(4.0, 4.0, t_max = 60, t_step = 0.25)$p, # Derived from Backer et al Eurosurveillance
    dIp = cm_delay_gamma(2.4, 4.0, t_max = 60, t_step = 0.25)$p, # Derived from Backer et al Eurosurveillance
    dIa = cm_delay_gamma(7.0, 4.0, t_max = 60, t_step = 0.25)$p, # Assumed 7 days subclinical shedding
    dIs = cm_delay_gamma(3.2, 3.7, t_max = 60, t_step = 0.25)$p, # Zhang et al 2020
    dC = 1, # no case reporting delay
    size = rep(1000, n_groups),
    imm0 = rep(0, n_groups),
    matrices = list(base = def_matrix(n_groups)),
    contact = rep(1, length(matrices)),
    contact_mult = numeric(),
    contact_lowerto = numeric(),
    
    u = rep(0.08, n_groups),
    y = rep(0.5, n_groups),
    fIp = rep(1, n_groups),
    fIs = rep(1, n_groups),
    fIa = rep(0.5, n_groups),
    omega = rep(0, n_groups),
    rho = rep(1, n_groups),
    tau = rep(1, n_groups),
    v = rep(0, n_groups),
    ev = rep(1, n_groups),
    wn = rep(0, n_groups),
    wv = rep(0, n_groups),
    A = rep(0, n_groups),
    B = rep(0, n_groups),
    D = rep(0, n_groups),
    
    season_A = 0,
    season_T = 365.25,
    season_phi = 0,
    
    seed_times = 1,
    dist_seed_ages = rep(1, n_groups),
    
    schedule = list(),
    observer = NULL,
    name = "pop_name",
    group_names = colnames(matrices[[1]]),
    ... # any unnamed elements will be ignored
) {
    # cannot use {} notation here, since creates a sub-environment
    for (key in c(
        "u", "y", "fIp", "fIs", "fIa", "omega", "rho", "tau", "v", "ev", "wn", "wv", "A", "B", "D"
    )) if (
        length(get(key)) == 1
    ) assign(key, rep(get(key), n_groups)) else if (
        length(get(key)) != n_groups
    ) stop(sprintf("Parameter %s is not length 1 or n_groups = %i.", key, n_groups))
    
    rm(key)
    return(as.list(environment()))
}

# Build parameters for a single location, SEI3R model
cm_build_pop_SEI3R = function(
    dem_location,
    mat_location = "guess",
    ...,
    matrices = cm_get_matrices(mat_location, dem_location),
    n_groups = nrow(matrices[[1]]),
    demographics = cm_get_demographics(dem_location, n_groups),
    size = demographics[, round((f + m) * 1000)],
    name = dem_location
) do.call(cm_base_pop_SEI3R, c(as.list(environment()), list(...)))

# Get default simulation parameters, SEI3R model
cm_base_parameters_SEI3R = function(
    n_groups = 1,
    model = "SEI3R",
    time_step = 0.25,
    date0 = "2020-01-01",
    time0 = 0,
    time1 = 365,
    report_every = 4,
    fast_multinomial = F,
    deterministic = T,
    pop = list(cm_base_pop_SEI3R(n_groups = n_groups)),
    travel = diag(length(pop)),
    processes = NULL
) {
    ret <- as.list(environment())
    ret$n_groups <- NULL
    cm_check_parameters(cm_translate_parameters(ret))
}

# Build parameters for one or several locations, SEI3R model
cm_parameters_SEI3R = function(
    dem_locations, mat_locations = "guess",
    date_start = "2020-03-01", date_end = "2021-03-01", deterministic = T,
    processes = NULL,
    ...
)
{
    # Check parameters
    if (length(mat_locations) != length(dem_locations)) {
        if (length(mat_locations) == 1) {
            mat_locations = rep_len(mat_locations, length(dem_locations));
        } else {
            stop("dem_locations and mat_locations must have the same length; or mat_locations can have length 1 and will be used for all dem_locations.");
        }
    }
    
    # Get population parameters.
    pop = list();
    for (i in seq_along(dem_locations))
    {
        pop[[i]] = cm_build_pop_SEI3R(dem_locations[i], mat_locations[i], ...);
    }

    # Build simulation parameters around this population.
    parameters = cm_base_parameters_SEI3R(
        n_groups = length(pop[[1]]$size),
        pop = pop, date0 = date_start,
        time0 = 0, time1 = date_end,
        deterministic = deterministic,
        processes = processes
    );
    
    return(parameters)
}

# Get regions for the UK.
cm_uk_locations = function(country, level) {
    # Check country code
    country = toupper(country);
    if (country == "UK") { 
        country = "EWSN";
    }
    if (!country %like% "^(UK|[EWSN]+)$") {
        stop("country must be UK, or a combination of E, W, S, and/or N.");
    }
    
    # Interpret level
    level = as.integer(level);
    if (level < 0 | level > 4) {
        stop("level must be 0, 1, 2, 3, or 4");
    }
    
    if (level == 0) {
        if (country != "EWSN") {
            stop("For level 0, country must be UK.");
        }
        return ("UK | UNITED KINGDOM");
    } else if (level == 1) {
        gE = "Country";
        gW = "Country";
        gS = "Country";
        gN = "Country";
    } else if (level == 2) {
        gE = "Region";
        gW = "Country";
        gS = "Country";
        gN = "Country";
    } else if (level == 3) {
        gE = c("Metropolitan County", "County", "Unitary Authority", "London Borough");
        gW = "Unitary Authority";
        gS = "Council Area";
        gN = "Local Government District";
    } else if (level == 4) {
        gE = c("Metropolitan District", "Non-metropolitan District", "Unitary Authority", "London Borough");
        gW = "Unitary Authority";
        gS = "Council Area";
        gN = "Local Government District";
    }
    
    # Extract locations
    locs = NULL;
    if (country %like% "E") { locs = c(locs, cm_structure_UK[Code %like% "^E" & Geography1 %in% gE, Name]); }
    if (country %like% "W") { locs = c(locs, cm_structure_UK[Code %like% "^W" & Geography1 %in% gW, Name]); }
    if (country %like% "S") { locs = c(locs, cm_structure_UK[Code %like% "^S" & Geography1 %in% gS, Name]); }
    if (country %like% "N") { locs = c(locs, cm_structure_UK[Code %like% "^N" & Geography1 %in% gN, Name]); }
    return (paste0("UK | ", locs));
}

# get prevalence of morbidities that increase risk of Covid per age-group
# by default, returns prevalence of every included morbidity per age-group
# to return overall prevalence, set aggregate = TRUE
#  to account for mutli-morbidities, set correlation of people with multiple morbidities multimorbidity_corr
cm_high_risk_prevalence <- function(country, aggregate = FALSE, multimorbidity_corr = 0.8){
    cnt <- country
    country_data <- cm_highrisk[country == cnt]
    if(aggregate){
        country_data <- country_data[, .(maxprev = max(prevalence, na.rm = T), cmbprev = 1-prod(1-prevalence, na.rm = T)), by=c("country", "age_from", "age_to")]
        country_data <- country_data[, .(highrisk = maxprev+((cmbprev-maxprev)*(1-multimorbidity_corr))), by=c("country", "age_from", "age_to")]
    }
    return(country_data)
}