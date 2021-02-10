require(wpp2019)
require(data.table)

#' odds ratio adjustment of a proportion
#' (p_target / (1- p_target)) / (p_reference / (1-p_reference)) = OR_target_over_reference
odds_shift = function(reference_group_p, or_target_over_ref) {
    a = reference_group_p / (1 - reference_group_p) * or_target_over_ref
    return(a / (a + 1))
}

#' Want:
#' IFR, IHR as logistic function of age + OR shift due to sex
#' to then compute aggregate IFR, IHR by age category for the population in question
#' 
#' Have:
#' mean IHR, IFR by age & sex (Salje), mean IFR by age (Levin)
#' ORs 

subpopulate <- function(countries) {
    filt <- if (missing(countries)) {
        expression(1:.N)
    } else expression(name %in% countries)
    pF <- as.data.table(get(data(popF)))[ eval(filt), .(name, age = factor(age, levels = unique(age), ordered = TRUE), popF = `2020`*1000) ]
    pM <- as.data.table(get(data(popM)))[ eval(filt), .(name, age = factor(age, levels = unique(age), ordered = TRUE), popM = `2020`*1000) ]
    pF[pM, on=.(name, age)][, prop_F := popF / (popM+popF) ][, lb := as.integer(gsub("\\+","",gsub("-\\d+$","", as.character(age)))) ]
}

# Probability of ICU given hospitalisation (derived from CO-CIN)
picu_cocin = function(
    x = c(-0.1309118, 0, 17.2398874, 65.7016492, 100),
    y = c(-2.1825091, -2.1407043, -1.3993552, -1.2344361, -8.8191062)
) {
    p = function(age) exp(splinefun(x, y)(age))
    return (function(age) p(age) / (1 + p(age)))
}()

#' IFR from https://doi.org/10.1007/s10654-020-00698-1
#' in that pub
#' log10(IFR in %) = -3.27 + 0.0524*age
#' IFR * 10^2 = 10^(-3.27)*10^(0.0524*age)
#' changing to
#' logit(IFR) basis
ifr_levin = function(age) exp(-7.56 + 0.121 * age) / (100 + exp(-7.56 + 0.121 * age))

#' Infection hospitalisation rate (derived from https://doi.org/10.1126/science.abc3517)
ihr_salje = function(age) exp(-7.37 + 0.068 * age) / (1 + exp(-7.37 + 0.068 * age));

subpops <- subpopulate(c("Pakistan","France"))

subpops[,{
    socialmixr::pop_age(
        .SD, 0:85, "lb", "popF"
    )  
},by=name]



#' 

get_burden_processes = function(
    lmic_shift = 1, critical2, pop_weight = subpops[name == "Pakistan", c(popF+popM)],
    prop_f = 0.5
) {
    
    # Amalgamate probabilities
    probabilities = data.table(
        age = 0:85,
        pweight = pop_weight / sum(pop_weight),
        prop_f = prop_f
    )[,
        c("ihr","ifr","picu") = .(ihr_salje(age), ifr_levin(age), picu_cocin(age))
    ][,
      age_group := pmin(15, age %/% 5)
    ]
    
    probabilities = probabilities[, lapply(.SD, mean), by = age_group, .SDcols = 2:4]
    
    # Create model burden processes
    P.critical = odds_shift(probabilities[, ihr * picu],       lmic_shift);
    P.severe   = odds_shift(probabilities[, ihr * (1 - picu)], lmic_shift);
    P.death    = odds_shift(probabilities[, ifr],              lmic_shift);
    
    burden_processes = list(
        list(source = "E", type = "multinomial", names = c("death", "null"), report = c("o", ""),
            prob = matrix(c(P.death, 1 - P.death), nrow = 2, ncol = 16, byrow = T),
            delays = matrix(c(cm_delay_gamma(26, 5, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 2, byrow = T)),

        list(source = "E", type = "multinomial", names = c("to_hosp_critical", "to_hosp_critical2", "to_hosp_severe", "null"), report = c("", "", "", ""),
            prob = matrix(c(P.critical * (1 - critical2), P.critical * critical2, P.severe, 1 - P.critical - P.severe), nrow = 4, ncol = 16, byrow = T),
            delays = matrix(c(cm_delay_gamma(8.5, 5, 60, 0.25)$p, cm_delay_gamma(8.5, 5, 60, 0.25)$p, cm_delay_gamma(8.5, 5, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 4, byrow = T)),

        list(source = "to_hosp_severe", type = "multinomial", names = "non_icu_severe", report = "pi",
            prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
            delays = matrix(cm_delay_gamma(14.6, 5, 60, 0.25)$p, nrow = 1, byrow = T)),

        list(source = "to_hosp_critical2", type = "multinomial", names = "non_icu_critical2", report = "pi",
            prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
            delays = matrix(cm_delay_gamma(15.6, 5, 60, 0.25)$p, nrow = 1, byrow = T)),

        list(source = "to_hosp_critical", type = "multinomial", names = "non_icu_critical", report = "pi",
            prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
            delays = matrix(cm_delay_gamma(6.0, 5, 60, 0.25)$p, nrow = 1, byrow = T)),

        list(source = "non_icu_critical", type = "multinomial", names = "icu_critical", report = "pi",
            prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
            delays = matrix(cm_delay_gamma(9.6, 5, 60, 0.25)$p, nrow = 1, byrow = T))
    )
    
    return (burden_processes);
}
