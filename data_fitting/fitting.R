library(data.table)
library(ggplot2)
library(cowplot)
library(lubridate)
library(here)
library(cowplot)
library(readxl)
library(socialmixr)
library(qs)
library(zoo)
library(stringr)
library(socialmixr)

# plots for presentation
if (0) {

    fitS0 = qread("~/Dropbox/Covid_LMIC/nCoV-LMIC/fit_sindh.qs")

    cm_source_backend(
        user_defined = fitS0$user_defined
    );

    test = cm_backend_sample_fit_test(cm_translate_parameters(fitS0$par), fitS0$post, 100, seed = 0);
    test = rbindlist(test)
    
    f1 = test[group == 2, .(y = sum(obs0)), by = .(t, run)]
    f2 = test[, .(y = sum(cases)), by = .(t, run)]
    
    ggplot(f1[t > 90]) + 
        geom_line(aes(x = t + ymd("2020-01-01"), y = y, group = run), colour = "#aaaaff") +
        geom_point(data = epi[location == "Sindh"], aes(x = date, y = deaths)) +
        cowplot::theme_cowplot() +
        labs(x = "Date", y = "Deaths")

    ggplot(f2[t > 90]) + 
        geom_line(aes(x = t + ymd("2020-01-01"), y = y * 0.02, group = run), colour = "#aaaaff") +
        geom_point(data = epi[location == "Sindh"], aes(x = date, y = cases)) +
        cowplot::theme_cowplot() +
        labs(x = "Date", y = "Cases")
    
    ggplot(f2[t > 90]) + 
        geom_line(aes(x = t + ymd("2020-01-01"), y = y * asc(t / 243, 0.005, 0.07, -15, 5), group = run), colour = "#aaaaff") +
        geom_point(data = epi[location == "Sindh"], aes(x = date, y = cases)) +
        cowplot::theme_cowplot() +
        labs(x = "Date", y = "Cases")
    
    dt = data.table(t = 0:243)
    dt[, y := asc(t / 243, 0.005, 0.07, -15, 5)]
    
    ggplot(dt[t > 90]) + 
        geom_line(aes(x = t + ymd("2020-01-01"), y = 100 * y), colour = "#cc6633") +
        cowplot::theme_cowplot() +
        labs(x = "Date", y = "Ascertainment rate (%)") +
        ylim(0, NA)
    
    posterior = as.data.table(fitS0$post)
    posterior = melt(posterior, id.vars = 1:4)
    ggplot(posterior) + geom_histogram(aes(value)) + facet_wrap(~variable, scales = "free") + cowplot::theme_cowplot()
   
}

#
# SETUP
#

# set up covidm
cm_path = "~/Dropbox/nCoV/covidm/";
cm_force_rebuild = F;
cm_build_verbose = T;
cm_version = 2;
source(paste0(cm_path, "/R/covidm.R"))

source("helper.R")
source("fitting_setup.R")
source("process_def.R")

# load epi and mobility data
epi = fread("./epi_data.csv")
mob = fread("./mob_data.csv")
birthrates = fread("./birthrates.csv")
mortality = fread("./mortality.csv")

#
# RUNS
#

make_params = function(dem, mat, mob_loc, lmic_shift = 1, waning = FALSE, demographics = FALSE)
{
    date0 = "2020-01-01"
    date1 = "2020-09-15"
    
    # Build parameters
    params = cm_parameters_SEI3R(dem, mat, deterministic = T, date_start = date0, date_end = date1,
        dE  = cm_delay_gamma(2.5, 2.5, t_max = 15, t_step = 0.25)$p,
        dIp = cm_delay_gamma(1.5, 4.0, t_max = 15, t_step = 0.25)$p,
        dIs = cm_delay_gamma(3.5, 4.0, t_max = 15, t_step = 0.25)$p,
        dIa = cm_delay_gamma(5.0, 4.0, t_max = 15, t_step = 0.25)$p);
    
    # Demographic change
    if (demographics) {
        params$pop[[1]]$A = rep(1 / (5 * 365.25), 16);
        params$pop[[1]]$B = c(birthrates[name == mat, birth_rate / 365.25], rep(0, length(params$pop[[1]]$size) - 1));
        mort = mortality[name == mat, mx]
        ex = data.table(age = 75:124, mx = c(rep(mort[16:20], each = 5), rep(mort[21], 25)))
        ex[, p := 1]
        for (i in 2:nrow(ex)) {
            ex[i]$p = ex[i - 1]$p * (1 - ex[i - 1]$mx);
        }
        lex_75 = weighted.mean(ex$age, ex$p) - 75;
        mort[16] = 1 / lex_75;
        params$pop[[1]]$D = c(mort[1:15], 1 / lex_75) / 365.25;
    }
    
    # TODO Note: no splitting of matrices into old vs young
    
    # Build time series of contact changes
    params$schedule = list(contact_schedule(mob, mob_loc, "google", date0, "2031-12-31"));
    
    # Load age-varying symptomatic rate
    covid_scenario = qread("./2-linelist_both_fit_fIa0.5-rbzvih.qs");
    covu = unname(rep(colMeans(covid_scenario[,  5:12]), each = 2));
    covy = unname(rep(colMeans(covid_scenario[, 13:20]), each = 2));

    # Adjust parameters for shift
    covy = odds(covy, lmic_shift);

    # Set susceptibility and clinical fraction
    for (i in seq_along(params$pop)) {
        params$pop[[i]]$u = covu / mean(covu);
        params$pop[[i]]$y = covy;
    }

    # Health burden processes
    params$processes = get_burden_processes(lmic_shift, 0);
    
    # Set susceptibility to get initial R0 = 1 (will be adjusted later)
    params$pop[[1]]$u = params$pop[[1]]$u * 1.0 / cm_calc_R0(params, 1);
    
    # Set natural waning
    if (is.numeric(waning)) {
        params$pop[[1]]$wn = rep(waning, 16);
    }
    
    return (params)
}


do_fitting = function(dem, mat, epi_loc, mob_loc, waning, contact, demographics = FALSE, lmic_shift = 1, burn_in = 2000, 
    R0prior = "N 2.4 1.2 T 0 6", smooth = FALSE)
{
    par = make_params(dem, mat, mob_loc, lmic_shift, waning, demographics);

    # Set priors
    priors = list(
        t0 = "U 0 60",
        R0 = R0prior,
        ad_sd = "N 0 5 T 1 100",
        ad_y0 = "B 1 1",
        ad_y_lo = "N 0 0.5 T 0 2",
        ad_s0 = "E 1 1",
        ad_s1 = "E 1 1",
        ac_sd = "N 0 50 T 1 1000",
        ac_y0 = "B 1 1",
        ac_y_lo = "N 0 0.5 T 0 2",
        ac_s0 = "E 0.1 0.1",
        ac_s1 = "E 0.1 0.1"
    );
    
    i_wn = NA;
    if (waning == TRUE) {
        i_wn = length(priors) + 1;
        priors = c(priors,
            wn = "N 0 0.002 T 0 0.1"
        );
    }
    
    i_contact = NA;
    if (contact == TRUE) {
        i_contact = length(priors) + 1;
        priors = c(priors,
            c_y0 = "B 3 3",
            c_y1 = "B 3 3",
            c_s0 = "N 10 10 T 0 100",
            c_s1 = "N 10 10 T 0 100"
        );
    }

    # likelihood function
    # create c++ likelihood components
    cpp_lik = function(params, epi, loc, i_contact)
    {
        epi2 = epi[location == loc];
        if (smooth) {
            epi2 = epi2[order(date)]
            epi2 = epi2[, .(date, cases = rollmean(cases, 7, fill = NA), deaths = rollmean(deaths, 7, fill = NA)), by = location]
        }
        ret = NULL;
    
        ret = c(ret,
            epi2[location == loc & !is.na(deaths) & deaths < 300 # & deaths != 0
                , paste0(
                #'ll += nbinom(', deaths, ', max(0.1, dyn.Obs(', as.numeric(ymd(date) - ymd(params$date0)), ', 0, 1, 0)), x[2]);'
                '{ double d = dyn.Obs(', as.numeric(ymd(date) - ymd(params$date0)), ', 0, 1, 0) - ', deaths, '; ll += -0.5 * (d / x[2]) * (d / x[2]) - log(x[2] * sqrt(2 * M_PI)); }'
            )],
            epi2[location == loc & !is.na(cases)
                , paste0(
                '{ double d = dyn.Obs(', as.numeric(ymd(date) - ymd(params$date0)), ', 0, 2, 0) - ', cases,  '; ll += -0.5 * (d / x[7]) * (d / x[7]) - log(x[7] * sqrt(2 * M_PI)); }'
            )]
        )
        
        if (!is.na(i_contact)) {
            ret = c(ret, 
                str_glue('if (x[{i_contact}] < x[{i_contact + 1}]) ll += -1000;')
            )
        }
            
        return (ret)
    }
    
    # changes
    # load user defined functions
    ud = list(
        model_v2 = list(
            cpp_changes = c(
                'for (auto& u : P.pop[0].u)',
                '    u *= x[1];',
                'P.pop[0].seed_times = vector<double>(10, (int)x[0]);',
                
                ## for waning
                ifelse(waning == TRUE,
                    str_glue('P.pop[0].wn = vector<double>(16, x[{i_wn - 1}]);'),
                    ""
                ),

                ## for custom contact
                ifelse(contact,
                    paste(sep = "\n",
                        'auto asc0 = [&](double x, double y0, double y1, double s0, double s1) {',
                        '    double xx = s0 + x * (s1 - s0);',
                        '    double h0 = exp(s0) / (1 + exp(s0));',
                        '    double h1 = exp(s1) / (1 + exp(s1));',
                        '    double h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0);',
                        '    return y0 + (y1 - y0) * h;',
                        '};',

                        str_glue('double Y0 = x[{i_contact - 1}], Y1 = x[{i_contact}], S0 = x[{i_contact + 1}], S1 = x[{i_contact + 2}];'),
                        'for (unsigned int ch = 0; ch < P.changes.ch[0].times.size(); ++ch) {',
                        '    double td = P.changes.ch[0].times[ch];',
                        '    double q = (td < 100) ? asc0(td / 100, 0, Y0, -32.4, 7.6) : ((td < 365) ? (asc0((td - 100) / (365 - 100), Y0, Y1, -S0, S1)) : Y1);',
                        '    P.changes.ch[0].values[td][0] = 1 + q * 0.3;',
                        '    P.changes.ch[0].values[td][1] = 1 - q;',
                        '    P.changes.ch[0].values[td][2] = (td < 81) ? 1 : 0;',
                        '    P.changes.ch[0].values[td][3] = 1 - q;',
                        '}'
                    ),
                    ""
                )
                
                ## case delay
                # 'P.pop[0].dC = delay_gamma(x[11], 4, 60, P.time_step);'
            ),
            cpp_loglikelihood = cpp_lik(par, epi, epi_loc, i_contact),
            cpp_observer = c(
                'auto odds = [&](double x, double odds_ratio) {',
                '    double a = x / (1 - x);',
                '    return a * odds_ratio / (a * odds_ratio + 1);',
                '};',

                'auto asc = [&](double x, double y0, double y_lo, double s0, double s1) {',
                '    double y1 = odds(y0, exp(y_lo));',
                '    double xx = s0 + x * (s1 - s0);',
                '    double h0 = exp(s0) / (1 + exp(s0));',
                '    double h1 = exp(s1) / (1 + exp(s1));',
                '    double h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0);',
                '    return y0 + (y1 - y0) * h;',
                '};',

                'dyn.Obs(t, 0, 0, 0) = estimate_Rt(P, dyn, t, 0, 50);',
                'dyn.Obs(t, 0, 1, 0) = dyn("death_o", t, {}, {}) * asc((t - P.time0) / (P.time1 - P.time0), x[3], x[4], -x[5], x[6]);',
                'dyn.Obs(t, 0, 2, 0) = dyn("cases_reported", t, {}, {}) * asc((t - P.time0) / (P.time1 - P.time0), x[8], x[9], -x[10], x[11]);'
            )
        )
    )
    
    cm_source_backend(
        user_defined = ud
    );

    post = cm_backend_mcmc_test(cm_translate_parameters(par), priors,
        seed = 0, burn_in = burn_in, iterations = 1000, n_threads = 6, classic_gamma = T);
    
    test = cm_backend_sample_fit_test(cm_translate_parameters(par), post, 50, seed = 0);
    test = rbindlist(test)

    return (list(post = post, par = par, user_defined = ud, sample_dynamics = test))
}

ggplot(fitS0$sample_dynamics[group == 1]) + geom_point(aes(x = t, y = obs0))
ggplot(fitS0$sample_dynamics[group == 2]) + geom_point(aes(x = t, y = obs0))
ggplot(fitS0$sample_dynamics[group == 3]) + geom_point(aes(x = t, y = obs0))

# trim epi data
epi = epi[date <= "2020-09-15"]
#epi = epi[date <= "2020-08-31" & date >= "2020-07-19"] ##### TEMP #####

sched = list(contact_schedule(mob, "Sindh", "google", "2020-01-01", "2031-12-31"));

dic = function(filename)
{
    fit = qread(filename)
    post = as.data.table(fit$post)
    post[, D := -2 * ll]
    post[, 0.5 * var(D) + mean(D)];
}


            cpp_observer = c(
                'auto odds = [&](double x, double odds_ratio) {',
                '    double a = x / (1 - x);',
                '    return a * odds_ratio / (a * odds_ratio + 1);',
                '};',

                'auto asc = [&](double x, double y0, double y_lo, double s0, double s1) {',
                '    x = min(1.0, max(0.0, x));',
                '    double y1 = odds(y0, exp(y_lo));',
                '    double xx = s0 + x * (s1 - s0);',
                '    double h0 = exp(s0) / (1 + exp(s0));',
                '    double h1 = exp(s1) / (1 + exp(s1));',
                '    double h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0);',
                '    return y0 + (y1 - y0) * h;',
                '};',

                'dyn.Obs(t, 0, 0, 0) = estimate_Rt(P, dyn, t, 0, 50);',
                'dyn.Obs(t, 0, 1, 0) = dyn("death_o", t, {}, {}) * asc((t - P.time0) / (258 - P.time0), x[3], x[4], -x[5], x[6]);',
                'dyn.Obs(t, 0, 2, 0) = dyn("cases_reported", t, {}, {}) * asc((t - P.time0) / (258 - P.time0), x[8], x[9], -x[10], x[11]);'
            )

dic("fitdS0.qs")
dic("fitdSl.qs")
dic("fitdSw_5.0.qs")
dic("fitdSw_2.5.qs")
dic("fitdSw_1.0.qs")
    

fit = qread("fitdS0.qs")
fit = qread("fitdSl.qs")
fit = qread("fitdSw_5.0.qs")
fit = qread("fitdSw_2.5.qs")
fit = qread("fitdSw_1.0.qs")

fit$par$schedule = sched

qsave(fit, "fitdS0.qs")
qsave(fit, "fitdSl.qs")
qsave(fit, "fitdSw_5.0.qs")
qsave(fit, "fitdSw_2.5.qs")
qsave(fit, "fitdSw_1.0.qs")


# Gauteng
fitG0 = do_fitting("Gauteng", "South Africa", "Gauteng", "Gauteng",         waning = FALSE, contact = FALSE, burn_in = 5000)
qsave(fitG0, "fitG0.qs")
fitGs = do_fitting("Gauteng", "South Africa", "Gauteng", "Gauteng",         waning = FALSE, contact = FALSE, burn_in = 5000, smooth = TRUE)
qsave(fitGs, "fitGs.qs")
fitGw = do_fitting("Gauteng", "South Africa", "Gauteng", "Gauteng",         waning = TRUE,  contact = FALSE, burn_in = 5000)
qsave(fitGw, "fitGw.qs")

fitD0 = do_fitting("NCT of Delhi",   "India", "Delhi", "Delhi",             waning = FALSE, contact = FALSE, burn_in = 5000)
qsave(fitD0, "fitD0.qs")
fitDs = do_fitting("NCT of Delhi",   "India", "Delhi", "Delhi",             waning = FALSE, contact = FALSE, burn_in = 5000, smooth = TRUE)
qsave(fitDs, "fitDs.qs")
fitDw = do_fitting("NCT of Delhi",   "India", "Delhi", "Delhi",             waning = TRUE,  contact = FALSE, burn_in = 5000)
qsave(fitDw, "fitDw.qs")
fitDc = do_fitting("NCT of Delhi",   "India", "Delhi", "Delhi",             waning = FALSE, contact = TRUE,  burn_in = 5000)
qsave(fitDc, "fitDc.qs")

fitN0 = do_fitting("Nairobi",        "Kenya", "Nairobi", "Nairobi County",  waning = FALSE, contact = FALSE, burn_in = 5000)
qsave(fitN0, "fitN0.qs")
fitNs = do_fitting("Nairobi",        "Kenya", "Nairobi", "Nairobi County",  waning = FALSE, contact = FALSE, burn_in = 5000, smooth = TRUE)
qsave(fitNs, "fitNs.qs")
fitNw = do_fitting("Nairobi",        "Kenya", "Nairobi", "Nairobi County",  waning = TRUE,  contact = FALSE, burn_in = 5000)
qsave(fitNw, "fitNw.qs")

fitS0 = do_fitting("Sind",        "Pakistan", "Sindh", "Sindh",             waning = FALSE, contact = FALSE, burn_in = 6000, R0prior = "N 2.4 1.2 T 2 6 I 2.39 2.41")
qsave(fitS0, "fitS0.qs")
fitSs = do_fitting("Sind",        "Pakistan", "Sindh", "Sindh",             waning = FALSE, contact = FALSE, burn_in = 6000, R0prior = "N 2.4 1.2 T 2 6 I 2.39 2.41", smooth = TRUE)
qsave(fitSs, "fitSs.qs")
fitSw = do_fitting("Sind",        "Pakistan", "Sindh", "Sindh",             waning = TRUE,  contact = FALSE, burn_in = 6000, R0prior = "N 2.4 1.2 T 2 6 I 2.39 2.41")
qsave(fitSw, "fitSw.qs")
fitSl = do_fitting("Sind",        "Pakistan", "Sindh", "Sindh",             waning = FALSE, contact = FALSE, burn_in = 6000, R0prior = "N 2.3 0.01 T 2.28 2.32")
qsave(fitSl, "fitSl.qs")

# Model sets for Sindh paper
# compare = qread("fitS0.qs")
# as.data.table(compare[[1]])
fitS0 = do_fitting("Sind",            "Pakistan", "Sindh", "Sindh",             waning = FALSE, demographics = TRUE, contact = FALSE, burn_in = 6000, R0prior = "N 2.4 1.2 T 2 6 I 2.39 2.41")
qsave(fitS0, "fitdS0.qs")
fitSl = do_fitting("Sind",            "Pakistan", "Sindh", "Sindh",             waning = FALSE, demographics = TRUE, contact = FALSE, burn_in = 6000, R0prior = "N 2.3 0.01 T 2.28 2.32")
qsave(fitSl, "fitdSl.qs")
fitSw_1.0 = do_fitting("Sind",        "Pakistan", "Sindh", "Sindh",             waning = 1 / (365 * 1.0), demographics = TRUE, contact = FALSE, burn_in = 6000, R0prior = "N 2.4 1.2 T 2 6 I 2.39 2.41")
qsave(fitSw_1.0, "fitdSw_1.0.qs")
fitSw_2.5 = do_fitting("Sind",        "Pakistan", "Sindh", "Sindh",             waning = 1 / (365 * 2.5), demographics = TRUE, contact = FALSE, burn_in = 6000, R0prior = "N 2.4 1.2 T 2 6 I 2.39 2.41")
qsave(fitSw_2.5, "fitdSw_2.5.qs")
fitSw_5.0 = do_fitting("Sind",        "Pakistan", "Sindh", "Sindh",             waning = 1 / (365 * 5.0), demographics = TRUE, contact = FALSE, burn_in = 6000, R0prior = "N 2.4 1.2 T 2 6 I 2.39 2.41")
qsave(fitSw_5.0, "fitdSw_5.0.qs")

check("fitdS0.qs", "Sindh") # yep
check("fitdSl.qs", "Sindh") # 
check("fitSl.qs", "Sindh") # 
check("fitdSw_1.0.qs", "Sindh") # 
check("fitdSw_2.5.qs", "Sindh") # 
check("fitdSw_5.0.qs", "Sindh") # 
w = qread("fitdS0.qs")
ww = as.data.table(w[[1]])
ggplot(ww) + geom_point(aes(x = trial, y = lp))
ggplot(melt(ww, id.vars = 1:4)) + geom_histogram(aes(x = value, fill = variable)) + facet_wrap(~variable, scales = "free")
library(GGally)
ggpairs
ggpairs(ww, columns = 5:16)
pairs(~t0+R0+ad_sd+ad_y0+ad_y_lo+ad_s0+ad_s1+ac_sd+ac_y0+ac_y_lo+ac_s0+ac_s1, data = ww)


fitA0 = do_fitting("Addis Abeba", "Ethiopia", "Addis Ababa", "Addis Ababa", waning = FALSE, contact = FALSE, burn_in = 5000)
qsave(fitA0, "fitA0.qs")
fitAs = do_fitting("Addis Abeba", "Ethiopia", "Addis Ababa", "Addis Ababa", waning = FALSE, contact = FALSE, burn_in = 5000, smooth = TRUE)
qsave(fitAs, "fitAs.qs")
fitAw = do_fitting("Addis Abeba", "Ethiopia", "Addis Ababa", "Addis Ababa", waning = TRUE,  contact = FALSE, burn_in = 5000)
qsave(fitAw, "fitAw.qs")

# test

check = function(fit_filename, epi_location)
{
    fit = qread(fit_filename)
    
    cm_source_backend(user_defined = fit$user_defined);
    #fit$par$time1 = "2020-12-31"
    test = cm_backend_sample_fit_test(cm_translate_parameters(fit$par), fit$post, 100, seed = 0);
    test = rbindlist(test)
    
    true_deaths = test[, .(deaths = sum(death_o)), by = .(run, t)]
    true_cases = test[, .(cases = sum(cases)), by = .(run, t)]
    total_pop = test[run == 1 & t == 0, sum(S)]
    
    pl1 = ggplot(test[group == 2 & t >= 59, .(d = obs0), by = .(run, t)]) + 
        geom_point(aes(x = t + ymd("2020-01-01"), y = d, colour = run)) +
        geom_line(data = epi[location == epi_location & date <= "2020-09-15"], aes(x = date, y = deaths)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        theme(legend.position = "none") + labs(x = NULL, y = "Deaths", title = epi_location)
    
    pl2 = ggplot(test[group == 3 & t >= 59, .(d = obs0), by = .(run, t)]) + 
        geom_point(aes(x = t + ymd("2020-01-01"), y = d, colour = run)) +
        geom_line(data = epi[location == epi_location & date <= "2020-09-15"], aes(x = date, y = cases)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        theme(legend.position = "none") + labs(x = NULL, y = "Cases")
    
    ad = quantile(test[group == 2, sum(obs0), by = run]$V1, c(0.025, 0.5, 0.975))
    ud = quantile(true_deaths[, sum(deaths), by = run]$V1 - test[group == 2, sum(obs0), by = run]$V1, c(0.025, 0.5, 0.975))
    ac = quantile(test[group == 3, sum(obs0), by = run]$V1, c(0.025, 0.5, 0.975))
    uc = quantile(true_cases[, sum(cases), by = run]$V1 - test[group == 3, sum(obs0), by = run]$V1, c(0.025, 0.5, 0.975))
    
    bars = data.table(
        indicator = c("Deaths (000s)", "Deaths (000s)", "Cases (000s)", "Cases (000s)"),
        asc = c("ascertained", "unascertained", "ascertained", "unascertained"));
    bars = cbind(bars, rbind(ad, ud, ac, uc));
    bars[, indicator := factor(indicator, unique(indicator))];
    bars[, asc := factor(asc, unique(asc))];
    pl3 = ggplot(bars) +
        geom_col(aes(x = indicator, y = `50%`/1000, fill = asc), colour = "black", size = 0.125, position = position_stack(reverse = TRUE)) +
        theme(legend.position = "bottom", axis.text.x = element_blank(), legend.key.size = unit(4, "pt"), strip.background = element_blank()) +
        facet_wrap(~indicator, scales = "free") +
        labs(x = NULL, y = NULL, fill = NULL)
    
    cowplot::plot_grid(cowplot::plot_grid(pl1, pl2, ncol = 1, rel_heights = c(1.2, 1)), pl3, rel_widths = c(2, 1))
}

check("fitG0.qs", "Gauteng") # yep
p1 = check("fitGs.qs", "Gauteng") # similar to G0 but narrower CrIs

check("fitD0.qs", "Delhi") # nope
p2 = check("fitDs.qs", "Delhi") # better but still not
check("fitDw.qs", "Delhi") # just about
check("fitDc.qs", "Delhi") # not really

check("fitN0.qs", "Nairobi") # more or less
check("fitNw.qs", "Nairobi") # more or less
p3 = check("fitNs.qs", "Nairobi") # better!

check("fitA0.qs", "Addis Ababa") # more or less
p4 = check("fitAs.qs", "Addis Ababa") # better

check("fitS0.qs", "Sindh") # yep
p5 = check("fitSs.qs", "Sindh") # 
check("fitSl.qs", "Sindh") #

plot = cowplot::plot_grid(p1, p2, p3, p4, p5, ncol = 2)
ggsave("~/Desktop/5_cities_fit.pdf", plot, width = 40, height = 30, units = "cm")
ggsave("~/Desktop/5_cities_fit.png", plot, width = 40, height = 30, units = "cm")

check("fitS0.qs", "Sindh") # yep
check("fitSw_1.0.qs", "Sindh") # yep
check("fitSw_2.5.qs", "Sindh") # yep
check("fitSw_5.0.qs", "Sindh") # yep

# Process fits for vaxco
fitS0 = qread("fitS0.qs")
fitSl = qread("fitSl.qs")

fitS0$sample_dynamics = NULL
fitSl$sample_dynamics = NULL

qsave(fitS0, "~/Documents/Git/vaxco/fit_sindh.qs")
qsave(fitSl, "~/Documents/Git/vaxco/fit_sindh_lower_R0.qs")

fitS_old = qread("~/Documents/Git/vaxco/fit_sindh_lower_R0.qs")
names(fitS_old)

fitG0 = qread("fitG0.qs")
as.data.table(fitG0$post)
fitS0 = qread("fitS0.qs")
as.data.table(fitS0$post)


ggplot(data = epi[location == "Sindh"]) +
    geom_line(aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = deaths))


w = melt(as.data.table(fit$post), id.vars = 1:4)
ggplot(w) + geom_histogram(aes(value)) + facet_wrap(~variable, scales = "free")

qsave(fitG, "fit_gauteng.qs")

test = cm_backend_sample_fit_test(cm_translate_parameters(fitG4$par), fitG4$post, 50, seed = 0);
test = rbindlist(test)

ggplot(test[group == 2, .(d = obs0), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Gauteng"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = rollmean(deaths, 1, fill = NA))) +
    theme(legend.position = "none") + labs(x = "Time (days)", y = "Deaths")

ggplot(test[group == 3, .(d = obs0), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Gauteng"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = rollmean(cases, 1, fill = NA))) +
    theme(legend.position = "none") + labs(x = "Time (days)", y = "Cases")

ggplot(test[, .(d = sum(cases) * 0.1), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Gauteng"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = cases))

lapply(fitG$post, summary)


# ALL SCENARIOS
# Delhi with synthetic changes -- note less78, should be true here but false everywhere else.

# Delhi
# Delhi scenario
# Lockdown centered on the 22nd March
fitD = do_fitting("NCT of Delhi", "India", "Delhi", "Delhi", burn_in = 5000)
qsave(fitD, "fit_delhi.qs")
fitD = qread("fit_delhi.qs")

# ideas
# immunity for Delhi: 0.0154 [0.0136-0.0185]
# immunity: test elsewhere -- waning consistent for Gauteng (0.12 [0.0012-0.025])
#                          -- Nairobi 0.009 [0.005-0.019] NO FITTING IS WEIRD
# asc-type curve for u
# introduce in younger population? does this change anything?

#fitD$post$um = 4.2
#fitD$par$pop[[1]]$wn = rep(2/365, 16)

fitD$post = data.frame(fitD$post)

fitD$post$t0 = 45
fitD$post$um = 8

fitD$post$a_y0 = 0.1
fitD$post$a_y_lo = 0
fitD$post$a_s0 = 1
fitD$post$a_s1 = 1

fitD$post$c_y0 = 0.22
fitD$post$c_y_lo = 100
fitD$post$c_s0 = 20
fitD$post$c_s1 = 7

test = cm_backend_sample_fit_test(cm_translate_parameters(fitD$par), fitD$post, 50, seed = 0);
test = rbindlist(test)

ggplot(test[group == 1, .(Rt = obs0), by = .(run, t)]) + 
    geom_point(aes(x = t, y = Rt, colour = run)) +
    ylim(0, 3)

ggplot(test[group == 2, .(d = obs0), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = test[, .(d = sum(death_o)), by = .(run, t)], aes(x = t, y = d, group = run), colour = "red") +
    geom_line(data = epi[location == "Delhi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = deaths)) +
    theme(legend.position = "none") + labs(x = "Time (days)", y = "Deaths")

ggplot(test[, .(d = sum(cases)), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d * 0.1, colour = run)) +
    geom_line(data = epi[location == "Delhi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = cases)) +
    theme(legend.position = "none") + labs(x = "Time (days)", y = "Cases / 10")

save_fitD("custom_contact_6_alt7")

save_fitD = function(name) {
    qsave(fitD, paste0("fitD_scenario_", name, ".qs"));
    
    # name = "death_delay_and_all_workplace"
    # fitD = qread(paste0("fitD_scenario_", name, ".qs"));
    # cm_source_backend(user_defined = fitD$user_defined)
    
    test = cm_backend_sample_fit_test(cm_translate_parameters(fitD$par), fitD$post, 50, seed = 0);
    test = rbindlist(test)
    
    pl1 = ggplot(test[group == 1, .(Rt = obs0), by = .(run, t)]) + 
        geom_point(aes(x = t, y = Rt, colour = run)) +
        ylim(0, 3) + labs(title = name)
    
    pl2 = ggplot(test[group == 2, .(d = obs0), by = .(run, t)]) + 
        geom_point(aes(x = t, y = d, colour = run)) +
        geom_line(data = test[, .(d = sum(death_o)), by = .(run, t)], aes(x = t, y = d, group = run), colour = "red") +
        geom_line(data = epi[location == "Delhi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = deaths)) +
        theme(legend.position = "none") + labs(x = "Time (days)", y = "Deaths")

    ggplot(test[group == 3, .(d = obs0), by = .(run, t)]) + 
        geom_point(aes(x = t, y = d, colour = run)) +
        geom_line(data = test[, .(d = sum(cases)), by = .(run, t)], aes(x = t, y = d, group = run), colour = "red") +
        geom_line(data = epi[location == "Delhi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = cases)) +
        theme(legend.position = "none") + labs(x = "Time (days)", y = "Cases")
    
    pl3 = ggplot(test[, .(d = sum(cases)), by = .(run, t)]) + 
        geom_point(aes(x = t, y = d * 0.1, colour = run)) +
        geom_line(data = epi[location == "Delhi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = cases)) +
        theme(legend.position = "none") + labs(x = "Time (days)", y = "Cases / 10")
    
    pl = plot_grid(pl1, pl2, pl3, nrow = 3)
    ggsave(paste0("~/Dropbox/Covid_LMIC/nCoV-LMIC/fitD_scenario_", name, ".pdf"), pl, width = 20, height = 30, units = "cm", useDingbats = FALSE);
}

# what combination of mobility indices comes closest to 
# fitted ascertainment rate
mobD = dcast(mob[location == "Delhi"], date ~ context)
mobD[, tx := as.numeric(ymd(date) - ymd("2020-01-01")) / as.numeric(ymd("2020-09-18") - ymd("2020-01-01"))];
lapply(fitD$post, mean)
mobD[, contact := asc(tx, 0.332, 3.11, -24.06, 7.606)];

model = lm(contact ~ ., data = mobD[, .SD, .SDcols = c("grocery_and_pharmacy", "residential", "retail_and_recreation", "transit_stations", "workplaces", "parks", "contact")])
model = lm(contact ~ 0+., data = mobD[, .SD, .SDcols = c("residential", "grocery_and_pharmacy", "contact")])
model
mobD$pred = predict(model, newdata = mobD)
ggplot(mobD) + geom_line(aes(x = tx, y = contact)) + geom_line(aes(x = tx, y = pred))


library(shmanipulate)
library(lubridate)

shmanipulate

shmanipulate({
    fitD$post$t0 = 45
    fitD$post$um = 8
    
    fitD$post$a_y0 = 0.2
    fitD$post$a_y_lo = 0
    fitD$post$a_s0 = 1
    fitD$post$a_s1 = 1
    
    fitD$post$c_y0 = 0.23
    fitD$post$c_y_lo = 100
    fitD$post$c_s0 = 15
    fitD$post$c_s1 = 5
    
    test = cm_backend_sample_fit_test(cm_translate_parameters(fitD$par), fitD$post, 1, seed = 0);
    test = rbindlist(test)
    
    p1 = ggplot(test[group == 1, .(Rt = obs0), by = .(run, t)]) + 
        geom_point(aes(x = t, y = Rt, colour = run)) +
        ylim(0, 3)
    
    p2 = ggplot(test[group == 2, .(d = obs0), by = .(run, t)]) + 
        geom_point(aes(x = t, y = d, colour = run)) +
        geom_line(data = test[, .(d = sum(death_o)), by = .(run, t)], aes(x = t, y = d, group = run), colour = "red") +
        geom_line(data = epi[location == "Delhi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = deaths)) +
        theme(legend.position = "none") + labs(x = "Time (days)", y = "Deaths")
    
    p1
}, a = c(0,1))





# Nairobi
fitN = do_fitting("Nairobi", "Kenya", "Nairobi", "Nairobi County")
qsave(fitN, "fit_nairobi.qs")
fitN = qread("fit_nairobi.qs")

cm_source_backend(user_defined = fitN$user_defined)
test = cm_backend_sample_fit_test(cm_translate_parameters(fitN$par), fitN$post, 50, seed = 0);
test = rbindlist(test)

ggplot(test[group == 2, .(d = obs0), by = .(run, t)]) + 
    geom_line(aes(x = t, y = d, colour = run, group = run)) +
    geom_point(data = epi[location == "Nairobi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = rollmean(deaths, 7, fill = NA))) +
    theme(legend.position = "none") + labs(x = "Time (days)", y = "Deaths")

ggplot(test[, .(d = sum(cases) * 0.05), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Nairobi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = cases))



# Sindh
fitS = do_fitting("Sind", "Pakistan", "Sindh", "Sindh")
qsave(fitS, "fit_sindh.qs")


# test "by hand"

# 1
fitS = qread("fitS0.qs")
qsave(fitS, "~/Documents/Git/vaxco/fit_sindh.qs")
NULL

# 2
fitS = qread("fitS0.qs")
sd(fitS$post$R0)
fitS$post$t0 = pmax(0, fitS$post$t0 - 14)
fitS$post$ad_y0 = fitS$post$ad_y0 * 1.9

fitS$post$ac_y0 = fitS$post$ac_y0 * 2
fitS$post$ac_s0 = fitS$post$ac_s0 + 0
fitS$post$ac_s1 = fitS$post$ac_s1 + 5
fitS$post$ac_y_lo = fitS$post$ac_y_lo - 1
#as.data.table(fitS$post)
#qsave(fitS, "~/Documents/Git/vaxco/fit_sindh_lower_R0.qs")
qsave(fitS, "fitS_lower_R0.qs")
check("fitS_lower_R0.qs", "Sindh")

#check("~/Documents/Git/vaxco/fit_sindh_lower_R0.qs", "Sindh")

# 3
fitS = qread("fitS0.qs")
fitS$post$um = fitS$post$um * 0.8
fitS$post$t0 = pmax(0, fitS$post$t0 - 14)
fitS$post$asc_da = fitS$post$asc_da * 2.9
fitS$user_defined$model_v2$cpp_changes[3] = "P.pop[0].seed_times = vector<double>(100, (int)x[0]);"

cm_source_backend(fitS$user_defined)

# run model
test = cm_backend_sample_fit_test(cm_translate_parameters(fitS$par), fitS$post, 50, seed = 0);
test = rbindlist(test)
# sc cases
# 1 27249204
# 2 22505949

# total pop 50588073
test[t == 182, 1 - sum(S) / 50588073, by = run][, mean(V1)]
test[t == 105, 1 - sum(S) / 50588073, by = run][, mean(V1)]

pl3 = ggplot(test[group == 2, .(d = obs0), by = .(run, t)]) + 
    geom_line(aes(x = t, y = d, colour = run, group = run)) +
    geom_point(data = epi[location == "Sindh"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = rollmean(deaths, 7, fill = NA))) +
    theme(legend.position = "none") + labs(x = "Time (days)", y = "Deaths", title = "R0 = 1.9, asc. = 20%")
pl3

pl1

ggplot(test[, .(d = sum(cases) * 0.025), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Sindh"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = cases))

pl4 = ggplot(mob[location == "Sindh" & context != "school"]) +
    geom_line(aes(x = date - ymd("2020-01-01"), y = rollmean(change * 100, 7, fill = NA), colour = context)) +
    xlim(0, 243) +
    theme(legend.position = c(0.7, 0.2)) +
    labs(x = "Time (days)", y = "Mobility change (%)", title = "Mobility metrics")

plot_grid(pl1, pl2, pl3, pl4, nrow = 4, labels = letters[1:4], rel_heights = c(1, 1, 1, 2))

# for Carl
# fitted up to September X
# waning of immunity
# reintroduction process
# deterministic version
# support prevent disease only?
# parameters to sweep
# - one versus two dose (don't worry about...)
# - vaccine efficacy
# - waning of vaccine efficacy
# - waning of natural immunity
# - different coverage
# - different age targeting
# demonstrating that the set of scenarios can be analysed for a setting.
# endpoints: cases, deaths, admissions
# a single task runs over the model posterior; params for sweeping one set per task.
# 


# Addis
fitA = do_fitting("Addis Abeba", "Ethiopia", "Addis Ababa", "Addis Ababa")

test = cm_backend_sample_fit_test(cm_translate_parameters(fitA$par), fitA$post, 50, seed = 0);
test = rbindlist(test)

ggplot(test[, .(d = sum(death_reported_o)), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Sindh"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = deaths))


# LMIC shift fits
fitGs = do_fitting("Gauteng", "South Africa", "Gauteng", "Gauteng", lmic_shift = 1.5, burn_in = 2500)
qsave(fitGs, "fit_gauteng_shift.qs")
fitDs = do_fitting("NCT of Delhi", "India", "Delhi", "Delhi", lmic_shift = 1.5)
qsave(fitDs, "fit_delhi_shift.qs")
fitNs = do_fitting("Nairobi", "Kenya", "Nairobi", "Nairobi County", lmic_shift = 1.5)
qsave(fitNs, "fit_nairobi_shift.qs")
fitSs = do_fitting("Sind", "Pakistan", "Sindh", "Sindh", lmic_shift = 1.5)
qsave(fitSs, "fit_sindh_shift.qs")



fitG = qread("fit_gauteng.qs")
fitD = qread("fit_delhi.qs")
fitN = qread("fit_nairobi.qs")
fitS = qread("fit_sindh.qs")

data_epi_wide_2 = rbind(
    load_fit("fit_delhi.qs", "Delhi", 1),
    load_fit("fit_nairobi.qs", "Nairobi", 3),
    load_fit("fit_gauteng.qs", "Gauteng", 4),
    load_fit("fit_sindh.qs", "Sindh", 5)
)

places = c("Addis Ababa", "Delhi", "Gauteng", "Nairobi", "Sindh")
theme_set(theme_cowplot())
pl1 = ggplot(epi[location %in% places]) + geom_point(aes(x = date, y = cases), colour = "black") + 
    facet_wrap(~location, nrow = 1) + labs(x = "Date", y = "Cases") + scale_x_date(date_breaks = "1 month", date_labels = "%b")
pl2 = ggplot(epi[location %in% places]) + geom_point(aes(x = date, y = deaths), colour = "#880000") + 
    facet_wrap(~location, nrow = 1) + labs(x = "Date", y = "Deaths") + scale_x_date(date_breaks = "1 month", date_labels = "%b")
pl3 = ggplot(mob[location %in% c(places, "Nairobi County")]) +
    geom_line(aes(x = date, y = change, colour = context)) +
    facet_wrap(~location, nrow = 1) + labs(x = "Date", y = "Change in mobility") +
    theme(legend.position = "bottom") + scale_x_date(date_breaks = "1 month", date_labels = "%b")

pl4 = plot_grid(plD, plG, plN, plS, nrow = 1, labels = c("Delhi", "Gauteng", "Nairobi", "Sindh"))
plot_grid(pl1, pl2, pl3, pl4, ncol = 1)



# Scratch
par2 = rlang::duplicate(par)
par2$schedule = NULL
test2 = cm_backend_sample_fit_test(cm_translate_parameters(par2), post, 50, seed = 0);
test2 = rbindlist(test2)

ggplot(test2[, .(d = sum(death_reported_o)), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Gauteng"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = deaths))

ggplot(test2[, .(d = sum(cases) * 0.1 * (t - 100) / 130), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Gauteng"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = cases))


test [, .(d = sum(death_o, na.rm = T)), by = .(run)][, mean(d)]
test2[, .(d = sum(death_o, na.rm = T)), by = .(run)][, mean(d)]




# Manual calibration, Delhi
fitD = do_fitting("NCT of Delhi", "India", "Delhi", "Delhi")
qsave(fitD, "fit_delhi.qs")
fitD = qread("fit_delhi.qs")

library(manipulate)

source("~/Documents/shmanipulate/shmanipulate.R")

shmanipulate(
    function(t0, um, asc_da, qhome, qwork, qpark, qgroc, qret, qtra, max_agegroup_introduction, seedn)
    {
        qother = qpark + qgroc + qret + qtra;
        qpark = qpark / qother;
        qgroc = qgroc / qother;
        qret = qret / qother;
        qtra = qtra / qother;
        
        fitD$par = make_params("NCT of Delhi", "India", "Delhi", 1.2, qhome = qhome, qwork = qwork, qpark = qpark, qgroc = qgroc, qret = qret, qtra = qtra)
        fitD$par$pop[[1]]$dist_seed_ages = c(rep(1, max_agegroup_introduction), rep(0, 16 - max_agegroup_introduction))
        fitD$post$t0 = t0;
        fitD$post$um = um;
        fitD$post$asc_da = asc_da;
        fitD$par$time1 = 365
        test = cm_backend_sample_fit_test(cm_translate_parameters(fitD$par), fitD$post, 1, seed = 0);
        test = rbindlist(test);
        ggplot(test[group == 2, .(d = obs0), by = .(run, t)]) +
            geom_point(aes(x = t, y = d, colour = run)) +
            geom_point(data = test[, .(cases = sum(cases)), by = .(run, t)], aes(x = t, y = cases * asc_da * 0.01), colour = "red") +
            geom_line(data = epi[location == "Delhi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = cases), colour = "#880000") +
            geom_line(data = epi[location == "Delhi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = deaths)) +
            theme(legend.position = "none") + labs(x = "Time (days)", y = "Deaths")
    },
    t0 = c(47, 0, 120),
    um = c(3, 0, 3.5),
    asc_da = c(0.11, 0, 1),
    qhome = c(1, 0, 1),
    qwork = c(1, 0, 1),
    qpark = c(1, 0, 10),
    qgroc = c(3, 0, 10),
    qret = c(3, 0, 10),
    qtra = c(3, 0, 10),
    max_agegroup_introduction = c(16, 1, 16, 1),
    seedn = c(1, 10001, 100)
)

#fitD$post$um = 4.2
#fitD$par$pop[[1]]$wn = rep(2/365, 16)

cm_source_backend(user_defined = fitD$user_defined);

test = cm_backend_sample_fit_test(cm_translate_parameters(fitD$par), fitD$post, 50, seed = 0);
test = rbindlist(test)

test[t == 100 & group == 1, S]

which_rows = cm_temp_which_rows(cm_translate_parameters(fitD$par), fitD$post, 50, seed = 0)

fitDp = data.frame(rlang::duplicate(fitD$post))
fitDp1 = fitDp[which_rows[10], ]
test2 = cm_backend_sample_fit_test(cm_translate_parameters(fitD$par), fitDp1, 1, seed = 0);
test2 = rbindlist(test2)

test2
test[run == 10]

ggplot(test[group == 2, .(d = obs0), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Delhi"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = deaths)) +
    theme(legend.position = "none") + labs(x = "Time (days)", y = "Deaths")




## TODO delete ...
# test of plotting asc functions ...
y0 = rnorm(1000, 0.4, 0.02)
ylo = rnorm(1000, 1.2, 0.2)
s0 = rnorm(1000, -3, 1)
s1 = rnorm(1000, 10, 2)

ggplot() + 
    plot_asc(y0 = y0, y_lo = ylo, s0 = s0, s1 = s1, x_start = ymd("2020-01-01"), x_span = 365, x_divs = 52, colour = "blue")

# for use with contact_6
with(fitD$post,
    ggplot() + 
        plot_asc(y0 = 1, y1 = 1 - c_y0, s0 = -32.4, s1 = 7.6, x_start = ymd("2020-01-01"), x_span = 100, x_divs = 100, colour = "blue") +
        plot_asc(y0 = 1 - c_y0, y1 = 1 - c_y1, s0 = -c_s0, s1 = c_s1, x_start = ymd("2020-01-01") + 100, x_span = 265, x_divs = 265, colour = "red") +
        geom_line(data = mob[location == "Delhi"], aes(x = date, y = 1 + change, colour = context))
)

## /TODO delete







    
    # case_delay = "N 10 10 T 0 30", #####
    # dd_mean = "N 30 30 T 0 75",
    # dd_shape = "N 30 30 T 0 75",
    # pow = "N 0 1 T 0 2"
    # q_res = "U 0 2",
    # q_work = "U 0 1",
    # q_retrec = "E 1 1",
    # q_gpharm = "E 1 1",
    # q_parks = "U 0 1",
    # q_transit = "U 0 1",
    # q_other = "U 0 1"


    cppv = function(name, v) {
        paste0('vector<double> ', name, ' = {', paste(v, collapse = ","), '};')
    }


                'for (auto& u : P.pop[0].u)',
                '    u *= x[1];',
                'P.pop[0].seed_times = vector<double>(10, (int)x[0]);',
                
                ## for death delay
                # 'P.processes[0].delays[0] = delay_gamma(x[7], x[8], 200, P.time_step);',
                
                # ## for powing
                #'for (vector<double>& v : P.changes.ch[0].values)',
                #'    v[3] = pow(v[3], x[7]);'
                ## for waning    
                #'P.pop[0].wn = vector<double>(16, x[7]);'

                ## for custom contact ...
                # 'auto odds = [&](double x, double odds_ratio) {',
                # '    double a = x / (1 - x);',
                # '    return a * odds_ratio / (a * odds_ratio + 1);',
                # '};',
                # 'auto asc = [&](double x, double y0, double y_lo, double s0, double s1) {',
                # '    double y1 = odds(y0, exp(y_lo));',
                # '    double xx = s0 + x * (s1 - s0);',
                # '    double h0 = exp(s0) / (1 + exp(s0));',
                # '    double h1 = exp(s1) / (1 + exp(s1));',
                # '    double h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0);',
                # '    return y0 + (y1 - y0) * h;',
                # '};',
                # 'for (unsigned int i = 0; i < P.changes.ch[0].values.size(); ++i) {',
                # '    double tx = (P.changes.ch[0].times[i] - P.time0) / (P.time1 - P.time0);',
                # '    P.changes.ch[0].values[i][0] = asc(tx, x[7], x[8], -x[9], x[10]);',
                # '    P.changes.ch[0].values[i][1] = asc(tx, x[7], x[8], -x[9], x[10]);',
                # '    P.changes.ch[0].values[i][2] = asc(tx, x[7], x[8], -x[9], x[10]);',
                # '    P.changes.ch[0].values[i][3] = asc(tx, x[7], x[8], -x[9], x[10]);',
                # '}'
                
                ## for custom contact, mk. 2 ...
                'auto asc0 = [&](double x, double y0, double y1, double s0, double s1) {',
                '    double xx = s0 + x * (s1 - s0);',
                '    double h0 = exp(s0) / (1 + exp(s0));',
                '    double h1 = exp(s1) / (1 + exp(s1));',
                '    double h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0);',
                '    return y0 + (y1 - y0) * h;',
                '};',
                #'P.changes.ch[0].times.assign(730, 0);',
                #'P.changes.ch[0].values.assign(730, vector<double>(4, 0));',
                'double Y0 = x[7], Y1 = x[8], S0 = x[9], S1 = x[10];',
                'for (unsigned int ch = 0; ch < P.changes.ch[0].times.size(); ++ch) {',
                '    double td = P.changes.ch[0].times[ch];',
                '    double q = (td < 100) ? asc0(td / 100, 0, Y0, -32.4, 7.6) : ((td < 365) ? (asc0((td - 100) / (365 - 100), Y0, Y1, -S0, S1)) : Y1);',
                '    P.changes.ch[0].values[td][0] = 1 + q * 0.3;',
                '    P.changes.ch[0].values[td][1] = 1 - q;',
                '    P.changes.ch[0].values[td][2] = (td < 81) ? 1 : 0;',
                '    P.changes.ch[0].values[td][3] = 1 - q;',
                '}',
                
                ## for everything on other
                # 'for (unsigned int i = 0; i < P.changes.ch[0].values.size(); ++i) {',
                # '    P.changes.ch[0].values[i][0] = P.changes.ch[0].values[i][3];',
                # '    P.changes.ch[0].values[i][1] = P.changes.ch[0].values[i][3];',
                # '    P.changes.ch[0].values[i][2] = P.changes.ch[0].values[i][3];',
                # '}'
                
                ## for mobility coefficients
                # cppv("retrec",  mob[location == mob_loc & context == "retail_and_recreation", change]),
                # cppv("gpharm",  mob[location == mob_loc & context == "grocery_and_pharmacy", change]),
                # cppv("parks",   mob[location == mob_loc & context == "parks", change]),
                # cppv("transit", mob[location == mob_loc & context == "transit_stations", change]),
                # cppv("work",    mob[location == mob_loc & context == "workplaces", change]),
                # cppv("res",     mob[location == mob_loc & context == "residential", change]),
                # 'for (unsigned int i = 0; i < P.changes.ch[0].values.size(); ++i) {',
                # '    unsigned int ii = min((unsigned long)i, retrec.size() - 1);',
                # '    P.changes.ch[0].values[i][0] = 1 + x[5] * res[ii];',
                # '    P.changes.ch[0].values[i][1] = 1 + x[6] * work[ii];',
                # '    P.changes.ch[0].values[i][3] = 1 + x[9] * (x[7] * retrec[ii] + x[8] * gpharm[ii] + transit[ii]) / (x[7] + x[8] + 1);',
                # '}'
                
                ## case delay
                'P.pop[0].dC = delay_gamma(x[11], 4, 60, P.time_step);'
            ),
