library(data.table)
library(ggplot2)
library(cowplot)
library(lubridate)
library(cowplot)
library(readxl)
library(socialmixr)
library(qs)
library(zoo)
library(stringr)
library(socialmixr)

.args <- if (interactive()) c(
    "helper.R", "fitting_setup.R", "process_def.R",
    "epi_data.rds", "mob_data.rds",
    "birthrates.csv", "mortality.csv",
    "../../covidm-vaxco",
    "output.rds"
)

#' set up covidm
cm_path = tail(.args, 2)[1];
cm_force_rebuild = F;
cm_build_verbose = T;
cm_force_shared = T;
cm_version = 2;
source(file.path(cm_path, "R","covidm.R"))

lapply(.args[1:3], source)

# load epi and mobility data
epi = readRDS(.args[4])
mob = readRDS(.args[5])
birthrates = fread(.args[6])
mortality = fread(.args[7])

end.date <- min(epi[, max(date)],mob[, max(date)])
endt <- as.integer(end.date - as.Date("2020-01-01"))
#
# RUNS
#

make_params = function(
    dem, mat, mob_loc, lmic_shift = 1, waning = FALSE, demographics = FALSE,
    date0 = "2020-01-01", date1 = end.date
) {
    
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
            epi2[location == loc & !is.na(deaths) 
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
                ,sprintf("dyn.Obs(t, 0, 1, 0) = dyn(\"death_o\", t, {}, {}) * asc((t - P.time0) / (%s - P.time0), x[3], x[4], -x[5], x[6]);",endt)         
                ,sprintf("dyn.Obs(t, 0, 2, 0) = dyn(\"cases_reported\", t, {}, {}) * asc((t - P.time0) / (%s - P.time0), x[8], x[9], -x[10], x[11]);",endt)
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

dic = function(filename)
{
    fit = qread(filename)
    post = as.data.table(fit$post)
    post[, D := -2 * ll]
    post[, 0.5 * var(D) + mean(D)];
}

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
        geom_line(data = epi[location == epi_location], aes(x = date, y = deaths)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        theme(legend.position = "none") + labs(x = NULL, y = "Deaths", title = epi_location)
    
    pl2 = ggplot(test[group == 3 & t >= 59, .(d = obs0), by = .(run, t)]) + 
        geom_point(aes(x = t + ymd("2020-01-01"), y = d, colour = run)) +
        geom_line(data = epi[location == epi_location], aes(x = date, y = cases)) +
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



# trim epi data
# epi = epi[date <= "2020-09-15"]

# Model sets for Sindh paper
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

dic("fitdS0.qs")
dic("fitdSl.qs")
dic("fitdSw_5.0.qs")
dic("fitdSw_2.5.qs")
dic("fitdSw_1.0.qs")


check("fitdS0.qs", "Sindh") # yep
check("fitdSl.qs", "Sindh") # 
check("fitSl.qs", "Sindh") # 
check("fitdSw_1.0.qs", "Sindh") # 
check("fitdSw_2.5.qs", "Sindh") # 
check("fitdSw_5.0.qs", "Sindh") # 

odds <- function(x, odds_ratio) {
    res <- (x / (1 - x))*odds_ratio
    return(res / (res + 1))
}  

asc.dt <- data.table(expand.grid(t=0:endt, sample = 1:100))
asc.dt[, x := t / endt ]
asc <- function(x, y0, y_lo, s0, s1) {
    y1 = odds(y0, exp(y_lo))                                                                                  
    xx = s0 + x * (s1 - s0)
    h0 = exp(s0) / (1 + exp(s0))
    h1 = exp(s1) / (1 + exp(s1))
    h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0)
    return(y0 + (y1 - y0) * h)
}


thing <- as.data.table(qread("fitdSw_5.0.qs")$post)[sample(.N, 100, replace = TRUE)][, sample := 1:.N ]

asc.dt[thing, on=.(sample), c("death_asc", "case_asc") := .(asc(x, ad_y0, ad_y_lo, -ad_s0, ad_s1), asc(x, ac_y0, ac_y_lo, -ac_s0, ac_s1))]

ggplot(melt(asc.dt, id.vars = c("sample","t"), measure.vars = c("case_asc","death_asc"))) +
    aes(t, value, group = sample) +
    facet_grid(variable ~ ., scales = "free_y") +
    geom_line(alpha = 0.05) +
    theme_minimal()
