library(data.table)
library(ggplot2)
library(lubridate)
library(qs)
library(zoo)

# Scenario parameters
p = list(
    n_samples = 100,        # number of samples from posterior
    rng_seed = 0            # RNG seed; 0 means default for RNG
)

# load covidm
cm_path = "~/Dropbox/nCoV/covidm/";
cm_force_rebuild = F;
cm_build_verbose = T;
cm_version = 2;
source(paste0(cm_path, "/R/covidm.R"))

# load fitted model for Sindh
fitS = qread("fit_sindh.qs")
cm_source_backend(user_defined = fitS$user_defined) # This will recompile covidm with certain components required to run 
                                                    # for this setting and model setup -- this step may need to be done 
                                                    # prior to batch execution of runs to avoid multiple threads trying to 
                                                    # recompile covidm at once.

# set parameters for this set of scenario runs
fitS$par$time1 = 365

delay_normal = function(mu, sd, t_max, t_step)
{
    t_points = seq(0, t_max, by = t_step);
    heights = pnorm(t_points + t_step/2, mu, sd) - 
        pnorm(pmax(0, t_points - t_step/2), mu, sd);
    return (data.table(t = t_points, p = heights / sum(heights)))
}

# note -- delay should actually be gamma (symptom onset) plus normal -- to be fixed.
# from Borremans et al
fitS$par$processes[[7]] =
    list(source = "E", type = "multinomial", names = "to_lfia_positive", report = c(""),
        prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
        delays = matrix(c(delay_normal(11.9 + 1.5, 65.3, 60, 0.25)$p), nrow = 1, byrow = T));
    
fitS$par$processes[[8]] =
    list(source = "to_lfia_positive", type = "multinomial", names = "lfia_positive", report = c("ip"),
        prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
        delays = matrix(cm_delay_gamma(90, 1, 730, 0.25)$p, nrow = 1, byrow = T));


# sample from posterior to generate runs
run = cm_backend_sample_fit_test(cm_translate_parameters(fitS$par), fitS$post, p$n_samples, seed = p$rng_seed);
run = rbindlist(run)

# show seropositivity over time
run2 = run[, .(seropositive = sum(lfia_positive_p) / 50588073), by = .(run, t)]
run2[t + ymd("2020-01-01") %between% ymd(c("2020-06-25", "2020-07-11")), mean(seropositive)]
run2 = run2[, .(lower = quantile(seropositive, 0.025), mid = quantile(seropositive, 0.5), upper = quantile(seropositive, 0.975)), by = t]

# sampling data
estimates = fread(
'timepoint, lower, mid, upper, setting
April, 0, 0.2, 0.7, "Empirical (District Malir, Karachi)"
April, 0, 0.4, 1.3, "Empirical (District East, Karachi)"
July, 5.1, 8.7, 13.1, "Empirical (District Malir, Karachi)"
July, 9.4, 15.1, 21.7, "Empirical (District East, Karachi)"')

prev1 = run2[(t + ymd("2020-01-01")) %between% ymd(c("2020-04-15", "2020-04-25")), mean(seropositive)]
prev2 = run2[(t + ymd("2020-01-01")) %between% ymd(c("2020-06-25", "2020-07-11")), mean(seropositive)]

ci1 = qbeta(c(0.025, 0.975), prev1 * 1000, (1 - prev1) * 1000)
ci2 = qbeta(c(0.025, 0.975), prev2 * 1000, (1 - prev2) * 1000)

estimates = rbind(estimates,
    data.table(timepoint = c("April", "July"), lower = 100 * c(ci1[1], ci2[1]), 
        mid = 100 * c(prev1, prev2), upper = 100 * c(ci1[2], ci2[2]),
        setting = "Model (Sindh)"))

ggplot(estimates) +
    geom_pointrange(aes(x = timepoint, y = mid, ymin = lower, ymax = upper, colour = setting), 
        position = position_dodge(width = 0.4), fatten = 0.2) +
    labs(x = "Time of survey", y = "Seroprevalence (%)", colour = "Setting", title = "Empirical versus modelled seroprevalence in Sindh")

ggplot(run2) + 
    geom_ribbon(aes(x = t + ymd("2020-01-01"), ymin = lower, ymax = upper), fill = "#88ccff") +
    geom_line(aes(x = t + ymd("2020-01-01"), y = mid), colour = "#446688") +
    geom_errorbar(data = estimates, aes(xmin = date0, xmax = date1, y = mid / 100, colour = setting)) +
    geom_errorbar(data = estimates, aes(x = date0 + (date1 - date0) / 2, ymin = lower / 100, ymax = upper / 100, colour = setting))
