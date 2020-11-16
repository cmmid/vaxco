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

# set parameters for this set of scenario runs
delay_normal = function(mu, sd, t_max, t_step)
{
    t_points = seq(0, t_max, by = t_step);
    heights = pnorm(t_points + t_step/2, mu, sd) - 
        pnorm(pmax(0, t_points - t_step/2), mu, sd);
    return (data.table(t = t_points, p = heights / sum(heights)))
}

# note -- delay should actually be gamma (symptom onset) plus normal -- to be fixed.
# from Borremans et al
p7 =
    list(source = "E", type = "multinomial", names = "to_lfia_positive", report = c(""),
        prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
        delays = matrix(c(delay_normal(13.3 + 1.5, 5.7, 60, 0.25)$p), nrow = 1, byrow = T));
    
p8 =
    list(source = "to_lfia_positive", type = "multinomial", names = "lfia_positive", report = c("ip"),
        prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
        delays = matrix(cm_delay_gamma(166, 1, 730, 0.25)$p, nrow = 1, byrow = T));


# scenario 1
fitS = qread("fit_sindh.qs")
fitS$par$processes[[7]] = p7
fitS$par$processes[[8]] = p8
fitS$par$time1 = 365
cm_source_backend(user_defined = fitS$user_defined)

# sample from posterior to generate runs
run1 = cm_backend_sample_fit_test(cm_translate_parameters(fitS$par), fitS$post, p$n_samples, seed = p$rng_seed);
run1 = rbindlist(run1)

# 2
fitS = qread("fit_sindh.qs")
fitS$par$processes[[7]] = p7
fitS$par$processes[[8]] = p8
fitS$par$time1 = 365
fitS$post$um = fitS$post$um * 0.9
fitS$post$t0 = pmax(0, fitS$post$t0 - 14)
fitS$post$asc_da = fitS$post$asc_da * 1.9
cm_source_backend(user_defined = fitS$user_defined)

# sample from posterior to generate runs
run2 = cm_backend_sample_fit_test(cm_translate_parameters(fitS$par), fitS$post, p$n_samples, seed = p$rng_seed);
run2 = rbindlist(run2)

# 3
fitS = qread("fit_sindh.qs")
fitS$par$processes[[7]] = p7
fitS$par$processes[[8]] = p8
fitS$par$time1 = 365
fitS$post$um = fitS$post$um * 0.8
fitS$post$t0 = pmax(0, fitS$post$t0 - 14)
fitS$post$asc_da = fitS$post$asc_da * 2.9
fitS$user_defined$model_v2$cpp_changes[3] = "P.pop[0].seed_times = vector<double>(100, (int)x[0]);"
cm_source_backend(fitS$user_defined)

# sample from posterior to generate runs
run3 = cm_backend_sample_fit_test(cm_translate_parameters(fitS$par), fitS$post, p$n_samples, seed = p$rng_seed);
run3 = rbindlist(run3)




# show seropositivity over time
run1b = run1[, .(seropositive = sum(lfia_positive_p) / 50588073), by = .(run, t)]
run1b[(t + ymd("2020-01-01")) %between% ymd(c("2020-06-25", "2020-07-11")), mean(seropositive)]
run2b = run2[, .(seropositive = sum(lfia_positive_p) / 50588073), by = .(run, t)]
run2b[(t + ymd("2020-01-01")) %between% ymd(c("2020-06-25", "2020-07-11")), mean(seropositive)]
run3b = run3[, .(seropositive = sum(lfia_positive_p) / 50588073), by = .(run, t)]
run3b[(t + ymd("2020-01-01")) %between% ymd(c("2020-06-25", "2020-07-11")), mean(seropositive)]

# sampling data
estimates = fread(
'timepoint, lower, mid, upper, setting
April, 0, 0.2, 0.7, "Empirical (District Malir, Karachi)"
April, 0, 0.4, 1.3, "Empirical (District East, Karachi)"
July, 5.1, 8.7, 13.1, "Empirical (District Malir, Karachi)"
July, 9.4, 15.1, 21.7, "Empirical (District East, Karachi)"')

prevA1 = run1b[(t + ymd("2020-01-01")) %between% ymd(c("2020-04-15", "2020-04-25")), mean(seropositive)]
prevB1 = run1b[(t + ymd("2020-01-01")) %between% ymd(c("2020-06-25", "2020-07-11")), mean(seropositive)]
prevA2 = run2b[(t + ymd("2020-01-01")) %between% ymd(c("2020-04-15", "2020-04-25")), mean(seropositive)]
prevB2 = run2b[(t + ymd("2020-01-01")) %between% ymd(c("2020-06-25", "2020-07-11")), mean(seropositive)]
prevA3 = run3b[(t + ymd("2020-01-01")) %between% ymd(c("2020-04-15", "2020-04-25")), mean(seropositive)]
prevB3 = run3b[(t + ymd("2020-01-01")) %between% ymd(c("2020-06-25", "2020-07-11")), mean(seropositive)]

ciA1 = qbeta(c(0.025, 0.975), prevA1 * 1000, (1 - prevA1) * 1000)
ciB1 = qbeta(c(0.025, 0.975), prevB1 * 1000, (1 - prevB1) * 1000)
ciA2 = qbeta(c(0.025, 0.975), prevA2 * 1000, (1 - prevA2) * 1000)
ciB2 = qbeta(c(0.025, 0.975), prevB2 * 1000, (1 - prevB2) * 1000)
ciA3 = qbeta(c(0.025, 0.975), prevA3 * 1000, (1 - prevA3) * 1000)
ciB3 = qbeta(c(0.025, 0.975), prevB3 * 1000, (1 - prevB3) * 1000)

estimates = rbind(estimates,
    data.table(timepoint = c("April", "July"), 
        lower = 100 * c(  ciA1[1], ciB1[1]), 
        mid =   100 * c(prevA1,  prevB1), 
        upper = 100 * c(  ciA1[2], ciB1[2]),
        setting = "Model (Sindh) R0 = 2.4"))

estimates = rbind(estimates,
    data.table(timepoint = c("April", "July"), 
        lower = 100 * c(  ciA2[1], ciB2[1]), 
        mid =   100 * c(prevA2,  prevB2), 
        upper = 100 * c(  ciA2[2], ciB2[2]),
        setting = "Model (Sindh) R0 = 2.15"))

estimates = rbind(estimates,
    data.table(timepoint = c("April", "July"), 
        lower = 100 * c(  ciA3[1], ciB3[1]), 
        mid =   100 * c(prevA3,  prevB3), 
        upper = 100 * c(  ciA3[2], ciB3[2]),
        setting = "Model (Sindh) R0 = 1.9"))

estimates[, setting := factor(setting, unique(setting))]

ggplot(estimates) +
    geom_pointrange(aes(x = timepoint, y = mid, ymin = lower, ymax = upper, colour = setting), 
        position = position_dodge(width = 0.4), fatten = 0.2) +
    labs(x = "Time of survey", y = "Seroprevalence (%)", colour = "Setting", title = "Empirical versus modelled seroprevalence in Sindh")

run3 = run3[, .(lower = quantile(seropositive, 0.025), mid = quantile(seropositive, 0.5), upper = quantile(seropositive, 0.975)), by = t]
ggplot(run3) + 
    geom_ribbon(aes(x = t + ymd("2020-01-01"), ymin = lower, ymax = upper), fill = "#88ccff") +
    geom_line(aes(x = t + ymd("2020-01-01"), y = mid), colour = "#446688")
