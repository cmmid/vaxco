require(data.table)

#' step 1: create a sample
#' from PHE SIREN report
day0 <- as.Date("2020-06-18")
dayf <- as.Date("2020-11-09")
n_initially_pos <- 6614
reinfs <- 44
cases <- readRDS("~/Dropbox/SA2UK/inputs/epi_data.rds")[iso3 == "GBR" & date < day0]
distro <- cases[which.max(cases > 0):.N][, .(Rdate = date - day0, proportion = cases / sum(cases)) ]
reporting_PSO_offset <- 30
starts <- distro[,sample(as.integer(Rdate), size = n_initially_pos, replace = TRUE, prob = proportion)] + reporting_PSO_offset
#' also need initial titres (eyeball from science paper)
#' TODO can get this from subject distro in supplement?
#' TODO update the `starts` to reflect PSO delays, after accounting for reporting shift in cases?
init_titre <- 10^rnorm(n_initially_pos, mean = 3, sd=0.5)

#' step 2: determine the expected values to observe the control outcome
n_initially_neg <- 14173
neg_infs <- 318 + 94
withindays <- as.integer(dayf - day0)
prop_convert <- neg_infs / n_initially_neg
#' want the foi such that
#'  - drawing from a geometric distro,
#'  - would observe an outcome less than or equal to `withindays`
#'  - `prop_convert` percent of the time
#'  - rgeom is k-failures until success version
#'  - CDF = 1-(1-p)^(k+1)
#'  - implies p = 1-(1-prop_convert)^(1/(withindays+1))
foi <- 1-(1-prop_convert)^(1/(withindays+1))

#' step 3: determine target
#' to observe `reinfs`, how many susceptible person days do we need at this foi?
#' typically
persondays_needed <- reinfs / foi
#' or for each person typically
#' persondays_needed/n_initially_pos

#' step 4: decay titres
thalf <- 103
decay_rate <- log(2)/thalf
decay_multiplier <- exp(-decay_rate*(0:(withindays-min(starts))))
res <- t(mapply(
    function(s) if (s < 0) {
        decay_multiplier[(0 : withindays)-s]
    } else {
        c(rep(1, s), decay_multiplier[(0 : (withindays-s))+1])
    },
    s = starts
)) * init_titre         

#' step 5: calculate threshold that gives sufficient person days from sample
threshold <- optimize(
    function(threshold) (sum(res < threshold)-persondays_needed)^2,
    interval = c(min(res[,145]), max(res[,1]))
)$minimum
