library(data.table)
library(lubridate)
library(stringr)
library(jsonlite)

# EPI DATA

# Sindh
sindh_get = function(i, string, n, data) {
    indices = 0:(n - 1);
    present_indices = setdiff(indices, data$nullIndex[[i]]) + 1;
    present_values = if (string) data$stringColumn$values[[i]] else data$doubleColumn$values[[i]];
    values = rep(NA, n);
    values[present_indices] = present_values;
    values
}

# From https://covid.gov.pk/stats/sindh
# Set the data range to 1 Jan 2020 to latest date with Developer Tools on the network tab (Google Chrome)
# The data is obtainable as a JSON element from there.
sindh_raw = fromJSON("./sindh-2021-01-20.json") 
sindh_case_n = sindh_raw$default$dataResponse$dataSubset[[1]]$dataset$tableDataset$totalCount
sindh_case_data  = sindh_raw$default$dataResponse$dataSubset[[1]]$dataset$tableDataset$column[[1]]
sindh_death_n = sindh_raw$default$dataResponse$dataSubset[[4]]$dataset$tableDataset$totalCount
sindh_death_data = sindh_raw$default$dataResponse$dataSubset[[4]]$dataset$tableDataset$column[[1]]

sindh_cases = data.table(location = "Sindh", 
    date   = ymd(sindh_get(1, TRUE, sindh_case_n, sindh_case_data)),
    cases  = sindh_get(2, FALSE, sindh_case_n, sindh_case_data)
);
sindh_cases = sindh_cases[c(F, diff(date) == 1)] # keep only data with identifiable 24 hour change
sindh_cases[, cases := c(NA, diff(cases))]

sindh_deaths = data.table(location = "Sindh", 
    date   = ymd(sindh_get(1, TRUE, sindh_death_n, sindh_death_data)),
    deaths = sindh_get(2, FALSE, sindh_death_n, sindh_death_data)
);
sindh_deaths = sindh_deaths[c(F, diff(date) == 1)]
sindh_deaths[, deaths := c(NA, diff(deaths))]

sindh = merge(sindh_cases, sindh_deaths, by = c("location", "date"), all = T)
sindh = sindh[order(date)];

ggplot(sindh) + geom_line(aes(date, cases, colour = "cases")) + geom_line(aes(date, deaths, colour = "deaths")) + scale_y_log10()

fwrite(sindh, "./epi_data.csv");


# MOBILITY DATA
# NOTE: These are large and publicly available files, so they have been left off the repo.
# For Google Community Mobility reports, download the "Global CSV" from https://www.google.com/covid19/mobility/
# For OxCGRT, download latest data CSV from https://github.com/OxCGRT/covid-policy-tracker/tree/master/data

google_mobility_filename = "~/Global_Mobility_Report-2021-01-07.csv"
oxcgrt_filename = "~/OxCGRT_latest-2021-01-12.csv"

g = fread(google_mobility_filename)
oxcgrt = fread(oxcgrt_filename)
worldpop = fread("worldpop5yr.lfs.csv")
worldpop[, pop := rowSums(.SD), .SDcols = f_0:m_80]

getgmob = function(g, country, sub_region = NULL)
{
    if (is.null(sub_region)) {
        gg = g[country_region == country & sub_region_1 == "" & sub_region_2 == "" & metro_area == ""];
        loc = country;
    } else {
        gg = g[country_region == country & sub_region_1 == sub_region & sub_region_2 == "" & metro_area == ""];
        loc = sub_region;
    }
    
    gg = melt(gg, measure.vars = 9:14);
    gg[, variable := str_replace(variable, "_percent_change_from_baseline", "")];
    gg[, .(location = ..loc, date = date, context = variable, change = value / 100)];
}

getschool = function(oxcgrt, country_name, as_location)
{
    sch = oxcgrt[CountryName == country_name & !is.na(`C1_School closing`), 
        .(location = as_location, context = "school", change = -`C1_School closing` / 3), by = .(date = as.IDate(ymd(Date)))]
    setcolorder(sch, c("location", "date", "context", "change"))
    sch
}

gmob_pakistan_2021_01_12 = function()
{
    gmob = getgmob(g, "Pakistan", "Sindh")
    contexts = gmob[, unique(context)]
    
    gmob = rbind(gmob,
        data.table(
            location = "Sindh",
            date = as.IDate(rep(ymd("2021-01-05") + 1:177, length(contexts))),
            context = rep(contexts, each = 177),
            change = 0
        )
    )
    gmob = gmob[order(context, date)]
    
    for (con in contexts) {
        gmob[context == con & date >= "2020-09-23" & date <= "2021-07-01",
            change := seq(change[1], 0, length.out = .N)]
    }
    
    gmob
}

school_pakistan_2021_01_12 = function()
{
    sch = getschool(oxcgrt, "Pakistan", "Sindh")[date < "2020-09-15"]
    sch = rbind(sch, data.table(
        location = "Sindh", 
        date = as.IDate(ymd("2020-09-16") + 0:138),
        context = "school", 
        change = c(
            seq(-1, 0, length.out = 14),
            rep(0, 57),
            rep(-1, 53),
            seq(-1, 0, length.out = 15))
    ))
    
    # Add regular school breaks
    sch = rbind(sch, data.table(
        location = "Sindh", 
        date = as.IDate(ymd("2021-02-01") + 1:as.numeric(ymd("2031-12-31") - ymd("2021-02-01"))),
        context = "school", 
        change = 0)
    )
    
    sch[date >= "2021-04-01" & month(date) %in% c(6, 7, 8), change := -1]

    sch
}

# Build mobility indices
mob = rbindlist(list(
    gmob_pakistan_2021_01_12(), #getgmob(g, "Pakistan", "Sindh"),
    school_pakistan_2021_01_12() #getschool(oxcgrt, "Pakistan", "Sindh")
));

fwrite(mob, "./mob_data.csv");
