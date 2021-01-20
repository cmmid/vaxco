# Functions to set parameters, etc., for fitting

# Pads (with before/after values) and smooths a mobility index series
mob_var_series = function(mob, mob_loc, source, mob_context, roll, ymd_start, ymd_end)
{
    # Create full series from ymd_start to ymd_end
    data = data.table(date = seq(ymd(ymd_start), ymd(ymd_end), by = "1 day"));
    data = merge(data, mob[location == mob_loc & context == mob_context, .(date, x = change)], by = "date", all.x = T);
    
    # Add baseline
    if (source == "google") {
        # for Google data, baseline is Jan 3rd to Feb 6th.
        data[date %between% c("2020-01-03", "2020-02-06"), x := 0.0];
    } else if (source == "facebook" & mob_context == "tiles_visited") {
        # for Facebook data, baseline is "in February".
        data[date %between% c("2020-02-01", "2020-02-29"), x := 0.0];
    }
    
    # Fill in intermediate NAs with na.approx
    data[, x := na.approx(x, na.rm = F)];
    
    # Smooth data
    data[, x := rollmean(x, roll, fill = NA)];

    # Fill in leading and trailing NAs
    first_nonNA_row = data[, head(which(!is.na(x)), 1)];
    last_nonNA_row = data[, tail(which(!is.na(x)), 1)];
    first = data[first_nonNA_row, x];
    last = data[last_nonNA_row, x];
    data[1:first_nonNA_row, x := first];
    data[last_nonNA_row:.N, x := last];
    
    return (data$x)
}

contact_schedule = function(mob, mob_loc, source, ymd_start, ymd_end)
{
    if (source == "google") {
        contact = cbind(
            1 + mob_var_series(mob, mob_loc, source, "residential", 7, ymd_start, ymd_end),
            1 + mob_var_series(mob, mob_loc, source, "workplaces", 7, ymd_start, ymd_end),
            1 + mob_var_series(mob, mob_loc, "oxcgrt", "school", 7, ymd_start, ymd_end),
            1 + 0.3 * mob_var_series(mob, mob_loc, source, "grocery_and_pharmacy", 7, ymd_start, ymd_end) +
                0.3 * mob_var_series(mob, mob_loc, source, "retail_and_recreation", 7, ymd_start, ymd_end) +
                0.3 * mob_var_series(mob, mob_loc, source, "transit_stations", 7, ymd_start, ymd_end) +
                0.1 * mob_var_series(mob, mob_loc, source, "parks", 7, ymd_start, ymd_end)
        )
    } else if (source == "facebook") {
        st = mob_var_series(mob, mob_loc, source, "single_tile", 7, ymd_start, ymd_end);
        tv = mob_var_series(mob, mob_loc, source, "tiles_visited", 7, ymd_start, ymd_end);
        contact = cbind(
            st / st[1],
            1 + tv,
            1 + mob_var_series(mob, mob_loc, "oxcgrt", "school", 7, ymd_start, ymd_end),
            1 + tv
        )
    } else {
        stop("source must be google or facebook.");
    }
    
    # Create schedule
    return (list(
        parameter = 'contact',
        pops = 0,
        mode = 'assign',
        values = split(contact, row(contact)),
        times = 0 : as.numeric(ymd(ymd_end) - ymd(ymd_start))
    ))
}

# Usage:
#  contact_schedule(mob, "Addis Ababa", "facebook", "2020-01-01", "2020-12-31")



#
# ADD LMIC REGIONS
#

worldpop = fread("./worldpop5yr.lfs.csv")

add_region = function(pid)
{
    dat = worldpop[id == pid];
    f = dat[, unlist(unname(.SD)), .SDcols = 10:26] / 1000;
    m = dat[, unlist(unname(.SD)), .SDcols = 27:43] / 1000;
    groups = limits_to_agegroups(seq(0, 80, by = 5));
    
    new_rows = data.table(country_code = 8888, name = dat[, unique(name)][1], age = groups, f = f, m = m, location_type = 4);
    
    cm_populations <<- rbind(cm_populations, new_rows)
}

add_region("PAK.8") # Sindh
