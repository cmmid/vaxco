// reporter.cpp

#include "reporter.h"
#include "parameters.h"
#include <numeric>

Reporter::Reporter(Parameters& P)
 : t0(P.time0),
   n_times((unsigned int)(P.time1 - P.time0) + 1),
   n_populations(P.pop.size()),
   n_age_groups(P.pop[0].size.size()),
   col_names(ref_col_names), user_defined_offset(ref_col_names.size())
{
    if (P.time_step != 1. / P.report_every)
        throw logic_error("Reporter requires P.time_step = 1 / P.report_every.");

    // Create space for built-in + user-process compartments
    data = vector<vector<double>>(
        col_names.size() +
        P.processes.recording_count,
        vector<double>(n_times * n_populations * n_age_groups, 0.)
    );
    
    // User-defined compartments
    for (auto pid : P.processes.prevalence_states) {
        col_names.push_back(P.processes.state_names[pid] + "_p");
    }
    for (auto iid : P.processes.incidence_states) {
        col_names.push_back(P.processes.state_names[iid] + "_i");
    }
    for (auto oid : P.processes.outcidence_states) {
        col_names.push_back(P.processes.state_names[oid] + "_o");
    }
    
}


// Access data, summed over populations and groups
double Reporter::operator()(string compartment, double t, initializer_list<unsigned int> pops, initializer_list<unsigned int> groups)
{
    // Locate compartment
    auto cc = find(col_names.begin(), col_names.end(), compartment);
    if (cc == col_names.end()) {
        cout << "No such compartment " << compartment << "\n" << flush;
        throw logic_error("No such compartment: " + compartment);
    }
    unsigned int c = cc - col_names.begin();

    // Define populations and groups to loop over
    // TODO optimize...
    vector<unsigned int> pp = pops;
    vector<unsigned int> gg = groups;
    if (pp.empty())
    {
        pp.assign(n_populations, 0);
        iota(pp.begin(), pp.end(), 0);
    }
    if (gg.empty())
    {
        gg.assign(n_age_groups, 0);
        iota(gg.begin(), gg.end(), 0);
    }

    // Sum over populations and groups
    double x = 0.0;
    for (auto p : pp)
        for (auto g : gg)
            x += (*this)(t, p, g, c);

    return x;
}

// Save data to file
void Reporter::Save(string basename, unsigned long int seed)
{
    ofstream fout(basename + ".txt");

    // Output header
    fout << "# t0 " << t0 << " n_times " << n_times << " n_populations " << n_populations << " n_groups " << n_age_groups << " seed " << seed << "\n";
    for (unsigned int cd = 0; cd < col_names.size(); ++cd)
        fout << col_names[cd] << (cd == col_names.size() - 1 ? "" : "\t");

    if (obs.size() > 0)
        for (unsigned int co = 0; co < obs.size(); ++co)
            fout << "\tobs" << co;

    fout << "\n";

    // Output data
    for (unsigned int r = 0; r < data[0].size(); ++r)
    {
        for (unsigned int cd = 0; cd < data.size(); ++cd)
            fout << data[cd][r] << (cd == data.size() - 1 ? "" : "\t");

        if (obs.size() > 0)
            for (unsigned int co = 0; co < obs.size(); ++co)
                fout << "\t" << obs[co][r];

        fout << "\n";
    }
    fout.close();

    // Output csv
    if (!csv.empty())
    {
        ofstream fcsv(basename + ".csv");
        fcsv << csv;
    }
}