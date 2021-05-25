// reporter.h

#ifndef REPORTER_H
#define REPORTER_H

struct Parameters;

#include <vector>
#include <iostream>
#include <fstream>
#include <stdexcept>
#include <algorithm>
#include <numeric>
#include <ctime>
#include <limits>
#include <initializer_list>
#include <string>

using namespace std;

enum ReportIndex
{
    riS = 0, riE, riIp, riIs, riIa, riR,
    ricases, ricases_reported, risubclinical,
    rilambda,
    riSv, riEv, riRv, rilambdav
};

const vector<string> ref_col_names = {
    "S", "E", "Ip", "Is", "Ia", "R",
    "cases", "cases_reported", "subclinical",
    "foi",
    "Sv", "Ev", "Rv", "foiv"
};

// For reporting results
class Reporter
{
public:
    Reporter(Parameters& P);

    // Access / modify data
    double& operator()(double t, unsigned int p, unsigned int a, unsigned int c)
    {
        unsigned int row = (unsigned int)(t - t0) * n_populations * n_age_groups + p * n_age_groups + a;
        return data[c][row];
    }

    // Access data, summed over populations and groups
    double operator()(string compartment, double t, initializer_list<unsigned int> p, initializer_list<unsigned int> a);

    // Access / modify observer data
    double& Obs(double t, unsigned int p, unsigned int a, unsigned int c)
    {
        if (c >= obs.size())
            obs.resize(c + 1, vector<double>(n_times * n_populations * n_age_groups, 0.));
        unsigned int row = (unsigned int)(t - t0) * n_populations * n_age_groups + p * n_age_groups + a;
        return obs[c][row];
    }

    // Save data to file
    void Save(string basename, unsigned long int seed);

//private:
    double t0;
    unsigned int n_times;
    unsigned int n_populations;
    unsigned int n_age_groups;
    vector<string> col_names;
    unsigned int user_defined_offset;

    vector<vector<double>> data;
    vector<vector<double>> obs;
    string csv;
};

#endif 
