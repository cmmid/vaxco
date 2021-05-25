// sim_compartment.h

#ifndef SIM_COMPARTMENT_H
#define SIM_COMPARTMENT_H

#include <vector>
using namespace std;
class Compartment;

//
// MODEL DYNAMICS
//

struct Parameters;
class Randomizer;
class Reporter;

// A population of individuals, with SEI3HR dynamics.
class Population
{
public:
    // Construct a population with the specified size by age group; initially all uninfected
    Population(Parameters& P, unsigned int pindex);

    // Do seeding and calculate contagiousness
    void Contagiousness(Parameters& P, Randomizer& Rand, double t, vector<double>& contag);

    // Execute one time step's events
    void Tick(Parameters& P, Randomizer& Rand, double t, vector<double>& infec, Reporter& rep);

    // Print full population details
    void DebugPrint() const;

//private:
    vector<double> lambda;
    vector<double> N, S, R;                     // Total number, Susceptible, recovered
    vector<double> Sv, Rv;                      // Susceptible and vaccinated, recovered and vaccinated
    vector<Compartment> E, Ev, Ip, Ia, Is, C;   // Exposed (Exposed-and-vaccinated), presymptomatic, asymptomatic, symptomatic, cases (reported)
    unsigned int seed_row;                      // Which seed event is next
    unsigned int p;                             // Which population this is

    // re-useable temporary storage for multinomial draws    
    vector<unsigned int> ni_out;
    vector<double> nd_out;
    
    // User-specified process compartments, indexed by process id, then group
    // e.g. pc[process_id][group] is current value of process_id state for group
    vector<vector<Compartment>> pc;
    // re-usable temporary storage for incidence / outcidence monitoring for process states
    vector<double> pci;
    vector<double> pco;
};

// A metapopulation, containing multiple subpopulations.
class Metapopulation
{
public:
    Metapopulation(Parameters& P);

    // Execute one time step's events
    bool Tick(Parameters& P, Randomizer& Rand, double t, unsigned int ts, Reporter& rep);

    // Run the model
    void Run(Parameters& P, Randomizer& Rand, Reporter& rep, vector<double> x_fit = vector<double>());

//private:
    vector<vector<double>> contag;
    vector<vector<double>> infec;
    vector<Population> pops;
    vector<double> x;
};

#endif