// parameters.cpp

#include "parameters.h"

// Helpers to set parameters
#define _CheckSet(variable) else if (name == #variable) { ParamSet(variable, value); }
#define _CheckSetParent(P, variable) else if (P != 0 && name == #variable) { ParamSet(P->variable, value); return false; }

void ParamSet(double& variable, Rcpp::RObject& value)
{
    variable = Rcpp::as<double>(value);
}

void ParamSet(Discrete& variable, Rcpp::RObject& value)
{
    variable = Rcpp::as<vector<double>>(value);
}

void ParamSet(vector<double>& variable, Rcpp::RObject& value)
{
    variable = Rcpp::as<vector<double>>(value);
}

void ParamSet(Matrix& variable, Rcpp::RObject& value)
{
    Rcpp::NumericMatrix rhs = Rcpp::as<Rcpp::NumericMatrix>(value);
    variable = Matrix(0.0, rhs.nrow(), rhs.ncol());
    for (int r = 0; r < rhs.nrow(); ++r)
        for (int c = 0; c < rhs.ncol(); ++c)
            variable(r, c) = rhs(r, c);
}

void ParamSet(vector<Matrix>& variable, Rcpp::RObject& value)
{
    Rcpp::List ml = Rcpp::as<Rcpp::List>(value);
    for (unsigned int i = 0; i < ml.size(); ++i)
    {
        Rcpp::RObject mat = Rcpp::as<Rcpp::RObject>(ml[i]);
        ParamSet(variable[i], mat);
    }
}

void ParamSet(ProcessList& variable, Rcpp::RObject& value)
{
    
    // translate each r object into a process object
    // ids are not set until all process objects available
    // and are added to a ProcessList
    Rcpp::List pl = Rcpp::as<Rcpp::List>(value);
    vector<ProcessSpec> processes;
    processes.reserve(pl.size() + 1);
    
    for (size_t i=0; i<pl.size(); i++) {
        Rcpp::List rprocessobj = Rcpp::as<Rcpp::List>(pl[i]);
        ProcessSpec process;
        process.source_name = Rcpp::as<string>(rprocessobj["source"]);
        process.type = Rcpp::as<string>(rprocessobj["type"]);
        process.names = Rcpp::as<vector<string>>(rprocessobj["names"]);
        
        process.report = Rcpp::as<vector<string>>(rprocessobj["report"]);
        
        Matrix m_prob, m_delays;
        Rcpp::RObject r_prob = Rcpp::as<Rcpp::RObject>(rprocessobj["prob"]);
        Rcpp::RObject r_delays = Rcpp::as<Rcpp::RObject>(rprocessobj["delays"]);
        ParamSet(m_prob, r_prob);
        ParamSet(m_delays, r_delays);
        
        for (unsigned int group = 0; group < m_prob.NCol(); ++group)
        {
            process.prob.push_back(vector<double>(m_prob.NRow(), 0.));
            for (unsigned int outcome = 0; outcome < m_prob.NRow(); ++outcome)
                process.prob[group][outcome] = m_prob(outcome, group);
        }
        
        for (unsigned int outcome = 0; outcome < m_delays.NRow(); ++outcome)
        {
            process.delays.push_back(Discrete());
            std::vector<double> uw(m_delays.NCol(), 0.);
            for (unsigned int c = 0; c < m_delays.NCol(); ++c)
                uw[c] = m_delays(outcome, c);
            process.delays.back() = uw;
        }
        
        processes.push_back(process);
        
    }
    
    
    variable.Update(processes);

}


bool PopulationParameters::Set(Parameters* parent, string& name, Rcpp::RObject& value)
{
    if (name == "contact" || name == "contact_mult" || name == "contact_lowerto" || name == "matrices")
        needs_recalc = true;

    if (false) {}
    _CheckSet(dE)
    _CheckSet(dEv)
    _CheckSet(dIp)
    _CheckSet(dIa)
    _CheckSet(dIs)
    _CheckSet(dC)
    _CheckSet(size)
    _CheckSet(imm0)
    _CheckSet(matrices)
    _CheckSet(contact)
    _CheckSet(contact_mult)
    _CheckSet(contact_lowerto)
    _CheckSet(u)
    _CheckSet(uv)
    _CheckSet(fIp)
    _CheckSet(fIa)
    _CheckSet(fIs)
    _CheckSet(y)
    _CheckSet(yv)
    _CheckSet(omega)
    _CheckSet(rho)
    _CheckSet(tau)
    _CheckSet(v)
    _CheckSet(ev)
    _CheckSet(wn)
    _CheckSet(wv)
    _CheckSet(A)
    _CheckSet(B)
    _CheckSet(D)
    _CheckSet(season_A)
    _CheckSet(season_T)
    _CheckSet(season_phi)
    _CheckSetParent(parent, travel)
    else
    {
        throw logic_error("Unrecognised parameter " + name + ".");
    }

    return true;
}



void ParamSet(double& variable, vector<double>& value)
{
    variable = value[0];
}

void ParamSet(Discrete& variable, vector<double>& value)
{
    variable = value;
}

void ParamSet(vector<double>& variable, vector<double>& value)
{
    variable = value;
}

bool PopulationParameters::Set(Parameters* parent, string& name, vector<double>& value)
{
    if (name == "contact" || name == "contact_mult" || name == "contact_lowerto" || name == "matrices")
        needs_recalc = true;

    if (false) {}
    _CheckSet(dE)
    _CheckSet(dEv)
    _CheckSet(dIp)
    _CheckSet(dIa)
    _CheckSet(dIs)
    _CheckSet(dC)
    _CheckSet(size)
    _CheckSet(imm0)
    //_CheckSet(matrices)
    _CheckSet(contact)
    _CheckSet(contact_mult)
    _CheckSet(contact_lowerto)
    _CheckSet(u)
    _CheckSet(uv)
    _CheckSet(fIp)
    _CheckSet(fIa)
    _CheckSet(fIs)
    _CheckSet(y)
    _CheckSet(yv)
    _CheckSet(omega)
    _CheckSet(rho)
    _CheckSet(tau)
    _CheckSet(v)
    _CheckSet(ev)
    _CheckSet(wn)
    _CheckSet(wv)
    _CheckSet(A)
    _CheckSet(B)
    _CheckSet(D)
    _CheckSet(season_A)
    _CheckSet(season_T)
    _CheckSet(season_phi)
    //_CheckSetParent(parent, travel)
    else
    {
        throw logic_error("Unrecognised parameter " + name + ".");
    }

    return true;
}

void Parameters::FilterForRun(unsigned int r)
{
    for (auto i = changes.ch.begin(); i != changes.ch.end(); )
    {
        if (i->runs.empty() || find(i->runs.begin(), i->runs.end(), r) != i->runs.end())
            ++i;
        else
            i = changes.ch.erase(i);
    }
}


// Helpers to set parameters
#define ParamAssign(t, v)               if (list.containsElementNamed(#v)) P.v = Rcpp::as<t>(list[#v]);
#define ParamMatrixAssign(v)            if (list.containsElementNamed(#v)) SetMatrix(P.v, Rcpp::as<Rcpp::NumericMatrix>(list[#v]));
#define ParamPopAssign(t, v, i)         if (popi.containsElementNamed(#v)) P.pop[i].v = Rcpp::as<t>(popi[#v]);
#define ParamPopMatrixAssign(v, i)      if (popi.containsElementNamed(#v)) SetMatrix(P.pop[i].v, Rcpp::as<Rcpp::NumericMatrix>(popi[#v]));

void SetMatrix(Matrix& mat, const Rcpp::NumericMatrix& rhs)
{
    mat = Matrix(0.0, rhs.nrow(), rhs.ncol());
    for (int r = 0; r < rhs.nrow(); ++r)
        for (int c = 0; c < rhs.ncol(); ++c)
            mat(r, c) = rhs(r, c);
}

void SetParameters(Parameters& P, Rcpp::List list, Randomizer& Rand)
{
    // TODO clean up and use namespace Rcpp
    // TODO use the ParamSet functions above for all of these parameter assignments
    ParamAssign(string, model);
    ParamAssign(double, time_step);
    ParamAssign(double, time0);
    ParamAssign(double, time1);
    ParamAssign(unsigned int, report_every);
    ParamAssign(bool, fast_multinomial);
    ParamAssign(bool, deterministic);
    
    if (P.report_every != 1/P.time_step)
        throw("report_every must be the reciprocal of time_step.");

    if (list.containsElementNamed("pop"))
    {
        Rcpp::List populations = Rcpp::as<Rcpp::List>(list["pop"]);
        unsigned int np = populations.size();
        P.pop.assign(np, PopulationParameters());

        for (unsigned int i = 0; i < np; ++i)
        {
            Rcpp::List popi = Rcpp::as<Rcpp::List>(populations[i]);

            ParamPopAssign(vector<double>, dE, i);
            ParamPopAssign(vector<double>, dEv, i);
            ParamPopAssign(vector<double>, dIp, i);
            ParamPopAssign(vector<double>, dIa, i);
            ParamPopAssign(vector<double>, dIs, i);
            ParamPopAssign(vector<double>, dC, i);

            if (P.fast_multinomial)
            {
                P.pop[i].dE.mn_approx.Set(P, Rand, P.pop[i].dE.weights);
                P.pop[i].dEv.mn_approx.Set(P, Rand, P.pop[i].dEv.weights);
                P.pop[i].dIp.mn_approx.Set(P, Rand, P.pop[i].dIp.weights);
                P.pop[i].dIa.mn_approx.Set(P, Rand, P.pop[i].dIa.weights);
                P.pop[i].dIs.mn_approx.Set(P, Rand, P.pop[i].dIs.weights);
                P.pop[i].dC.mn_approx.Set(P, Rand, P.pop[i].dC.weights);
            }

            ParamPopAssign(vector<double>, size, i);
            ParamPopAssign(vector<double>, imm0, i);

            if (popi.containsElementNamed("matrices"))
            {
                Rcpp::List ml = Rcpp::as<Rcpp::List>(popi["matrices"]);
                for (unsigned int m = 0; m < ml.size(); ++m)
                {
                    Matrix mat;
                    SetMatrix(mat, Rcpp::as<Rcpp::NumericMatrix>(ml[m]));
                    P.pop[i].matrices.push_back(mat);
                }
            }

            ParamPopAssign(vector<double>, contact, i);
            ParamPopAssign(vector<double>, contact_mult, i);
            ParamPopAssign(vector<double>, contact_lowerto, i);
            ParamPopAssign(vector<double>, u, i);
            ParamPopAssign(vector<double>, uv, i);
            ParamPopAssign(vector<double>, fIp, i);
            ParamPopAssign(vector<double>, fIa, i);
            ParamPopAssign(vector<double>, fIs, i);
            ParamPopAssign(vector<double>, y, i);
            ParamPopAssign(vector<double>, yv, i);
            ParamPopAssign(vector<double>, omega, i);
            ParamPopAssign(vector<double>, rho, i);
            ParamPopAssign(vector<double>, tau, i);
            ParamPopAssign(vector<double>, v, i);
            ParamPopAssign(vector<double>, ev, i);
            ParamPopAssign(vector<double>, wn, i);
            ParamPopAssign(vector<double>, wv, i);
            ParamPopAssign(vector<double>, A, i);
            ParamPopAssign(vector<double>, B, i);
            ParamPopAssign(vector<double>, D, i);

            ParamPopAssign(double, season_A, i);
            ParamPopAssign(double, season_T, i);
            ParamPopAssign(double, season_phi, i);

            ParamPopAssign(vector<double>, seed_times, i);
            ParamPopAssign(vector<double>, dist_seed_ages, i);

            // Names
            ParamPopAssign(string, name, i);
            ParamPopAssign(vector<string>, group_names, i);

            // Calculate
            P.pop[i].Recalculate();
        }
    }
    
    // Read in processes
    if (list.containsElementNamed("processes"))
    {
        Rcpp::RObject r_processes = Rcpp::as<Rcpp::RObject>(list["processes"]);
        ParamSet(P.processes, r_processes);
    }
    
    ParamMatrixAssign(travel);

    // Read in schedule (change set)
    if (list.containsElementNamed("schedule"))
    {
        Rcpp::List schedule = Rcpp::as<Rcpp::List>(list["schedule"]);
        for (unsigned int j = 0; j < schedule.size(); ++j)
        {
            Rcpp::List sched = Rcpp::as<Rcpp::List>(schedule[j]);

            string param_name = Rcpp::as<string>(sched["parameter"]);
            vector<unsigned int> pops = Rcpp::as<vector<unsigned int>>(sched["pops"]);
            vector<unsigned int> runs;
            if (sched.containsElementNamed("runs"))
                runs = Rcpp::as<vector<unsigned int>>(sched["runs"]);

            string mode_name = Rcpp::as<string>(sched["mode"]);
            Change::Mode mode = Change::Assign;
            if (mode_name == "assign")          mode = Change::Assign;
            else if (mode_name == "add")        mode = Change::Add;
            else if (mode_name == "multiply")   mode = Change::Multiply;
            else if (mode_name == "lowerto")    mode = Change::LowerTo;
            else if (mode_name == "raiseto")    mode = Change::RaiseTo;
            else                                throw std::logic_error("Unrecognized change mode " + mode_name + ".");

            vector<double> times = Rcpp::as<vector<double>>(sched["times"]);
            Rcpp::List values_list = Rcpp::as<Rcpp::List>(sched["values"]);
            vector<vector<double>> values;

            for (unsigned int k = 0; k < values_list.size(); ++k)
                values.push_back(Rcpp::as<vector<double>>(values_list[k]));

            P.changes.ch.push_back(Change(P, pops, runs, param_name, mode, times, values));
        }
    }
    
}
