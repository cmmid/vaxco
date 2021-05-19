// mcmc.cpp

#include <cmath>
#include <iostream>
#include <iomanip>
#include <vector>
#include <limits>
#include <string>
#include <stdexcept>
#include <omp.h>
#include "mcmc.h"
#include "user_defined.h"
#include "Rcpp_interface.h"
using namespace std;

MCMCReporter::MCMCReporter(unsigned int iterations, unsigned int chains, vector<string>& param_names)
 : n_chains(chains), n_samp(iterations * n_chains), 
   trial(n_samp, 0), chain(n_samp, 0), lp(n_samp, 0.0), ll(n_samp, 0.0), 
   theta(param_names.size(), vector<double>(n_samp, 1.0)), pnames(param_names)
{
}

void MCMCReporter::operator()(int TR, double LP, double CH, double LL, vector<double>& TH)
{
    if (TR < 0)
        return;

    int i = TR * n_chains + CH;

    trial[i] = TR;
    chain[i] = CH;
    lp[i] = LP;
    ll[i] = LL;
    for (unsigned int j = 0; j < TH.size(); ++j)
        theta[j][i] = TH[j];
}


Likelihood::Likelihood(Parameters& bp, unsigned int m_seed)
 : base_parameters(bp), model_seed(m_seed)
{ }

double Likelihood::operator()(const vector<double>& theta)
{
    Randomizer r(model_seed);
    Parameters p(base_parameters);

    CppChanges(theta, p);

    Reporter rep = RunSimulation(p, r, theta);

    double ll = CppLogLikelihood(theta, rep);

    return ll;
}


void DEMCMC_Priors(Randomizer& R, Likelihood& likelihood, MCMCReporter& report,
    int burn_in, int iterations, int n_chains, vector<Distribution>& priors,
    bool verbose, vector<string> param_names,
    bool reeval_likelihood, bool in_parallel, int n_threads, 
    bool classic_gamma, 
    vector<vector<double>> init, int init_iter)
{
    #ifdef _OPENMP
        if (in_parallel && n_threads > 0)
            omp_set_num_threads(n_threads);
    #endif

    if (n_chains < 3)
        throw runtime_error("Cannot use DE-MCMC with fewer than 3 chains.");

    // Store acceptance rates
    unsigned int ar_size = 1000;
    unsigned int ar_i = 0;
    bool show_ar = false;
    vector<bool> acceptances(ar_size, false);

    // Storage for chains and settings
    int n_theta = priors.size();
    vector<vector<double>> chains(n_chains, vector<double>(n_theta, 0.0));
    vector<double> p(n_chains, 0.0);    // log probability for each chain
    vector<double> l(n_chains, 0.0);    // log likelihood for each chain
    double b = 0.001;
    double adjusted_stepsize = 1.0;     // TODO - experimental

    // Storage for calls to Randomizer - to make thread-safe
    vector<vector<double>> random_perturbations(n_chains, vector<double>(n_theta, 0.0));
    vector<double> random_tests(n_chains, 0.0);
    vector<double> random_gammas(n_chains, 0.0);
    vector<vector<int>> random_chains(n_chains, vector<int>(2, 0));

    // Assemble target func
    auto target = [&](vector<double>& theta, double& l)
    {
        double p = 0;
        for (int d = 0; d < n_theta; ++d)
        {
            double pd = priors[d].LogProbability(theta[d]);
            if (pd == -numeric_limits<double>::infinity())
            {
                l = -numeric_limits<double>::infinity();
                return -numeric_limits<double>::infinity();
            }
            p += pd;
        }

        l = likelihood(theta);

        return l + p;
    };

    // Initialize chains . . .
    if (init.empty())
    {
        // . . . from prior
        for (int c = 0; c < n_chains; ++c)
            for (int d = 0; d < n_theta; ++d)
                chains[c][d] = priors[d].RandomInit(R);
    }
    else
    {
        // . . . from initial values supplied
        if ((int)init.size() != n_chains || (int)init[0].size() != n_theta)
            throw runtime_error("init vector supplied is not the right size.");
        chains = init;
    }

    // Set initial probabilities and observables
    if (verbose)
        cout << "Initializing chains...\n";

    #pragma omp parallel for if(in_parallel) schedule(dynamic)
    for (int c = 0; c < n_chains; ++c)
        p[c] = target(chains[c], l[c]);

    // Do iterations
    if (verbose)
        cout << "Iterating...\n" << flush;
    for (int i = init_iter; i < burn_in + iterations; ++i)
    {
        // If requested, re-evaluate likelihood for next iteration
        if (reeval_likelihood)
        {
            #pragma omp parallel for if(in_parallel) schedule(dynamic)
            for (int c = 0; c < n_chains; ++c)
                p[c] = target(chains[c], l[c]);
        }

        // Prepare storage and random variates
        bool migration = i < burn_in * 0.75 ? R.Bernoulli(0.05) : false;
        vector<int> migration_indices(n_chains, 0);
        if (migration)
        {
            for (int c = 0; c < n_chains; ++c)
                migration_indices[c] = c;
            R.Shuffle(migration_indices);
        }
        for (int c = 0; c < n_chains; ++c)
        {
            for (int d = 0; d < n_theta; ++d)
                random_perturbations[c][d] = R.Uniform(-b, b);
            random_tests[c] = R.Uniform();
            if (!migration)
            {
                if (classic_gamma)
                    random_gammas[c] = (i % 10 == 0 ? 1.0 : 2.38 / sqrt(2 * n_theta));
                else
                    random_gammas[c] = R.LogNormal(-0.5, 0.5) * adjusted_stepsize; //R.Uniform(0.5, 1.0) * adjusted_stepsize;
                do random_chains[c][0] = R.Discrete(n_chains); while (random_chains[c][0] == c);
                do random_chains[c][1] = R.Discrete(n_chains); while (random_chains[c][1] == c || random_chains[c][1] == random_chains[c][0]);
            }
        }
        auto saved_chains = chains;
        vector<int> accept(n_chains, 0);

        #pragma omp parallel for if(in_parallel) schedule(dynamic)
        for (int c = 0; c < n_chains; ++c)
        {
            vector<double> theta_p = chains[c];
            int c_from = c;

            // Generate proposal, either by migration...
            if (migration)
            {
                c_from = migration_indices[c];
                theta_p = saved_chains[migration_indices[(c + 1) % n_chains]];
                for (int d = 0; d < n_theta; ++d)
                    theta_p[d] += random_perturbations[c][d];
            }
            else // ... or by directed mutation
            {
                for (int d = 0; d < n_theta; ++d)
                    theta_p[d] += random_gammas[c] * (saved_chains[random_chains[c][1]][d] - saved_chains[random_chains[c][0]][d]) + random_perturbations[c][d];
            }

            // Calculate log-probability and accept or reject
            double l_p = 0;
            double p_p = target(theta_p, l_p);
            if ( (p_p == -numeric_limits<double>::infinity() && p[c_from] == -numeric_limits<double>::infinity() && random_tests[c] < 0.5)
                || (p_p > -numeric_limits<double>::infinity() && random_tests[c] < exp(p_p - p[c_from])) )
            {
                accept[c_from] = 1;
                chains[c_from] = theta_p;
                p[c_from] = p_p;
                l[c_from] = l_p;
            }
        }

        // Update acceptances
        for (int c = 0; c < n_chains; ++c)
        {
            if (ar_i == ar_size - 1) show_ar = true;
            acceptances[ar_i] = accept[c];
            ar_i = (ar_i + 1) % ar_size;
        }

        // Report results of this iteration
        for (int c = 0; c < n_chains; ++c)
            report(i - burn_in, p[c], c, l[c], chains[c]);

        // Print progress
        if (verbose)
        {
            if (i % 100 == 0)
            {
                cout << "\n" << (i < burn_in ? "Burn-in" : "Main") << " iteration " << i - burn_in << ":";
                if (!param_names.empty())
                {
                    cout << "\n         " << setw(12) << right << "log (P)" << setw(12) << right << "log (L)";
                    for (auto n : param_names)
                        cout << setw(12) << right << n;
                }
                for (int c = 0; c < n_chains; ++c)
                {
                    cout << "\nchain" << setw(4) << right << c << setw(12) << right << p[c] << setw(12) << right << l[c];
                    for (int d = 0; d < n_theta; ++d)
                        cout << setw(12) << right << chains[c][d];
                }

                double acceptance_rate = show_ar ? (double)count(acceptances.begin(), acceptances.end(), true) / ar_size : -1;
                cout << "\nacceptance rate: " << acceptance_rate << "\n" << flush;
            }
        }

        // Adjust step size?
        if (i % 100 == 0 && i < burn_in * 0.75 && show_ar == true)
        {
            double acceptance_rate = show_ar ? (double)count(acceptances.begin(), acceptances.end(), true) / ar_size : -1;
            if (acceptance_rate < 0.25)
                adjusted_stepsize /= 1.1;
            else
                adjusted_stepsize *= 1.1;
        }
    }
    if (verbose)
        cout << "\n";
}

// Global optimization algorithm based on Runarsson & Yao; https://notendur.hi.is/~tpr/papers/RuYa05.pdf
struct Particle
{
    Particle(Randomizer& rand, const vector<double>& lb, const vector<double>& ub, vector<Distribution>& priors)
    {
        for (unsigned int i = 0; i < lb.size(); ++i)
        {
            x.push_back(priors[i].RandomInit(rand));
            s.push_back((ub[i] - lb[i]) / sqrt(lb.size()));
        }
    }

    vector<double> x;
    vector<double> s;
    double f;
};

void Optimize_Priors(Randomizer& R, Likelihood& likelihood, MCMCReporter& report, std::vector<Distribution>& priors,
    unsigned int maxeval, double ftol_abs, bool verbose, bool in_parallel, unsigned int n_threads)
{
    #ifdef _OPENMP
        if (in_parallel && n_threads > 0)
            omp_set_num_threads(n_threads);
    #endif

    unsigned int d = priors.size();

    unsigned int np = 20 * (d + 1);         // number of particles
    unsigned int mu = np / 4;               // number of fittest particles to reproduce
    double alpha = 0.2;                     // exponential smoothing
    double gamma = 0.85;                    // differential variation step size
    double taup = 1.0 / sqrt(2. * d);       // step size evolution parameter
    double tau = 1.0 / sqrt(2. * sqrt(d));  // step size evolution parameter

    // Determine bounds
    vector<double> lb, ub;
    for (unsigned int i = 0; i < priors.size(); ++i)
    {
        lb.push_back(priors[i].LowerBound());
        ub.push_back(priors[i].UpperBound());
    }

    // Initialize particles
    vector<Particle> y;
    for (int i = 0; i < np; ++i)
        y.push_back(Particle(R, lb, ub, priors));
    vector<Particle> yn = y;
    Particle best = y[0];
    best.f = -numeric_limits<double>::infinity();

    // Assemble target func
    auto target = [&](vector<double>& theta, double& l)
    {
        double p = 0;
        for (int i = 0; i < d; ++i)
        {
            double pd = priors[i].LogProbability(theta[i]);
            if (pd == -numeric_limits<double>::infinity())
            {
                l = -numeric_limits<double>::infinity();
                return -numeric_limits<double>::infinity();
            }
            p += pd;
        }

        l = likelihood(theta);

        return l + p;
    };

    double last_f = -numeric_limits<double>::infinity();

    // Iterate
    unsigned int t = 0;
    for (;; ++t)
    {
        // Evaluate objective function
        #pragma omp parallel for if(in_parallel) schedule(dynamic)
        for (unsigned int i = 0; i < y.size(); ++i)
        {
            double l;
            y[i].f = target(y[i].x, l);
        }

        // Rank particles by fitness
        sort(y.begin(), y.end(), [](const Particle& a, const Particle& b) { return a.f > b.f; });

        // Remember best particle
        if (y[0].f > best.f)
            best = y[0];

        // Report on progress
        if (verbose && t % 10 == 0)
        {
            cout << "Iteration " << t << ": ";
            for (auto& b : y[0].x)
                cout << b << " ";
            cout << "with lp = " << y[0].f << "\n";
        }

        // Quit if end condition reached
        double this_f = y[0].f;
        if (abs(this_f - last_f) < ftol_abs || t == maxeval - 1)
            break;
        last_f = this_f;

        // Update particles
        for (unsigned int k = 0; k < np; ++k)
        {
            unsigned int i = k % mu;

            double norm = R.Normal();
            if (k < mu) // differential variation
            {
                yn[k] = y[i];
                for (unsigned int j = 0; j < d; ++j)
                    yn[k].x[j] += gamma * (y[0].x[j] - y[i + 1].x[j]);
            }
            else        // standard mutation
            {
                yn[k] = y[i];
                for (unsigned int j = 0; j < d; ++j)
                {
                    yn[k].s[j] *= exp(taup * norm + tau * R.Normal());
                    do {            
                        yn[k].x[j] = yn[i].x[j] + yn[k].s[j] * R.Normal();
                    } while (yn[k].x[j] < lb[j] || yn[k].x[j] > ub[j]);
                    yn[k].s[j] = y[i].s[j] + alpha * (yn[k].s[j] - y[i].s[j]);
                    yn[k].s[j] = min(yn[k].s[j], (ub[j] - lb[j]) / sqrt(d));
                }
            }

            // clamp to range
            for (unsigned int j = 0; j < d; ++j)
                yn[k].x[j] = max(lb[j], min(ub[j], yn[k].x[j]));
        }

        swap(y, yn);
    }

    // Report back
    double l;
    target(best.x, l);
    report(0, best.f, 0, l, best.x);
}
