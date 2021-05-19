// convenience.cpp

// Helper functions
#include "convenience.h"
#include "parameters.h"
#include <boost/math/special_functions/gamma.hpp>
#include <cmath>
#include <iostream>
using namespace std;
using boost::math::lgamma;
using boost::math::gamma_p;


vector<double> seq(double x0, double x1, double by)
{
    int steps = int((x1 - x0) / by + by * 1e-3);
    if (steps >= 0)
    {
        vector<double> ret(steps);
        for (int i = 0; i < steps; ++i)
            ret[i] = x0 + by * i;
        return ret;
    }
    return vector<double>(0);
}

double gamma_P(double x, double shape, double scale)
{
    return boost::math::gamma_p(x / scale, shape);
}

// binomial log density
double binom(double k, double n, double p)
{
    return lgamma(n + 1.) - lgamma(k + 1.) - lgamma(n - k + 1.) + k * log(p) + (n - k) * log(1. - p);
}

// negative binomial log density
double nbinom(unsigned int x, double mean, double size)
{
    double k = x;
    double p = size / (size + mean);
    double n = mean * p / (1 - p);

    return lgamma(n + k) - lgamma(k + 1) - lgamma(n) + n * log(p) + k * log(1 - p);
}

// negative binomial log density with retrospective confirmation
double nbinom_gammaconf(unsigned int x, double mean, double size, double days_ago, double conf_delay_mean, double conf_delay_shape)
{
    double conf_delay_scale = conf_delay_mean / conf_delay_shape;
    double prop_confirmed = gamma_P(days_ago, conf_delay_shape, conf_delay_scale);
    return nbinom(x, mean * prop_confirmed, size);
}

// construct a delay distribution following a gamma distribution with mean mu and shape parameter shape.
vector<double> delay_gamma(double mu, double shape, double t_max, double t_step, double mult)
{
    double scale = mu / shape;
    vector<double> height;

    for (double t = 0.0; t < t_max + 0.5 * t_step; t += t_step)
        height.push_back(mult * (gamma_P(t + t_step/2, shape, scale) - 
            gamma_P(max(0.0, t - t_step/2), shape, scale)));
    return height;
}

// estimate the basic reproduction number
double estimate_R0(Parameters& P, double t, unsigned int p, unsigned int iter)
{
    vector<double> inf(P.pop[p].size.size(), 1.0);
    vector<double> inf2 = inf;

    double dIp = P.pop[p].dIp.Mean() * P.time_step;
    double dIs = P.pop[p].dIs.Mean() * P.time_step;
    double dIa = P.pop[p].dIa.Mean() * P.time_step;

    double n_inf = P.pop[p].size.size();
    double n_inf2 = 0;
    double R0 = 1;

    double seas = 1.0 + P.pop[p].season_A * cos(2. * M_PI * (t - P.pop[p].season_phi) / P.pop[p].season_T);

    for (unsigned int i = 0; i < iter; ++i)
    {
        n_inf2 = 0;
        for (unsigned int a = 0; a < inf.size(); ++a)
        {
            inf2[a] = 0;
            for (unsigned int b = 0; b < inf.size(); ++b)
            {
                inf2[a] += inf[b] * P.pop[p].cm(a, b) * P.pop[p].u[a] * seas * (
                    P.pop[p].y[b] * (P.pop[p].fIp[b] * dIp + P.pop[p].fIs[b] * dIs) +
                    (1 - P.pop[p].y[b]) * P.pop[p].fIa[b] * dIa
                );
            }
            n_inf2 += inf2[a];
        }

        R0 = n_inf2 / n_inf;

        swap(n_inf2, n_inf);
        swap(inf2, inf);
    }

    return R0;
}

// estimate the effective reproduction number
double estimate_Rt(Parameters& P, Reporter& dyn, double t, unsigned int p, unsigned int iter)
{
    vector<double> inf(P.pop[p].size.size(), 1.0);
    vector<double> inf2 = inf;
    vector<double> S(P.pop[p].size.size(), 0.0);

    for (unsigned int a = 0; a < S.size(); ++a)
        S[a] = dyn(t, p, a, 0) / P.pop[p].size[a];

    double dIp = P.pop[p].dIp.Mean() * P.time_step;
    double dIs = P.pop[p].dIs.Mean() * P.time_step;
    double dIa = P.pop[p].dIa.Mean() * P.time_step;

    double n_inf = P.pop[p].size.size();
    double n_inf2 = 0;
    double Rt = 1;

    double seas = 1.0 + P.pop[p].season_A * cos(2. * M_PI * (t - P.pop[p].season_phi) / P.pop[p].season_T);

    for (unsigned int i = 0; i < iter; ++i)
    {
        n_inf2 = 0;
        for (unsigned int a = 0; a < inf.size(); ++a)
        {
            inf2[a] = 0;
            for (unsigned int b = 0; b < inf.size(); ++b)
            {
                inf2[a] += S[a] * inf[b] * P.pop[p].cm(a, b) * P.pop[p].u[a] * seas * (
                    P.pop[p].y[b] * (P.pop[p].fIp[b] * dIp + P.pop[p].fIs[b] * dIs) +
                    (1 - P.pop[p].y[b]) * P.pop[p].fIa[b] * dIa
                );
            }
            n_inf2 += inf2[a];
        }

        Rt = n_inf2 / n_inf;

        swap(n_inf2, n_inf);
        swap(inf2, inf);
    }

    return Rt;
}

// clamp a number between two limits
double clamp(double x, double x0, double x1)
{
    return min(max(x, x0), x1);
}

// smootherstep function
double smootherstep(double x0, double x1, double y0, double y1, double x)
{
    x = clamp((x - x0) / (x1 - x0));
    return y0 + x * x * x * (x * (x * 6. - 15.) + 10.) * (y1 - y0);
}
