// randomizer.cpp

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]

#include "randomizer.h"
#include <numeric>
#include <boost/random/uniform_real_distribution.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/lognormal_distribution.hpp>
#include <boost/random/cauchy_distribution.hpp>
#include <boost/random/exponential_distribution.hpp>
#include <boost/random/gamma_distribution.hpp>
#include <boost/random/beta_distribution.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <boost/random/bernoulli_distribution.hpp>
#include <boost/random/binomial_distribution.hpp>
#include <boost/random/poisson_distribution.hpp>
#include <boost/random/geometric_distribution.hpp>
using namespace boost::random;

Randomizer::Randomizer(unsigned long int s)
 : seed(s)
{
    Reset();
}

void Randomizer::Reset()
{
    if (seed == 0)
        generator.seed();
    else
        generator.seed(seed);
}

double Randomizer::Uniform(double min, double max)
{
    uniform_real_distribution<double> d(min, max);
    return d(generator);
}

double Randomizer::RoundedUniform(double min, double max, double shoulder)
{
    if (min >= max)
        return min;
    double z = Uniform();
    double sd = shoulder * (max - min) / ((1 - shoulder) * 2.50662827463);
    if (z < shoulder / 2)
        return min - abs(Normal(0, sd));
    else if (z < shoulder)
        return max + abs(Normal(0, sd));
    else
        return Uniform(min, max);
}

double Randomizer::Normal(double mean, double sd)
{
    normal_distribution<double> d(mean, sd);
    return d(generator);
}

double Randomizer::Normal(double mean, double sd, double clamp)
{
    double n;
    do n = Normal(mean, sd); while (std::fabs(n - mean) > clamp);
    return n;
}

double Randomizer::LogNormal(double zeta, double sd)
{
    lognormal_distribution<double> d(zeta, sd);
    return d(generator);
}

double Randomizer::Cauchy(double x0, double gamma)
{
    cauchy_distribution<double> d(x0, gamma);
    return d(generator);
}

double Randomizer::Exponential(double rate)
{
    exponential_distribution<double> d(rate);
    return d(generator);
}

double Randomizer::Gamma(double shape, double scale)
{
    gamma_distribution<double> d(shape, scale);
    return d(generator);
}

double Randomizer::Beta(double alpha, double beta)
{
    beta_distribution<double> d(alpha, beta);
    return d(generator);
}

unsigned int Randomizer::Discrete(unsigned int size)
{
    uniform_int_distribution<unsigned int> d(0, size - 1);
    return d(generator);
}

int Randomizer::Discrete(int min, int max)
{
    uniform_int_distribution<int> d(min, max);
    return d(generator);
}

void Randomizer::Multinomial(unsigned int N, std::vector<double>& p, std::vector<unsigned int>& n_out)
{
    unsigned int n = N;
    double p_denom = std::accumulate(p.begin(), p.end(), 0.0);
    for (unsigned int i = 0; i < p.size() - 1; ++i)
    {
        n_out[i] = Binomial(n, p[i] / p_denom);
        n -= n_out[i];
        p_denom -= p[i];
    }
    n_out[p.size() - 1] = n;
}

bool Randomizer::Bernoulli(double p)
{
    if (p <= 0) return false;
    if (p >= 1) return true;
    bernoulli_distribution<double> d(p);
    return d(generator);
}

unsigned int Randomizer::Binomial(unsigned int n, double p)
{
    if (p <= 0) return 0;
    binomial_distribution<int, double> d(n, p);
    return d(generator);
}

unsigned int Randomizer::BetaBinomial(unsigned int n, double p, double a_plus_b)
{
    if (a_plus_b > 0) {
        p = Beta(a_plus_b * p, a_plus_b * (1 - p));
    }
    return Binomial(n, p);
}

int Randomizer::Poisson(double mean)
{
    if (mean <= 0) return 0;
    poisson_distribution<unsigned int, double> d(mean);
    return d(generator);
}

int Randomizer::Geometric(double p)
{
    if (p <= 0) return 0;
    geometric_distribution<unsigned int, double> d(p);
    return d(generator);
}

int Randomizer::Round(double x)
{
    int sign = x < 0 ? -1 : 1;
    double intpart, fracpart;
    fracpart = std::modf(std::fabs(x), &intpart);
    return sign * (intpart + Bernoulli(fracpart));
}

unsigned int Randomizer::operator()()
{
    return generator();
}

unsigned int Randomizer::operator()(unsigned int size)
{
    return Discrete(size);
}
