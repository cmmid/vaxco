// randomizer.h
// (C) 2013-2020 Nicholas G Davies

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(BH)]]

#ifndef RANDOMIZER_H
#define RANDOMIZER_H

#include <vector>
#include <limits>
#include <cmath>
#include <stdexcept>
#include <string>
#include <algorithm>
#include <boost/random/mersenne_twister.hpp>

class Randomizer
{
public:
    Randomizer(unsigned long int seed = 0);

    void Reset();

    double Uniform(double min = 0.0, double max = 1.0);
    double RoundedUniform(double min = 0.0, double max = 1.0, double shoulder = 0.01);
    double Normal(double mean = 0.0, double sd = 1.0);
    double Normal(double mean, double sd, double clamp);
    double Cauchy(double x0 = 0.0, double gamma = 1.0);
    double LogNormal(double zeta = 0.0, double sd = 1.0);
    double Exponential(double rate = 1.0);
    double Gamma(double shape, double scale);
    double Beta(double alpha, double beta);
    unsigned int Discrete(unsigned int size);
    int Discrete(int min, int max);
    void Multinomial(unsigned int N, std::vector<double>& p, std::vector<unsigned int>& n_out);
    bool Bernoulli(double p);
    unsigned int Binomial(unsigned int n, double p);
    unsigned int BetaBinomial(unsigned int n, double p, double a_plus_b);
    int Poisson(double mean);
    int Geometric(double p);
    int Round(double x);

    unsigned int operator()();
    unsigned int operator()(unsigned int size);

    template <typename T>
    void Shuffle(std::vector<T>& vec);

    inline unsigned long int Seed() const { return seed; }

private:
    unsigned long int seed;
    boost::random::mt19937 generator;
};

template <typename T>
void Randomizer::Shuffle(std::vector<T>& vec)
{
    std::random_shuffle(vec.begin(), vec.end(), *this);
}


#endif