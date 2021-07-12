# Overview

Provides the framework for performing the analysis in [this manuscript on vaccination in Sindh, Pakistan](https://www.medrxiv.org/content/10.1101/2021.02.24.21252338v1).

The various steps and their relationships are captured in the Makefile.

To run the analysis, you'll need R and the assorted dependencies described in `covidm/INSTALL.md`, as well as access to a high-performance computer setting.

The full workflow is designed to be run as an embarassingly-parallel array job, then consolidated via statistical summary before download from the HPC.
