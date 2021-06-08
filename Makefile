
-include local.makefile

DRPBXPTH ?= ~/Dropbox/Covid-WHO-vax
IDIR = ${DRPBXPTH}/inputs
ODIR = ${DRPBXPTH}/outputs
FDIR = ${DRPBXPTH}/figures

${IDIR} ${ODIR} ${FDIR} ${ODIR}/econ ${ODIR}/sim:
	mkdir -p $@

R = Rscript $^ $@
Rpipe = Rscript $^ $| $@
Rstarp = Rscript $^ $* $| $@

METPAT ?= ${ODIR}/metrics_
OTHPAT ?= ${ODIR}/other_

CONFDB ?= ${ODIR}/config.rds
DBPAT := ${METPAT}%.sqlite
ODBPAT := ${OTHPAT}%.sqlite

# local copy required
CMPTH := covidm
CMURL := git@github.com:nicholasdavies/covidm.git

${CMPTH}:
	cd $(dir $@); git clone --single-branch --branch ngmupdate ${CMURL} $(notdir $@)

DATAPTH ?= .
FITS := fit_sindh.qs $(shell cd ${DATAPTH}; ls fit_sindh_waning_*.qs)
DFITS := fitd_sindh.qs $(shell cd ${DATAPTH}; ls fitd_sindh_waning_*.qs)
DATASRC := $(addprefix ${DATAPTH}/,fitd_combined.qs epi_data.csv mob_data.csv)

cm: ${CMPTH}

# warning: this will misbehave on HPC if run in parallel
# needs to be executed prior to the parallel work
setup: setup.R $(firstword ${DFITS}) | ${IDIR} ${ODIR} ${FDIR}
	Rscript $^ ${CMPTH}

# TODO add params.json
${CONFDB}: build_db.R $(firstword ${DFITS}) | ${CMPTH} ${IDIR}
	${Rpipe}

db: ${CONFDB} ${IDIR}/scenarios.csv

cleandb:
	rm ${CONFDB}

${DATAPTH}/fit_combined.qs: merge_fits.R ${FITS}
	${R}

${DATAPTH}/fitd_combined.qs: merge_fits.R ${DFITS}
	${R}

${ODIR}/sim/%.rds: compute.R ${DATASRC} ${CONFDB} | ${CMPTH} ${ODIR}/sim
	Rscript $^ $* $(firstword $|) $@

.PRECIOUS: ${ODIR}/sim/%.rds

test/%.rds: test_compute.R ${DATASRC} ${CONFDB} | ${CMPTH} ${ODIR}/sim
	Rscript $^ $* $(firstword  $|) $@

BASESIM := 18433 18434 18435 18436 18437 18438 18439 18440
MINSIM := ${BASESIM}\
09921 12993 16065 09922 12994 16066 09923 12995 16067 09924 12996 16068\
03778 12865 13122 14018 12930 13026\
19146 20170 13506 13002 13010\
03650 03906 04034 12866 13250 09729\
00714 09930 00716 09932 03788 13004

minsim: $(patsubst %,${ODIR}/sim/%.rds,${MINSIM})

testsim: ${ODIR}/sim/00001.rds

ECONDATA := covid_other_costs.csv covid_vac_costs_per_dose.csv daly_scenarios.csv

${ODIR}/epi_baseline.rds: epi_baseline.R $(patsubst %,${ODIR}/sim/%.rds,${BASESIM}) | ${ODIR}/sim ${CONFDB}
	Rscript $< $| $@

${ODIR}/epiq/%.rds: epi_quantile.R ${ODIR}/sim/%.rds ${CONFDB} ${ODIR}/epi_baseline.rds
	Rscript $^ $* $@

epitest: $(patsubst %,test/%.rds,3778 3786 3794 3802 12994 13002 13010 13018 18434)

cleanepitest:
	rm test/*.rds

epiall: $(patsubst %,${ODIR}/epiq/%.rds,$(shell seq -f%05g 1 20488))
epimin: $(patsubst %,${ODIR}/epiq/%.rds,${MINSIM})


${ODIR}/epi_quantile.rds: qmerge.R $(filter-out ${SUMMARIES}, $(wildcard ${ODIR}/epiq/*.rds)) | ${ODIR}/epiq epimin
	Rscript $< $(firstword $|) $@

epiq: ${ODIR}/epi_quantile.rds

ECONDATA := covid_other_costs.csv covid_vac_costs_per_dose.csv daly_scenarios.csv

# compute the econ scenarios for each epi scenario - these are quantiles
${ODIR}/econ/%.rds: econ.R ${ECONDATA} ${CONFDB} ${ODIR}/sim/%.rds ${ODIR}/econ/baseline.rds
	Rscript econ.R ${ECONDATA} ${CONFDB} ${ODIR}/sim/$*.rds $@

# compute all the baseline scenarios - full series, unquantiled
${ODIR}/econ/baseline.rds: econ.R ${ECONDATA} ${CONFDB} $(patsubst %,${ODIR}/sim/%.rds,${BASESIM}) | ${ODIR}/econ
	Rscript econ.R ${ECONDATA} ${CONFDB} ${ODIR}/sim $@

ebaseline: ${ODIR}/econ/baseline.rds
eone: $(patsubst %,${ODIR}/econ/%.rds,00001)
eall: $(patsubst %,${ODIR}/econ/%.rds,$(shell seq -f%05g 1 20488))
econmin: $(patsubst %,${ODIR}/econ/%.rds,${MINSIM})

# merge the econ quantiled scenarios
${ODIR}/econ_quantile.rds: qmerge.R $(filter-out ${SUMMARIES}, $(wildcard ${ODIR}/econ/*.rds)) | ${ODIR}/econ econmin
	Rscript $< $(firstword $|) $@

${ODIR}/validation.rds: validation_set.R ${CONFDB} $(wildcard ${OTHPAT}*.sqlite)
	Rscript $(wordlist 1,2,$^) ${OTHPAT} $@

digest: ${ODIR}/epi_quantile.rds ${ODIR}/econ_quantile.rds

${ODIR}/sim_model.rds: sim_model_fit.R fitd_combined.qs sindh_data.csv | ${CMPTH}
	${Rpipe}

simmodel: ${ODIR}/sim_model.rds







# MT FIG2 - validation figure
${FDIR}/baseline.png: fig_epi_baseline.R ${ODIR}/epi_quantile.rds ${CONFDB} | ${FDIR}
	${R}

# MT FIG3 - cases/deaths averted for initial 4000k doses per day, allornothing-infection vaccine
# 2.5 year natural immunity, varying vaccine immunity duration
${FDIR}/averted_4000.png: fig_epi_averted.R ${ODIR}/epi_quantile.rds ${CONFDB} | ${FDIR}
	${R}

${FDIR}/model_fit.png ${FDIR}/model_fit_ext.png: fig_model_fit.R ${ODIR}/sim_model.rds data_fitting/epi_data.csv fitd_combined.qs
	${R}

# SI FIG S4 S5 - hosp outcomes averted for initial 4000k doses per day, allornothing-infection vaccine
# 2.5 year natural immunity, varying vaccine immunity duration
${FDIR}/other_averted_4000.png ${FDIR}/other_averted_4000_non.png: fig_epi_averted_other.R ${ODIR}/epi_quantile.rds ${CONFDB} | ${FDIR}
	${R}

#${FDIR}/incremental.png: fig_incremental.R ${IDIR}/scenarios.rds ${ODIR}/quantiles.rds ${ODIR}/baseline.rds
#	${R}

#${FDIR}/validation.png: fig_validation.R ${IDIR}/scenarios.rds ${ODIR}/validation.rds
#	${R}

#${FDIR}/icer.png: fig_icer.R ${IDIR}/config_high.sqlite ${ODIR}/icer.rds
#	${R}

figs: $(patsubst %,${FDIR}/%.png,baseline averted_4000 other_averted_4000 model_fit)


${ODIR}/exti/%.rds: rq_compute_reintro.R ${DATASRC} ${CONFDB} | ${CMPTH} ${ODIR}/exti
	Rscript $^ $* $(firstword $|) $@

${ODIR}/exto/%.rds: rq_compute_from50.R ${DATASRC} ${CONFDB} | ${CMPTH} ${ODIR}/exto
	Rscript $^ $* $(firstword $|) $@

${ODIR}/exti/q_%.rds: epi_quantile.R ${ODIR}/exti/%.rds ${CONFDB} ${ODIR}/exti/18434.rds | ${ODIR}/extq
	Rscript $^ $* $@

${ODIR}/exto/q_%.rds: epi_quantile.R ${ODIR}/exto/%.rds ${CONFDB} ${ODIR}/sim/18434.rds | ${ODIR}/extq
	Rscript $^ $* $@

rqs: ${ODIR}/exti/q_12994.rds ${ODIR}/exto/q_12994.rds