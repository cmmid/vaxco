
-include local.makefile

DRPBXPTH ?= ~/Dropbox/Covid-WHO-vax
IDIR = ${DRPBXPTH}/inputs
ODIR = ${DRPBXPTH}/outputs
FDIR = ${DRPBXPTH}/figures

${IDIR} ${ODIR} ${FDIR}:
	mkdir -p $@

R = Rscript $^ $@
Rpipe = Rscript $^ $| $@
Rstarp = Rscript $^ $* $| $@

METPAT ?= ${ODIR}/metrics_
OTHPAT ?= ${ODIR}/other_

CONFDB ?= ${ODIR}/config.sqlite
CONFEXT ?= ${ODIR}/config_ext.sqlite
DBPAT := ${METPAT}%.sqlite
ODBPAT := ${OTHPAT}%.sqlite

CMPTH ?= ../covidm-vaxco
CMURL := git@github.com:nicholasdavies/covidm.git

${CMPATH}:
	cd $(dir ${CMPATH}); git clone ${CMURL} $(notdir ${CMPATH})

DATAPTH ?= .
FITS := fit_sindh.qs $(shell cd ${DATAPTH}; ls fit_sindh_waning_*.qs)
DFITS := fitd_sindh.qs $(shell cd ${DATAPTH}; ls fitd_sindh_waning_*.qs)
DATASRC := $(addprefix ${DATAPTH}/,fitd_combined.qs epi_data.csv mob_data.csv)

# TODO add params.json
${CONFDB}: build_db.R $(firstword ${DFITS}) | ${CMPTH} ${IDIR}
	${Rpipe}

setup: setup.R $(firstword ${DFITS}) | ${IDIR} ${ODIR} ${FDIR}
	Rscript $^ ${CMPTH}

db: ${CONFDB} ${IDIR}/scenarios.csv

cleandb:
	rm ${CONFDB}

${DATAPTH}/fit_combined.qs: merge_fits.R ${FITS}
	${R}

${DATAPTH}/fitd_combined.qs: merge_fits.R ${DFITS}
	${R}

${ODIR}/sim_model.rds: sim_model_fit.R ${DATAPTH}/fitd_combined.qs sindh_data.csv | ${CMPTH}
	Rscript $^ $| $@

smodel: ${ODIR}/sim_model.rds

${ODIR}/%_ext.rds: compute.R ${DATASRC} ${CONFEXT} | ${CMPTH}
	Rscript $^ $* $| $@

${ODIR}/%.rds: compute.R ${DATASRC} ${CONFDB} | ${CMPTH}
	Rscript $^ $* $| $@

intconfig: $(patsubst %,${ODIR}/%.rds,$(shell seq 1 3072))
baseconfig: $(patsubst %,${ODIR}/%.rds,$(shell seq 3073 3080))
extendconfig: $(patsubst %,${ODIR}/%_ext.rds,$(shell seq 3081 4616))

${ODIR}/epi_quantile.rds: epi_quantile.R $(filter-out ${SUMMARIES}, $(wildcard ${ODIR}/*.rds)) | ${ODIR} ${CONFDB}
	Rscript $< $| $@

ECONDATA := covid_other_costs.csv covid_vac_costs_per_dose.csv daly_scenarios.csv

${ODIR}/econ_quantile.rds: econ_quantile.R ${ODIR}/epi_quantile.rds ${ECONDATA} ${CONFDB}
	${R}

testscn: $(patsubst %,${ODIR}/%.rds,0001 0002 0003 0004 0005 3076 3077 3078 3079 3080)

merge: ${ODIR}/all_metrics.sqlite

${IDIR}/scenarios.csv: ${CONFDB}
	sqlite3 -header -csv $< "SELECT * FROM scenario;" > $@

${ODIR}/diffs.rds: diffs.R ${CONFDB} $(wildcard ${METPAT}*.sqlite)
	Rscript $(wordlist 1,2,$^) ${METPAT} $@

${ODIR}/baseline.rds: baseline.R ${CONFDB} $(wildcard ${METPAT}*.sqlite)
	Rscript $(wordlist 1,2,$^) ${METPAT} $@

${ODIR}/quantiles.rds: quantiles.R ${ODIR}/diffs.rds
	${R}

${ODIR}/validation.rds: validation_set.R ${CONFDB} $(wildcard ${OTHPAT}*.sqlite)
	Rscript $(wordlist 1,2,$^) ${OTHPAT} $@

${ODIR}/dalys.rds: econ_summaries.R \
$(patsubst %,${IDIR}/%.csv,daly_scenarios covid_vac_cost_inputs covid_other_cost_inputs) \
${IDIR}/config_high.sqlite $(wildcard ${METPAT}*.sqlite)
	Rscript $(wordlist 1,5,$^) ${METPAT} ${ODIR}/dalys.rds ${ODIR}/icer.rds ${ODIR}/costs.rds

${ODIR}/costs.rds ${ODIR}/costs_averted.rds ${ODIR}/dalys_averted.rds: ${ODIR}/dalys.rds

econ: ${ODIR}/dalys.rds ${ODIR}/costs.rds

${IDIR}/scenarios.rds: scenarios.R ${CONFDB}
	${R}

digest: ${ODIR}/diffs.rds ${ODIR}/baseline.rds ${ODIR}/quantiles.rds ${IDIR}/scenarios.rds ${ODIR}/validation.rds

${FDIR}/incremental.png: fig_incremental.R ${IDIR}/scenarios.rds ${ODIR}/quantiles.rds ${ODIR}/baseline.rds
	${R}

${FDIR}/validation.png: fig_validation.R ${IDIR}/scenarios.rds ${ODIR}/validation.rds
	${R}

${FDIR}/icer.png: fig_icer.R ${IDIR}/config_high.sqlite ${ODIR}/icer.rds
	${R}

figs: ${FDIR}/incremental.png ${FDIR}/validation.png