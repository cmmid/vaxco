
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

CONFDB ?= ${IDIR}/config.sqlite
DBPAT := ${METPAT}%.sqlite
ODBPAT := ${OTHPAT}%.sqlite

CMPTH ?= ../covidm-vaxco
CMURL := git@github.com:nicholasdavies/covidm.git

${CMPATH}:
	cd $(dir ${CMPATH}); git clone ${CMURL} $(notdir ${CMPATH})

DATAPTH ?= .
FITS := fit_sindh.qs $(shell cd ${DATAPTH}; ls fit_sindh_waning_*.qs)
DATASRC := $(addprefix ${DATAPTH}/,fit_combined.qs epi_data.csv mob_data.csv)

# TODO add params.json
${CONFDB}: build_db.R $(firstword ${FITS}) | ${CMPTH} ${IDIR}
	${Rpipe}

setup: setup.R $(firstword ${FITS}) | ${IDIR} ${ODIR} ${FDIR}
	Rscript $^ ${CMPTH}

db: ${CONFDB} ${IDIR}/scenarios.csv

cleandb:
	rm ${CONFDB}

${DATAPTH}/fit_combined.qs: merge_fits.R ${FITS}
	${R}

${ODIR}/%.rds: compute.R ${DATASRC} ${CONFDB} | ${CMPTH}
	Rscript $^ $* $| $@

SUMMARIES := ${ODIR}/interventions.rds ${ODIR}/baseline.rds

$(word 1,${SUMMARIES}): rollup.R $(filter-out ${SUMMARIES}, $(wildcard ${ODIR}/*.rds)) | ${ODIR} ${CONFDB}
	Rscript $< $| $@

$(word 2,${SUMMARIES}): $(word 1,${SUMMARIES})

testscn: ${ODIR}/001.rds

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