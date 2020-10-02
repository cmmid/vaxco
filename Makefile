
-include local.makefile

DRPBXPTH ?= ~/Dropbox/Covid-WHO-vax
IDIR = ${DRPBXPTH}/inputs
ODIR = ${DRPBXPTH}/outputs
FDIR = ${DRPBXPTH}/figures

R = Rscript $^ $@
Rpipe = Rscript $^ $| $@
Rstarp = Rscript $^ $* $| $@

CONFDB ?= ${IDIR}/config.sqlite
DBPAT ?= ${ODIR}/metrics_%.sqlite
ODBPAT ?= ${ODIR}/other_%.sqlite

CMPTH ?= ../covidm

DATAPTH ?= .
DATASRC := $(addprefix ${DATAPTH}/,fit_sindh_lower_R0.qs epi_data.csv mob_data.csv)

# TODO add params.json
${CONFDB}: build_db.R fit_sindh_lower_R0.qs | ${CMPTH} ${IDIR}
	${Rpipe}

setup: setup.R fit_sindh_lower_R0.qs | ${CMPTH}
	Rscript $^ $|

db: ${CONFDB} ${IDIR}/scenarios.csv

tests: $(patsubst %,${DBPAT},$(shell seq -f%02g 1 65))

# this should be set from command line, e.g. `make run SCNID=01`
# corresponds to setting the scenario
SCNID ?= 01

${DBPAT}: compute.R ${DATASRC} ${CONFDB} | ${CMPTH}
	${Rstarp}

${ODBPAT}: ${DBPAT}

# TODO make this work
${ODIR}/all_metrics.sqlite: $(wildcard ${ODIR}/metrics_*.sqlite)
	cd ${ODIR} && for f in ${ODIR}/metrics_*.sqlite; do echo $$f; done

#	for f in ${ODIR}/metrics_*.sqlite; do sqlite3 .dump $$f | sqlite3 $@; done

merge: ${ODIR}/all_metrics.sqlite

${IDIR}/scenarios.csv: ${CONFDB}
	sqlite3 -header -csv $< "SELECT * FROM scenario;" > $@

METPAT := ${ODIR}/metrics_
OTHPAT := ${ODIR}/other_

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