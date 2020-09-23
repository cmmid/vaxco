
-include local.makefile

R = Rscript $^ $@

DB ?= results.sqlite

# TODO add params.json
${DB}: build_db.R
	${R}

CMPTH ?= ../covidm

DATAPTH ?= .
DATASRC := $(addprefix ${DATAPTH}/,fit_sindh.qs epi_data.csv mob_data.csv)

# this should be set from command line, e.g. `make run ARGID=1`
ARGID ?= 1

run: run.R ${DATASRC} ${DB} | ${CMPTH}
	Rscript $^ ${ARGID} $|