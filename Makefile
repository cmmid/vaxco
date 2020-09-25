
-include local.makefile

R = Rscript $^ $@

DB ?= results.sqlite

CMPTH ?= ../covidm

DATAPTH ?= .
DATASRC := $(addprefix ${DATAPTH}/,fit_sindh.qs epi_data.csv mob_data.csv)

# TODO add params.json
${DB}: build_db.R
	${R}

setup: setup.R fit_sindh.qs | ${CMPTH}
	Rscript $^ $|

db: ${DB}

# this should be set from command line, e.g. `make run ARGID=1`
# corresponds to setting the scenario
SCNID ?= 1

run: compute.R ${DATASRC} ${DB} | ${CMPTH}
	Rscript $^ ${SCNID} $|