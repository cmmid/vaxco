
-include local.makefile

DRPBXPTH ?= ~/Dropbox/Covid-WHO-vax
IDIR = ${DRPBXPTH}/inputs
ODIR = ${DRPBXPTH}/outputs

R = Rscript $^ $@
Rpipe = Rscript $^ $| $@
Rstarp = Rscript $^ $* $| $@

CONFDB ?= ${IDIR}/config.sqlite
DBPAT ?= ${ODIR}/metrics_%.sqlite
ODBPAT ?= ${ODIR}/other_%.sqlite

CMPTH ?= ../covidm

DATAPTH ?= .
DATASRC := $(addprefix ${DATAPTH}/,fit_sindh.qs epi_data.csv mob_data.csv)

# TODO add params.json
${CONFDB}: build_db.R fit_sindh.qs | ${CMPTH} ${IDIR}
	${Rpipe}

setup: setup.R fit_sindh.qs | ${CMPTH}
	Rscript $^ $|

db: ${CONFDB}

tests: $(patsubst %,${DBPAT},01 02 03 04 05)

# this should be set from command line, e.g. `make run SCNID=01`
# corresponds to setting the scenario
SCNID ?= 01

${DBPAT}: compute.R ${DATASRC} ${CONFDB} | ${CMPTH}
	${Rstarp}

${ODBPAT}: ${DBPAT}

scenarios.csv: ${CONFDB}
	sqlite3 -header -csv $< "SELECT * FROM scenario;" > $@