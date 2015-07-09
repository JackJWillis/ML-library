all: tanzania ghana table

clean:
	rm results/*

table: results/scores.csv

results: 
	mkdir -p results

results/scores.csv: $(wildcard results/*.csv)
	Rscript analyses/join.R $^

### Ghana ###
ghana: results/ghana.html

results/ghana.html: data/ghana_cv_out.csv data/ghana.csv R/plot.R
	Rscript analyses/report.R ghana

data/ghana_cv_out.csv: analyses/ghana.R R/model.R
	Rscript analyses/ghana2.R

### Tanzania ###
tanzania: results/tanzania.html

results/tanzania.html: data/tanzania_cv_out.csv data/tanzania.csv R/plot.R
	Rscript analyses/report.R tanzania

data/tanzania_cv_out.csv: analyses/tanzania.R R/model.R
	Rscript analyses/tanzania.R

### Mexico ###
mexico: results/mexico.html

results/mexico.html: data/mexico_cv_out.csv data/mexico.csv R/plot.R
	Rscript analyses/report.R mexico

data/mexico_cv_out.csv: analyses/mexico.R R/model.R
	Rscript analyses/mexico.R

### Niger ###
niger: results/niger_agricultural.html niger_pastoral.html

results/niger_%.html: data/niger_%_cv_out.csv R/plot.R
	Rscript analyses/report.R $@

data/niger_pastoral_cv_out.csv: analyses/niger.R R/model.R
	Rscript analyses/niger.R

data/niger_agricultural_cv_out.csv: analyses/niger.R R/model.R
	Rscript analyses/niger.R
