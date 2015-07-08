all: tanzania ghana table

clean:
	rm results/*

table: results/scores.csv

results: 
	mkdir -p results

results/scores.csv: $(wildcard results/*.csv)
	Rscript analyses/join.R $^

ghana: results/ghana.html

results/ghana.html: data/ghana_cv_out.csv data/ghana.csv R/plot.R
	Rscript analyses/report.R ghana

data/ghana_cv_out.csv: analyses/ghana.R R/model.R
	Rscript analyses/ghana.R

data/ghana.csv: analyses/ghana.R R/model.R
	Rscript analyses/ghana.R

tanzania: results/tanzania.html

results/tanzania.html: data/tanzania_cv_out.csv data/tanzania.csv R/plot.R
	Rscript analyses/report.R tanzania

data/tanzania_cv_out.csv: analyses/tanzania.R R/model.R
	Rscript analyses/tanzania.R

data/tanzania.csv: analyses/tanzania.R R/model.R
	Rscript analyses/tanzania.R
