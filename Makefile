all: tanzania

tanzania: analyses/tanzania.html

analyses/tanzania.html: data/tanzania_cv_out.csv data/tanzania.csv
	Rscript analyses/report.R tanzania

data/tanzania_cv_out.csv: analyses/tanzania.R R/model.R
	Rscript analyses/tanzania.R

data/tanzania.csv: analyses/tanzania.R R/model.R
	Rscript analyses/tanzania.R
