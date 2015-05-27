all: tanzania

tanzania: data/tanzania_cv_out.csv data/tanzania.csv
	R -e "rmarkdown::render('analyses/tanzania.Rmd')"

data/tanzania_cv_out.csv: analyses/tanzania.R R/model.R
	Rscript analyses/tanzania.R

data/tanzania.csv: analyses/tanzania.R R/model.R
	Rscript analyses/tanzania.R
