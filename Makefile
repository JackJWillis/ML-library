all: tanzania ghana table

clean:
	rm results/*

table: reach mse l1

results: 
	mkdir -p results

reach: $(wildcard results/*_reach.csv)
	Rscript analyses/join.R $^

mse: $(wildcard results/*_mse.csv)
	Rscript analyses/join.R $^

l1: $(wildcard results/*_l1.csv)
	Rscript analyses/join.R $^

### Ghana ###
ghana: results/ghana.html results/ghana_pe.html

results/ghana_pe.html: ddata/ghana_pe_cv_out.csv data/ghana.csv R/plot.R
	Rscript analyses/report.R ghana_pe

results/ghana.html: data/ghana_cv_out.csv data/ghana.csv R/plot.R
	Rscript analyses/report.R ghana

data/ghana_pe_cv_out.csv: analyses/ghana2.R/model.R
	Rscript analyses/ghana2.R

data/ghana_cv_out.csv: analyses/ghana2.R R/model.R
	Rscript analyses/ghana2.R

### Mexico ###
mexico: results/mexico.html

results/mexico.html: data/mexico_cv_out.csv data/mexico.csv R/plot.R
	Rscript analyses/report.R mexico

data/mexico_cv_out.csv: analyses/mexico.R R/model.R
	Rscript analyses/mexico.R

### Niger ###
niger: results/niger_agricultural.html results/niger_pastoral.html

results/niger_pastoral.html: data/niger_pastoral_cv_out.csv R/plot.R
	Rscript analyses/report.R niger_pastoral

results/niger_agricultural.html: data/niger_agricultural_cv_out.csv R/plot.R
	Rscript analyses/report.R niger_agricultural

data/niger_pastoral_cv_out.csv: analyses/niger.R R/model.R
	Rscript analyses/niger.R

data/niger_agricultural_cv_out.csv: analyses/niger.R R/model.R
	Rscript analyses/niger.R

### Tanzania ###
tanzania: results/tanzania.html

results/tanzania.html: data/tanzania_cv_out.csv data/tanzania.csv R/plot.R
	Rscript analyses/report.R tanzania

data/tanzania_cv_out.csv: analyses/tanzania.R R/model.R
	Rscript analyses/tanzania.R


### TANZANIA PANEL ###
tanzania_panel: results/tanzania_panel.html results/tanzania_panel_split.html

results/tanzania_panel.html: data/tanzania_panel_cv_out.csv data/tanzania_panel.csv R/plot.R
	Rscript analyses/report.R tanzania_panel

data/tanzania_panel_cv_out.csv: analyses/tanzania_panel.R R/model.R
	Rscript analyses/tanzania_panel.R

results/tanzania_panel_split.html: data/tanzania_panel_split_cv_out.csv data/tanzania_panel_split.csv R/plot.R
	Rscript analyses/report.R tanzania_panel_split

data/tanzania_panel_split_cv_out.csv: analyses/tanzania_panel.R R/model.R
	Rscript analyses/tanzania_panel.R
