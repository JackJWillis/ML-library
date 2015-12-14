all: preval val

val: south_africa iraq

preval: tanzania ghana niger mexico 

### Ghana ###
ghana: data/ghana_pe_validation_cv_out.csv data/ghana_validation_cv_out.csv

data/ghana_pe_validation_cv_out.csv: analyses/ghana2.R R/validation.R
	Rscript analyses/ghana2.R

data/ghana_validation_cv_out.csv: analyses/ghana2.R R/validation.R
	Rscript analyses/ghana2.R

### Mexico ###
mexico: data/mexico_validation_cv_out.csv

data/mexico_validation_cv_out.csv: analyses/mexico.R R/validation.R
	Rscript analyses/mexico.R

### Niger ###
niger: data/niger_pastoral_validation_cv_out.csv data/niger_agricultural_validation_cv_out.csv

data/niger_pastoral_validation_cv_out.csv: analyses/niger.R R/validation.R
	Rscript analyses/niger.R

data/niger_agricultural_validation_cv_out.csv: analyses/niger.R R/validation.R
	Rscript analyses/niger.R

### Tanzania ###
tanzania: data/tanzania_validation_cv_out.csv

data/tanzania_validation_cv_out.csv: analyses/tanzania.R R/validation.R
	Rscript analyses/tanzania.R


### South Africa ###
south_africa: data/south_africa_validation_cv_out.csv

data/south_africa_validation_cv_out.csv: analyses/south_africa.R R/validation.R
	Rscript analyses/south_africa.R

### Iraq ###

iraq: data/iraq_validation_cv_out.csv

data/iraq_validation_cv_out.csv: analyses/iraq.R R/validation.R
	Rscript analyses/iraq.R

### Brazil  ###

brazil: data/brazil_validation_cv_out.csv

data/brazil_validation_cv_out.csv: analyses/brazil.R R/validation.R
	Rscript analyses/brazil.R

