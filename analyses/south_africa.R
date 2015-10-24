library(dplyr)
library(foreign)

EXCLUDE_ADMIN <- c(
  'h_pcode',
  'h_pcode_pid',
  'h_phase',
  'h_preflng',
  'h_intvr_c',
  'h_intvr_imp',
  'h_intrv_d',
  'h_intrv_m',
  'h_intrv_y',
  'h_intrvsrt')
EXCLUDE_FOOD_CONS <- 'h_fd'
EXCLUDE_NFOOD_CONS <-  'h_nf'

get_data <- function(prefix, base_name) {
  fname <- paste(prefix, base_name, sep='_')
  fpath <- paste(TARGETING_DATA_IN, 'south_africa', fname, sep='/')
  read.dta(fpath)
}

p_wave_column <- function(wave_number, colname) {
  paste('w', wave_number, '_', colname, sep='')
}

create_dataset <- function(wave_number) {
  pwave <- function(colname) p_wave_column(wave_number, colname)
  base_name <- paste('W', wave_number, '_Anon_V5.3.dta', sep='')
  hhderived <- get_data('hhderived')
  hhquest <- get_data('HHQuestionnaire')
  
  hhid <- pwave('hhid')
  df <- merge(expenditures, hhquest, by=pwave)
  
  cons_colnames <- pwave('expenditure')
  df <- rename_(df, .dots=setNAMES(cons_colnames, TARGET_VARIABLE))
  df <- select(df, -one_of(hhid))
  
  exclude_admin <- sapply(EXCLUDE_ADMIN, pwave)
  df <- select(df, -one_of(exclude_admin))
  
  exclude_food_cons <- pwave(EXCLUDE_FOOD_CONS)
  exclude_nfood_cons <- pwave(EXCLUDE_FOOD_CONS)
  df <- select(df, -contains(exclude_food_cons))
  df <- select(df, -contains(exclude_nfood_cons))
  df
}

run_wave <- function(wave_number) {
  wave_name <- paste('south_africa_', 'w', wave_number, sep='')
  df <- create_dataset(wave_number)
  df <- na_indicator(df)
  df <- standardize_predictors(df, TARGET_VARIABLE)
  output <- test_all(df)
  save_validation_models_(year_name, output)
}

print(1)
run_wave(1)
print(2)
run_wave(2)
print(3)
run_wave(3)