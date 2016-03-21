library(MLlibrary)
library(dplyr)
library(foreign)
library(purrr)

BASE_NAMES <- c('W1_Anon_V5.3.dta', 'W2_Anon_V2.3.dta', 'W3_Anon_V1.3.dta')
# Integer codes: -9: Don't know, -8: Refuse
HOLDOUT_FRACTION <- 0.2
SEED <- 1
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
  'h_intrvsrt',
  'h_negdthfrin',
  'h_intrvend',
  'h_ownrnt',
  'h_refage',
  'h_mrkvoth',
  'h_intnon')
EXCLUDE_SUFFIX <- c(
  '_outcome',
  'op',
  'ss',
  'wd',
  'pay',
  'pot',
  'mrkv',
  '_v',
  'expnd',
  'tot',
  'spn',
  'gft',
  'pay',
  'prd',
  'mn',
  'yr',
  'cst',
  'inc',
  'val',
  'con',
  'num',
  'rev',
  'gv',
  'sll',
  'har',
  'own',
  'lss',
  'hm',
  'inv',
  'sel',
  'amt',
  'kg',
  'sl',
  'sf',
  '_o'
)

EXCLUDE_REGEX <- list(
  EXCLUDE_INC='h_tinc',
  EXCLUDE_UNITS_REGEX='ag.*u$',
  EXCLUDE_LNG_REGEX='intlng[^1]',
  EXCLUDE_INTERVIEW='intres',
  EXCLUDE_MORTALITY_REGEX='mrt.*\\d$',
  EXCLUDE_OWN_REGEX='own.*\\d$',
  EXCLUDE_POS_REGEX='pos.*v$',
  EXLUDE_REFUSE='_ref.*$'
)



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
  cons_colnames <- pwave('expenditure')
  base_name <- BASE_NAMES[wave_number]
  hhderived <- get_data('hhderived', base_name)
  hhderived <- rename_(hhderived, .dots=setNames(cons_colnames, TARGET_VARIABLE))
  hhid <- pwave('hhid')
  hhderived <- select(hhderived, one_of(TARGET_VARIABLE, hhid))
  
  hhquest <- get_data('HHQuestionnaire', base_name)
  
  df <- merge(hhderived, hhquest, by=hhid)
  df <- select(df, -one_of(hhid))
  df[, TARGET_VARIABLE] <- log(df[, TARGET_VARIABLE])
  
  
  exclude_admin <- sapply(EXCLUDE_ADMIN, pwave)
  df <- select(df, -one_of(exclude_admin))
  # workaround for dplyr bug with no matches
  for (suffix in EXCLUDE_SUFFIX) {
    if (ncol(select(df, ends_with(suffix))) > 0) {
      df <- select(df, -ends_with(suffix))
    }
  }
  for (regx in EXCLUDE_REGEX) {
    if (ncol(select(df, matches(regx))) > 0) {
      df <- select(df, -matches(regx))
    }
  }
  for (name in colnames(keep(df, is.integer))) {
    df[!is.na(df[, name]) & df[, name] < 0, name] <- NA
  }
  df
}

run_wave <- function(wave_number) {
  wave_name <- paste('south_africa_', 'w', wave_number, sep='')
  df <- create_dataset(wave_number)
  df <- set_aside_holdout(wave_name, df)
  df <- na_indicator(df)
  df <- standardize_predictors(df, TARGET_VARIABLE)
  save_dataset(wave_name, df)
  output <- test_all(df)
  save_validation_models_(wave_name, output)
}

print(1)
run_wave(1)
print(2)
run_wave(2)
print(3)
run_wave(3)
