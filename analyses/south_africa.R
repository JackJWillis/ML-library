library(dplyr)
library(foreign)

base_name <- 'W1_Anon_V5.3.dta'
get_data <- function(prefix) {
  fname <- paste(prefix, base_name, sep='_')
  fpath <- paste(TARGETING_DATA_IN, 'south_africa', fname, sep='/')
  read.dta(fpath)
}

admin <- get_data('Admin')
adult <- get_data('Adult')
child <- get_data('Child')
hhderived <- get_data('hhderived')
hhquest <- get_data('HHQuestionnaire')
household_roster <- get_data('HouseholdRoster')
indderived <- get_data('indderived')
proxy <- get_data('Proxy')

expenditures <- select(hhderived, w1_hhid, w1_expenditure)

exclude_quest <- c(
  'w1_h_pcode',
  'w1_h_pcode_pid',
  'w1_h_phase',
  'w1_h_preflng',
  'w1_h_intvr_c',
  'w1_h_intvr_imp',
  'w1_h_intrv_d',
  'w1_h_intrv_m',
  'w1_h_intrv_y',
  'w1_h_intrvsrt')
hhquest <- select(hhquest, -one_of(exclude_quest))
df <- merge(expenditures, hhquest, by='w1_hhid')
df <- rename(df, yyyyy=w1_expenditure)

quick_test <- function(survey_df, method=ols) {
  survey_df <- discard(survey_df, ~sum(is.na(.x)) / nrow(survey_df) > .7)
  survey_df <- na_indicator(survey_df)
  holdout_fraction <- 0.1
  assignments <- as.logical(rbinom(nrow(survey_df), 1, 1 - holdout_fraction))
  train <- survey_df[assignments, ]
  test <- survey_df[!assignments, ]
  train <- keep(train, ~length(unique(.x)) > 1)
  fold <- list(train=train, test=test)
  method(fold)
}