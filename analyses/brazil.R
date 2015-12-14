library(MLlibrary)
library(dplyr)
library(readstata13)

NAME <- 'brazil'

exclude <- c(
  'tipo_reg',
  'cod_uf',
  'num_seq',
  'num_dv',
  'cod_domc',
  'perd_cod_p_visit_realm_em',
  'renda_bruta_monetaria',
  'renda_bruta_nao_monetaria',
  'id_dom',
  'vvp',
  'urbano',
  'cod_tempo_moradia'
)
exclude_regex <- list(
  factor_expansion='fator_expansao\\d$',
  expenditure_credit='^crd.\\d+$',
  nonmonetary_expenditure='^nmd.\\d+$',
  income='^vare\\d+$',
  income2='^vre\\d+$'
)

expenditure_regex <- '^vad.\\d+$'

fpath <- paste(TARGETING_DATA_IN, 'pof2008_dom_standard.dta', sep='/')
bz <- read.dta13(fpath)

df <- select(bz, -one_of(exclude))
for (regx in exclude_regex) {
  df <- select(df, -matches(regx))
}
expenditures <- select(df, matches(expenditure_regex))
expenditure <- log(rowSums(expenditures, na.rm=TRUE))
df[, TARGET_VARIABLE] <- expenditure
df <- select(df, -matches(expenditure_regex))

df <- set_aside_holdout(NAME, df)
df <- na_indicator(df)
df <- standardize_predictors(df)
save_dataset(NAME, df)
output <- test_all(df)
save_validation_models_(NAME, output)