library(MLlibrary)
library(dplyr)
library(readstata13)

NAME <- 'brazil_tuned'

exclude <- c(
  'tipo_reg',
  'cod_uf',
  'num_seq',
  'num_dv',
  'cod_domc',
  'perd_cod_p_visit_realm_em',
  'renda_bruta_monetaria',
  'renda_bruta_nao_monetaria',
  'renda_total',
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
cnames <- colnames(df)
categoricals <- cnames[!grepl('qtd', cnames) & !grepl('num', cnames) & TARGET_VARIABLE != cnames]
df[, categoricals] <- lapply(df[, categoricals], factor)

df <- set_aside_holdout(NAME, df)
df <- na_indicator(df)
df <- standardize_predictors(df)

save_dataset(NAME, df)
clear_config(NAME)
output <- test_all_named(NAME, df, test_fraction=0.2)
save_validation_models_(NAME, output)

NAME <- 'brazil_sendhil_all'
# TEST_FRACTION <- 0.2
# # save_dataset(NAME, df)
# output2 <- test_all(df, method_list=SENDHIL_METHODS, test_fraction=TEST_FRACTION, seed=100)
# save_validation_models_(NAME, output)
