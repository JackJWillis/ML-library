library(MLlibrary)
library(haven)
library(dplyr)
library(purrr)

# YEARS

# 2008 kapita
# 2010 kapita
# exclude b5r26, 
# hhead == 1, b4k3 2008, hb 2010

JOIN_COLUMNS <- c("b1r1", "b1r2", "b1r3", "b1r4", "b1r5", "b1r7", "b1r8")

print_labels <- function(df) {
  map(df, ~attr(., 'label'))
}

make_path <- function(year, suffix) {
  fname <- paste('susenas', year, 'mar_', suffix, '.dta', sep='')
  paste(TARGETING_DATA_IN, 'indonesia', fname, sep='/')
}


make_df <- function(year, per_cap=TRUE) {
  expenditure_df <- read_dta(make_path(year, '43'))
  ind_char_df <- read_dta(make_path(year, 'ki'))
  hh_char_df <- read_dta(make_path(year, 'kr'))
  
  hhead_df <- process_char_ind(year, ind_char_df)
  
  expenditure_df <- select(expenditure_df, one_of(c(JOIN_COLUMNS, 'kapita', 'wert')))
  per_cap <- log(expenditure_df$kapita)
  attr(per_cap, 'label') <- NULL
  expenditure_df[, TARGET_VARIABLE] <- per_cap
  expenditure_df$kapita <- NULL
  
  hh_char_df <- select(hh_char_df, -c(weind, wert))
  
  df <- inner_join(hhead_df, hh_char_df, by=JOIN_COLUMNS)
  df <- inner_join(df, expenditure_df, by=JOIN_COLUMNS)
  df <- select(df, -one_of(JOIN_COLUMNS))
  df <- labels_to_colnames(df)
  
  labelleds <- map_lgl(df, ~class(.) == 'labelled')
  df[, labelleds] <- map(df[, labelleds], as_factor)
  numerics <- c(
    'Highest.Diploma.held.',
    'Highest.level.and.type.of.education.previously.or.being.attended.',
    'Highest.grade.previously.or.being.attended.'
  )
  df[, numerics] <- map(df[, numerics], as.numeric)
  df <- standardize_df(df)
  df
}


make_pmt_df <- function(year='10') {
  # Get per capita consumption
  expenditure_df <- read_dta(make_path(year, '43'))
  expenditure_df <- select(
    expenditure_df,
    one_of(c(JOIN_COLUMNS, 'kapita')),
    hhsize = b2r1)

  # Characteristics of household head
  ind_char_df <- read_dta(make_path(year, 'ki'))
  ind_char_df <- mutate(
    ind_char_df,
    hhmale = jk == 1,
    age = umur,
    hhage = age,
    hhmarried = kwn == 2,
    educ = b5r18,
    hhsd = educ %in% c(2, 3),
    hhsmp = educ %in% c(4, 5),
    hhsma = educ >= 6,
    hhage2 = hhage ^ 2,
    hhmalemarr = (hhmale & hhmarried),
    sector = b5r24,
    hhagr = sector == 1,
    hhind = sector %in% 2:6,
    hhserve = sector > 6,
    informal = 0 # ask Jack how to find this
  )
  hhead_df <- filter(ind_char_df, hb == 1)

  # Characteristics of household
  hh_char_df <- read_dta(make_path(year, 'kr'))
  hh_char_df <- mutate(
    urban = b1r5 == 1,
    hh_char_df,
    floorsize = b6r5,
    tfloor = b6r4 == 1,
    twall = b6r3 == 1,
    troof = b6r2 %in% c(1, 2),
    toilet = b6r9a == 1,
    dwater = b6r6a %in% c(1, 2, 3, 4, 6),
    lighting = b6r10 == 1,
    house = b6r1 == 1,
    micro = b7r3a == 1,
    asset_sepeda = b7r4a == 1,
    asset_kulkas = b7r4b == 1,
    asset_tbggas = b7r4c == 1,
    asset_motor = b7r4d == 1,
    asset_mobilboat = b7r4e == 1)




  # Other household members
  ind_char_df <- mutate(
    ind_char_df,
    is_child = 4 <= age & age <= 18)
  hh_mem_df <- group_by(ind_char_df, b1r1, b1r2, b1r3, b1r4, b1r5, b1r7, b1r8) %>%
    summarize(
      nage04 = sum(age < 4),
      hhmaxedsd = max(educ) %in% c(2, 3),
      hhmaxedsmp = max(educ) %in% c(4, 5),
      hhmaxedsma = max(educ) >= 6,
      depratio = (sum(age < 14) + sum(age > 64)) / sum(14 < age  & age < 65),
      eschild = sum(educ  %in% c(2, 3) & is_child),
      smpchild = sum(educ %in% c(4, 5) & is_child),
      sschild = sum(educ >= 6 & is_child))
  merged <- inner_join(hhead_df, hh_char_df, by=JOIN_COLUMNS)
  merged <- inner_join(merged, hh_mem_df, by=JOIN_COLUMNS)
  merged <- inner_join(merged, expenditure_df, by=JOIN_COLUMNS)
  
  merged <-mutate(
    merged,
    hhsize2 = hhsize ^2,
    pcfloor = floorsize / hhsize)
  
#   podes <- read_dta('inst/extdata/indonesia/olken/coding_podes08.dta')
#   podes$hhea <- as.integer(podes$hhea)
#   merged <- inner_join(merged, podes, by=c('b1r4'='hhea'))
  
#   podes_variables <- c('bidan_pds',
#     'credit_pds',
#     'doctor_pds',
#     'polindes_pds',
#     'popdensity_pds',
#     'posyandu_pds',
#     'puskesmas_pds',
#     'road_pds',
#     'sd_pds',
#     'smp_pds',
#     'market_pds')
  
  merged2 <- select(
    merged,
    urban,
    depratio_ssn = depratio,
#     bidan_pds,
#     credit_pds,
    # distance_pds,
#     doctor_pds,
#     polindes_pds,
#     popdensity_pds,
#     posyandu_pds,
#     puskesmas_pds,
#     road_pds,
#     sd_pds,
#     smp_pds,
#     market_pds,
    dwater_ssn = dwater,
    hhage2_ssn = hhage2,
    hhage_ssn = hhage,
    hhagr_ssn = hhagr,
    hhind_ssn = hhind,
    hhinformal_ssn = informal,
    hhmale_ssn = hhmale,
    hhmarried_ssn = hhmarried,
    hhmalemarr_ssn = hhmalemarr,
    hhmaxedsd_ssn = hhmaxedsd,
    hhmaxedsma_ssn = hhmaxedsma,
    hhmaxedsmp_ssn = hhmaxedsmp,
    hhsd_ssn = hhsd,
    hhserv_ssn = hhserve,
    hhsize2_ssn = hhsize2,
    hhsize_ssn = hhsize,
    hhsma_ssn = hhsma,
    hhsmp_ssn = hhsmp,
    house_ssn = house,
    lighting_ssn = lighting,
    micro_ssn = micro,
    nage04_ssn = nage04,
    nchildsd_ssn = eschild,
    nchildsma_ssn = smpchild,
    nchildsmp_ssn = sschild,
    pcfloor_ssn = pcfloor,
    tfloor_ssn = tfloor,
    toilet_ssn = toilet,
    troof_ssn = troof,
    twall_ssn = twall,
    asset_sepeda_ssn = asset_sepeda,
    asset_kulkas_ssn = asset_kulkas,
    asset_motor_ssn = asset_motor,
    asset_mobilboat_ssn = asset_mobilboat,
    asset_tbggas_ssn = asset_tbggas,
    kapita
  )
  merged2
}

get_pmt_train_test <- function(year='10') {
  train <- make_pmt_df(year)
  train <- rename(train, yyyyy=kapita) %>%
    mutate(yyyyy = log(yyyyy))
  logicals <- map_lgl(train, is.logical)
  train[, logicals] <- map(train[, logicals], as.numeric)
  train <- filter(train, !is.infinite(depratio_ssn))
  
  test <- readstata13::read.dta13('inst/extdata/indonesia/olken/ST_baseline_matched_plus_features.dta')
  logicals <- map_lgl(test, is.logical)
  test[, logicals] <- map(test[, logicals], as.numeric)
  test$yyyyy <- test$logconsumption
  test <- filter(test, !is.na(logconsumption))
  df <- rbind(train, test[, colnames(train)]) %>% standardize_df()
  train <- df[1:nrow(train), ]
  test2 <- df[-(1:nrow(train)), ]
  test2$povertyline1 <- test$povertyline1
  test2$selftargeting <- test$selftargeting
  list(train=train, test=test2, fold_number=1)
}

get_pmt_score <- function(fold) {
  fold$test$PMTSCORE
}

test_on_fold <- function(name, fold, method_list) {
  named_method_list <- lapply(method_list, function(method) named_method(name, method))
  method_results <- lapply(
    names(named_method_list), 
    function(method_name) {
      print(method_name)
      method <- named_method_list[method_name]
      res_df <- test_method_on_splits(method, list(fold))[[1]]
      res_df$pline <- 0.8 * fold$test$povertyline1
      res_df$selftargeting <- fold$test$selftargeting
      res_df
    }
  )
  bind_rows(method_results)
}

print_reach <- function(output) {
  rpt <- reach_by_pct_targeted(output, c(.05, .1, .2))
  rpt$tp <- rpt$true_poor
  rpt <- mutate(rpt, value=cumsum(tp) / n())
  print(table_stat(rpt))
}


standardize_df <- function(df) {
  df <- as.data.frame(df)
  df <- na_indicator(df)
  df
}

labels_to_colnames <- function(df) {
  label_names <- map(df, ~attributes(.)$label) %>%
    map(~if (is.null(.)) . else iconv(., "latin1", "ASCII", sub=""))
  colnames(df)[!map_lgl(label_names, is.null)] <- make.names(discard(label_names, is.null))
  df
}


process_char_ind <- function(year, ind_df) {
  if (year == '08') {
    hhead_df <- filter(ind_df, b4k3 == 1) %>%
      select(-c(b4k1, b4k3, wert, weind)) %>%
      select(-c(b5r14b, b5r14a)) %>% # "Year/Month drop out of school"
      select(-b5r26) # "How much net income (money and in-kind) do you usually earn a month"
    # Some questions apply only to children
    # Since we only look at household head these do not apply
    hhead_df <- discard(hhead_df, ~all(is.na(.))) 
  }
  if (year == '10') {
    hhead_df <- filter(ind_df, hb == 'Head of household')
  }
  hhead_df
}

name <- 'indonesia'
#clear_config(name)
fold <- get_pmt_train_test('10')
save_dataset(name, fold$train)
output <- test_on_fold(name, fold, METHOD_LIST)
save_validation_models_(name, output)
