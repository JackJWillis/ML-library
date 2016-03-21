library(MLlibrary)
library(dplyr)
library(purrr)
library(foreign)
library(readstata13)
library(ggplot2)
library(rpart)
library(RColorBrewer)
library(reshape2)
library(rpart.plot)
library(gridExtra)
library(xlsx)
library(scales)
library(kknn)


# folder <- 'sendhil_class'
# in_f <- function(path) paste(folder, path, sep='/')

folder <- 'sendhil_class'
in_f <- function(path) paste(folder, 'model_menu', path, sep='/')

summarize_output <- function(output, threshold) {
  random <- filter(output, method=='ols')
  random$method <- 'random'
  random$predicted <- rnorm(nrow(random))
  best <- filter(output, method=='ols')
  best$method <- 'best'
  best$predicted <- best$true
  x <- rbind_all(list(output, random, best))
  g <- group_by(x, method)
  g <- mutate(g, cons_threshold_t=quantile(true, threshold)) %>%
    mutate(true_poor=true <= cons_threshold_t, threshold=threshold) %>%
    arrange(predicted) %>%
    mutate(targeted=row_number() / n() <= threshold)
  df <- summarize(g, 
    reach=sum(targeted * (true_poor)) / sum(true_poor),
    avg_1_c=sum(targeted * 1/exp(true)) / sum(targeted)
  )
  ols_reach <- filter(df, method=='ols')$reach
  rf_reach <- filter(df, method=='tuned_forest')$reach
  to_reach <- mutate(g, reach=cumsum(true_poor) / sum(true_poor)) %>%
    summarize(to_ols_reach=sum(reach <= ols_reach) / n(), to_rf_reach=sum(reach <= rf_reach) / n())
  percentile <- ecdf(output$true)
  df <- merge(df, to_reach, by='method')
  df <- select(df, method=method, reach=reach, to_ols_reach=to_ols_reach, to_rf_reach=to_rf_reach, avg_1_c=avg_1_c)
  best_1_c <- filter(df, method=='best')$avg_1_c
  random_1_c <- filter(df, method=='random')$avg_1_c
  df$avg_1_c_normalized <- (df$avg_1_c - random_1_c) / (best_1_c - random_1_c)
  df
  # df <- filter(df, method %in% c('best', 'random', 'ols', 'tuned_rf'))
  # df[match(c('ols', 'tuned_rf', 'random', 'best'), df$method), ]
}


# mutate(g, reach=cumsum(true_poor) / sum(true_poor)) %>%
#   summarize(sum(reach < .4186))
# 
# res <- rbind_all(map(c(.05, .1, .2), ~summarize_output(output, .x)))

write_all <- function(output) {
  thresholds <- c(.05, .1, .2)
  for (thresh in thresholds) {
    s <- summarize_output(output, thresh)
    write.csv(s, paste('results_', thresh*100, '.csv', sep=''))
  }
}


dataset_stats <- function(df, bz=NULL) {
  stats_df <- data.frame(
    n=nrow(df),
    k=ncol(df),
    mean_lconsumption_bzreal=mean(df$log_consumption),
    median_lconsumption_bzreal=median(df$log_consumption),
    median_yearly_consumption_usd=median(exp(df$log_consumption)) * .26,
    pct_below_1.90usd=6,
    pct_below_3.10usd=14
  )
  if (!is.null(bz)) {
    stats_df$median_monthly_income_usd <- median(bz$renda_total) * .26
  }
  write.csv(stats_df, in_f('stats.csv'))
  p <- ggplot(df, aes(x=exp(log_consumption) * .26)) +
    geom_line(stat='density', size=1.5) +
    scale_x_log10() +
    xlab('yearly consumption (usd)')
  ggsave(in_f('consumption_log.pdf'), plot=p)
  thresholds <- c(.05, .1, .25, .5)
  min_thresh <- quantile(exp(df$log_consumption) * .26, .05)
  p3 <- p
  for (thresh in thresholds) {
    cons_thresh <- quantile(exp(df$log_consumption) * .26, thresh)
    p2 <- p +
      geom_vline(xintercept = cons_thresh) +
      annotate('label', x=min_thresh - 400, y=.85, label=paste('$', formatC(cons_thresh, format='d', big.mark=','), sep=''), size=8)
    p3 <- p3 + geom_vline(xintercept = cons_thresh)
    ggsave(in_f(paste('consumption_log_', thresh, '.pdf', sep='')), plot=p2)
  }
  ggsave(in_f(paste('consumption_log_all.pdf')), plot=p3)
  
  p <- ggplot(df, aes(x=exp(log_consumption) * .26)) +
    geom_histogram(bins=100) +
    xlim(c(0, 6e4)) +
    xlab('yearly consumption (usd)')
  ggsave(in_f('consumption.pdf'), plot=p)
  
  cut_df <- select(
    df,
    number.of.residents=qtd_morador_domc,
    sewage.type=cod_esgoto_sanit,
    number.of.rooms=qtd_comodos_domc,
    floor.material=cod_material_piso,
    log_consumption=log_consumption)
  levels(cut_df$floor.material) <- c('1'='carpet', '2'='ceramic', '3'='milled wood', '4'='cement', '5'='rough wood', '6'='earth', '7'='other')
  cut_df$floor.material <- as.character(cut_df$floor.material)
  levels(cut_df$sewage.type) <- c('1'='sewer', '2'='septic', '3'='pit (rudimentary)', '4'='ditch', '5'='river', '6'='other', '7'='none')
  cut_df$sewage.type <- as.character(cut_df$sewage.type)
  p.res <- ggplot(cut_df, aes(x=number.of.residents)) + geom_bar()
  p.sew <- ggplot(cut_df, aes(x=sewage.type)) + geom_bar() + ylab('')
  p.room <- ggplot(cut_df, aes(x=number.of.rooms)) + geom_bar() + ylab('')
  p.floor <- ggplot(cut_df, aes(x=floor.material)) + geom_bar()
  pdf(in_f('other_variable_stats.pdf'), width=12, height=10)
  grid.arrange(p.res, p.sew, p.floor, p.room, ncol=2)
  dev.off()
  
  g <- group_by(cut_df, number.of.rooms) %>%
    summarize(log_consumption=mean(log_consumption))
  p <- ggplot(cut_df, aes(factor(number.of.rooms), log_consumption)) +
    geom_boxplot() +
    ylab('consumption (log)')
    xlab('number of rooms')
  ggsave(in_f('rooms_v_consumption_boxplot.pdf'), plot=p)
}


save_tree <- function(model, fname) {
  pdf(in_f(fname))
  prp(model, varlen=0)
  dev.off()
}

little_trees <- function(df) {
  df <- rename(
    df,
    number.of.bathrooms=qtd_banheiros,
    floor.type=cod_material_piso,
    number.of.residents=qtd_comodos_domc,
    number.of.rooms=qtd_morador_domc)
  model <- rpart(log_consumption ~ ., df, maxdepth=1)
  save_tree(model, 'tree_1.pdf')
  model <- rpart(log_consumption ~ ., df, maxdepth=2)
  save_tree(model, 'tree_2.pdf')
  model <- rpart(log_consumption ~ ., df, maxdepth=3)
  save_tree(model, 'tree_3.pdf')
}

plot_partition <- function(new_df, colors, values=NULL) {
  p <- ggplot(new_df, aes(y=qtd_comodos_domc, x=qtd_morador_domc, color=predicted_log_consumption)) + 
    geom_point() +
    geom_jitter() +
    xlab('number of residents') +
    ylab('number of rooms')
  if (is.null(values)) {
    p <- p + scale_color_manual(values=colors)
  }
  else {
    p <- p + scale_color_gradientn(colors=colors, values=vals)
  }
  p
}

partition_graph2 <- function(df) {
  new_df <- df[, c('qtd_comodos_domc', 'qtd_morador_domc', 'log_consumption')]
  test_id <- sample(1:nrow(new_df), 10000)
  new_df <- new_df[test_id, ]
  tree_model <- rpart(log_consumption ~ qtd_comodos_domc + qtd_morador_domc, df)
  new_df$predicted_log_consumption <- as.factor(predict(tree_model, new_df))
  levs <- as.numeric(levels(new_df$predicted_log_consumption))
  colors <- brewer.pal(length(levs) + 2, "RdYlBu")
  p <- plot_partition(new_df, colors[2:(length(colors)-1)])
  ggsave(in_f('partition_plots/tree.pdf'), plot=p)
  
  
  count <- 100
  qtd_comodos_domc <- seq(min(new_df$qtd_comodos_domc), max(new_df$qtd_comodos_domc), length.out=count)
  qtd_morador_domc <- seq(min(new_df$qtd_morador_domc), max(new_df$qtd_morador_domc), length.out=count)
  new_df2 <- expand.grid(qtd_comodos_domc=qtd_comodos_domc, qtd_morador_domc=qtd_morador_domc)
  new_df2$predicted_log_consumption <- as.factor(predict(tree_model, new_df2))
  p <- plot_partition(new_df2, colors[2:(length(colors)-1)] )
  ggsave(in_f('partition_plots/tree_filled.pdf'), plot=p)
  
  min_cons <- min(new_df$log_consumption)
  max_cons <- max(new_df$log_consumption)
  vals <- rescale(c(min_cons, levs, max_cons))
  
  p <- ggplot(new_df, aes(y=qtd_comodos_domc, x=qtd_morador_domc, color=log_consumption)) + 
    geom_point() +
    geom_jitter() +
    xlab('number of residents') +
    ylab('number of rooms') +
    scale_color_gradientn(colors=colors, values=vals)
  ggsave(in_f('partition_plots/true.pdf'), p)
  
  m <- lm(log_consumption ~ qtd_comodos_domc + qtd_morador_domc, df)
  eq <- substitute(log_consumption == a + b %.% rooms + c %.% residents,
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        c = format(coef(m)[3], digits = 3)))
  eq_str <- as.character(as.expression(eq))
  
  new_df$predicted_log_consumption <- predict(m, new_df)
  p <- plot_partition(new_df, colors, vals)
  p <- p + annotate('text', y=20, x=10, label=eq_str, parse=TRUE, size=5)
  ggsave(in_f('partition_plots/ols.pdf'), plot=p)
  
  new_df2$predicted_log_consumption <- predict(m, new_df2)
  p <- plot_partition(new_df2, colors, vals)
  p <- p + annotate('text', y=20, x=10, label=eq_str, parse=TRUE, size=5) 
  ggsave(in_f('partition_plots/ols_filled.pdf'), plot=p)
  
  # KNN
  for (k in c(1, 3, 7)) {
    m_k <- kknn(log_consumption ~ qtd_comodos_domc + qtd_morador_domc, train=df[-test_id, ], new_df, k=k)
    new_df$predicted_log_consumption <- predict(m_k)
    p <- plot_partition(new_df, colors, vals)
    ggsave(in_f(paste('partition_plots/knn_', k, '.pdf', sep='')), plot=p)
    
    m_k <- kknn(log_consumption ~ qtd_comodos_domc + qtd_morador_domc, train=df[-test_id, ], new_df2, k=k)
    new_df2$predicted_log_consumption <- predict(m_k)
    p <- plot_partition(new_df2, colors, vals)
    ggsave(in_f(paste('partition_plots/knn_', k, '_filled.pdf', sep='')), plot=p)
  }
  
  # SPLINES
  x <- model.matrix(log_consumption ~ qtd_comodos_domc + qtd_morador_domc, new_df)
  y <- new_df$log_consumption
  m <- polspline::polymars(y, x)
  new_df$predicted_log_consumption <- predict(m, x)
  p <- plot_partition(new_df, colors, vals)
  ggsave(in_f('partition_plots/spline.pdf'), p)
  
  x <- model.matrix(predicted_log_consumption ~ qtd_comodos_domc + qtd_morador_domc, new_df2)
  new_d2f$predicted_log_consumption <- predict(m, x)
  p <- plot_partition(new_df2, colors, vals)
  ggsave(in_f('partition_plots/spline_filled.pdf'), p)
  
  # RF
  fmla <- log_consumption ~ qtd_comodos_domc + qtd_morador_domc
  m_rf <- ranger::ranger(fmla, new_df, num.trees=50,  write.forest=TRUE)
  new_df$predicted_log_consumption <- predict(m_rf, new_df)$predictions
  p <- plot_partition(new_df, colors, vals)
  ggsave(in_f('partition_plots/random_forest.pdf'), p)
  
  new_df2$predicted_log_consumption <- predict(m_rf, new_df2)$predictions
  p <- plot_partition(new_df2, colors, vals)
  ggsave(in_f('partition_plots/random_forest_filled.pdf'), p)
  
  # REGRESSION TREE
  m <- RWeka::M5P(fmla, new_df)
  new_df$predicted_log_consumption <- predict(m, new_df)
  p <- plot_partition(new_df, colors, vals)
  ggsave(in_f('partition_plots/regression_tree.pdf'), p)
  
  new_df2$predicted_log_consumption <- predict(m, new_df2)
  p <- plot_partition(new_df2, colors, vals)
  ggsave(in_f('partition_plots/regression_tree_filled.pdf'), p)
  
#   # KERNEL
#   x <- df[, c('qtd_comodos_domc', 'qtd_morador_domc')]
#   y <- df$log_consumption
#   sm <- fields::smooth.2d(Y=y, x=x, surface=FALSE)
#   new_df3 <- data.frame(qtd_comodos_domc=sm$x, qtd_morador_domc=sm$y, predicted_log_consumption=sm$z)
#   p <- plot_partition(new_df3, colors, vals)
#   ggsave(in_f('partition_plots/kernel_filled.pdf'), plot=p)
}

partition_graph <- function(df) {
  new_df <- df[, c('qtd_comodos_domc', 'qtd_morador_domc', 'log_consumption')]
  new_df <- new_df[sample(1:nrow(new_df), 10000), ]
  tree_model <- rpart(log_consumption ~ qtd_comodos_domc + qtd_morador_domc, df)
  new_df$predicted_log_consumption <- as.factor(predict(tree_model, new_df))
  levs <- as.numeric(levels(new_df$predicted_log_consumption))
  colors <- brewer.pal(length(levs) + 2, "RdYlBu")
  p <- ggplot(new_df, aes(y=qtd_comodos_domc, x=qtd_morador_domc, color=predicted_log_consumption)) + 
    geom_point() +
    scale_color_manual(values=colors[2:(length(colors)-1)]) +
    geom_jitter() +
    xlab('number of residents') +
    ylab('number of rooms')
  ggsave(in_f('tree_partition.pdf'), plot=p)
  
  count <- 100
  qtd_comodos_domc <- seq(min(new_df$qtd_comodos_domc), max(new_df$qtd_comodos_domc), length.out=count)
  qtd_morador_domc <- seq(min(new_df$qtd_morador_domc), max(new_df$qtd_morador_domc), length.out=count)
  new_df2 <- expand.grid(qtd_comodos_domc=qtd_comodos_domc, qtd_morador_domc=qtd_morador_domc)
  new_df2$predicted_log_consumption <- as.factor(predict(tree_model, new_df2))
  p <- ggplot(new_df2, aes(y=qtd_comodos_domc, x=qtd_morador_domc, color=predicted_log_consumption)) + 
    geom_point() +
    scale_color_manual(values=colors[2:(length(colors)-1)]) +
    geom_jitter() +
    xlab('number of residents') +
    ylab('number of rooms')
  ggsave(in_f('tree_partition_filled.pdf'), plot=p)
  
  
  min_cons <- min(new_df$log_consumption)
  max_cons <- max(new_df$log_consumption)
  vals <- rescale(c(min_cons, levs, max_cons))
  p <- ggplot(new_df, aes(y=qtd_comodos_domc, x=qtd_morador_domc, color=log_consumption)) + 
    geom_point() +
    scale_color_gradientn(colors=colors, values=vals) +
    geom_jitter() +
    xlab('number of residents') +
    ylab('number of rooms')
  ggsave(in_f('covariates.pdf'), plot=p)
  
  
  m <- lm(log_consumption ~ qtd_comodos_domc + qtd_morador_domc, df)
  eq <- substitute(log_consumption == a + b %.% rooms + c %.% residents,
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        c = format(coef(m)[3], digits = 3)))
  eq_str <- as.character(as.expression(eq))
  
  new_df$predicted_log_consumption <- predict(m, new_df)
  p <- ggplot(new_df, aes(y=qtd_comodos_domc, x=qtd_morador_domc, color=predicted_log_consumption)) + 
    geom_point() +
    scale_color_gradientn(colors=colors, values=vals) +
    geom_jitter() +
    annotate('text', y=20, x=10, label=eq_str, parse=TRUE, size=5) +
    xlab('number of residents') +
    ylab('number of rooms')
  ggsave(in_f('ols_partition.pdf'), plot=p)
  
  new_df2$predicted_log_consumption <- predict(m, new_df2)
  p <- ggplot(new_df2, aes(y=qtd_comodos_domc, x=qtd_morador_domc, color=predicted_log_consumption)) + 
    geom_point() +
    scale_color_gradientn(colors=colors, values=vals) +
    geom_jitter() +
    annotate('text', y=20, x=10, label=eq_str, parse=TRUE, size=5) +
    xlab('number of residents') +
    ylab('number of rooms')
  ggsave(in_f('ols_partition_filled.pdf'), plot=p)
}

tree_complexity <- function(df) {
  model <- rpart(log_consumption ~ ., df, minsplit=2, cp=5e-5)
  cp_df <- as.data.frame(model$cptable)
  cp_df$CP <- NULL
  cp_df[, 'in_sample_error'] <- cp_df[, 'rel error']
  cp_df[, 'rel error'] <- NULL
  cp_df <- rename(cp_df, holdout_error=xerror)
  cp_df <- filter(cp_df, holdout_error < 1)
  cp_df_long <- melt(cp_df, id.vars=c('nsplit', 'xstd'))
  cp_df_long <- mutate(cp_df_long, ymin=value-xstd, ymax=value+xstd)
  p <- ggplot(cp_df_long, aes(x=nsplit, y=value, color=variable)) + 
    geom_line(size=1.5) +
    geom_errorbar(aes(ymin=ymin, ymax=ymax))
  ggsave(in_f('tree_complexity.pdf'), plot=p)
  
  p <- ggplot(cp_df_long, aes(x=nsplit, y=value, color=variable)) + 
    geom_line(size=1.5) +
    geom_errorbar(aes(ymin=ymin, ymax=ymax)) +
    scale_x_log10()
  ggsave(in_f('tree_complexity_log.pdf'), plot=p)
}

plot_roc <- function(tree_roc, ols_roc=NULL) {
  if (!is.null(ols_roc)) {
    tree_roc_df <- data.frame(method='tree', sensitivity=tree_roc$sensitivities, specificity=tree_roc$specificities)
    ols_roc_df <- data.frame(method='ols', sensitivity=ols_roc$sensitivities, specificity=ols_roc$specificities)
    text_df <- data.frame(
      method=c('ols', 'tree'),
      specificity=c(.77, .72),
      sensitivity=c(.8, .6),
      auc=paste('auc:', c(round(ols_roc$auc, digits=3), round(tree_roc$auc, digits=3))))
    roc_df <- rbind(tree_roc_df, ols_roc_df)
    ggplot(roc_df, aes(x=specificity, y=sensitivity, color=method)) + 
      geom_line() +
      scale_x_reverse() +
      geom_text(data=text_df, aes(label=auc))
  } else {
    tree_roc_df <- data.frame(sensitivity=tree_roc$sensitivities, specificity=tree_roc$specificities)
    ggplot(tree_roc_df, aes(x=specificity, y=sensitivity)) + 
      geom_line(size=1.5) +
      scale_x_reverse() +
      geom_polygon(data=rbind(tree_roc_df, data.frame(specificity=0, sensitivity=0)), alpha=0.2) +
      geom_text(
        data=filter(tree_roc_df, row_number() == 70),
        label=paste('auc:', round(tree_roc$auc, digits=3)),
        size=12,
        position=position_nudge(.1, -.1))
  }
}

plot_reach <- function(pred_df, consumption_threshold) {
  only_tree <- filter(pred_df, method=='tree')
  random <- mutate(only_tree, predicted=runif(n()), method='random')
  tree_and_random <- rbind(only_tree, random)
  g <- group_by(tree_and_random, method) %>%
    arrange(predicted) %>%
    mutate(pct_targeted=row_number() / n()) %>%
    mutate(reach=cumsum(true < consumption_threshold) / sum(true < consumption_threshold))
  ggplot(g, aes(x=pct_targeted, y=reach, color=method)) +
    geom_line(size=1.5) +
    xlab('percent targeted')
}

plot_swf <- function(pred_df, consumption_threshold) {
  write('welfare = sum(1 / exp(log_consumption))', in_f('welfare.txt'))
  only_tree <- filter(pred_df, method=='tree')
  random <- mutate(only_tree, predicted=runif(n()), method='random')
  tree_and_random <- rbind(only_tree, random)
  g <- group_by(tree_and_random, method) %>%
    arrange(predicted) %>%
    mutate(optimal=sort(true)) %>%
    mutate(pct_targeted=row_number() / n()) %>%
    mutate(welfare=cumsum(1/exp(true)) / cumsum(1/exp(optimal)))
  ggplot(g, aes(x=pct_targeted, y=welfare, color=method)) +
    geom_line(size=1.5) +
    xlab('percent targeted') +
    ylab('welfare (divided by best possible)')
}

final_tree <- function(df, min_cp=1e-4, threshold=0.2) {
  df <- translate_colnames(df)
  fold <- split_test_train(df, test_fraction=.1)[[1]]
  consumption_threshold <- quantile(fold$test$log_consumption, threshold)
  full_model <- rpart(log_consumption ~ ., fold$train, cp=min_cp)
  cp_df <- as.data.frame(full_model$cptable)
  cp_best <- cp_df$CP[which.min(cp_df$xerror)]
  model <- prune(full_model, cp=cp_best)
  
  imp_df <- data.frame(feature=names(model$variable.importance), importance=model$variable.importance) %>%
    arrange(desc(importance)) %>%
    filter(row_number() < 10)
  imp_df$feature <- factor(imp_df$feature, levels=rev(imp_df$feature))
  
  p <- ggplot(imp_df, aes(x=feature, y=importance)) +
    geom_bar(stat='identity') +
    coord_flip()
  ggsave(in_f('variable_importance.pdf'), plot=p)
  
  
  pdf(in_f('full_tree.pdf'))
  prp(model)
  dev.off()
  
  
  tree_preds <- data.frame(method='tree', predicted=predict(model, fold$test), true=fold$test$log_consumption)
  tree_roc <- pROC::roc(response=tree_preds$true <= consumption_threshold, predictor=tree_preds$predicted)
  ols <- lm(log_consumption ~ ., fold$train)
#   ols_preds <- data.frame(method='ols', predicted=predict(ols, fold$test), true=fold$test$log_consumption)
#   ols_roc <- pROC::roc(response=ols_preds$true <= consumption_threshold, predictor=ols_preds$predicted)
  
  # pred_df <- rbind(tree_preds, ols_preds)
  pred_df <- tree_preds
  p <- plot_roc(tree_roc)
  ggsave(in_f('roc.pdf'), plot=p)
  
  p <- plot_reach(pred_df, consumption_threshold)
  ggsave(in_f('reach.pdf'), plot=p)
  
  p <- plot_swf(pred_df, consumption_threshold)
  ggsave(in_f('welfare.pdf'), plot=p)
  
  
  p <- ggplot(tree_preds, aes(x=true, y=predicted)) + 
    geom_point() + 
    geom_smooth(method=lm, se=FALSE) +
    xlim(c(6, 13)) +
    ylim(c(6, 13)) +
    coord_fixed()
  
  ggsave(in_f('residuals.pdf'), plot=p)
  
  tree_preds_long <- melt(tree_preds, id.vars='method')
  p <- ggplot(tree_preds_long, aes(x=value, color=variable)) + 
    geom_line(stat='density', size=1.5, alpha=1) +
    scale_color_brewer(type='qual', palette=1)+
    xlab('log(consumption) (Brazilian real)')
  ggsave(in_f('residuals_density.pdf'), plot=p)
  
  pred_df <- select(
    fold$test,
    number.of.bathrooms,
    number.of.rooms,
    wall.type,
    electric.water.heater,
    number.of.residents,
    sewage.type,
    garbage.disposal,
    mail.distribution.service,
    street.pavement,
    household.type,
    consumption.units,
    number.of.bedrooms,
    number.of.families,
    gas.stove,
    electricity.source.other)
  tree_pred_df <- mutate(pred_df, log_consumption=tree_preds$predicted)
  linear <- lm(log_consumption ~ ., tree_pred_df)
  sink(in_f('tree_preds_linear_model.txt'))
  print(summary(linear))
  sink()
  
  coef_df <- data.frame(summary(linear)$coef)
  coef_df$feature <- row.names(coef_df)
  coef_df <- coef_df %>%
    arrange(desc(abs(Estimate))) %>%
    filter(!grepl('Intercept', feature)) %>%
    filter(!grepl('imput', feature)) %>%
    filter(row_number() < 10) %>%
    select(Feature=feature, Estimate, Std.Error=Std..Error)
  coef_df <- rbind(data.frame(Feature='linear', Estimate=summary(linear)$r.squared, Std.Error=NA), coef_df)
  coef_df <- rbind(coef_df, data.frame(Feature='nonlinear', Estimate=1-summary(linear)$r.squared, Std.Error=NA))
}

make <- function() {
  df <- load_dataset('brazil_sendhil_all')
  df$X <- NULL
  cnames <- colnames(df)
  categoricals <- cnames[!grepl('qtd', cnames) & !grepl('num', cnames) & TARGET_VARIABLE != cnames]
  df[, categoricals] <- lapply(df[, categoricals], factor)
  df <- rename(df, log_consumption=yyyyy)
  df2 <- mutate(df, log_consumption=log_consumption/qtd_familia)
  fpath <- paste(TARGETING_DATA_IN, 'pof2008_dom_standard.dta', sep='/')
  bz <- read.dta13(fpath)
  
  dataset_stats(df, bz)
  partition_graph(df2)
  tree_complexity(df)
  little_trees(df)
  final_tree(df)
}

forest_graph <- function(df) {
  fold <- split_test_train(df, test_fraction=0.2)[[1]]
  train <- fold$train
  nodesizes <- c(5, 10, 20, 50, 100, 200, 350, 500)
  mtries <- seq(2, 40, by=4)
  parameters <- expand.grid(ns=nodesizes, mt=mtries)
  forests <- mcmapply(
    function(nodesize, mtry) {
      print(paste(mtry, nodesize))
      ranger::ranger(log_consumption ~ ., data=train, num.trees=50, mtry=mtry, min.node.size=nodesize, write.forest=TRUE)
    },
    parameters$ns,
    parameters$mt, 
    SIMPLIFY=FALSE,
    mc.cores=4
  )
  test <- fold$test
  losses <- purrr::map_dbl(forests, ~mean((predict(.x, test)$predictions-test$log_consumption)^2))
  colors <- rev(brewer.pal(5, "Greys")[c(1, 3, 5)])
  res_df <- data.frame(min.nodesize=parameters$ns, mtry=parameters$mt, loss=losses)
  vals <- rescale(c(min(losses), median(losses), max(losses)))
  p <- ggplot(res_df, aes(x=min.nodesize, y=mtry, color=loss)) +
    geom_point(size=8) +
    scale_x_log10() +
    # scale_color_gradientn(colors=colors, values=vals) +
    xlab('Minimum nodesize') +
    ylab('Features per iteration')
  ggsave(in_f('forest_tuning.pdf'), plot=p)
}

lasso_variables <- function(df) {
  trans_df <- translate_colnames(df)
  x <- model.matrix(log_consumption ~ ., trans_df)
  y <- df[, 'log_consumption']
  model <- glmnet::cv.glmnet(x, y, alpha=1)
  lambdas <- model$lambda
  nzero <- model$nzero
  nz_df <- data.frame(lambda=lambdas, nzero=nzero)
  p <- ggplot(nz_df, aes(x=lambda, y=nzero)) + 
    geom_point() +
    ylab('nonzero coefficients')
  ggsave(in_f('lasso_nonzero.pdf'), plot=p)
  
  change_lambdas <- lambdas[c(TRUE, (diff(nzero) > 0)) & (nzero <= 10)]
  coefs <- coef(model, s=change_lambdas)
  coefs <- coefs[apply(coefs > 0, 1, any), ]
  colnames(coefs) <- change_lambdas
  write.csv(as.matrix(coefs), in_f('lambda_features.csv'))
}

trace_regression_coefs <- function(df) {
  trans_df <- translate_colnames(df)
  fname <- 'regression_coef_v_lambda.pdf'
  x <- model.matrix(log_consumption ~ . - 1, trans_df)
  y <- df[, 'log_consumption']
  lambdas <- seq(0, 1, length.out=100)
  lasso_model <- glmnet::cv.glmnet(x, y, alpha=1, lambda=lambdas)
  ridge_model <- glmnet::cv.glmnet(x, y, alpha=0, lambda=lambdas)
  ols_model <- glmnet::glmnet(x, y, lambda=0)
  
  feature <- 'number.of.bathrooms'
  lasso_trace <- coef(lasso_model, lasso_model$lambda)[feature, ]  
  ridge_trace <- coef(ridge_model, ridge_model$lambda)[feature, ]  
  ols_trace <- rep(coef(ols_model)[feature, ], length(lambdas))
  
  lasso_coef_names <- map(lasso_model$lambda, ~names(which(coef(lasso_model, .)[, 1] != 0)) %>% keep(~. != '(Intercept)'))
  post_lasso_models <- map(lasso_coef_names, ~if (length(.) < 2) NA else glmnet::glmnet(x[, .], y, lambda=0))
  post_lasso_trace <- map_dbl(post_lasso_models, ~if (is.na(.)) NA else coef(.)[feature, ])
  post_lasso_trace[is.na(post_lasso_trace)] <- 0
   
  lasso_df <- data.frame(beta_hat=lasso_trace, lambda=lasso_model$lambda, feature=feature, method='lasso')
  ridge_df <- data.frame(beta_hat=ridge_trace, lambda=ridge_model$lambda, feature=feature, method='ridge')
  post_lasso_df <- data.frame(beta_hat=post_lasso_trace, lambda=lasso_model$lambda, feature=feature, method='post_lasso')
  ols_df <- data.frame(beta_hat=ols_trace, lambda=lasso_model$lambda, feature=feature, method='ols')
  
  trace_df <- rbind(lasso_df, ridge_df, post_lasso_df, ols_df)
  trace_df$method <- factor(trace_df$method, levels=c('ols', 'ridge', 'lasso', 'post_lasso'), ordered=T)
  colors <- brewer.pal(4, "Set1")
  names(colors) <- levels(trace_df$method)
  max_beta <- max(c(ols_trace, post_lasso_trace, lasso_trace, ridge_trace))
  p1 <- ggplot(filter(trace_df, method=='ols'), aes(x=lambda, y=beta_hat, color=method)) +
    geom_point() +
    scale_color_manual(values=colors) +
    ylim(0, max_beta) + 
    ylab(expression(hat(beta))) + 
    xlab(expression(lambda))
  p2 <- ggplot(filter(trace_df, (method=='ols' | method=='ridge')), aes(x=lambda, y=beta_hat, color=method)) +
    geom_point() +
    scale_color_manual(values=colors) +
    ylim(0, max_beta) + 
    ylab(expression(hat(beta))) +
    xlab(expression(lambda))
   p3 <- ggplot(filter(trace_df, (method=='ols' | method=='lasso' | method=='ridge')), aes(x=lambda, y=beta_hat, color=method)) +
    geom_point() +
    scale_color_manual(values=colors) +
    ylim(0, max_beta) + 
    ylab(expression(hat(beta))) + 
    xlab(expression(lambda))
   p4 <- ggplot(filter(trace_df, (method=='ols' | method=='lasso' | method=='post_lasso')), aes(x=lambda, y=beta_hat, color=method)) +
    geom_point() +
    scale_color_manual(values=colors) +
    ylim(0, max_beta) + 
    ylab(expression(hat(beta))) + 
    xlab(expression(lambda))
   p5 <- ggplot(trace_df, aes(x=lambda, y=beta_hat, color=method)) +
    geom_point() +
    scale_color_manual(values=colors) +
    ylim(0, max_beta) + 
    ylab(expression(hat(beta))) + 
    xlab(expression(lambda))
  
  ggsave(in_f('beta_trace_ols.pdf'), p1)
  ggsave(in_f('beta_trace_ols_ridge.pdf'), p2)
  ggsave(in_f('beta_trace_ols_ridge_lasso.pdf'), p3)
  ggsave(in_f('beta_trace_ols_lasso_post_lasso.pdf'), p4)
  ggsave(in_f('beta_trace_ols_ridge_lasso_post_lasso.pdf'), p5)
  
}

regression_coefs <- function(df, filter_features='all') {
  trans_df <- translate_colnames(df)
  fname <- 'regression_coefs_all.pdf'
  if (filter_features == 'numeric') {
    trans_df <- trans_df[, sapply(trans_df, is.numeric)]
    fname <- 'regression_coefs_numeric.pdf'
  }
  
  if (filter_features == 'numeric+binary') {
    trans_df <- keep(trans_df, ~is.numeric(.) | nlevels(.) == 2)
    fname <- 'regression_coefs_numeric_and_binary.pdf'
  }
  
  x <- model.matrix(log_consumption ~ . - 1, trans_df)
  y <- df[, 'log_consumption']
  lasso_model <- glmnet::cv.glmnet(x, y, alpha=1)
  ridge_model <- glmnet::cv.glmnet(x, y, alpha=0)
  ols_model <- glmnet::glmnet(x, y, lambda=0)
  
  lasso_coef <- coef(lasso_model)
  ridge_coef <- coef(ridge_model)
  ols_coef <- coef(ols_model)
  
  lasso_coef_names <- names(which(lasso_coef[, 1] != 0))
  
  lasso_coef_names <- purrr::discard(lasso_coef_names, ~ . == '(Intercept)')
  post_lasso_model <- glmnet::glmnet(x[, lasso_coef_names], y, lambda=0)
  post_lasso_coef <- coef(post_lasso_model)
  
  coef_dfs <- map(c(lasso_coef, ridge_coef, ols_coef, post_lasso_coef), ~data.frame(as.matrix(.)))
  coef_df <- bind_cols(coef_dfs[1:3])
  colnames(coef_df) <- c('lasso', 'ridge', 'ols')
  coef_df$feature <- row.names(ols_coef)
  
  post_lasso_coef_df <- coef_dfs[[4]]
  colnames(post_lasso_coef_df) <- 'post.lasso'
  post_lasso_coef_df$feature <- row.names(post_lasso_coef)
  coef_df <- left_join(coef_df, post_lasso_coef_df, by='feature')
  coef_df[is.na(coef_df)] <- 0.
  
  coef_df_long <- melt(coef_df, id.vars='feature', value.name='coefficient')
  coef_df_long <- filter(coef_df_long, feature!='(Intercept)')
  coef_df_long$coefficient <- abs(coef_df_long$coefficient)
  
  feature_order <- row.names(ols_coef)[order(abs(ols_coef[, 1]), decreasing=TRUE)]
  coef_df_long$feature <- factor(coef_df_long$feature, levels=feature_order, ordered=TRUE)
  
  p <- ggplot(coef_df_long, aes(x=feature, y=coefficient, fill=variable)) +
    geom_bar(position='dodge', stat='identity') +
    ylab(expression(abs(beta))) + 
    coord_flip()
  ggsave(in_f(fname), plot=p)
}

translate_colnames <- function(df) {
  rename(
    df,
    number.of.bathrooms=qtd_banheiros,
    number.of.rooms=qtd_comodos_domc,
    wall.type=cod_material_piso,
    electric.water.heater=energia_17,
    number.of.residents=qtd_morador_domc,
    sewage.type=cod_esgoto_sanit,
    garbage.disposal=cod_lixo,
    mail.distribution.service=cod_servico_distribuicao,
    street.pavement=cod_exist_pavim,
    household.type=cod_tipo_domc,
    consumption.units=qtd_uc,
    number.of.bedrooms=qtd_comd_serv_dormit,
    number.of.families=qtd_familia,
    gas.stove=gas_18,
    electricity.source.other=propria_15
  )
}

# tree_stability <- function(df) {
#   trans_df <- translate_colnames(df)
#   N <- nrow(trans_df)
#   models <- lapply(1:4, function(i) {
#     sample_df <- trans_df[sample(1:N, floor(N/10)), c('log_consumption', 'gas.stove', 'electric.water.heater', 'sewage.type')]
#     rpart(log_consumption ~ ., sample_df)
#   })
#   
#   imp_df <- data.frame(feature=names(model$variable.importance), importance=model$variable.importance) %>%
#     arrange(desc(importance)) %>%
#     filter(row_number() < 10)
#   imp_df$feature <- factor(imp_df$feature, levels=rev(imp_df$feature))
# }

results_table <- function() {
  output <- load_validation_models('brazil_sendhil_all')[, -1]
  
  ols <- filter(output, method=='ols') %>% arrange(true)
  forest <- filter(output, method=='tuned_forest') %>% arrange(true)
  ensemble_df <- data.frame(true=ols$true, ols=ols$predicted, forest=forest$predicted)
  m <- lm(true ~  ols + forest, ensemble_df)
  ensemble_table <- data.frame(
    value=c(m$coefficients, summary(m)$r.squared),
    row.names=c('Intercept', 'ols', 'forest', 'r^2'))
  ensemble_df <- data.frame(true=ols$true, ols=ols$predicted, forest=forest$predicted)
  
  m2 <- lm(true ~  ols + forest + ols * forest, ensemble_df)
  ensemble_table <- data.frame(
    value=c(m$coefficients, 0, summary(m)$r.squared),
    value2=c(m2$coefficients, summary(m2)$r.squared),
    row.names=c('Intercept', 'ols', 'forest', 'ols * forest', 'r^2'))
  
  ensemble_latex <- xtable::xtable(ensemble_table)
  sink(in_f('ensemble_coefficients_table.tex'))
  
  ensemble_latex <- xtable::xtable(ensemble_table)
  print(ensemble_latex)
  sink()
  sink(in_f('ensemble_coefficients_table.tex'))
  print(ensemble_latex)
  sink()
  
  ensemble_output <- data.frame(true=ensemble_df$true, predicted=predict(m, ensemble_df), method='ensemble', fold=1)
  ensemble2_output <- data.frame(true=ensemble_df$true, predicted=predict(m2, ensemble_df), method='ensemble2', fold=1)
  e_output <- rbind(output, ensemble_output, ensemble2_output)
 
  output_table <- summarize_output(e_output, .05)
  row.names(output_table) <- output_table$method
  ordered_table <- output_table[c('tree', 'knn', 'svm', 'ols', 'lasso', 'ridge', 'lasso_ix', 'regression_tree', 'tuned_forest', 'boosted_tree', 'ensemble', 'ensemble2'), ]
  ordered_table <- ordered_table[, c('reach', 'avg_1_c', 'avg_1_c_normalized')]
  colnames(ordered_table) <- c(
    '% Poor Reached',
    "E[u'(c)]*10^4",
    "Fraction of max"
  )
  
  results_latex <- xtable::xtable(
    ordered_table, 
    digits=8)
  sink(in_f('results_table.tex'))
  print(results_latex)
  sink()
}

make2 <- function() {
  df <- load_dataset('brazil_sendhil_all')
  df$X <- NULL
  cnames <- colnames(df)
  categoricals <- cnames[!grepl('qtd', cnames) & !grepl('num', cnames) & TARGET_VARIABLE != cnames]
  df[, categoricals] <- lapply(df[, categoricals], factor)
  df <- rename(df, log_consumption=yyyyy)
  df <- select(df, -contains('imput'))
  df <- select(df, -num_ext_renda)
  df2 <- mutate(df, log_consumption=log_consumption/qtd_familia)
  
  lasso_variables(df)
  regression_coefs(df)
  regression_coefs(df, 'numeric')
  regression_coefs(df, 'numeric+binary')
}


# lasso ridge, beta hat as a function of lambda one per graph
# instability in the tree
# ols, ols + ridge, ols + ridge + lasso, ols + lasso + post_lasso
# knn , splines, rfs, reg trees, kernel