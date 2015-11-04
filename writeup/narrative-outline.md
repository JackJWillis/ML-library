+ As quantity of data explodes, machine learning is being used to improve forecasting accuracy across a wide range of human activity [cite examples]
+ In development economics, the quintessential forecasting problem is poverty targeting. Historically this has been led by TA teams from the WB using data from surveys constructed specifically for monitoring poverty (ie LSMS) and methods such as OLS, potentially with variable selection techniques.  This raises the question of whether we can do better using machine learning methods. New formulas have no running costs and gains for the poor could be large.
+ In this paper we compare the targeting performance of techniques currently used for PMTs to those achievable using machine learning methods.
+ We consider three different types of learning methods: regularized linear methods, nonlinear methods and novel loss functions.

-------------------

new focus: three main results  

+ OLS does surprisingly well
+ Ensemble does a bit better
+ The gain from using ensemble is equivalent to around Y% budget increase.  Try to compare this against other actual things (e.g. improvement from community based targeting?) that makes meaningfulness clear.  Give cost/benefit of doing this.
+ Return to larger n and larger k is larger with ML than with OLS (?)

-------------------

+ We find little gain from adding regularization to linear methods.  [reach vs lambda by country]  We show this reflects the fact that the optimal regularization parameter is negligible, leading to almost no change in models [average percentage change in y hat or beta hats for optimal regularized vs ols].
+ n vs k. formula for extent of regularization assuming linear model
+ Somewhat surprisingly, we also find that non-linear methods do if anything worse than the status quo.  [normalized reach plot at 40/40]. We have two hypotheses:
+ We find that the relationship uncovered by random forests, our best performing nonlinear method, is itself highly linear [ols results of y_rf on x]. This suggests that the true relationship is linear. 
+ Measurement error could reduce the ability of nonlinear methods to uncover whatever nonlinear relationships do exist in the data [simulation results]. In trees errors can become compounded. Local methods are able to fit more complex relationships, but at the cost of global smoothing.
+ Nor do we find improvements from obvious changes to the loss function. Weighting observations by their welfare gains (or losses) if targeted does not improve targeting, even when looking at social welfare rather than reach.
+ We do find meaningful gains, however, from ensemble methods that are increasingly popular in the literature.  The median gain from ensemble relative to status quo across N countries is X% increase in reach. [table: ensemble reach difference, budget difference by country]  This is equivalent to a Y% increase in anti-poverty spending in those contexts, holding the targeting procedure fixed.
+ Ensemble of OLS and random forests
+ OLS with random forests on residuals
+ These methods take little effort to implement and could be incorporated into targeting programs.
+ Increase in reach is increasing with sample size (check in new data)
+ We find that these gains are preserved when using a reduced feature set as in a PMT. This is true even though we select the set of features which allow for the best linear fit. [same table as above, on reduced variable set]
+ We find that these gains [are/are not] preserved with panel data.
+ Finally we investigate the relationship between targeting performance and data set size.
+ We look at both the effect of reducing the size of the training data and increasing the size of the survey.
+ These experiments give context to the improvements realized by ensemble. A 3% improvement in reach is achievable with OLS by increasing the training data by N observations
+ These gains cannot be achieved by increasing K in the PMT, given the variables we have

