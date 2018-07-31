# ML-library
This repo contains experiments with machine learning for proxy means tests.

# Installing

From a terminal window, naviagate to project home. Then run `R`.

```R
> install.packages('devtools')
> library('devtools')
> build()
> install()
```

Alternatively, open the project in RStudio and run the above.

# Code organization

The code is organized into two sections.
* The folder `R/` contains an in an installable R package implementing prediction methods and analysis utilities.
* The folder `analyses/` contains a set of scripts which use functions from the library to train and test machine learning methods for proxy means tests on a variety of consumption datasets from various countries.

## Country Analyses

Country analyses live in the `analyses/` folder.
These scripts expect you to move the raw survey datasets (in Box) into the `data/` folder.
Country analyses typically consist of two logical tasks.
First, we have to transform the raw survey dataset into the form expected by the models.
The expected form is a dataframe consisting of one row per survey participant and an outcome variable representing their calculated consumption.
The process for preparing each dataset is typically unique to that dataset and so we usually don't use any library functions for this task.
After processing the survey data into a standard dataframe, we call three library functions.
First, set aside a holdout set using `set_aside_holdout`.
This saves 20% of the data as a final holdout.
We set the random seed such that the same rows are held out if the function is rerun.
The code for this function is in `util.R` in the package.
We then call two data processing fuctions: `standardize_predictors`, which centers and scales the predictors by the variance, and `na_indicator` which adds indicators for missing values (we assume the missingness contains information).
The definitions for these functions can be found in `transform.R`.

After preparing the datasets, we test the prediction functions using `test_all_named`.
This function tests a standard set of methods using cross validation.
All of the code for running these methods is contained in `validation.R`.
Testing relies on two important functions, both contained in the file.
First we call `split_test_train` to divide the dataset into k test and train splits for cross validation.
Once we have our splits, we loop through the list of methods `method_list` and call the function `test_method_on_splits`.
This function takes two arguments:
* `method` is a singleton list `list(<method_name>=<method>)` where `<method>` is a function which implements training and testing on a fold. The methods are defined in the `validation.R` file and the standard list is in the `METHOD_LIST` constant at the bottom of the file.
* `folds` which is the output of `split_test_train`.
`test_method_on_splits` loops over each fold in the set of folds, calls the method on the fold and then coerces the predictions into a standard output (using the function `test_one`).

## Analyzing the results

The best place to see our analysis of the results to date is in `slides.Rmd`.
Note that these slides rely on some standard assessment functions we have implemented in `validation.R`.





