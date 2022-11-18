# Superager penalised regression analysis

This is an analysis of the superager data set using elastic net regression in R.
File names with `caret` or `glmnet` refer to the package used to do the analysis.
`caret` is a wrapper package that includes `glmnet` but it doesn't have all of the
functionality so using `glmnet` gives more flexibility to modify how the modelling
is performed.

The contents of the repo are:

* __Data prep__: These scripts clean and rearrange the data before analyses.
  * `prep_data.R`: Rearrange and filter data before main analysis.
  * `prep_morphometric.R`: Equivalent to MRI prep.
  * `rm_poor_quality.R`: Given a list of 3T patient to remove.
  * `clean_sa_raw_data.R`: Given a list of patients to include that were missing from the original dataset.
  * `create_xover_3T_7T_samples.R`: Match patient between 3T and 7T.
  
* __Analysis__: Elastic net regression model fitting using either _caret_ or _glmnet_ packages.
  * `elasticnet_caret_networkonly.R`
  * `elasticnet_glmnet_morphometic.R`
  * `elasticnet_caret_networkonly.R`
  * `elasticnet_glmnet_morphometric.R`
  
* __Output__: Plots and stats.
  * `output_plots_caret.R`
  * `output_caret_performance_stats_plots.R`
  * `output_glmnet_performance_stats_plots.R`





