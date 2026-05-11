# UIUC STAT 431 Final Project: Used Car Prices

A bayesian hierarchical regression model for used-car listing prices on the Kaggle used-car dataset, fit with JAGS and compared to OLS on held-out data. The workflow covers preprocessing, MCMC diagnostics, prior sensitivity, in-sample information criteria, and posterior prediction with uncertainty.

teammates: Jack Liu (jackliu3), Xing Sun (xingsun3), Ning Xu (ningx4), Martin Wang(hanran2)

## Layout

- `data/raw/` — source listing data
- `data/processed/` — cleaned splits and model inputs
- `models/jags/` — JAGS model definitions
- `results/` — outputs by stage (`eda`, `mcmc`, `prior_sensitivity`, `model_comparison`, `prediction`)
- `scripts/` — Python helpers for table export and CSV rounding
- `report/` — written report, proposal, and `stat431_final_report_0506.Rmd`
- `miscellaneous/` — extra exploratory notebook and data copy

Reproduce analysis from the project root with `report/stat431_final_report_0506.Rmd` (set the knit working directory to the project root, or run from `report/` so paths resolve via `data/raw/`). `.rds` MCMC objects are gitignored and are recreated when the Rmd is run.
