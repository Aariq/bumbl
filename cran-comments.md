## Test environments
* local R installation, macOS 11.5, R 4.0.2

With GitHub Actions:

* windows-latest, R 4.2.0
* windows-devel, R development version
* macOS-latest, R 4.2.0
* ubuntu-20.04, R 4.2.0
* ubuntu-20.04, R development version

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a patch update to the `bumbl` package to address failing tests on r-devel related to changes in BLAS affecting convergence errors (https://cran.rstudio.com//web/checks/check_results_bumbl.html).
