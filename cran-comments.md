## Test environments
* local R installation, macOS 11.5, R 4.0.2

With GitHub Actions:

* windows-latest, R 4.1.0
* macOS-latest, R 4.1.0
* ubuntu-20.04, R 4.1.0
* ubuntu-20.04, R development version

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a minor update to the `bumbl` package

* There is one failing test at https://cran.rstudio.com//web/checks/check_results_bumbl.html.  This is a model convergence error due to different behavior of `stats::optim()` with OpenBLAS that I'm unable to re-create.  I have skipped this test on CRAN for now and opened an issue to look into it for the next version.
