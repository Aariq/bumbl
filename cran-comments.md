## Test environments
* local R installation, macOS 13.2.1, R 4.2.2

With GitHub Actions:

* windows-latest, R 4.2.2
* windows-devel, R development version
* macOS-latest, R 4.2.2
* ubuntu-20.04, R 4.2.2
* ubuntu-20.04, R development version

R-hub:

* Windows Server 2022, R development version

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a patch update to the `bumbl` package to address check warnings about S3 generic/method consistency

* A note on the R-hub windows build (detritus in temp directory lastMiKTeXException) is an rhub issue: https://github.com/r-hub/rhub/issues/503
