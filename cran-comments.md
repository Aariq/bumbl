## Test environments
* local R installation, macOS 15.5, R 4.5.0

With GitHub Actions:

* windows-latest, R 4.5.1
* windows-devel, R development version
* macOS-latest, R 4.5.1
* ubuntu-latest, R 4.5.1
* ubuntu-latest, R development version
* ubuntu-latest, R 4.4.3

R-hub:

* Windows Server 2022 x64 (build 20348), R development version

## R CMD check results

0 errors | 0 warnings | 1 note

* The only note is "unable to verify current time", which I belive is a known bug

* This is a patch update to the `bumbl` package to address check warnings about cross-references in documentation

