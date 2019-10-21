# bumbl (development version)
* changed `bumbl()` to use `glm()` instead of `lm()` to fit models.  Uses log-link gaussian error by default.
* `family` argument in `bumbl()` now passed directly to `glm()`
* Added `bumbl.nb()`  for overdispersed count data.  It uses `MASS::glm.nb()` to fit models.
* Added `...` argument to `bumbl()` and `bumbl.nb()` to pass additional arguments (e.g. offset) to `glm()` and `glm.nb()` respectively

# bumbl 0.0.2
* changed `bumbl()` to use a log-link glm by default instead of an lm.  Use untransformed response variables.
* added argument to `bumbl()` to allow for count data
* added change in mass to `bombus` data
* now requires tidyr version 1.0.0 or greater


# bumbl 0.0.1
* `bumbl()` no longer errors when a single colony produces an error
* added a vignette
* updated `bombus` data to include cumulative floral resources
* fixed major bug (issue #12) that cause `bumbl()` to return the incorrect colony IDs
* added `bubml_plot()` for checking the output of `bumbl()`

# bumbl 0.0.0.9001

* Added a `NEWS.md` file to track changes to the package.
* This is the first minimally functioning version of bumbl.  More features to come.  Feedback welcome.
