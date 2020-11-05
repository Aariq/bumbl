# bumbl (development version)
* changed `bumbl()` to use `glm()` instead of `lm()` to fit models.  Uses log-link gaussian error by default.
* `family` argument in `bumbl()` now passed directly to `glm()` unless `"negbin"`, in which case `MASS::glm.nb()` is used.
* Added `...` argument to `bumbl()` to pass additional arguments (e.g. offset) to `glm()` or `glm.nb()`
* Added a `keep.model` argument to `bumbl()` which outputs a list-column with the models fit to each colony.
* added change in mass to `bombus` data
* removed `bumbl_plot()` and instead added a `plot()` method for base R plots and an `autoplot()` method for `ggplot2`
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
