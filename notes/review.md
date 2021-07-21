---
editor_options: 
  markdown: 
    wrap: 999
bibliography: references.bib
---

# Review round 2

## G1

G1.0 Statistical Software should list at least one primary reference from published academic literature.

> \@rkillick:\
> Not met as no paper reference in the documentation or package description

-   added a literature reference to `bumbl` documentation

G1.1 All statistical terminology should be clarified and unambiguously defined.

> \@rkillick:\
> Not met. As one example the bumbl function states "Fits models" with no reference in the details for what model forms or assumptions are made - mathematically speaking.

-   added "generalized linear models" to the function description for `bumbl`
-   "Details" section now refers users to the vignette which has the math laid out.
-   Clarified meaning of "decay" in output of `bumbl` in documentation.

G1.2 Software should use roxygen to documentation all functions.

> \@Paula-Moraga:\
> Documentation should be improved. For example, the documentation of functions `bumbl()` and `bumbl.nb()` is the same and the user cannot understand the difference between the two functions unless they read the R code. The documentation says `bumbl.nb()`: passes arguments to `MASS::glm.nb()` rather than `glm()`. This is not enough to understand the differences between the two functions.

-   I've removed `bumbl.nb()` and `brkpt.nb()` and instead made it so when `family = "negbin"`, `glm.nb()` is used.

-   I've clarified the behavior and meaning of the `family` argument in the documentation.

> \@Paula-Moraga:\
> The same happens with `brkpt()` and `brkpt.nb()`. The documentation should clearly state the purpose of each function.
>
> Also the documentation of `brkpt()` says the value is "a tibble with a column for the winning tau and a column for the winning model". This description can be improved with an explanation of what is tau and what is the winning model.

-   `brkpt` is not exported, so I didn't spent much time on the documentation of this function. I've added a note that this function is typically only used internally by `bumbl()`. `bumbl()` has better documentation on what tau and the winning model are.

G1.3 Software should include all code necessary to reproduce results which form the basis of performance claims made in associated publications.

> \@rkillick:\
> Not met. When I build the vignette I get errors on the convergence of the glm fit.

-   For me these are warnings, not errors, and I can build the vignette just fine.
-   I've added a column to the output of `bumbl()` that shows whether the final model converged or not
-   It turns out these warnings were not coming from any of the winning models (with the `tau` that maximizes likelihood).
-   I've silenced all warnings and messages from these intermediate models in `brkpt().` Convergence warnings in the "winning" model should still show up.

> \@Paula-Moraga:\
> The package does not provide the code to reproduce the results given in the reference provided (Crone and Williams (2016)).

-   The reference did extensive comparative testing of several methods for modeling bumblebee colony growth. The `bumbl` package is intended to be a simplified adaptation of the code used in Crone and WIlliams (2016) that implemented the best of those methods.

-   The `bumbl()` function is based on `glm()` while Crone and Williams

    (2016) used generalized linear mixed effects models with the `lme4` package. Therefore I can't reproduce their results with `bumbl()`

## G2

G2.1 Provide explicit secondary documentation of expectations on data types of all vector inputs (see the above list).

> \@rkillick:\
> Not met. It is not clear that the columns of the data frame for input into the bumbl function need to be numeric

-   added description of minimum requirements of dataframe passed in as `data`

G2.3a Use match.arg() or equivalent where applicable to only permit expected values.

> \@Paula-Moraga:\
> Functions should check the type of arguments. For example, right now we can call `bumbl()` by providing a character value for `taus`.

-   `bumbl()` should now check that taus are numeric

G2.10 Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.

> \@rkillick:\
> Not met. Checks are there for missing colony names and missing output but not for missing data in rows.\
> \@Paula-Moraga:\
> Missing data are not checked.

-   I've added a check that there are no missing values in the time variable. However, it doesn't make sense to me for `bumbl` to check for missing data in other variables because it passes data to `glm` which has methods for handling missing data.

G2.11 Where possible, all functions should provide options for users to specify how to handle missing (NA) data

> \@rkillick:\
> Not met. No options for user-defined NA handling exist.

> \@Paula-Moraga:\
> The package does not provide options to handle missing data.

-   The user can pass `na.action` to `glm` or `glm.nb` to control handling of `NA`s

G2.12 Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default na.rm = FALSE-type parameters

> \@rkillick:\
> Not met. Mutate function called on line 324 of colony-growth.R function with na.rm=TRUE.

-   I removed this particular `na.rm=TRUE` because I'm not sure it was doing anything. It was not removing missing values from the data, just ensuring that the maximum value returned by `predict()` was numeric and not `NA`. But I don't know why `predict` would ever return an `NA`.

> \@Paula-Moraga:\
> There are functions that could pass data with missing values to base routines with default `na.rm = FALSE`. For example, in\
> line 45 of <https://github.com/Aariq/bumbl/blob/master/R/colony-growth.R>

-   Thank you for pointing me to this specific error. I added a check for missing values in the time variable. I can't think of a reason that time should ever contain `NA`s in the context of measuring bumblebee colony growth.

G2.13 All functions should also provide options to handle undefined values (e.g., NaN, Inf and -Inf), including potentially ignoring or removing such values.

> \@rkillick:\
> Not met. No handling of this in functions.
>
> \@Paula-Moraga:\
> The package does not provide options to handle undefined data.

-   I've added a check for undefined values in the time variable (with `is.finite()`)
-   Otherwise, I think I'll leave handling of these values to `glm()`.

## G4

G4.2b Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.

> \@rkillick:\
> Not met Example: Warning "Some taus were not used because they were outside of range of" not covered.

-   I added a test for this

G4.4 Correctness tests to test that statistical algorithms produce expected results to some fixed test data sets.

> \@rkillick:\
> Not met, no correctness tests in testing, just class and no warnings. Skipping the rest of the guidelines for parameter correctness etc. as none are given. All not met.

-   I've added a function `sim_colony()` to simulate colony growth and decay data with the parameters used in the simulation returned as attributes.
-   `sim_colony()` is used to test that expected parameters are returned within some tolerance.

G4.5 Correctness tests should be run with a fixed random seed.

> \@rkillick:\
> Not met. No seed in the sample command for the plotting example

-   added `set.seed()` to example
-   new correctness test run with fixed seed

G4.8 Edge condition tests to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties.

> \@rkillick:\
> Not met. None included.

-   Data is passed to `glm()` with only slight modification, so I'm letting `glm()` handle those edge cases. I'll think about better ways to pass `glm()` errors to the user (e.g. additional columns in the output containing errors or warnings for each colony's GLM?).

G4.9 Noise susceptibility tests. Packages should test for expected stochastic behaviour.

> \@rkillick:\
> Not met. None included, I would expect to see small perturbations in the input data using jitter() to give the same output for tau and very similar parameter estimates.

-   I added theses suggested tests

G.4.10-12 Extended testing.

> \@rkillick:\
> All not met as no extended tests are provided. For this package longer datasets will cause problems with the default "taus" argument behaviour.

-   I'm not sure what is meant by "extended tests". I suspect the `bombus` dataset I included in the package is a typical size for bumblebee colony growth data.
-   In terms of length of the timeseries, I can see how only 50 values for tau might not be enough to find the maximum likelihood estimate for the switchpoint correctly. I've added a note to the documentation saying as much.

## TS1

TS1.0 Time Series Software should explicitly document the types and classes of input data able to be passed to each function

> \@rkillick:\
> Not met. Type assumptions in df are not described.
>
> \@Paula-Moraga:\
> This is not well documented. For example, `bumbl()` documentation says argument `data` should be a data.frame but does not describe what should be the columns of this data,frame.

-   Minimum assumptions added to documentation.

TS1.1 Time Series Software should accept input data in as many time series specific classes as possible.

> \@rkillick:\
> No time series classes are supported. As such the user could enter the rows in the "wrong" unnatural time order and this is not checked by the software. This drastically affects all output and plotting.

-   I don't think this is true, and I've added a relevant test to show that row order doesn't matter.

> \@Paula-Moraga:\
> This is not done. The documentation does not explain the type of data that should be passed, and the R code does not check the arguments passed.

-   `bumbl()` currently doesn't support time series.

-   I don't plan to implement support for time series because I'm not very familiar with using time series with `glm()`

TS1.2 Time Series Software should implement validation routines to confirm that inputs are of acceptable classes

> \@rkillick:\
> Not met. No validation of inputs completed.
>
> \@Paula-Moraga:\
> This is not done. For example, one can call `bumbl()` with data that has a character in argument `t` and this error is not handled.

-   Added a check that the time variable, `t`, is numeric.
-   Other checks done by `glm()` (e.g. you can't have a character response variable).

TS1.3 Time Series Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions (the tsbox package provides one convenient approach for this).

> \@Paula-Moraga:\
> This is not done. There is some validation code at the beginning of the `bumbl()` function. Then other functions such as `brkpt()` do not execute this validation code.

-   This is actually how I would interpret this standard (but correct me if I'm wrong \@mpadge). I feel OK doing validation in `bumbl()` and not in `brkpt()`. This way it's only done once for the whole dataset, not one time per colony.
-   I can't pre-process the data in `bumbl()` because the pre-processing (primarily the creation of the `.post` column, representing time past the switchpoint, `tau`) is done iteratively for each colony separately as part of model fitting.

TS1.7 Accept inputs defined via the units package for attributing SI units to R vectors.

> \@rkillick:\
> Not clear if this is supported or whether it should be.

-   This doesn't seem appropriate to me as the units for time, colony growth, and covariates are not important.

TS1.8 Where time intervals or periods may be days or months, be explicit about the system used to represent such, particularly regarding whether a calendar system is used, or whether a year is presumed to have 365 days, 365.2422 days, or some other value.

> \@rkillick:\
> Documentation for bumbl function is woolly on this, needs clarification.

-   Time intervals are arbitrary, in the sense that the output `tau` will be in whatever units are used for time in the model.
-   The interpretation of tau is noted in the documentation already.

-   I now notice that if `Date`s were supplied, they were silently coerced to numeric by an `ifelse()` statement. To be safe I've restricted the time column to numeric for now, and opened an issue to properly deal with date input.

## TS2

TS2.0 Appropriate checks for missing data, and associated transformation routines, should be performed as part of initial pre-processing prior to passing data to analytic algorithms.

> \@rkillick:\
> No checks for missing data are completed.

-   I'm not sure this is relevant as `glm()` handles the missing data (see previous comments)

> \@Paula-Moraga:\
> The package does not check for missing data.\
> For example, one can execute `bumbl()` where one `d.mass` value is NA.

-   This is intended. Missing values are common in ecological data, and users of `bumbl()` should be used to understanding how `glm()` handles missing data. I want users to be able to have missing values in their data frame.

TS2.1 Time Series Software which presumes or requires regular data should only allow explicit\* missing values, and should issue appropriate diagnostic messages, potentially including errors, in response to any implicit missing values.\*

> \@rkillick:\
> No handling as no checks are made.

-   `bumbl` does not presume or require regular data, so I don't think this standard applies.

TS2.2 Where possible, all functions should provide options for users to specify how to handle missing data,

> \@rkillick:\
> No options are given to the user
>
> \@Paula-Moraga:\
> The package does not provide functions to specify how to handle missing data.

-   I've noted in the documentation that that `…` can be used to pass arguments to `glm()`. This includes arguments for handling missing data.

TS2.3 Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default na.rm = FALSE-type parameters

-   See comments in response to G2.12

TS2.5 Explicitly document all assumptions and/or requirements of stationarity

> \@rkillick:\
> Not met. Assumptions of independence also required to be clarified.
>
> \@Paula-Moraga:\
> Documentation should be improved to explain the type of arguments to be passed and possible assumptions.

-   The assumptions are the same as a GLM, so homogeneity of variance I think covers stationarity (a new term to me).

-   I now mention that GLM assumptions apply in the documentation.

TS2.6 Implement appropriate checks for all relevant forms of stationarity,

> \@rkillick:\
> Not met. In this instance, some model diagnostics would be appropriate.

-   I've added a `keep.model` argument that when `TRUE` returns the GLMs fit for each colony as a list-column. In the documentation for `bumbl()` I've referenced the vignette from `tidyr` for working with models in list-columns. Users should be able to follow this to perform model diagnostics.
-   In the updated vignette for this package, I try to emphasize that model validation is up to the user, just as it is with any GLM and show one simple example of using the list-column of `glm`s to get R^2^ values for each colony's model.

## TS5

TS5.0 Implement default plot methods for any implemented class system.

> \@rkillick:\
> Not met. Should use the plot method.

-   I've removed `bumbl_plot()` and replaced it with `plot()` and `autoplot()` methods.

TS5.5 Provide options to determine whether plots of data with missing values should generate continuous or broken lines

> \@rkillick:\
> No handling of missing values so not met.
>
> \@Paula-Moraga:\
> `bumbl_plot()` does not provide options to determine how to visualize missing data.

-   Users can obtain data for making plots by running `bumbl()` with `augment = TRUE` and handle plotting of missing values.

-   Now that time can no longer contain `NA`s, I think this issue is resolved.

TS5.8 By default provide clear visual distinction between model (input) values and forecast (output) values.

> \@Paula-Moraga:\
> Plots clearly distinguish between data which are represented by points and model predictions which are represented with red lines, but a legend would help in understanding the plot better.

-   I've described what the lines and points mean in the documentation for `plot.bumbldf()` and `autoplot.bumbldf()` (as well as links to those functions from the `bumbl()` documenation).

## Other Comments

> It would be helpful to include in the documentation a description of the problems that the package can solve and a summary of the methods implemented without the need to look at the reference provided.

-   The vignette contains a summary of the mathematical model implemented by `bumbl()` and the methods it uses.

> This description should be written in a way that is understandable for general users. For example, in this package terms such as "gyne" could be unknown for some users.

-   I've clarified that the switch is from producing workers to producing reproductive individuals.

> There is one reference to the method implemented but it is behind a paywall. If possible the reference should be accessible by everyone.

-   Unfortunately, I don't know of another reference that implements this technique. This is the paper that created this method.

> I think repeated code should be avoided. In this package, functions `brkpt()` and `brkpt.nb()`, and functions `bumbl()` and `bumbl.nb()` are very similar.

-   I agree. I originally did this to mirror the way `glm()` and `MASS::glm.nb()` exist/work, but now I've removed `bumbl.nb()` and running `bumbl()` with `family = "negbin"` now eventually calls `glm.nb()`. All other values for `family` get passed to `glm()`
