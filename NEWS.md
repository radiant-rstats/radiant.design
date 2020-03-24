# radiant.design 1.3.4.0

* Minor adjustments in anticipation of dplyr 1.0.0

# radiant.design 1.3.0.0

* Allow for missing values in `randomizer` and `sampling` functions
* Added note that the design factors used as input to calculate a correlation using polycor::hetcor are assumed to be ordinal

# radiant.design 1.2.0.0

* Update action buttons that initiate calculations when one or more relevant inputs are changed. When, for example, an experimental design should be updated, a spinning "refresh" icon will be shown
* Allow fractions as input for the `Random assignment` tool

# radiant.design 1.1.3.0

* Added `estimable` function that can be used to determine which coefficients could be estimated based on a partial factorial design. Adapted from a function written by Blakeley MsShane at https://github.com/fzettelmeyer/mktg482/blob/master/R/expdesign.R
* Documentation updates (i.e., key functions for each tool)
* New `Random assignment` tool based on the `randomizr` package. Uses the `randomizr::block_ra` function for stratified random sampling. See the help file for more information
* Various enhancements to make _Design > Random sampling_ more flexible. See the updated help file 

# radiant.design 1.1.0.0

* Numerous small code changes to support enhanced auto-completion, tooltips, and annotations in shinyAce 0.4.1

# radiant.design 0.9.9.0

* Checked for issues with upcoming dplyr 0.8.0
* Option to pass additional arguments to `shiny::runApp` when starting radiant such as the port to use. For example, radiant.design::radiant.design("https://github.com/radiant-rstats/docs/raw/gh-pages/examples/demo-dvd-rnd.state.rda", port = 8080) 
* Use the `pwr` package for sample size calculations when comparing groups (i.e., Design > Sample size (compare))
* Load a state file on startup by providing a (relative) file path or a url

# radiant.design 0.9.7.0

## Major changes

* Using [`shinyFiles`](https://github.com/thomasp85/shinyFiles) to provide convenient access to data located on a server

## Minor changes

* Revert from `svg` to `png` for plots in `_Report > Rmd_ and _Report > R_. `svg` scatter plots with many point get to big for practical use on servers that have to transfer images to a local browser
* Removed dependency on `methods` package

# radiant.design 0.9.5.0

## Major changes

* Various changes to the code to accomodate the use of `shiny::makeReactiveBinding`. The advantage is that the code generated for _Report > Rmd_ and _Report > R_ will no longer have to use `r_data` to store and access data. This means that code generated and used in the Radiant browser interface will be directly usable without the browser interface as well.

# radiant.design 0.9.2.0

## Major changes

* Upload and download data using the Rstudio file browser. Allows using relative paths to files (e.g., data or images inside an Rstudio project)
* Enhanced keyboard shortcuts
* `Create design` button indicates when the design should be updated based on changes in user input

# radiant.design 0.8.9.0

## Minor changes

* Upgraded tidyr dependency to 0.7.2
* Upgraded dplyr dependency to 0.7.4
* Applied `styler` on code

# radiant.design 0.8.1.0

## Minor changes

- Code cleanup
- Documentation updates

## Bug fixes

- Fix for incomplete final line warning in sampling.md

# radiant.design 0.8.0.0

## Minor changes

- Option to set random seed in Design > Sampling
- UI updates for DOE
- Show df name in output
- Use ALT-enter as a short-cut to report
- Documentation added on how to customize plots

## Bug fixes

- Fix for random seed when input is NA
- Cleanup report arguments for sample size calculations
- Print full factorial up to 5,000 lines
- Check that return value from optFederov was not a try-error

## Deprecated

- Use of *_each is deprecated
