# CHANGES IN radiant.design 0.9.0.1



# CHANGES IN radiant.design 0.9.0

* Enhanced keyboard shortcuts
* `Create design` button indicates when the design should be updated based on changes in user input

# CHANGES IN radiant.design 0.8.9

* Upgraded tidyr dependency to 0.7.2
* Upgraded dplyr dependency to 0.7.4
* Applied `styler` on code

# CHANGES IN radiant.design 0.8.1

- Code cleanup
- Documentation updates
- Fix for incomplete final line warning in sampling.md

# CHANGES IN radiant.design 0.8.0

## Feature

- option to set random seed in Design > Sampling
- UI updates for DOE
- Show df name in output
- Use ALT-enter as a short-cut to report
- Documentation added on how to customize plots

## BUG FIXES

- Fix for random seed when input is NA
- Cleanup report arguments for sample size calculations
- Print full factorial up to 5,000 lines
- Check that return value from optFederov was not a try-error

## Deprecated

- Use of *_each is deprecated
