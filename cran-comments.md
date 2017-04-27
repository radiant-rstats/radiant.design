## Resubmission

This is a resubmission (0.8.0). In this version I have fixed some bugs and added 
some new features (see NEWS.md for details).

Please note that this version addresses the reverse dependency check warnings from radiant.data for radiant.design. Deprecating the `*_each` commands used in the 0.6.0 versions of the `radiant.*` packages is related to the deprecation of the `*_each` functions in dplyr. I will update the remaining `radiant.*` packages asap.

## Test environments

* local OS X install, R 3.3.3
* local Windows install, R 3.3.3
* ubuntu 14.04 (on travis-ci), R 3.3.3 and R-dev
* win-builder (release)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE about a possibly mis-
spelled word (see below). The spelling is correct however.

Possibly mis-spelled words in DESCRIPTION:
  Analytics (2:40)
  
## Previous cran-comments

## Test environments

* local OS X install, R 3.3.1
* local Windows install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE: New submission
