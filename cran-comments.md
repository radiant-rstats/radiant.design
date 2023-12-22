## Resubmission

This is a resubmission. In this version I require shiny version 1.8.0 which fixed a bug that caused issues in the radiant apps. See NEWS.md for details.

## Test environments

* macOS (ARM) and R 4.3.2
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

# Previous cran-comments

## Resubmission

This is a resubmission. In this update I address a documentation issue connected to roxygen2

## Test environments

* macOS (ARM) and R 4.3.1
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. In this update I address the following warning. See NEWS.md for updates. 

Result: WARN
    Found the following significant warnings:
     Warning: replacing previous import ‘mvtnorm::standardize’ by ‘radiant.data::standardize’ when loading ‘radiant.design’

## Test environments

* macOS (ARM), R 4.2.3 and R 4.3.1
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. See NEWS.md for updates. I also updated the dependency on radiant.data to 1.5.0.

## Test environments

* macOS, R 4.2.2
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. In this version is have added a feature to the shiny interface to create screenshots of application settings. See NEWS.md. 

## Test environments

* macOS, R 4.2.1
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## Resubmission

This is a resubmission. In this version is addressed a function clash with `rlang` (i.e., `is_empty`) and made adjustments to work with the latest version of `shiny` and `bootstrap4`

## Test environments

* macOS, R 4.2.1
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. In this version is addressed a function clash with `rlang` (i.e., `is_empty`) and made adjustments to work with the latest version of `shiny` and `bootstrap4`

## Test environments

* local Ubuntu 20.04 install, R 4.1.0
* local Ubuntu 20.04 through WSL2, R 4.0.5
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details). This version is dependent on the new version of radiant.data that was accepted yesterday.

## Test environments

* local OS X install, R 3.6.3
* local Windows install, R 3.6.2
* Ubuntu "trusty" (on travis-ci), R oldrel, release, and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

# Previous cran-comments

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local OS X install, R 3.5.2
* local Windows install, R 3.5.2
* Ubuntu "trusty" (on travis-ci), R oldrel, release, and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

radiant.design was recently archived and removed from CRAN. Uwe Ligges confirmed that multiple emails were sent to my radiant@rady.ucsd.edu email address but, unfortunately, I cannot find any such emails in my inbox. I hope you will accept radiant.design as a resubmission.

## Test environments

* local OS X install, R 3.5.1
* local Windows install, R 3.5.1
* Ubuntu "trusty" (on travis-ci), R oldrel, release, and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

radiant.design depends on the simultaneously submitted radiant.data package. From the last time I submitted updates to CRAN I seem to recall Uwe Ligges suggested I submit all radiant packages to be updated at the same time. I hope my recollection is correct and that this is indeed the preferred approach that will minimize workload for CRAN.

## Test environments

* local OS X install, R 3.5.0
* local Windows install, R 3.5.0
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## Previous cran-comments

## Test environments

* local OS X install, R 3.3.1
* local Windows install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE: New submission
