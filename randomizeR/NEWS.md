# Changes in randomizeR version 2.0

# Bug Fixes
* Minor bug fixes

# New Features
* Added a class for survival endpoints
* Added a class for exponential endpoints
* Selection bias support for exponential endpoints
* Chronological bias support for exponential endpoints
* Assessment of chronological and selection bias for exponential endpoints

## Minor Changes
* Added authors of the current version
* Added new examples 

# Major Changes
* Changed parameterization of chronological bias

# Changes in randomizeR version 1.4

# Bug Fixes
* Minor Bug fixes

# New Features
* Desirability-Functions
* Selection Bias support for K > 2
* Chronological Bias support for K > 2
* Assessment of chronological and selection bias for K > 3

# Changes in randomizeR version 1.3

## Bug Fixes
* CreateParam needs parameter N for design PBR.

# New Features
* CreateParam takes parameter filledBlock for Design RPBR and RTBD.
* Added new vignette for the Assessment and Implementation of Randomization in Clinical Trials (paper submitted to the Journal of Statistical Software)

# Minor changes
* Improved help functions for issues.
* Improved clarity of vignette comparison-example.

# Changes in randomizeR version 1.2

## Bug Fixes
* Efrons Bias Coin Admits odd N

## Minor Changes
* Changed test setting so that tests allocate less memory
* Added authors of the current version

# Changes in randomizeR version 1.1

## Bug Fixes
* Random truncated binomial design produces `NA`.

## New Features
* Added a reference card containing an overview of the functions of randomizeR, see `vignette(randomizeR-refCard )`.
* Added randomization procedures, see `?randPar`:
	+ Adjustable Biased Coin Design
	+ Bayesian Biased Coin Design
	+ Generalized Biased Coin Design
	+ Chens Design.
* Included combined additive combination of selection bias and chronological bias, see `?combineBias`.
* Included the function `saveAssess()` to save the output of the assess function to a file.

## Minor Changes
* Changed show function of `assess` objects, i.e. appearance of output.
* `saveRand()` now saves randomization list as column vector 
