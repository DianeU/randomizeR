# Changes in randomizeR version 1.1.9000

* Desirability-Functions (Mui)

Please write new changes above this line.
____________________________________________
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