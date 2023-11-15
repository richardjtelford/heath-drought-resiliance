# LANDPRESS Resilience analysis

This repo includes code to reproduce the analyses in the LANDPRESS resilience paper.

## Install packages

This analysis uses the `renv` packages to keep track of the version of all R packages used.
To install all the packages, run

```r
renv::restore()
```

## Accessing the data

The raw data are on OSF at https://osf.io/mv84d/

The code will automatically download the data. 

## Running the analysis

This analysis uses the `targets` package to download the data and run the analyses.

File `run.R` will run the analysis with function `targets::tar_make()`.