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
This repo is currently private but will be opened when the paper is published. 
To get access now, you need a PAT from OSF - see `?osfr::osf_auth` for instructions.
The PAT needs to be saved in your `.Renviron` file. 
Use `usethis::edit_r_environ()` to edit `.Renviron`.

DO NOT commit the `.Renviron` file to GitHub.

The code will automatically download the data. 

## Running the analysis

This analysis uses the `targets` package to run the analyses.

Source file `_targets.R` to run the analyses.