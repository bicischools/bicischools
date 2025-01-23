
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bicischools

<!-- badges: start -->

[![R-CMD-check](https://github.com/bicischools/bicischools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bicischools/bicischools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of bicischools is to provide functions supporting development
of ‘bike bus’ routes.

Work in progress…

## Installation

You can install the development version of bicischools from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("bicischools/bicischools")
```

<!-- ## Setup -->
<!-- The info below shows how we set-up the package. -->

``` r
# Create readme:
usethis::use_readme_rmd()
# get deps
getwd()
pkg_deps = renv::dependencies("code/case-study-school.R")
names(pkg_deps)
pkg_deps = as.data.frame(pkg_deps)
pkg_deps$Package
lapply(pkg_deps$Package, usethis::use_package)
# See if checks are happy now:
devtools::check()
usethis::use_mit_license()
usethis::use_github_action("check-standard")
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
