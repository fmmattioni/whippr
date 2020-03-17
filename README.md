
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whippr <img src='man/figures/logo.png' align="right" height="240" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/whippr)](https://CRAN.R-project.org/package=whippr)
<!-- badges: end -->

The goal of `whippr` is to provide a set of tools for manipulating gas
exchange data from cardiopulmonary exercise testing.

## Why `whippr`?

The name of the package is in honor of [Prof. Brian J
Whipp](https://erj.ersjournals.com/content/39/1/1) and his invaluable
contribution to the field of exercise physiology.

## Installation

You can install the development version of `whippr` from
[Github](https://github.com/fmmattioni/whippr) with:

``` r
# install.packages("remotes")
remotes::install_github("fmmattioni/whippr")
```

## Use

So far, the package has one function only: `read_data()`, which reads in
the exported raw data from the metabolic cart. Currently, data from
**COSMED** and **CORTEX** are supported. In case you have data from a
different system, please contact me and I will be happy to add that to
the function.

The package is still in its
[experimental](https://www.tidyverse.org/lifecycle/#experimental) phase,
do do not hesitate to open any issues and/or suggest improvements.

### Functions to be added

  - interpolation
  - bin- and rolling-averaging
  - gas exchange threshold and respiratory compensation point detection
  - outliers (“bad breaths”) detection

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct.html).
By participating in this project you agree to abide by its
terms.

## Support

<a href="https://www.buymeacoffee.com/XQauwUWGm" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" style="height: 41px !important;width: 174px !important;box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;-webkit-box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;" ></a>

<div>

Icons made by
<a href="https://www.flaticon.com/authors/monkik" title="monkik">monkik</a>
from
<a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a>

</div>
