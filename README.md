
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whippr <img src='man/figures/logo.png' align="right" height="240" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/whippr)](https://CRAN.R-project.org/package=whippr)
[![Codecov test
coverage](https://codecov.io/gh/fmmattioni/whippr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/fmmattioni/whippr?branch=master)
[![R-CMD-check](https://github.com/fmmattioni/whippr/workflows/R-CMD-check/badge.svg)](https://github.com/fmmattioni/whippr/actions)
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

### Read data

``` r
library(whippr)

## example file that comes with the package for demonstration purposes
path_example <- system.file("example_cosmed.xlsx", package = "whippr")

df <- read_data(path = path_example, metabolic_cart = "cosmed")

df
#> # Metabolic cart: COSMED 
#> # Data status: raw data
#> # Time column: t
#> # A tibble: 754 × 119
#>        t    Rf    VT    VE   VO2  VCO2 O2exp CO2exp `VE/VO2` `VE/VCO2` `VO2/Kg`
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>     <dbl>    <dbl>
#>  1     2  8.08 1.19   9.60  380.  301.  185.   52.9     25.3      31.9     4.58
#>  2     4 23.2  0.915 21.2   864.  665.  141.   40.8     24.5      31.9    10.4 
#>  3     8 15.6  2.11  32.9  1317. 1075.  325.   97.2     25.0      30.6    15.9 
#>  4    11 20.6  1.18  24.4   894.  714.  188.   49.2     27.3      34.1    10.8 
#>  5    14 23.3  0.947 22.1   822.  647.  150.   39.4     26.9      34.1     9.90
#>  6    18 14.7  2.28  33.6  1347. 1126.  351.  108.      24.9      29.8    16.2 
#>  7    23 11.2  2.32  26.1   980.  848.  364.  107.      26.6      30.7    11.8 
#>  8    28 13.2  2.18  28.8  1147.  981.  336.  105.      25.2      29.4    13.8 
#>  9    31 17.7  1.51  26.7  1048.  860.  234.   68.8     25.5      31.0    12.6 
#> 10    35 14.2  1.68  23.8   973.  794.  257.   79.3     24.5      30.0    11.7 
#> # … with 744 more rows, and 108 more variables: R <dbl>, FeO2 <dbl>,
#> #   FeCO2 <dbl>, HR <dbl>, `VO2/HR` <dbl>, Load1 <dbl>, Load2 <dbl>,
#> #   Load3 <dbl>, Phase <dbl>, Marker <lgl>, FetO2 <dbl>, FetCO2 <dbl>,
#> #   FiO2 <dbl>, FiCO2 <dbl>, Ti <dbl>, Te <dbl>, Ttot <dbl>, `Ti/Ttot` <dbl>,
#> #   IV <dbl>, PetO2 <dbl>, PetCO2 <dbl>, `P(a-et)CO2` <dbl>, SpO2 <dbl>,
#> #   `VD(phys)` <dbl>, `VD/VT` <dbl>, `Env. Temp.` <dbl>, `Analyz. Temp.` <dbl>,
#> #   `Analyz. Press.` <dbl>, `Env. Press.` <dbl>, Batteries <dbl>, …
```

### Interpolate

``` r
df %>% 
  interpolate()
#> # Metabolic cart: COSMED 
#> # Data status: interpolated data
#> # Time column: t
#> # A tibble: 2,159 × 114
#>        t    Rf    VT    VE   VO2  VCO2 O2exp CO2exp `VE/VO2` `VE/VCO2` `VO2/Kg`
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>     <dbl>    <dbl>
#>  1     2  8.08 1.19   9.60  380.  301.  185.   52.9     25.3      31.9     4.58
#>  2     3 15.6  1.05  15.4   622.  483.  163.   46.8     24.9      31.9     7.50
#>  3     4 23.2  0.915 21.2   864.  665.  141.   40.8     24.5      31.9    10.4 
#>  4     5 21.3  1.21  24.1   978.  767.  187.   54.9     24.6      31.6    11.8 
#>  5     6 19.4  1.51  27.1  1091.  870.  233.   69.0     24.8      31.3    13.1 
#>  6     7 17.5  1.81  30.0  1204.  973.  279.   83.1     24.9      30.9    14.5 
#>  7     8 15.6  2.11  32.9  1317. 1075.  325.   97.2     25.0      30.6    15.9 
#>  8     9 17.3  1.80  30.1  1176.  955.  279.   81.2     25.7      31.8    14.2 
#>  9    10 19.0  1.49  27.2  1035.  834.  233.   65.2     26.5      33.0    12.5 
#> 10    11 20.6  1.18  24.4   894.  714.  188.   49.2     27.3      34.1    10.8 
#> # … with 2,149 more rows, and 103 more variables: R <dbl>, FeO2 <dbl>,
#> #   FeCO2 <dbl>, HR <dbl>, `VO2/HR` <dbl>, Load1 <dbl>, Load2 <dbl>,
#> #   Load3 <dbl>, Phase <dbl>, FetO2 <dbl>, FetCO2 <dbl>, FiO2 <dbl>,
#> #   FiCO2 <dbl>, Ti <dbl>, Te <dbl>, Ttot <dbl>, `Ti/Ttot` <dbl>, IV <dbl>,
#> #   PetO2 <dbl>, PetCO2 <dbl>, `P(a-et)CO2` <dbl>, SpO2 <dbl>,
#> #   `VD(phys)` <dbl>, `VD/VT` <dbl>, `Env. Temp.` <dbl>, `Analyz. Temp.` <dbl>,
#> #   `Analyz. Press.` <dbl>, `Env. Press.` <dbl>, Batteries <dbl>, …
```

### Perform averages

#### Bin-average

``` r
## example of performing 30-s bin-averages
df %>% 
  interpolate() %>% 
  perform_average(type = "bin", bins = 30)
#> # Metabolic cart: COSMED 
#> # Data status: averaged data - 30-s bins
#> # Time column: t
#> # A tibble: 73 × 114
#>        t    Rf    VT    VE   VO2  VCO2 O2exp CO2exp `VE/VO2` `VE/VCO2` `VO2/Kg`
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>     <dbl>    <dbl>
#>  1     0  19.0  1.33  24.0  932.  744.  207.   58.9     25.8      32.5     11.2
#>  2    30  15.3  1.85  27.1 1097.  904.  284.   87.0     24.8      30.1     13.2
#>  3    60  19.6  1.47  27.1 1133.  892.  223.   69.6     24.1      30.7     13.7
#>  4    90  13.3  2.29  26.0 1043.  885.  353.  111.      24.9      29.5     12.6
#>  5   120  20.5  1.43  27.1 1107.  883.  218.   66.9     24.6      31.0     13.3
#>  6   150  14.4  1.57  22.1  928.  751.  239.   75.5     24.1      29.7     11.2
#>  7   180  23.0  1.18  26.4 1071.  849.  180.   54.4     24.8      31.3     12.9
#>  8   210  16.1  2.17  28.7 1070.  941.  342.  101.      27.0      30.6     12.9
#>  9   240  18.9  1.43  26.1 1058.  880.  219.   68.8     24.7      29.8     12.7
#> 10   270  15.1  1.65  24.5  987.  847.  253.   81.4     24.8      28.9     11.9
#> # … with 63 more rows, and 103 more variables: R <dbl>, FeO2 <dbl>,
#> #   FeCO2 <dbl>, HR <dbl>, `VO2/HR` <dbl>, Load1 <dbl>, Load2 <dbl>,
#> #   Load3 <dbl>, Phase <dbl>, FetO2 <dbl>, FetCO2 <dbl>, FiO2 <dbl>,
#> #   FiCO2 <dbl>, Ti <dbl>, Te <dbl>, Ttot <dbl>, `Ti/Ttot` <dbl>, IV <dbl>,
#> #   PetO2 <dbl>, PetCO2 <dbl>, `P(a-et)CO2` <dbl>, SpO2 <dbl>,
#> #   `VD(phys)` <dbl>, `VD/VT` <dbl>, `Env. Temp.` <dbl>, `Analyz. Temp.` <dbl>,
#> #   `Analyz. Press.` <dbl>, `Env. Press.` <dbl>, Batteries <dbl>, …
```

#### Rolling-average

``` r
## example of performing 30-s rolling-averages
df %>% 
  interpolate() %>% 
  perform_average(type = "rolling", rolling_window = 30)
#> # Metabolic cart: COSMED 
#> # Data status: averaged data - 30-s rolling average
#> # Time column: t
#> # A tibble: 2,130 × 114
#>        t    Rf    VT    VE   VO2  VCO2 O2exp CO2exp `VE/VO2` `VE/VCO2` `VO2/Kg`
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>     <dbl>    <dbl>
#>  1  16.5  16.4  1.75  26.5 1033.  852.  271.   80.1     25.7      31.3     12.4
#>  2  17.5  16.6  1.76  27.0 1054.  870.  273.   80.7     25.7      31.3     12.7
#>  3  18.5  16.7  1.78  27.3 1067.  882.  276.   81.6     25.7      31.3     12.9
#>  4  19.5  16.4  1.80  27.4 1071.  887.  280.   82.8     25.7      31.2     12.9
#>  5  20.5  16.2  1.82  27.4 1071.  888.  282.   83.6     25.7      31.1     12.9
#>  6  21.5  16.0  1.82  27.3 1068.  885.  282.   83.8     25.7      31.1     12.9
#>  7  22.5  16.0  1.81  27.1 1062.  880.  280.   83.4     25.7      31.1     12.8
#>  8  23.5  16.0  1.78  26.9 1052.  871.  277.   82.4     25.6      31.0     12.7
#>  9  24.5  16.1  1.77  26.7 1048.  867.  274.   81.8     25.5      31.0     12.6
#> 10  25.5  16.1  1.76  26.6 1050.  868.  273.   81.9     25.4      30.8     12.6
#> # … with 2,120 more rows, and 103 more variables: R <dbl>, FeO2 <dbl>,
#> #   FeCO2 <dbl>, HR <dbl>, `VO2/HR` <dbl>, Load1 <dbl>, Load2 <dbl>,
#> #   Load3 <dbl>, Phase <dbl>, FetO2 <dbl>, FetCO2 <dbl>, FiO2 <dbl>,
#> #   FiCO2 <dbl>, Ti <dbl>, Te <dbl>, Ttot <dbl>, `Ti/Ttot` <dbl>, IV <dbl>,
#> #   PetO2 <dbl>, PetCO2 <dbl>, `P(a-et)CO2` <dbl>, SpO2 <dbl>,
#> #   `VD(phys)` <dbl>, `VD/VT` <dbl>, `Env. Temp.` <dbl>, `Analyz. Temp.` <dbl>,
#> #   `Analyz. Press.` <dbl>, `Env. Press.` <dbl>, Batteries <dbl>, …
```

### Perform VO<sub>2</sub> kinetics analysis

``` r
results_kinetics <- vo2_kinetics(
  .data = df,
  intensity_domain = "moderate",
  vo2_column = "VO2",
  protocol_n_transitions = 3,
  protocol_baseline_length = 360,
  protocol_transition_length = 360,
  cleaning_level = 0.95,
  cleaning_baseline_fit = c("linear", "exponential", "exponential"),
  fit_level = 0.95,
  fit_bin_average = 5,
  fit_phase_1_length = 20,
  fit_baseline_length = 120,
  fit_transition_length = 240,
  verbose = TRUE
)
#> ──────────────────────────  * V̇O₂ kinetics analysis *  ─────────────────────────
#> ✔ Detecting outliers
#> • 14 outlier(s) found in transition 1
#> • 15 outlier(s) found in transition 2
#> • 13 outlier(s) found in transition 3
#> ✔ Processing data...
#> ✔       └─ Removing outliers
#> ✔       └─ Interpolating each transition
#> ✔       └─ Ensemble-averaging transitions
#> ✔       └─ Performing 5-s bin averages
#> ✔ Fitting data...
#> ✔       └─ Fitting baseline
#> ✔       └─ Fitting transition
#> ✔       └─ Calculating residuals
#> ✔       └─ Preparing plots
#> ──────────────────────────────────  * DONE *  ──────────────────────────────────
```

### Perform VO<sub>2max</sub> analysis

``` r
df_incremental <- read_data(path = system.file("ramp_cosmed.xlsx", package = "whippr"), metabolic_cart = "cosmed")

vo2_max(
  .data = df_incremental, ## data from `read_data()`
  vo2_column = "VO2",
  vo2_relative_column = "VO2/Kg",
  heart_rate_column = "HR",
  rer_column = "R",
  detect_outliers = TRUE,
  average_method = "bin",
  average_length = 30,
  plot = TRUE,
  verbose = TRUE,
  ## arguments for `incremental_normalize()`
  incremental_type = "ramp",
  has_baseline = TRUE,
  baseline_length = 240, ## 4-min baseline
  work_rate_magic = TRUE, ## produce a work rate column
  baseline_intensity = 20, ## baseline was performed at 20 W
  ramp_increase = 25, ## 25 W/min ramp
  ## arguments for `detect_outliers()`
  test_type = "incremental",
  cleaning_level = 0.95, 
  method_incremental = "linear"
)
#> ────────────────────────────  * V̇O₂ max analysis *  ────────────────────────────
#> ✔ Normalizing incremental data...
#> ✔ Detecting outliers
#> • 2 outlier(s) found in baseline
#> • 15 outlier(s) found in ramp
#> ✔ Filtering out outliers...
#> ✔ Interpolating from breath-by-breath into second-by-second...
#> ✔ Performing averages...
#> # A tibble: 1 × 6
#>   VO2max_absolute VO2max_relative POpeak HRmax RERmax plot  
#>             <dbl>           <dbl>  <int> <dbl>  <dbl> <list>
#> 1           3524.            46.0    303   193   1.13 <gg>
```

## Metabolic carts currently supported

-   [COSMED](https://www.cosmed.com/en/)
-   [CORTEX](https://cortex-medical.com/EN)
-   [NSpire](https://www.pressebox.de/pressemitteilung/nspire-health-gmbh/ZAN-100-Diagnostische-Spirometrie/boxid/745555)
-   Parvo Medics
-   [Geratherm
    Respiratory](https://www.geratherm-respiratory.com/product-groups/cpet/)
-   [CardioCoach](https://korr.com/go/cardiocoach/)

## Online app

Would you like to perform VO<sub>2</sub> kinetics analyses but don’t
know R? No problem! You can use our online app: [VO2 Kinetics
App](https://exphyslab.com/kinetics/)

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct.html).
By participating in this project you agree to abide by its terms.

<div>

Icons made by
<a href="https://www.flaticon.com/authors/monkik" title="monkik">monkik</a>
from
<a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a>

</div>
