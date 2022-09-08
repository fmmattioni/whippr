# whippr 0.1.2

* Fixed a bug in `read_data.cosmed()` that made the time column to return `NA` values when the test was longer than one hour.
* Added a `custom` option to `read_data()`.

# whippr 0.1.1

* Updated docs with `roxygen 7.2.1`.

# whippr 0.1.0

* General cleanup has been performed to reduce dependencies.

# whippr 0.0.0.9000

## Breaking changes

* The following function calls were simplified:

    * `vo2_kinetics()` and `detect_outliers()` = `time_column` argument not needed anymore (this is automatically taken from `read_data()` now).
    * `plot_outliers()` = `test_type` argument not needed anymore (this is automatically taken from `detect_outliers()` now).
    * `perform_kinetics()` = `time_column` and `vo2_column` arguments not needed anymore (this is automatically taken from `read_data()` and `detect_outliers()`).

## New functions/methods

* Added function `read_data()` to read data from metabolic cart (COSMED and CORTEX).
    * Now you can specify the name of the time column, in case your system is not in English. Default is set to "t".
    * Added option to read data from NSpire system (thanks to [@peter__leo](https://twitter.com/peter__leo)).
    * Added option to read data from Parvo Medics system (thanks to [@EatsleepfitJeff](https://twitter.com/EatsleepfitJeff)).
    * Added option to read data from Geratherm Respiratory system (thanks to [@marcorsini61](https://twitter.com/marcorsini61))
    
* Added function `interpolate()` to interpolate breath-by-breath data into second-by-second.

* Added function `perform_average()` to perform bin- and rolling-averages.

* Added `run_manual_cleaner()`.

* Added testing of functions (internal modification only, not visible to the user).

* Added new print method.

* Added new functionality for analyzing data from incremental exercise.

* Added `perform_max()` and `vo2_max()`.

* Added support for the `CardioCoach` metabolic cart.

## Bug fixes

* Fixed issue with `read_data()` when using the COSMED metabolic cart: previously, character columns were being coerced into `NA`(thanks @Scott-S-Hannah #4).

* Added extra argument to `read_data()` to automatically fix the issue when the work rate column is coerced as a character column (thanks to [@ThibauxV](https://twitter.com/ThibauxV)).

* Improved error messages in case `read_data()` cannot find the name of the time column provided.

* Removed the `time_column` argument from the `interpolate()` and `perform_average()` functions. This is only necessary in `read_data()` now.

* Make sure that data does not contain rows and cols with only `NA` in `interpolate()` (thanks @Scott-S-Hannah).

* Fixed issue renaming work rate column in `read_data()` (thanks @Scott-S-Hannah #6).

## VO2 kinetics analyses

* Added a set of tools for VO2 kinetics analyses.

## Incremental test analyses

* Added a set of tools for incremental test analyses: data standardization and normalization, detection of 'bad breaths', mean response time, maximal values (i.e., VO2max, HRmax, maximal RER, etc), and ventilatory thresholds.
