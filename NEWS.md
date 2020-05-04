# whippr 0.0.0.9000

* Added function `read_data()` to read data from metabolic cart (COSMED and CORTEX).
    * Now you can specify the name of the time column, in case your system is not in English. Default is set to "t".
    * Added option to read data from NSpire system (thanks to [@peter__leo](https://twitter.com/peter__leo)).
    * Added option to read data from Parvo Medics system (thanks to [@EatsleepfitJeff](https://twitter.com/EatsleepfitJeff)).
    
* Added function `interpolate()` to interpolate breath-by-breath data into second-by-second.

* Added function `perform_average()` to perform bin- and rolling-averages.

* Improved error messages in case `read_data()` cannot find the name of the time column provided.

* Removed the `time_column` argument from the `interpolate()` and `perform_average()` functions. This is only necessary in `read_data()` now.

* Make sure that data does not contain rows and cols with only `NA` in `interpolate()` (thanks @Scott-S-Hannah)

## VO2 kinetics analysis

* Added set of tools for VO2 kinetics analysis
