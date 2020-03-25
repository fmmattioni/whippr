# whippr 0.0.0.9000

* Added function `read_data()` to read data from metabolic cart (COSMED and CORTEX).
    * Now you can specify the name of the time column, in case your system is not in English. Default is set to "t".
    * Added option to read data from NSpire system (thanks to [@peter__leo](https://twitter.com/peter__leo))
    
* Added function `interpolate()` to interpolate breath-by-breath data into second-by-second.

* Added function `perform_average()` to perform bin- and rolling-averages.

* Improved error messages in case `read_data()` cannot find the name of the time column provided.
