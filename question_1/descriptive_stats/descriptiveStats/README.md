descriptiveStats
================

# descriptiveStats

The goal of descriptiveStats is to provide helper functions for commonly
used descriptive statistics, such as mean, median and mode. It also
includes functions to estimate quartiles (Q1, Q3 and interquartile
range) to better understand data distribution.

The package currently includes six functions: `calc_mean`,
`calc_median`, `calc_mode`, `calc_q1`, `calc_q3` and `calc_iqr`. All
functions validate inputs using checkmate, handle missing values
consistently, provide informative warnings and errors and rely on stable
base R or well-established packages.

## Installation

You can install the development version of descriptiveStats from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("GilKCL/ads_assessment/question_1/descriptive_stats/descriptiveStats")
```

## Example

All exported functions require a numeric input. For example, to
calculate the mean:

``` r
library(descriptiveStats)

data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
calc_mean(data)
#> [1] 4.3
```

To calculate the interquartile range:

``` r
data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
calc_iqr(data)
#> [1] 2.75
```

The functions will error if input is NULL or include non-numerical
values. For example:

``` r
data <- c(1, 2, 2, "3", 4, 5, 5, 5, 6, 10)
calc_mode(data)
```

If the input contains missing data, a warning will be generated. NA
values will be removed and computation will be performed on the
remaining values as shown below:

``` r
data <- c(1, 2, 2, NA, 4, 5, 5, 5, 6, 10)
calc_median(data)
```
