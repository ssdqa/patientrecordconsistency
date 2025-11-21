# *Multi Site, Exploratory, Longitudinal*

*Multi Site, Exploratory, Longitudinal*

## Usage

``` r
prc_ms_exp_la(
  process_output,
  dist_from_stat = "mean",
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  the output from `prc_process`

- dist_from_stat:

  the statistic from which distance should be measured acceptable values
  are `mean` or `median`

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a line graph displaying the distance from the overall mean or median
proportion of patients for each site and event type
