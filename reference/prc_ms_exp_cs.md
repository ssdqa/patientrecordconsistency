# *Multi Site, Exploratory, Cross-Sectional*

*Multi Site, Exploratory, Cross-Sectional*

## Usage

``` r
prc_ms_exp_cs(process_output, large_n = FALSE, large_n_sites = NULL)
```

## Arguments

- process_output:

  the output from `prc_process`

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a bar graph displaying the proportion of patients at each site with only
event a, only event b, both events, or neither event present in their
record
