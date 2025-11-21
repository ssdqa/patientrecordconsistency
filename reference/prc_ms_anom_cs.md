# *Multi Site, Anomaly Detection, Cross-Sectional*

*Multi Site, Anomaly Detection, Cross-Sectional*

## Usage

``` r
prc_ms_anom_cs(process_output, large_n = FALSE, large_n_sites = NULL)
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

a dot plot where the shape of the dot represents whether the point is
anomalous, the color of the dot represents the jaccard similarity index
for a given follow up window, and the size of the dot represents the
mean index across all sites

        if there were no groups eligible for analysis, a heat map showing the jaccard index
        and a dot plot showing each site's average standard deviation away from the mean
        index is returned instead
