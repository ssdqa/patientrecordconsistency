# *Single Site, Anomaly Detection, Longitudinal*

*Single Site, Anomaly Detection, Longitudinal*

## Usage

``` r
prc_ss_anom_la(process_output, event_filter, facet = NULL)
```

## Arguments

- process_output:

  the output from `prc_process`

- event_filter:

  the event type of interest for the analysis; can be either `a`, `b`,
  `both`, or `neither`

- facet:

  vector of variables to be used to facet the graph

## Value

if analysis was executed by year or greater, a P Prime control chart is
returned with outliers marked with orange dots

        if analysis was executed by month or smaller, an STL regression is
        conducted and outliers are marked with red dots. the graphs representing
        the data removed in the regression are also returned
