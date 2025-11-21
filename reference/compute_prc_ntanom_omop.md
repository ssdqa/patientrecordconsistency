# Compute Jaccard Index based on years of follow up for not over time anomaly detection

Compute Jaccard Index based on years of follow up for not over time
anomaly detection

## Usage

``` r
compute_prc_ntanom_omop(
  cohort,
  site_col,
  grouped_list = "site",
  event_tbl = read_codeset("pes_events_1", "ccccc"),
  grp_breaks = c(0, 1, 3, 8, 11, 15, 25, 50, 100)
)
```

## Arguments

- cohort:

  table of cohort members with at least the site_col, `person_id`,
  `start_date`, and `end_date`

- site_col:

  the name of the column with the site names

- grouped_list:

  list of columns that should be used to group the table

- event_tbl:

  a table with the definitions for each event with the columns

  - `event` - A or B

  - `event_label` - a descriptive label for the event

  - `domain_tbl` - the default CDM table from which data is retrieved

  - `concept_field` - the field in the table where the codes of interest
    are stored

  - `date_field` - the date field to be used to establish the index &
    occurrence dates

  - `vocabulary_field` - (PCORnet only) The name of the column in the
    domain table where the vocabulary type is stored

  - `codeset_name` - the name of the codeset in the specs directory to
    define the variable of interest

  - `filter_logic` - a string indicating any filter logic that should be
    applied to establish the event ex: an Hba1c \> 6.5

- grp_breaks:

  a numeric vector that defines how to group different windows of follow
  up time

## Value

a dataframe containing the jaccard index of the two user defined events
within each follow up window as definted by grp_breaks
