# Compute the counts of each event within the patient record and across the cohort

Compute the counts of each event within the patient record and across
the cohort

## Usage

``` r
compute_event_counts_omop(
  cohort,
  grouped_list,
  site_col,
  time = FALSE,
  event_tbl = patientrecordconsistency::prc_event_file
)
```

## Arguments

- cohort:

  table of cohort members with at least the site_col, `person_id`,
  `start_date`, and `end_date`

- grouped_list:

  list of columns that should be used to group the table

- site_col:

  the name of the column with the site names

- time:

  logical to determine whether to output the check across time

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

## Value

a dataframe with the proportion of patients that have event A, have
event B, have both events, or have neither event within their patient
record
