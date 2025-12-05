# Patient Record Consistency

This is a concordance module that will assess consistency within a
patient record by evaluating the presence or absence of two
user-provided clinical events (`prc_event_file`). The checks in the
module will establish whether one of, neither, or both events are
present in each patient record and summarize these results across the
full cohort. A sample version of the input file is accessible with
`patientrecordconsistency::`. This function is compatible with both the
OMOP and the PCORnet CDMs based on the user's selection.

## Usage

``` r
prc_process(
  cohort,
  prc_event_file,
  omop_or_pcornet,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  age_groups = NULL,
  patient_level_tbl = FALSE,
  fu_breaks = c(0, 1, 3, 8, 11, 15, 25, 50, 100),
  p_value = 0.9,
  time = FALSE,
  time_span = c("2012-01-01", "2020-01-01"),
  time_period = "year"
)
```

## Arguments

- cohort:

  *tabular input* \|\| **required**

  The cohort to be used for data quality testing. This table should
  contain, at minimum:

  - `site` \| *character* \| the name(s) of institutions included in
    your cohort

  - `person_id` / `patid` \| *integer* / *character* \| the patient
    identifier

  - `start_date` \| *date* \| the start of the cohort period

  - `end_date` \| *date* \| the end of the cohort period

  Note that the start and end dates included in this table will be used
  to limit the search window for the analyses in this module.

- prc_event_file:

  *tabular input* \|\| **required**

  A table with the definitions of each of the events. This table should
  contain two rows, one for each event, with the following columns:

  - `event` \| *character* \| a string, either A or B, representing the
    event type. A will be treated as the event that is expected to occur
    first in a sequence

  - `event_label` \| *character* \| a descriptive label for the event

  - `domain_tbl` \| *character* \| the name of the CDM table where the
    event is defined

  - `concept_field` \| *character* \| the string name of the field in
    the domain table where the concepts are located

  - `date_field` \| *character* \| the name of the field in the domain
    table with the date that should be used for temporal filtering

  - `vocabulary_field` \| *character* \| for PCORnet applications, the
    name of the field in the domain table with a vocabulary identifier
    to differentiate concepts from one another (ex: dx_type); can be set
    to NA for OMOP applications

  - `codeset_name` \| *character* \| the name of the codeset that
    defines the event of interest

  - `filter_logic` \| *character* \| logic to be applied to the
    domain_tbl in order to achieve the definition of interest; should be
    written as if you were applying it in a dplyr::filter command in R

  To see an example of this input, see
  [`?patientrecordconsistency::prc_event_file`](https://ssdqa.github.io/patientrecordconsistency/reference/prc_event_file.md)

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

  - `omop`: run the
    [`prc_process_omop()`](https://ssdqa.github.io/patientrecordconsistency/reference/prc_process_omop.md)
    function against an OMOP CDM instance

  - `pcornet`: run the
    [`prc_process_pcornet()`](https://ssdqa.github.io/patientrecordconsistency/reference/prc_process_pcornet.md)
    function against a PCORnet CDM instance

- multi_or_single_site:

  *string* \|\| defaults to `single`

  A string, either `single` or `multi`, indicating whether a single-site
  or multi-site analysis should be executed

- anomaly_or_exploratory:

  *string* \|\| defaults to `exploratory`

  A string, either `anomaly` or `exploratory`, indicating what type of
  results should be produced.

  Exploratory analyses give a high level summary of the data to examine
  the fact representation within the cohort. Anomaly detection analyses
  are specialized to identify outliers within the cohort.

- age_groups:

  *tabular input* \|\| defaults to `NULL`

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and use it as input to this
  parameter:

  - `min_age` \| *integer* \| the minimum age for the group (i.e. 10)

  - `max_age` \| *integer* \| the maximum age for the group (i.e. 20)

  - `group` \| *character* \| a string label for the group (i.e. 10-20,
    Young Adult, etc.)

  If you would *not* like to stratify by age group, leave as `NULL`

- patient_level_tbl:

  *boolean* \|\| defaults to `FALSE`

  A boolean indicating whether an additional table with patient level
  results should be output.

  If `TRUE`, the output of this function will be a list containing both
  the summary and patient level output. Otherwise, this function will
  just output the summary dataframe

- fu_breaks:

  *vector* \|\| defaults to `c(0, 1, 3, 8, 11, 15, 25, 50, 100)`

  A numeric vector that defines how to group different windows of follow
  up time. This parameter is used for both
  `Anomaly Detection, Cross-Sectional` checks

- p_value:

  *numeric* \|\| defaults to `0.9`

  The p value to be used as a threshold in the Multi-Site, Anomaly
  Detection, Cross-Sectional analysis

- time:

  *boolean* \|\| defaults to `FALSE`

  A boolean to indicate whether to execute a longitudinal analysis

- time_span:

  *vector - length 2* \|\| defaults to `c('2012-01-01', '2020-01-01')`

  A vector indicating the lower and upper bounds of the time series for
  longitudinal analyses

- time_period:

  *string* \|\| defaults to `year`

  A string indicating the distance between dates within the specified
  time_span. Defaults to `year`, but other time periods such as `month`
  or `week` are also acceptable

## Value

This function will return a dataframe summarizing the co-occurrence of
events within a patient record. For a more detailed description of
output specific to each check type, see the PEDSpace metadata repository

## Examples

``` r
#' Source setup file
source(system.file('setup.R', package = 'patientrecordconsistency'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'prc_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)
#> Connected to: :memory:@NA

#' Build mock study cohort
cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-5000), # RSQLite does not store date objects,
                                      # hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Build function input table
prc_events <- tidyr::tibble(event = c('a', 'b'),
                            event_label = c('hypertension', 'inpatient/ED visit'),
                            domain_tbl = c('condition_occurrence', 'visit_occurrence'),
                            concept_field = c('condition_concept_id', 'visit_concept_id'),
                            date_field = c('condition_start_date', 'visit_start_date'),
                            vocabulary_field = c(NA, NA),
                            codeset_name = c('dx_hypertension', 'visit_edip'),
                            filter_logic = c(NA, NA))

#' Execute `prc_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
prc_process_example <- prc_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   prc_event_file = prc_events) %>%
  suppressMessages()
#> ┌ Output Function Details ──────────────────────────────────────┐
#> │ You can optionally use this dataframe in the accompanying     │
#> │ `prc_output` function. Here are the parameters you will need: │
#> │                                                               │
#> │ Always Required: process_output                               │
#> │                                                               │
#> │ See ?prc_output for more details.                             │
#> └───────────────────────────────────────────────────────────────┘

prc_process_example
#> # A tibble: 5 × 8
#>   site     event_a_num event_a_name event_b_num event_b_name     pt_ct total_pts
#>   <chr>          <dbl> <chr>              <dbl> <chr>            <int>     <int>
#> 1 combined           0 hypertension           0 inpatient/ED vi…     6        12
#> 2 combined           0 hypertension           1 inpatient/ED vi…     1        12
#> 3 combined           1 hypertension           1 inpatient/ED vi…     2        12
#> 4 combined           1 hypertension           2 inpatient/ED vi…     2        12
#> 5 combined           1 hypertension           5 inpatient/ED vi…     1        12
#> # ℹ 1 more variable: output_function <chr>

#' Execute `prc_output` function
prc_output_example <- prc_output(process_output = prc_process_example)

prc_output_example


#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(prc_output_example)

{"x":{"html":"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='ggiraph-svg' role='graphics-document' id='svg_e9e96ad491e95a97' viewBox='0 0 432 360'>\n <defs id='svg_e9e96ad491e95a97_defs'>\n  <clipPath id='svg_e9e96ad491e95a97_c1'>\n   <rect x='0' y='0' width='432' height='360'/>\n  <\/clipPath>\n  <clipPath id='svg_e9e96ad491e95a97_c2'>\n   <rect x='37.44' y='23.32' width='365.5' height='304.97'/>\n  <\/clipPath>\n <\/defs>\n <g id='svg_e9e96ad491e95a97_rootg' class='ggiraph-svg-rootg'>\n  <g clip-path='url(#svg_e9e96ad491e95a97_c1)'>\n   <rect x='0' y='0' width='432' height='360' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.75' stroke-linejoin='round' stroke-linecap='round' class='ggiraph-svg-bg'/>\n   <rect x='0' y='0' width='432' height='360' fill='#FFFFFF' fill-opacity='1' stroke='none'/>\n  <\/g>\n  <g clip-path='url(#svg_e9e96ad491e95a97_c2)'>\n   <polyline points='37.44,286.71 402.94,286.71' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,231.26 402.94,231.26' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,175.81 402.94,175.81' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,120.36 402.94,120.36' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,64.91 402.94,64.91' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,314.43 402.94,314.43' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,258.98 402.94,258.98' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,203.53 402.94,203.53' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,148.08 402.94,148.08' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,92.63 402.94,92.63' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='37.44,37.18 402.94,37.18' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='105.97,328.29 105.97,23.32' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='220.19,328.29 220.19,23.32' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='334.41,328.29 334.41,23.32' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <rect id='svg_e9e96ad491e95a97_e1' x='54.58' y='83.39' width='102.8' height='231.04' fill='#FF4D6F' fill-opacity='1' stroke='none' title='Event Name: Both&amp;lt;br/&amp;gt;Proportion: 0.417&amp;lt;br/&amp;gt;Patient Count: 5&amp;lt;br/&amp;gt;Avg No. Event A: 0.417&amp;lt;br/&amp;gt;Avg No. Event B: 1'/>\n   <rect id='svg_e9e96ad491e95a97_e2' x='168.8' y='268.22' width='102.8' height='46.21' fill='#5D7298' fill-opacity='1' stroke='none' title='Event Name: inpatient/ED visit&amp;lt;br/&amp;gt;Proportion: 0.083&amp;lt;br/&amp;gt;Patient Count: 1&amp;lt;br/&amp;gt;Avg No. Event A: 0.417&amp;lt;br/&amp;gt;Avg No. Event B: 1'/>\n   <rect id='svg_e9e96ad491e95a97_e3' x='283.01' y='37.18' width='102.8' height='277.25' fill='#BD777A' fill-opacity='1' stroke='none' title='Event Name: Neither&amp;lt;br/&amp;gt;Proportion: 0.5&amp;lt;br/&amp;gt;Patient Count: 6&amp;lt;br/&amp;gt;Avg No. Event A: 0.417&amp;lt;br/&amp;gt;Avg No. Event B: 1'/>\n  <\/g>\n  <g clip-path='url(#svg_e9e96ad491e95a97_c1)'>\n   <text x='18.53' y='317.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>0.0<\/text>\n   <text x='18.53' y='262.19' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>0.1<\/text>\n   <text x='18.53' y='206.74' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>0.2<\/text>\n   <text x='18.53' y='151.29' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>0.3<\/text>\n   <text x='18.53' y='95.84' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>0.4<\/text>\n   <text x='18.53' y='40.39' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>0.5<\/text>\n   <text x='407.88' y='317.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>0<\/text>\n   <text x='407.88' y='225.22' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>2<\/text>\n   <text x='407.88' y='132.81' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>4<\/text>\n   <text x='407.88' y='40.39' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>6<\/text>\n   <text x='79.46' y='339.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>Both Events<\/text>\n   <text x='191.71' y='339.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>Event B Only<\/text>\n   <text x='304.17' y='339.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>Neither Event<\/text>\n   <text transform='translate(13.50,227.95) rotate(-90.00)' font-size='8.25pt' font-family='DejaVu Sans'>Proportion Patients<\/text>\n   <text transform='translate(418.50,138.58) rotate(90.00)' font-size='8.25pt' font-family='DejaVu Sans'>Patient Count<\/text>\n   <text x='37.44' y='15.1' font-size='9.9pt' font-family='DejaVu Sans'>Proportion Patients with Each Event<\/text>\n  <\/g>\n <\/g>\n<\/svg>","js":null,"uid":"svg_e9e96ad491e95a97","ratio":1.2,"settings":{"tooltip":{"css":".tooltip_SVGID_ { padding:5px;background:black;color:white;border-radius:2px;text-align:left; ; position:absolute;pointer-events:none;z-index:999;}","placement":"doc","opacity":0.9,"offx":10,"offy":10,"use_cursor_pos":true,"use_fill":false,"use_stroke":false,"delay_over":200,"delay_out":500},"hover":{"css":".hover_data_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_data_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_data_SVGID_ { fill:orange;stroke:black; }\nline.hover_data_SVGID_, polyline.hover_data_SVGID_ { fill:none;stroke:orange; }\nrect.hover_data_SVGID_, polygon.hover_data_SVGID_, path.hover_data_SVGID_ { fill:orange;stroke:none; }\nimage.hover_data_SVGID_ { stroke:orange; }","reactive":true,"nearest_distance":null},"hover_inv":{"css":""},"hover_key":{"css":".hover_key_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_key_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_key_SVGID_ { fill:orange;stroke:black; }\nline.hover_key_SVGID_, polyline.hover_key_SVGID_ { fill:none;stroke:orange; }\nrect.hover_key_SVGID_, polygon.hover_key_SVGID_, path.hover_key_SVGID_ { fill:orange;stroke:none; }\nimage.hover_key_SVGID_ { stroke:orange; }","reactive":true},"hover_theme":{"css":".hover_theme_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_theme_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_theme_SVGID_ { fill:orange;stroke:black; }\nline.hover_theme_SVGID_, polyline.hover_theme_SVGID_ { fill:none;stroke:orange; }\nrect.hover_theme_SVGID_, polygon.hover_theme_SVGID_, path.hover_theme_SVGID_ { fill:orange;stroke:none; }\nimage.hover_theme_SVGID_ { stroke:orange; }","reactive":true},"select":{"css":".select_data_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_data_SVGID_ { stroke:none;fill:red; }\ncircle.select_data_SVGID_ { fill:red;stroke:black; }\nline.select_data_SVGID_, polyline.select_data_SVGID_ { fill:none;stroke:red; }\nrect.select_data_SVGID_, polygon.select_data_SVGID_, path.select_data_SVGID_ { fill:red;stroke:none; }\nimage.select_data_SVGID_ { stroke:red; }","type":"multiple","only_shiny":true,"selected":[]},"select_inv":{"css":""},"select_key":{"css":".select_key_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_key_SVGID_ { stroke:none;fill:red; }\ncircle.select_key_SVGID_ { fill:red;stroke:black; }\nline.select_key_SVGID_, polyline.select_key_SVGID_ { fill:none;stroke:red; }\nrect.select_key_SVGID_, polygon.select_key_SVGID_, path.select_key_SVGID_ { fill:red;stroke:none; }\nimage.select_key_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"select_theme":{"css":".select_theme_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_theme_SVGID_ { stroke:none;fill:red; }\ncircle.select_theme_SVGID_ { fill:red;stroke:black; }\nline.select_theme_SVGID_, polyline.select_theme_SVGID_ { fill:none;stroke:red; }\nrect.select_theme_SVGID_, polygon.select_theme_SVGID_, path.select_theme_SVGID_ { fill:red;stroke:none; }\nimage.select_theme_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"zoom":{"min":1,"max":1,"duration":300,"default_on":false},"toolbar":{"position":"topright","pngname":"diagram","tooltips":null,"fixed":false,"hidden":[],"delay_over":200,"delay_out":500},"sizing":{"rescale":true,"width":1}}},"evals":[],"jsHooks":[]}
```
