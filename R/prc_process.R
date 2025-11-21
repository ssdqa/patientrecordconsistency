
#' Patient Record Consistency
#'
#' This is a concordance module that will assess consistency within a patient
#' record by evaluating the presence or absence of two user-provided clinical
#' events (`prc_event_file`). The checks in the module will establish whether
#' one of, neither, or both events are present in each patient record and
#' summarize these results across the full cohort. A sample version of the
#' input file is accessible with `patientrecordconsistency::`. This function
#' is compatible with both the OMOP and the PCORnet CDMs based on the user's
#' selection.
#'
#' @param cohort *tabular input* || **required**
#'
#'   The cohort to be used for data quality testing. This table should contain,
#'   at minimum:
#'   - `site` | *character* | the name(s) of institutions included in your cohort
#'   - `person_id` / `patid` | *integer* / *character* | the patient identifier
#'   - `start_date` | *date* | the start of the cohort period
#'   - `end_date` | *date* | the end of the cohort period
#'
#'   Note that the start and end dates included in this table will be used to
#'   limit the search window for the analyses in this module.
#'
#' @param prc_event_file *tabular input* || **required**
#'
#'   A table with the definitions of each of the events. This table should
#'   contain two rows, one for each event, with the following columns:
#'   - `event` | *character* | a string, either A or B, representing the event type. A will be treated as the event that is expected to occur first in a sequence
#'   - `event_label` | *character* | a descriptive label for the event
#'   - `domain_tbl` | *character* | the name of the CDM table where the event is defined
#'   - `concept_field` | *character* | the string name of the field in the domain table where the concepts are located
#'   - `date_field` | *character* | the name of the field in the domain table with the date that should be used for temporal filtering
#'   - `vocabulary_field` | *character* | for PCORnet applications, the name of the field in the domain table with a vocabulary identifier to differentiate concepts from one another (ex: dx_type); can be set to NA for OMOP applications
#'   - `codeset_name` | *character* | the name of the codeset that defines the event of interest
#'   - `filter_logic` | *character* | logic to be applied to the domain_tbl in order to achieve the definition of interest; should be written as if you were applying it in a dplyr::filter command in R
#'
#'   To see an example of this input, see `?patientrecordconsistency::prc_event_file`
#'
#' @param omop_or_pcornet *string* || **required**
#'
#'   A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#'    - `omop`: run the [prc_process_omop()] function against an OMOP CDM instance
#'    - `pcornet`: run the [prc_process_pcornet()] function against a PCORnet CDM instance
#'
#' @param multi_or_single_site *string* || defaults to `single`
#'
#'   A string, either `single` or `multi`, indicating whether a single-site or
#'   multi-site analysis should be executed
#'
#' @param anomaly_or_exploratory *string* || defaults to `exploratory`
#'
#'   A string, either `anomaly` or `exploratory`, indicating what type of results
#'   should be produced.
#'
#'   Exploratory analyses give a high level summary of the data to examine the
#'   fact representation within the cohort. Anomaly detection analyses are
#'   specialized to identify outliers within the cohort.
#'
#' @param age_groups *tabular input* || defaults to `NULL`
#'
#'   If you would like to stratify the results by age group, create a table or
#'   CSV file with the following columns and use it as input to this parameter:
#'
#'   - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#'   - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#'   - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#'   If you would *not* like to stratify by age group, leave as `NULL`
#'
#' @param patient_level_tbl *boolean* || defaults to `FALSE`
#'
#'   A boolean indicating whether an additional table with patient level results should be output.
#'
#'   If `TRUE`, the output of this function will be a list containing both the summary and patient level
#'   output. Otherwise, this function will just output the summary dataframe
#'
#' @param fu_breaks *vector* || defaults to `c(0, 1, 3, 8, 11, 15, 25, 50, 100)`
#'
#'   A numeric vector that defines how to group different windows of follow up time. This
#'   parameter is used for both `Anomaly Detection, Cross-Sectional` checks
#'
#' @param p_value *numeric* || defaults to `0.9`
#'
#'   The p value to be used as a threshold in the Multi-Site,
#'   Anomaly Detection, Cross-Sectional analysis
#'
#' @param time *boolean* || defaults to `FALSE`
#'
#'   A boolean to indicate whether to execute a longitudinal analysis
#'
#' @param time_span *vector - length 2* || defaults to `c('2012-01-01', '2020-01-01')`
#'
#'   A vector indicating the lower and upper bounds of the time series for longitudinal analyses
#'
#' @param time_period *string* || defaults to `year`
#'
#'   A string indicating the distance between dates within the specified time_span.
#'   Defaults to `year`, but other time periods such as `month` or `week` are
#'   also acceptable
#'
#' @return This function will return a dataframe summarizing the
#'         co-occurrence of events within a patient record. For a
#'         more detailed description of output specific to each check type,
#'         see the PEDSpace metadata repository
#'
#' @example inst/example-prc_process_output.R
#'
#' @export
#'
prc_process <- function(cohort,
                        prc_event_file,
                        omop_or_pcornet,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        age_groups = NULL,
                        patient_level_tbl = FALSE,
                        fu_breaks = c(0, 1, 3, 8, 11, 15, 25, 50, 100),
                        p_value = 0.9,
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue'),
                            inform = list(color = 'green')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'prc',
                                             as.list(environment())))

  if(tolower(omop_or_pcornet) == 'omop'){

    prc_rslt <- prc_process_omop(cohort = cohort,
                                 prc_event_file = prc_event_file,
                                 multi_or_single_site = multi_or_single_site,
                                 anomaly_or_exploratory=anomaly_or_exploratory,
                                 age_groups = age_groups,
                                 patient_level_tbl = patient_level_tbl,
                                 fu_breaks = fu_breaks,
                                 p_value = p_value,
                                 time = time,
                                 time_span = time_span,
                                 time_period = time_period)

  }else if(tolower(omop_or_pcornet) == 'pcornet'){

    prc_rslt <- prc_process_pcornet(cohort = cohort,
                                    prc_event_file = prc_event_file,
                                    multi_or_single_site = multi_or_single_site,
                                    anomaly_or_exploratory=anomaly_or_exploratory,
                                    age_groups = age_groups,
                                    patient_level_tbl = patient_level_tbl,
                                    fu_breaks = fu_breaks,
                                    p_value = p_value,
                                    time = time,
                                    time_span = time_span,
                                    time_period = time_period)

  }else{cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: this function is only compatible with {.code omop} or {.code pcornet}')}

  if('list' %in% class(prc_rslt)){
    prc_rslt[[1]] <- prc_rslt[[1]] %>% mutate(output_function = output_type$string)
  }else{
    prc_rslt <- prc_rslt %>% mutate(output_function = output_type$string)
  }

  print(cli::boxx(c('You can optionally use this dataframe in the accompanying',
                    '`prc_output` function. Here are the parameters you will need:', '', output_type$vector, '',
                    'See ?prc_output for more details.'), padding = c(0,1,0,1),
                  header = cli::col_cyan('Output Function Details')))

  return(prc_rslt)

}
