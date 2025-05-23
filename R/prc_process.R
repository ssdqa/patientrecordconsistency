
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
#' @param cohort *tabular input* | cohort for SQUBA testing; required fields:
#' - `site` | *character*
#' - `person_id` / `patid` | *integer* / *character*
#' - `start_date` | *date*
#' - `end_date` | *date*
#' @param prc_event_file *tabular input* | a table with the definitions for each event with the columns
#' - `event` | *character* | A or B
#' - `event_label` | *character* | a descriptive label for the event
#' - `domain_tbl` | *character* | the default CDM table from which data is retrieved
#' - `concept_field` | *character* | the field in the table where the codes of interest are stored
#' - `date_field` | *character* | the date field to be used to establish the index & occurrence dates
#' - `vocabulary_field` | *character* | (PCORnet only) The name of the column in the domain table where the vocabulary type is stored
#' - `codeset_name` | *character* | the name of the codeset in the specs directory to define the variable of interest
#' - `filter_logic` | *character* | a string indicating any filter logic that should be applied to establish the event
#' ex: an Hba1c > 6.5
#' @param omop_or_pcornet *string* | Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' - `omop`: run the [prc_process_omop()] function against an OMOP CDM instance
#' - `pcornet`: run the [prc_process_pcornet()] function against a PCORnet CDM instance
#' @param multi_or_single_site *string* | Option to run the function on a single vs multiple sites
#' - `single` - run the function for a single site
#' - `multi` - run the function for multiple sites
#' @param anomaly_or_exploratory *string* | Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#'                               level summary of the data to examine the fact representation within the cohort. Anomaly detection
#'                               analyses are specialized to identify outliers within the cohort.
#' @param age_groups *tabular input* | If you would like to stratify the results by age group,  create a table or CSV file with the following
#'                   columns and include it as the `age_groups` function parameter:
#' - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#' - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#' - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#' If you would *not* like to stratify by age group, leave the argument as NULL
#' @param patient_level_tbl *boolean* | logical controlling whether patient level output is returned or not
#' @param fu_breaks *vector* | a numeric vector that defines how to group different windows of follow up time
#' @param p_value *numeric* | the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param time *boolean* | a logical that tells the function whether you would like to look at the output over time
#' @param time_span *vector - length 2* | when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                  formatted as c(start date, end date) in yyyy-mm-dd date format
#' @param time_period *string* | when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return a dataframe summarizing how often two events occur and co-occur within a patient record
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
