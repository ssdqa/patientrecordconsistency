
#' Patient Record Consistency
#'
#' @param cohort cohort for SSDQA testing; required fields:
#'               `site` | `person_id` | `start_date` | `end_date` where start and end date
#' @param prc_event_file a table with the definitions for each event with the columns
#'
#'                         - @event - A or B
#'                         - @event_label - a descriptive label for the event
#'                         - @domain_tbl - the default CDM table from which data is retrieved
#'                         - @concept_field - the field in the table where the codes of interest are
#'                                            stored
#'                         - @date_field - the date field to be used to establish the index &
#'                                         occurrence dates
#'                         - @codeset_name - the name of the codeset in the specs directory to
#'                                           define the variable of interest
#'                         - @filter_logic - a string indicating any filter logic that should be
#'                                           applied to establish the event
#'
#'                                           ex: an Hba1c > 6.5
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#'                               - @single - run the function for a single site
#'                               - @multi - run the function for multiple sites
#' @param anomaly_or_exploratory Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#'                               level summary of the data to examine the fact representation within the cohort. Anomaly detection
#'                               analyses are specialized to identify outliers within the cohort.
#' @param age_groups If you would like to stratify the results by age group, fill out the provided `age_group_definitions.csv` file
#'                     with the following information:
#'                     - @min_age: the minimum age for the group (i.e. 10)
#'                     - @max_age: the maximum age for the group (i.e. 20)
#'                     - @group: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#'                     Then supply this csv file as the age_groups argument (i.e. read.csv('path/to/age_group_definitions.csv'))
#'
#'                     If you would not like to stratify by age group, leave the argument as NULL
#' @param patient_level_tbl logical controlling whether patient level output is returned or not
#' @param fu_breaks a numeric vector that defines how to group different windows of follow up time
#' @param p_value the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param time a logical that tells the function whether you would like to look at the output over time
#' @param time_span when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                  formatted as c(start date, end date) in yyyy-mm-dd date format
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return a dataframe summarizing how often two events occur and co-occur within a patient record
#'
#' @export
#'
#' @import argos
#' @import ssdqa.gen
#' @import dplyr
#' @import cli
#' @importFrom stringr str_wrap
#' @importFrom tidyr separate_wider_delim
#'
prc_process <- function(cohort,
                        prc_event_file,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        age_groups = NULL,
                        patient_level_tbl = FALSE,
                        fu_breaks = c(0, 1, 3, 8, 11, 15, 25, 50, 100),
                        p_value = 0.9,
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'){

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'prc',
                                              as.list(environment())))


  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj

  # Set up grouped list

  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}

  # Prep cohort

  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups,
                                codeset = NULL) %>%
    group_by(!!! syms(grouped_list))

  if(!time){

    if(multi_or_single_site == 'single' & anomaly_or_exploratory == 'anomaly'){

      prc_tbl_int <- compute_prc_ntanom(cohort = cohort_prep,
                                    site_col = site_col,
                                    grouped_list = grouped_list,
                                    event_tbl = prc_event_file,
                                    grp_breaks = fu_breaks
                                    )

      prc_tbl <- prc_tbl_int$summary_output %>%
        separate_wider_delim(cols = grp, delim = "_", names = c(site_col, 'fu_bin'))

      prc_ptlv <- prc_tbl_int$pt_lv_output

    }else if(multi_or_single_site == 'multi' & anomaly_or_exploratory == 'anomaly'){

      prc_tbl_jacc_int <- compute_prc_ntanom(cohort = cohort_prep,
                                         site_col = site_col,
                                         grouped_list = grouped_list,
                                         event_tbl = prc_event_file,
                                         grp_breaks = fu_breaks
                                         )

      prc_tbl_jacc <- prc_tbl_jacc_int$summary_output %>%
        separate_wider_delim(cols = grp, delim = "_", names = c('site', 'fu_bin'))

      prc_ptlv <- prc_tbl_jacc_int$pt_lv_output

      prc_tbl_int <- compute_dist_anomalies(df_tbl = prc_tbl_jacc,
                                            grp_vars = c('fu_bin'),
                                            var_col = 'jaccard_index',
                                            denom_cols = c('fu_bin'))

      prc_tbl <- detect_outliers(df_tbl = prc_tbl_int,
                                 tail_input = 'both',
                                 p_input = p_value,
                                 column_analysis = 'jaccard_index',
                                 column_variable = 'fu_bin')

    }else{

      prc_tbl_int <- compute_event_counts(cohort = cohort_prep,
                                      grouped_list = grouped_list,
                                      site_col = site_col,
                                      time = time,
                                      event_tbl = prc_event_file)

      prc_tbl <- prc_tbl_int$summary_output

      prc_ptlv <- prc_tbl_int$pt_lv_output

      }

  }else{

      prc_tbl <- compute_fot(cohort = cohort_prep,
                             site_col = site_col,
                             reduce_id = NULL,
                             time_period = time_period,
                             time_span = time_span,
                             site_list = site_list_adj,
                             check_func = function(dat){
                               compute_event_counts(cohort = dat,
                                                    grouped_list = grouped_list,
                                                    site_col = site_col,
                                                    time = time,
                                                    event_tbl = prc_event_file)[1]
                             })

      if(patient_level_tbl){
        prc_pt_lv <- compute_fot(cohort = cohort_prep,
                                    site_col = site_col,
                                    reduce_id = NULL,
                                    time_period = time_period,
                                    time_span = time_span,
                                    site_list = site_list_adj,
                                    check_func = function(dat){
                                      compute_event_counts(cohort = dat,
                                                           grouped_list = grouped_list,
                                                           site_col = site_col,
                                                           time = time,
                                                           event_tbl = prc_event_file)[2]
                               })
       }

    if(multi_or_single_site == 'single' & anomaly_or_exploratory == 'anomaly'){

      prc_expanded <- prc_tbl %>%
        uncount(pt_ct) %>%
        mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                     event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                     event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                     event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
        group_by(!!sym(site_col), time_start, time_increment, event_a_name, event_b_name, total_pts, stat_type) %>%
        summarise(stat_ct = n(),
                  prop_event = stat_ct / total_pts) %>% ungroup()

      prc_tbl <- anomalize_ss_anom_at(fot_input_tbl = prc_expanded %>% distinct(),
                                      grp_vars = 'stat_type',
                                      time_var = 'time_start',
                                      var_col = 'prop_event')

    }else if(multi_or_single_site == 'multi' & anomaly_or_exploratory == 'anomaly'){

      prc_expanded <- prc_tbl %>%
        uncount(pt_ct) %>%
        mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                     event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                     event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                     event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
        group_by(!!sym(site_col), time_start, time_increment, event_a_name, event_b_name, total_pts, stat_type) %>%
        summarise(stat_ct = n(),
                  prop_event = stat_ct / total_pts) %>% ungroup()

      prc_tbl <- ms_anom_euclidean(fot_input_tbl = prc_expanded %>% distinct(),
                                   grp_vars = c('site', 'stat_type'),
                                   var_col = 'prop_event')

    }

  }

  cli::cli_inform(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
                                   output function in prc_output: ', output_type, '.')))

  if(patient_level_tbl){

    output <- list('prc_summary_output' = prc_tbl %>% replace_site_col(),
                   'prc_patient_level_output' = prc_ptlv %>% replace_site_col())

    return(output)

  }else{return(prc_tbl %>% replace_site_col())}

}
