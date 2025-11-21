
#' Compute the counts of each event within the patient record and across the cohort
#'
#' @param cohort table of cohort members with at least the site_col, `patid`, `start_date`, and `end_date`
#' @param grouped_list list of columns that should be used to group the table
#' @param site_col the name of the column with the site names
#' @param time logical to determine whether to output the check across time
#' @param event_tbl a table with the definitions for each event with the columns
#' - `event` - A or B
#' - `event_label` - a descriptive label for the event
#' - `domain_tbl` - the default CDM table from which data is retrieved
#' - `concept_field` - the field in the table where the codes of interest are stored
#' - `date_field` - the date field to be used to establish the index & occurrence dates
#' - `vocabulary_field` - (PCORnet only) The name of the column in the domain table where the vocabulary type is stored
#' - `codeset_name` - the name of the codeset in the specs directory to define the variable of interest
#' - `filter_logic` - a string indicating any filter logic that should be applied to establish the event
#' ex: an Hba1c > 6.5
#'
#' @return a dataframe with the proportion of patients that have event A, have event B,
#'         have both events, or have neither event within their patient record
#'
#' @keywords internal
#'
compute_event_counts_pcnt <- function(cohort,
                                      grouped_list,
                                      site_col,
                                      time = FALSE,
                                      event_tbl = patientrecordconsistency::prc_event_file){

  ## Make sure time var is included in group if needed
  if(time){grouped_list <- grouped_list %>%
    append('time_start') %>% append('time_increment') %>% unique()}

  ## Pull event information
  event_list <- split(event_tbl, seq(nrow(event_tbl)))

  if(length(event_list) > 2){cli::cli_abort('Please only select 2 events to compare')}

  ## Get event data
  event_rslt <- list()
  total_rslt <- list()

  for(i in 1:length(event_list)){

    if(!time){
      event_domain <- cdm_tbl(event_list[[i]]$domain_tbl) %>%
        inner_join(cohort) %>%
        group_by(patid, !!!syms(grouped_list)) %>%
        filter(!!sym(event_list[[i]]$date_field) >= start_date,
               !!sym(event_list[[i]]$date_field) <= end_date)
    }else{
      event_domain <- cdm_tbl(event_list[[i]]$domain_tbl) %>%
        inner_join(cohort) %>%
        filter(!!sym(event_list[[i]]$date_field) >= start_date,
               !!sym(event_list[[i]]$date_field) <= end_date) %>%
        filter(!!sym(event_list[[i]]$date_field) >= time_start,
               !!sym(event_list[[i]]$date_field) <= time_end) %>%
        group_by(patid, !!!syms(grouped_list))


      total_rslt[[i]] <- event_domain %>% ungroup() %>%
        distinct(!!!syms(grouped_list), patid)
    }

    join_cols <- set_names('concept_code', event_list[[i]]$concept_field)

    if(!is.na(event_list[[i]]$vocabulary_field)){
      join_cols2 <- set_names('vocabulary_id', event_list[[i]]$vocabulary_field)
      join_cols <- join_cols %>% append(join_cols2)
    }

    if(is.na(event_list[[i]]$filter_logic)){
      event_cts <- event_domain %>%
        inner_join(load_codeset(event_list[[i]]$codeset_name), by = join_cols) %>%
        summarise(event_count = n(), .groups = 'keep') %>%
        distinct(patid, !!!syms(grouped_list), event_count) %>%
        collect() %>%
        mutate(event_type = event_list[[i]]$event,
               event_name = event_list[[i]]$event_label)
    }else{
      event_cts <- event_domain %>%
        inner_join(load_codeset(event_list[[i]]$codeset_name), by = join_cols) %>%
        filter(!! rlang::parse_expr(event_list[[i]]$filter_logic)) %>%
        summarise(event_count = n(), .groups = 'keep') %>%
        distinct(patid, !!!syms(grouped_list), event_count) %>%
        collect() %>%
        mutate(event_type = event_list[[i]]$event,
               event_name = event_list[[i]]$event_label)
    }

    event_rslt[[i]] <- event_cts

  }

  event_combo <- purrr::reduce(.x = event_rslt,
                               .f = dplyr::union)

  if(time){cohort <- purrr::reduce(.x = total_rslt,
                                   .f = dplyr::union) %>% collect()}

  ## Reformat event data
  grp <- group_vars(event_combo)
  new_grp <- grp[!grp %in% 'patid']

  eventa <- event_combo %>% filter(toupper(event_type) == 'A') %>%
    rename(event_a_num = event_count,
           event_a_name = event_name) %>% select(-event_type)

  eventb <- event_combo %>% filter(toupper(event_type) == 'B') %>%
    rename(event_b_num = event_count,
           event_b_name = event_name) %>% select(-event_type)

  event_ptlv <- eventa %>% full_join(eventb) %>%
    full_join(cohort %>% select(!!!syms(new_grp), patid) %>% collect()) %>%
    mutate(event_a_num = ifelse(is.na(event_a_num), 0, event_a_num),
           event_b_num = ifelse(is.na(event_b_num), 0, event_b_num)) %>%
    ungroup() %>%
    fill(event_b_name, .direction = 'updown') %>% fill(event_a_name, .direction = 'updown')

  if(time){
    time_increment_str <- event_combo %>% ungroup() %>% distinct(time_increment) %>% pull()
    time_start_str <- event_combo %>% ungroup() %>% distinct(time_start) %>% pull()

    event_ptlv <- event_ptlv %>%
      mutate(time_start = time_start_str,
             time_increment = time_increment_str)
  }

  ## Aggregate results

  event_agg <- event_ptlv %>%
    group_by(!!!syms(new_grp), event_a_num, event_a_name,
             event_b_num, event_b_name) %>%
    summarise(pt_ct = n()) %>%
    group_by(!!!syms(new_grp)) %>%
    mutate(total_pts = sum(pt_ct)) %>% ungroup()


  output <- list('summary_output' = event_agg,
                 'pt_lv_output' = event_ptlv)

  return(output)

}


#' Compute Jaccard Index based on years of follow up for not over
#' time anomaly detection
#'
#' @param cohort table of cohort members with at least the site_col, `person_id`, `start_date`, and `end_date`
#' @param site_col the name of the column with the site names
#' @param grouped_list list of columns that should be used to group the table
#' @param event_tbl a table with the definitions for each event with the columns
#' - `event` - A or B
#' - `event_label` - a descriptive label for the event
#' - `domain_tbl` - the default CDM table from which data is retrieved
#' - `concept_field` - the field in the table where the codes of interest are stored
#' - `date_field` - the date field to be used to establish the index & occurrence dates
#' - `vocabulary_field` - (PCORnet only) The name of the column in the domain table where the vocabulary type is stored
#' - `codeset_name` - the name of the codeset in the specs directory to define the variable of interest
#' - `filter_logic` - a string indicating any filter logic that should be applied to establish the event
#' ex: an Hba1c > 6.5
#'
#' @param grp_breaks a numeric vector that defines how to group different windows of follow up time
#'
#' @return a dataframe containing the jaccard index of the two user defined events within
#'         each follow up window as definted by grp_breaks
#'
#' @keywords internal
#'
compute_prc_ntanom_pcnt <- function(cohort,
                                    site_col,
                                    grouped_list = 'site',
                                    event_tbl = patientrecordconsistency::prc_event_file,
                                    grp_breaks = c(0, 1, 3, 8, 11, 15, 25, 50, 100)){

  ## Pull event information
  event_list <- split(event_tbl, seq(nrow(event_tbl)))

  if(length(event_list) > 2){cli::cli_abort('Please only select 2 events to compare')}

  ## Get event data
  event_rslt <- list()

  for(i in 1:length(event_list)){

    event_domain <- cdm_tbl(event_list[[i]]$domain_tbl) %>%
      inner_join(cohort) %>%
      group_by(patid, !!!syms(grouped_list)) %>%
      filter(!!sym(event_list[[i]]$date_field) >= start_date,
             !!sym(event_list[[i]]$date_field) <= end_date)

    join_cols <- set_names('concept_code', event_list[[i]]$concept_field)

    if(!is.na(event_list[[i]]$vocabulary_field)){
      join_cols2 <- set_names('vocabulary_id', event_list[[i]]$vocabulary_field)
      join_cols <- join_cols %>% append(join_cols2)
    }

    if(is.na(event_list[[i]]$filter_logic)){
      event_cts <- event_domain %>%
        inner_join(load_codeset(event_list[[i]]$codeset_name), by = join_cols) %>%
        summarise(event_count = n(), .groups = 'keep') %>%
        distinct(patid, !!!syms(grouped_list), event_count) %>%
        collect() %>%
        mutate(event_type = event_list[[i]]$event,
               event_name = event_list[[i]]$event_label)
    }else{
      event_cts <- event_domain %>%
        inner_join(load_codeset(event_list[[i]]$codeset_name), by = join_cols) %>%
        filter(!! rlang::parse_expr(event_list[[i]]$filter_logic)) %>%
        summarise(event_count = n(), .groups = 'keep') %>%
        distinct(patid, !!!syms(grouped_list), event_count) %>%
        collect() %>%
        mutate(event_type = event_list[[i]]$event,
               event_name = event_list[[i]]$event_label)
    }

    event_rslt[[i]] <- event_cts

  }

  event_combo <- purrr::reduce(.x = event_rslt,
                               .f = dplyr::union)

  ## Reformat event data
  grp <- group_vars(event_combo)
  new_grp <- grp[!grp %in% 'patid']

  eventa <- event_combo %>% filter(toupper(event_type) == 'A') %>%
    rename(event_a_num = event_count,
           event_a_name = event_name) %>% select(-event_type)

  eventb <- event_combo %>% filter(toupper(event_type) == 'B') %>%
    rename(event_b_num = event_count,
           event_b_name = event_name) %>% select(-event_type)

  ## Get Patient - Level data
  event_ptlv <- eventa %>% full_join(eventb) %>%
    full_join(cohort %>% select(!!!syms(new_grp), patid, fu) %>% collect()) %>%
    mutate(event_a_num = ifelse(is.na(event_a_num), 0, event_a_num),
           event_b_num = ifelse(is.na(event_b_num), 0, event_b_num)) %>%
    ungroup() %>%
    fill(event_b_name, .direction = 'updown') %>% fill(event_a_name, .direction = 'updown')


  ## Bin FU time
  grp_breaks <- grp_breaks %>% append(Inf) %>% unique()
  grouped_list <- grouped_list %>% append('fu_bins')

  event_type_cts <- as_tibble(event_ptlv) %>%
    pivot_wider(names_from = 'event_a_name',
                values_from = 'event_a_num') %>%
    pivot_wider(names_from = 'event_b_name',
                values_from = 'event_b_num') %>%
    pivot_longer(cols = !c('patid', !!sym(site_col), fu),
                 names_to = 'event_name') %>%
    filter(value != 0)

  binned_fu_time <- event_type_cts %>%
    ungroup() %>%
    mutate(fu = as.numeric(fu)) %>%
    mutate(fu_bins = cut(fu, breaks = grp_breaks, right = FALSE)) %>%
    unite(facet_col, !!!syms(grouped_list), sep = '_')

  facet_list <- group_split(binned_fu_time %>% group_by(facet_col))

  jacc_list <- list()

  for(i in 1:length(facet_list)){

    grp <- facet_list[[i]] %>% distinct(facet_col) %>% pull()

    jaccards <- compute_jaccard(jaccard_input_tbl = facet_list[[i]],
                                var_col = 'event_name',
                                omop_or_pcornet = 'pcornet') %>%
      mutate(grp = grp)

    jacc_list[[i]] <- jaccards

  }

  jacc_reduce <- purrr::reduce(.x = jacc_list,
                               .f = dplyr::union)

  output <- list('summary_output' = jacc_reduce,
                 'pt_lv_output' = event_ptlv)

  return(output)

}
