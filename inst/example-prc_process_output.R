
#' Source setup file
source(system.file('setup.R', package = 'patientrecordconsistency'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'prc_process_test',
                      working_directory = getwd(),
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = system.file('extdata',
                                        package = 'patientrecordconsistency'),
                      cdm_schema = NA)

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
                                   prc_event_file = prc_events)

prc_process_example

#' Execute `prc_output` function
prc_output_example <- prc_output(process_output = prc_process_example)

prc_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(prc_output_example)
