## code to prepare `prc_event_file` dataset goes here

prc_event_file <- tidyr::tibble('event' = c('A', 'B'),
                                'event_label' = c('Hba1c greater than 5.8', 'T2DM diagnosis'),
                                'domain_tbl' = c('measurement', 'condition_occurrence'),
                                'concept_field' = c('measurement_concept_id', 'condition_concept_id'),
                                'date_field' = c('measurement_date', 'condition_start_date'),
                                'vocabulary_field' = c(NA, NA),
                                'codeset_name' = c('lab_hba1c', 'dx_t2dm'),
                                'filter_logic' = c('value_as_number > 5.8', NA))

usethis::use_data(prc_event_file, overwrite = TRUE)
