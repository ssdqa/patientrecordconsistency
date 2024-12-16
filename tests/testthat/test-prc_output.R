
test_that('errors on incorrect output_function', {

  tbl_test <- data.frame('test'= c(1, 2, 3))

  expect_error(prc_output(process_output = tbl_test,
                          output_function = 'pes_test'))
})


test_that('single site, exploratory, no time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'),
                            event_a_num = c(0,1,2,3,4,5,6,7,8),
                            event_b_num = c(1,3,5,7,9,11,13,15,17),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1', 't1', 't1', 't1', 't1'),
                            pt_ct = c(4, 2, 7, 3, 100, 5, 2, 8, 6),
                            total_pts = c(250, 250, 250, 250, 250, 250, 250, 250, 250))

  expect_no_error(prc_output(process_output = tbl_test,
                             output_function = 'prc_ss_exp_cs'))

})


test_that('multi site, exploratory, no time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b'),
                            event_a_num = c(0,1,2,3,4,5,6,7,8),
                            event_b_num = c(1,3,5,7,9,11,13,15,17),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1', 't1', 't1', 't1', 't1'),
                            pt_ct = c(4, 2, 7, 3, 100, 5, 2, 8, 6),
                            total_pts = c(250, 250, 250, 250, 250, 250, 250, 250, 250))

  expect_no_error(prc_output(process_output = tbl_test,
                             output_function = 'prc_ms_exp_cs'))

})


test_that('single site, anomaly detection, no time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a'),
                            'fu_bin' = c('[0,3)','[3,6)','[6,10)'),
                            'concept1' = c('t1', 't1', 't1'),
                            'concept2' = c('t2', 't2', 't2'),
                            'cocount' = c(5,6,7),
                            'concept1_ct' = c(1,2,3),
                            'concept2_ct' = c(4,5,6),
                            'concept_count_union' = c(4,5,6),
                            'jaccard_index' = c(0.1, 0.2, 0.3),
                            'concept1_prop' = c(0.1,0.2,0.3),
                            'concept2_prop' = c(0.1,0.2,0.3))

  expect_no_error(prc_output(process_output = tbl_test,
                             output_function = 'prc_ss_anom_cs'))

})


test_that('multi site, anomaly detection, no time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'b', 'c'),
                            'fu_bin' = c('[0,3)','[0,3)','[0,3)'),
                            'concept1' = c('t1', 't1', 't1'),
                            'concept2' = c('t2', 't2', 't2'),
                            'cocount' = c(5,6,7),
                            'concept1_ct' = c(1,2,3),
                            'concept2_ct' = c(4,5,6),
                            'concept_count_union' = c(4,5,6),
                            'jaccard_index' = c(0.1, 0.2, 0.3),
                            'concept1_prop' = c(0.1,0.2,0.3),
                            'concept2_prop' = c(0.1,0.2,0.3),
                            'mean_val' = c(0.85, 0.85, 0.85),
                            'median_val' = c(0.82, 0.82, 0.82),
                            'sd_val' = c(0.05, 0.05, 0.05),
                            'mad_val' = c(0.02, 0.02, 0.02),
                            'cov_val' = c(0.01, 0.01, 0.01),
                            'max_val' = c(0.95, 0.95, 0.95),
                            'min_val' = c(0.79, 0.79, 0.79),
                            'range_val' = c(0.16, 0.16, 0.16),
                            'total_ct' = c(3,3,3),
                            'analysis_eligible' = c('yes','yes','yes'),
                            'lower_tail' = c(0.8134, 0.8134, 0.8134),
                            'upper_tail' = c(0.932, 0.932, 0.932),
                            'anomaly_yn' = c('no outlier', 'outlier', 'outlier'))

  expect_no_error(prc_output(process_output = tbl_test,
                             output_function = 'prc_ms_anom_cs'))

  expect_no_error(prc_output(process_output = tbl_test %>% dplyr::mutate(anomaly_yn = 'no outlier in group'),
                             output_function = 'prc_ms_anom_cs'))

})


test_that('single site, exploratory, across time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a',
                                     'a', 'a', 'a', 'a', 'a'),
                            event_a_num = c(0,1,2,3,4,5,6,7,8,9),
                            event_b_num = c(1,2,3,4,5,6,7,8,9,10),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01', '2022-01-01', '2022-01-01'),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            pt_ct = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            total_pts = c(100, 100, 100, 100, 100, 100,
                                          100, 100, 100, 100),
                            time_increment = c('year', 'year', 'year', 'year', 'year',
                                               'year', 'year', 'year', 'year', 'year'))

  expect_no_error(prc_output(process_output = tbl_test,
                             output_function = 'prc_ss_exp_la'))

})

test_that('multi site, exploratory, across time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a',
                                     'b', 'b', 'b', 'b', 'b'),
                            event_a_num = c(0,1,2,3,4,5,6,7,8,9),
                            event_b_num = c(1,2,3,4,5,6,7,8,9,10),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01', '2022-01-01', '2022-01-01'),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            pt_ct = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            total_pts = c(100, 100, 100, 100, 100, 100,
                                          100, 100, 100, 100),
                            time_increment = c('year', 'year', 'year', 'year', 'year',
                                               'year', 'year', 'year', 'year', 'year'))

  expect_no_error(prc_output(process_output = tbl_test,
                             output_function = 'prc_ms_exp_la'))

})


test_that('single site, anomaly detection, across time -- year', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a',
                                     'a', 'a', 'a', 'a', 'a'),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01', '2022-01-01', '2022-01-01'),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            stat_ct = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            total_pts = c(100, 100, 100, 100, 100, 100,
                                          100, 100, 100, 100),
                            prop_event = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95),
                            stat_type = c('Event A Only', 'Event A Only', 'Event B Only',
                                          'Event B Only', 'Both Events', 'Both Events',
                                          'Neither Event', 'Neither Event', 'Neither Event',
                                          'Both Events'),
                            time_increment = c('year', 'year', 'year', 'year', 'year',
                                               'year', 'year', 'year', 'year', 'year'))

  expect_no_error(prc_output(process_output = tbl_test,
                             event_filter = 'a',
                             output_function = 'prc_ss_anom_la'))

})

test_that('single site, anomaly detection, across time -- month', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'a', 'a',
                                     'a', 'a', 'a', 'a', 'a'),
                            time_start = c('2018-01-01', '2018-02-01', '2018-03-01',
                                           '2018-04-01', '2018-05-01', '2018-06-01',
                                           '2018-07-01', '2018-08-01', '2018-09-01', '2018-10-01'),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1', 't1'),
                            stat_ct = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            total_pts = c(100, 100, 100, 100, 100, 100,
                                          100, 100, 100, 100),
                            prop_event = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95),
                            stat_type = c('Event A Only', 'Event A Only', 'Event B Only',
                                          'Event B Only', 'Both Events', 'Both Events',
                                          'Neither Event', 'Neither Event', 'Neither Event',
                                          'Both Events'),
                            time_increment = c('month', 'month', 'month', 'month', 'month',
                                               'month', 'month', 'month', 'month', 'month'),
                            'observed' = c(0.5, 0.6, 0.7, 0.8, 0.9,
                                           0.5, 0.6, 0.7, 0.8, 0.9),
                            'season' = c(1,2,3,4,5,1,2,3,4,5),
                            'trend' = c(1,2,3,4,5,1,2,3,4,5),
                            'remainder' = c(0.46, 0.57, 0.69, 0.82, 0.88,
                                            0.46, 0.57, 0.69, 0.82, 0.88),
                            'seasonadj' = c(1,2,3,4,5,1,2,3,4,5),
                            'anomaly' = c('Yes', 'No', 'No', 'No', 'Yes',
                                          'Yes', 'No', 'No', 'No', 'Yes'),
                            'anomaly_direction' = c(-1,0,0,0,1,-1,0,0,0,1),
                            'anomaly_score' = c(1,2,3,4,5,1,2,3,4,5),
                            'recomposed_l1' = c(0.44, 0.6, 0.5, 0.49, 0.46,
                                                0.44, 0.6, 0.5, 0.49, 0.46),
                            'recomposed_l2' = c(0.84, 0.8, 0.8, 0.89, 0.86,
                                                0.84, 0.8, 0.8, 0.89, 0.86),
                            'observed_clean' = c(0.46, 0.57, 0.69, 0.82, 0.88,
                                                 0.46, 0.57, 0.69, 0.82, 0.88))

  expect_no_error(prc_output(process_output = tbl_test,
                             event_filter = 'a',
                             output_function = 'prc_ss_anom_la'))

})


test_that('multi site, anomaly detection, across time', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'b', 'b',
                                     'b', 'c', 'c', 'c'),
                            time_start = c('2018-01-01', '2018-01-01', '2019-01-01',
                                           '2019-01-01', '2020-01-01', '2020-01-01',
                                           '2021-01-01', '2021-01-01', '2022-01-01'),
                            event_a_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1'),
                            event_b_name = c('t1', 't1', 't1', 't1', 't1',
                                             't1', 't1', 't1', 't1'),
                            stat_type = c('Event A Only', 'Event A Only', 'Event B Only',
                                          'Event B Only', 'Both Events', 'Both Events',
                                          'Neither Event', 'Neither Event', 'Neither Event'),
                            stat_ct = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                            total_pts = c(100, 100, 100, 100, 100, 100,
                                          100, 100, 100),
                            prop_event = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
                            'mean_allsiteprop' = c(0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83),
                            'median' = c(0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87),
                            'date_numeric' = c(17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000),
                            'site_loess' = c(0.84, 0.87, 0.89, 0.91, 0.89, 0.73, 0.81, 0.83, 0.94),
                            'dist_eucl_mean' = c(0.84,0.84,0.84,0.84,0.84,0.9,0.9,0.9,0.9))

  expect_no_error(prc_output(process_output = tbl_test,
                             event_filter = 'both',
                             output_function = 'prc_ms_anom_la'))

})
