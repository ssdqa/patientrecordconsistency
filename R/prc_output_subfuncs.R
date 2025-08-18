
#'
#' @import ggplot2
#' @import ggiraph
#' @import gt
#' @importFrom tidyr tibble
#' @importFrom tidyr uncount
#' @importFrom qicharts2 qic
#' @importFrom timetk plot_anomalies
#' @importFrom timetk plot_anomalies_decomp
#' @importFrom stats median
#' @importFrom graphics text
#' @importFrom patchwork plot_layout
#'
NULL


#' *Single Site, Exploratory, Cross-Sectional*
#'
#' @param process_output the output from `prc_process`
#'
#' @return a bar graph displaying the proportion (left y-axis) and raw count (right y-axis) of
#'         patients who have only event a, only event b, both events, or neither event present
#'         in their record
#'
prc_ss_exp_cs <- function(process_output){

  expand_cts <- process_output %>%
    uncount(pt_ct)

  avgs <- expand_cts %>%
    summarise(eventb_mean = mean(event_b_num),
              eventa_mean = mean(event_a_num))

  stat_labs <- expand_cts %>%
    mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                 event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                 event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                 event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
    group_by(site, event_a_name, event_b_name, total_pts, stat_type) %>%
    summarise(stat_ct = n()) %>%
    mutate(tooltip = case_when(stat_type == 'Neither Event' ~ 'Event Name: Neither',
                               stat_type == 'Event A Only' ~ paste0('Event Name: ', event_a_name),
                               stat_type == 'Event B Only' ~ paste0('Event Name: ', event_b_name),
                               stat_type == 'Both Events' ~ 'Event Name: Both'),
           prop_event = stat_ct / total_pts,
           tooltip = paste0(tooltip,
                            '\nProportion: ', round(prop_event, 3),
                            '\nPatient Count: ', format(stat_ct, big.mark = ','),
                            '\nAvg No. Event A: ', round(avgs$eventa_mean, 3),
                            '\nAvg No. Event B: ', round(avgs$eventb_mean, 3)))

  total_mult <- stat_labs %>% distinct(total_pts) %>% pull()

  g <- ggplot(stat_labs, aes(x = stat_type, y = prop_event, fill = stat_type)) +
    geom_col_interactive(aes(tooltip = tooltip), show.legend = FALSE) +
    scale_fill_squba() +
    theme_minimal() +
    scale_y_continuous(sec.axis = sec_axis(~.*total_mult, name="Patient Count")) +
    labs(y = 'Proportion Patients',
         x = '',
         title = 'Proportion Patients with Each Event')

  g[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                            'tooltip' = TRUE)

  return(g)

}


#' *Multi Site, Exploratory, Cross-Sectional*
#'
#' @param process_output the output from `prc_process`
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally generate a filtered visualization
#'
#' @return a bar graph displaying the proportion of patients at each site with
#'         only event a, only event b, both events, or neither event present in
#'         their record
#'
prc_ms_exp_cs <- function(process_output,
                          large_n = FALSE,
                          large_n_sites = NULL){

  expand_cts <- process_output %>%
    uncount(pt_ct)

  avgs <- expand_cts %>%
    group_by(site) %>%
    summarise(eventb_mean = mean(event_b_num),
              eventa_mean = mean(event_a_num))

  stat_labs <- expand_cts %>%
    left_join(avgs) %>%
    mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                 event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                 event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                 event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
    group_by(site, event_a_name, event_b_name, total_pts, stat_type,
             eventa_mean, eventb_mean) %>%
    summarise(stat_ct = n()) %>%
    mutate(tooltip = case_when(stat_type == 'Neither Event' ~ 'Event Name: Neither',
                               stat_type == 'Event A Only' ~ paste0('Event Name: ', event_a_name),
                               stat_type == 'Event B Only' ~ paste0('Event Name: ', event_b_name),
                               stat_type == 'Both Events' ~ 'Event Name: Both'),
           prop_event = stat_ct / total_pts,
           tooltip = paste0(tooltip,
                            '\nProportion: ', round(prop_event, 3),
                            '\nAvg No. Event A: ', round(eventa_mean, 3),
                            '\nAvg No. Event B: ', round(eventb_mean, 3)))

  if(!large_n){

    g <- ggplot(stat_labs, aes(y = site, x = prop_event, fill = site)) +
      geom_col_interactive(aes(tooltip = tooltip), show.legend = FALSE) +
      facet_wrap(~stat_type, ncol = 2) +
      scale_fill_squba() +
      theme_minimal() +
      labs(y = 'Site',
           x = 'Proportion Patients',
           title = 'Proportion Patients with Each Event per Site')
  }else{

    allsite_avs <- expand_cts %>%
      summarise(eventb_mean = mean(event_b_num),
                eventa_mean = mean(event_a_num)) %>%
      mutate(site = 'all sites')

    allsite_summs <- stat_labs %>%
      group_by(stat_type) %>%
      summarise(allsite_median = median(prop_event),
                allsite_q1 = stats::quantile(prop_event, 0.25),
                allsite_q3 = stats::quantile(prop_event, 0.75)) %>%
      mutate(site = 'all sites')

    allsite_totpat <- process_output %>%
      distinct(site, total_pts) %>%
      summarise(total_pts = sum(total_pts)) %>%
      mutate(site = 'all sites')

    allsite_stats <- expand_cts %>%
      mutate(site = 'all sites') %>%
      left_join(allsite_avs) %>%
      select(-total_pts) %>%
      left_join(allsite_totpat) %>%
      mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                   event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                   event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                   event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
      group_by(site, event_a_name, event_b_name, total_pts, stat_type,
               eventa_mean, eventb_mean) %>%
      summarise(stat_ct = n()) %>%
      mutate(tooltip = case_when(stat_type == 'Neither Event' ~ 'Event Name: Neither',
                                 stat_type == 'Event A Only' ~ paste0('Event Name: ', event_a_name),
                                 stat_type == 'Event B Only' ~ paste0('Event Name: ', event_b_name),
                                 stat_type == 'Both Events' ~ 'Event Name: Both'),
             prop_event = stat_ct / total_pts,
             tooltip = paste0(tooltip,
                              '\nProportion: ', round(prop_event, 3),
                              '\nAvg No. Event A: ', round(eventa_mean, 3),
                              '\nAvg No. Event B: ', round(eventb_mean, 3))) %>%
      left_join(allsite_summs)

    g <- ggplot(allsite_stats, aes(y = site, x = prop_event)) +
      geom_col_interactive(aes(tooltip = tooltip), fill = 'gray', show.legend = FALSE) +
      geom_errorbar(aes(y=site, x=prop_event, xmin=allsite_q1, xmax=allsite_q3)) +
      geom_point(aes(y = site, x = allsite_median)) +
      geom_point_interactive(data = stat_labs %>% filter(site %in% large_n_sites),
                             aes(x = prop_event, y = 'all sites', color = site,
                                 tooltip = paste0('Site: ', site, '\nProp.: ', round(prop_event, 3))),
                             shape = 8,size = 3) +
      facet_wrap(~stat_type, ncol = 2) +
      scale_color_squba() +
      theme_minimal() +
      guides(shape = 'none',
             fill = 'none') +
      labs(y = 'Site',
           x = 'Proportion Patients',
           title = 'Proportion Patients with Each Event')
  }

  g[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                            'tooltip' = TRUE)

  return(g)

}


#' *Single Site, Exploratory, Longitudinal*
#'
#' @param process_output the output from `prc_process`
#'
#' @return a line graph the displaying the proportion of patients with only event a,
#'         only event b, both events, or neither event across time
#'
prc_ss_exp_la <- function(process_output){

  expand_cts <- process_output %>%
    uncount(pt_ct)

  avgs <- expand_cts %>%
    group_by(site, time_start, time_increment) %>%
    summarise(eventb_mean = mean(event_b_num),
              eventa_mean = mean(event_a_num))

  stat_labs <- expand_cts %>%
    left_join(avgs) %>%
    mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                 event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                 event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                 event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
    group_by(site, time_start, time_increment, event_a_name, event_b_name, total_pts, stat_type,
             eventa_mean, eventb_mean) %>%
    summarise(stat_ct = n()) %>%
    mutate(tooltip = case_when(stat_type == 'Neither Event' ~ 'Event Name: Neither',
                               stat_type == 'Event A Only' ~ paste0('Event Name: ', event_a_name),
                               stat_type == 'Event B Only' ~ paste0('Event Name: ', event_b_name),
                               stat_type == 'Both Events' ~ 'Event Name: Both'),
           prop_event = stat_ct / total_pts,
           tooltip = paste0(tooltip,
                            '\nSite: ',site,
                            '\nProportion: ', round(prop_event, 3),
                            '\nAvg No. Event A: ', round(eventa_mean, 3),
                            '\nAvg No. Event B: ', round(eventb_mean, 3)))

  g <- ggplot(stat_labs, aes(x = time_start, y = prop_event, color = stat_type,
                             group = stat_type, text = tooltip)) +
    geom_line() +
    scale_color_squba() +
    theme_minimal() +
    labs(y = 'Proportion Patients',
         x = '',
         title = 'Proportion Patients with Each Event',
         color = 'Event Type')

  g[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  return(g)

}


#' *Multi Site, Exploratory, Longitudinal*
#'
#' @param process_output the output from `prc_process`
#' @param dist_from_stat the statistic from which distance should be measured
#'                       acceptable values are `mean` or `median`
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally generate a filtered visualization
#'
#' @return a line graph displaying the distance from the overall mean or median
#'         proportion of patients for each site and event type
#'
prc_ms_exp_la <- function(process_output,
                          dist_from_stat = 'mean',
                          large_n = FALSE,
                          large_n_sites = NULL){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  expand_cts <- process_output %>%
    uncount(pt_ct)

  avgs <- expand_cts %>%
    group_by(site, time_start, time_increment) %>%
    summarise(eventb_mean = mean(event_b_num),
              eventa_mean = mean(event_a_num))

  stat_labs <- expand_cts %>%
    left_join(avgs) %>%
    mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                 event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                 event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                 event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
    group_by(site, time_start, time_increment, event_a_name, event_b_name, total_pts, stat_type,
             eventa_mean, eventb_mean) %>%
    summarise(stat_ct = n()) %>%
    mutate(tooltip = case_when(stat_type == 'Neither Event' ~ 'Event Name: Neither',
                               stat_type == 'Event A Only' ~ paste0('Event Name: ', event_a_name),
                               stat_type == 'Event B Only' ~ paste0('Event Name: ', event_b_name),
                               stat_type == 'Both Events' ~ 'Event Name: Both'),
           prop_event = stat_ct / total_pts)

  if(dist_from_stat == 'mean'){
    plot_dat <- stat_labs %>%
      group_by(time_start, time_increment, stat_type) %>%
      mutate(allsite_mean = mean(prop_event),
             dist_col = prop_event - allsite_mean,
             tooltip = paste0(tooltip,
                              '\nDist. from Mean: ', round(dist_col, 3),
                              '\nRaw Proportion: ', round(prop_event, 3),
                              '\nAvg No. Event A: ', round(eventa_mean, 3),
                              '\nAvg No. Event B: ', round(eventb_mean, 3)))

    title_str <- 'All-Site Mean'
  }else if(dist_from_stat == 'median'){
    plot_dat <- stat_labs %>%
      group_by(time_start, time_increment, stat_type) %>%
      mutate(allsite_median = median(prop_event),
             dist_col = prop_event - allsite_median,
             tooltip = paste0(tooltip,
                              '\nDist. from Median: ', round(dist_col, 3),
                              '\nRaw Proportion: ', round(prop_event, 3),
                              '\nAvg No. Event A: ', round(eventa_mean, 3),
                              '\nAvg No. Event B: ', round(eventb_mean, 3)))

    title_str <- 'All-Site Median'
  }else(cli::cli_abort('Invalid dist_from_stat: please choose either {.code mean} or {.code median}'))

  if(!large_n){
    g <- ggplot(plot_dat, aes(x = time_start, y = dist_col, color = site,
                              group = site, text = tooltip)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = 'dotted',
                 alpha = 0.5) +
      facet_wrap(~stat_type, ncol = 2) +
      scale_color_squba() +
      theme_minimal() +
      labs(y = 'Distance',
           x = '',
           title = paste0('Distance from ', title_str,' Proportion of Patients with Each Event'))
  }else{

    if(!is.null(large_n_sites)){

      g <- ggplot(plot_dat %>% filter(site %in% large_n_sites),
                  aes(x = time_start, y = dist_col, color = site,
                      group = site, text = tooltip)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = 'dotted',
                   alpha = 0.5) +
        facet_wrap(~stat_type, ncol = 2) +
        scale_color_squba() +
        theme_minimal() +
        labs(y = 'Distance',
             x = '',
             title = paste0('Distance from ', title_str,' Proportion of Patients with Each Event'))

    }else{

      if(dist_from_stat == 'median'){stat_col <- 'allsite_median'}else{stat_col <- 'allsite_mean'}

      g <- ggplot(plot_dat %>% distinct(time_start, !!sym(stat_col)) %>%
                    mutate(site = stat_col),
                  aes(x = time_start, y = !!sym(stat_col), color = site,
                      group = site, text = !!sym(stat_col))) +
        geom_line() +
        # geom_hline(yintercept = 0, linetype = 'dotted',
        #            alpha = 0.5) +
        facet_wrap(~stat_type, ncol = 2) +
        scale_color_squba() +
        theme_minimal() +
        labs(y = title_str,
             x = '',
             title = paste0(title_str,' of Patients with Each Event'))

    }

  }

  g[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  return(g)

}


#' *Single Site, Anomaly Detection, Cross-Sectional*
#'
#' @param process_output the output from `prc_process`
#'
#' @return a bar graph displaying the jaccard similarity index for the two events
#'         within each user defined follow up window
#'
prc_ss_anom_cs <- function(process_output){


  dat_to_plot <- process_output %>%
    mutate(tooltip = paste0('Jaccard Index: ', round(jaccard_index, 3),
                            '\nEvent A: ', concept2,
                            '\nEvent B: ', concept1,
                            '\nPatients w/ Both: ', cocount,
                            '\nPatients w/ Either: ', concept_count_union))

  grph <- ggplot(dat_to_plot, aes(x = fu_bin, y = jaccard_index, fill = fu_bin,
                                  tooltip = tooltip)) +
    geom_col_interactive(show.legend = FALSE) +
    scale_fill_squba() +
    theme_minimal() +
    labs(x = 'Length of F/U',
         y = 'Jaccard Similarity Index',
         title = 'Co-Occurrence of Events per Years of F/U')

  grph[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

  return(grph)
}


#' *Multi Site, Anomaly Detection, Cross-Sectional*
#'
#' @param process_output the output from `prc_process`
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally generate a filtered visualization
#'
#' @return a dot plot where the shape of the dot represents whether the point is
#'         anomalous, the color of the dot represents the jaccard similarity index
#'         for a given follow up window, and the size of the dot represents the mean index
#'         across all sites
#'
#'         if there were no groups eligible for analysis, a heat map showing the jaccard index
#'         and a dot plot showing each site's average standard deviation away from the mean
#'         index is returned instead
#'
prc_ms_anom_cs <- function(process_output,
                           large_n = FALSE,
                           large_n_sites = NULL){

  comparison_col = 'jaccard_index'

  check_n <- process_output %>%
    filter(anomaly_yn != 'no outlier in group')

  dat_to_plot <- process_output %>%
    mutate(text=paste("Years of F/U: ",fu_bin,
                      "\nSite: ",site,
                      "\nJaccard Index: ",round(!!sym(comparison_col),2),
                      "\nPatients w/ Both: ",cocount,
                      "\nPatients w/ Either: ",concept_count_union,
                      "\nMean Index:",round(mean_val,2),
                      #'SD: ', round(sd_val,2),
                      "\nMedian Index: ",round(median_val,2)
                      #"\nMAD: ", round(mad_val,2)
    )) %>%
    mutate(anomaly_yn = ifelse(anomaly_yn == 'no outlier in group', 'not outlier', anomaly_yn))

  if(!large_n){
    if(nrow(check_n) > 0){

      plt<-ggplot(dat_to_plot,
                  aes(x=site, y=fu_bin, text=text, color=!!sym(comparison_col)))+
        geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
        geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'),
                               aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
        scale_color_squba(palette = 'diverging', discrete = FALSE) +
        scale_shape_manual(values=c(19,8))+
        scale_y_discrete(labels = label_wrap_gen()) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=60, hjust = 1)) +
        labs(y = "Years of F/U",
             x = 'Site',
             size="",
             title=paste0('Anomalous Event Co-Occurrence per Years of F/U by Site'),
             subtitle = 'Dot size is the mean Jaccard index per F/U group') +
        guides(color = guide_colorbar(title = 'Jaccard Index'),
               shape = guide_legend(title = 'Anomaly'),
               size = 'none')

      plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                  'tooltip' = TRUE)

      return(plt)

    }else{

      dat_to_plot <- process_output %>%
        mutate(text=paste("Years of F/U: ",fu_bin,
                          "\nSite: ",site,
                          "\nJaccard Index: ",round(!!sym(comparison_col),2),
                          "\nPatients w/ Both: ",cocount,
                          "\nPatients w/ Either: ",concept_count_union,
                          "\nMean Index:",round(mean_val,2),
                          #'SD: ', round(sd_val,2),
                          "\nMedian Index: ",round(median_val,2)
                          #"\nMAD: ", round(mad_val,2)
        ))

      plt <- ggplot(dat_to_plot, aes(x = site, y = fu_bin, fill = jaccard_index,
                                     tooltip = text)) +
        geom_tile_interactive() +
        theme_minimal() +
        scale_fill_squba(discrete = FALSE, palette = 'diverging') +
        labs(y = 'Years of F/U',
             x = 'Site',
             fill = 'Jaccard Index')

      # Test Site Score using SD Computation
      test_site_score <- process_output %>%
        mutate(dist_mean = (!!sym(comparison_col) - mean_val)^2) %>%
        group_by(site) %>%
        summarise(n_grp = n(),
                  dist_mean_sum = sum(dist_mean),
                  overall_sd = sqrt(dist_mean_sum / n_grp)) %>%
        mutate(tooltip = paste0('Site: ', site,
                                '\nStandard Deviation: ', round(overall_sd, 3)))

      ylim_max <- test_site_score %>% filter(overall_sd == max(overall_sd)) %>% pull(overall_sd) + 1
      ylim_min <- test_site_score %>% filter(overall_sd == min(overall_sd)) %>% pull(overall_sd) - 1

      g2 <- ggplot(test_site_score, aes(y = overall_sd, x = site, color = site,
                                        tooltip = tooltip)) +
        geom_point_interactive(show.legend = FALSE) +
        theme_minimal() +
        scale_color_squba() +
        geom_hline(yintercept = 0, linetype = 'solid') +
        #geom_hline(yintercept = 1, linetype = 'dotted', color = 'gray', linewidth = 1) +
        #geom_hline(yintercept = -1, linetype = 'dotted', color = 'gray', linewidth = 1) +
        #ylim(ylim_min, ylim_max) +
        labs(title = 'Average Standard Deviation per Site',
             y = 'Average Standard Deviation',
             x = 'Site')

      plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                  'tooltip' = TRUE)
      g2[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                 'tooltip' = TRUE)

      opt <- list(plt,
                  g2)

      return(opt)

    }
  }else{
    suppressWarnings(
      far_site <- process_output %>%
        # filter(anomaly_yn != 'no outlier in group') %>%
        mutate(zscr = (!!sym(comparison_col) - mean_val) / sd_val,
               zscr = ifelse(is.nan(zscr), NA, zscr),
               zscr = abs(zscr)) %>%
        group_by(fu_bin) %>%
        filter(zscr == max(zscr, na.rm = TRUE)) %>%
        summarise(farthest_site = site,
                  nvar = n())

    )

    if(any(far_site$nvar > 1)){
      far_site <- far_site %>%
        summarise_all(toString) %>% select(-nvar)
    }else{
      far_site <- far_site %>% select(-nvar)
    }

    suppressWarnings(
      close_site <- process_output %>%
        # filter(anomaly_yn != 'no outlier in group') %>%
        mutate(zscr = (!!sym(comparison_col) - mean_val) / sd_val,
               zscr = ifelse(is.nan(zscr), NA, zscr),
               zscr = abs(zscr)) %>%
        group_by(fu_bin) %>%
        filter(zscr == min(zscr, na.rm = TRUE)) %>%
        summarise(closest_site = site,
                  nvar = n())
    )

    if(any(close_site$nvar > 1)){
      close_site <- close_site %>%
        summarise_all(toString) %>% select(-nvar)
    }else{
      close_site <- close_site %>% select(-nvar)
    }

    nsite_anom <- process_output %>%
      group_by(fu_bin, anomaly_yn) %>%
      summarise(site_w_anom = n_distinct(site)) %>%
      filter(anomaly_yn == 'outlier') %>%
      ungroup() %>%
      select(-anomaly_yn)

    sitesanoms <- process_output %>%
      filter(anomaly_yn == 'outlier') %>%
      group_by(fu_bin) %>%
      summarise(site_anoms = toString(site)) %>%
      select(fu_bin, site_anoms)

    tbl <- process_output %>%
      group_by(fu_bin) %>%
      mutate(iqr_val = stats::IQR(!!sym(comparison_col))) %>%
      ungroup() %>%
      distinct(fu_bin, mean_val, sd_val, median_val, iqr_val) %>%
      left_join(nsite_anom) %>%
      left_join(sitesanoms) %>%
      left_join(far_site) %>%
      left_join(close_site) %>%
      mutate(delim = sub("^([^,]+,){5}([^,]+).*", "\\2", site_anoms),
             site_anoms = ifelse(site_w_anom > 5,
                                 stringr::str_replace(site_anoms, paste0(",", delim, '(.*)'), ' . . .'),
                                 site_anoms)) %>%
      select(-delim) %>%
      gt::gt() %>%
      gt::tab_header('Large N Anomaly Detection Summary Table') %>%
      gt::cols_label(fu_bin = 'Follow-Up Band',
                 site_anoms = 'Site(s) with Anomaly',
                 mean_val = 'Mean',
                 sd_val = 'Standard Deviation',
                 median_val = 'Median',
                 iqr_val = 'IQR',
                 site_w_anom = 'No. Sites w/ Anomaly',
                 farthest_site = 'Site(s) Farthest from Mean',
                 closest_site = 'Site(s) Closest to Mean') %>%
      gt::sub_missing(missing_text = 0,
                  columns = site_w_anom) %>%
      gt::sub_missing(missing_text = '--',
                  columns = c(farthest_site, closest_site, site_anoms)) %>%
      gt::fmt_number(columns = c(mean_val, median_val, sd_val, iqr_val),
                 decimals = 3) %>%
      gt::opt_stylize(style = 2)

    if(!is.null(large_n_sites)){
      plt<-ggplot(dat_to_plot %>% filter(site %in% large_n_sites),
                  aes(x=site, y=fu_bin, text=text, color=!!sym(comparison_col)))+
        geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
        geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier',
                                                             site %in% large_n_sites),
                               aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
        scale_color_squba(palette = 'diverging', discrete = FALSE) +
        scale_shape_manual(values=c(19,8))+
        scale_y_discrete(labels = label_wrap_gen()) +
        theme_minimal() +
        labs(y = "Variable",
             size="",
             title=paste0('Anomalous Event Co-Occurrence per Years of F/U by Site'),
             subtitle = 'Dot size is the mean proportion per variable') +
        guides(color = guide_colorbar(title = 'Proportion'),
               shape = guide_legend(title = 'Anomaly'),
               size = 'none')

      plt[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                                  'tooltip' = TRUE)

      opt <- list(plt,
                  tbl)

      return(opt)
    }else{
      return(tbl)
    }
  }


}


#' *Single Site, Anomaly Detection, Longitudinal*
#'
#' @param process_output the output from `prc_process`
#' @param event_filter the event type of interest for the analysis; can be either
#'                     `a`, `b`, `both`, or `neither`
#' @param facet vector of variables to be used to facet the graph
#'
#' @return if analysis was executed by year or greater, a P Prime control chart
#'         is returned with outliers marked with orange dots
#'
#'         if analysis was executed by month or smaller, an STL regression is
#'         conducted and outliers are marked with red dots. the graphs representing
#'         the data removed in the regression are also returned
#'
prc_ss_anom_la <- function(process_output,
                           event_filter,
                           facet = NULL){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(tolower(event_filter) %in% c('a', 'b', 'both', 'neither')){
    check_var <- NULL
  }else{cli::cli_abort('Invalid event_filter: please choose {.code A}, {.code B}, {.code Both}, or {.code Neither} (case insensitive)')}

  time_inc <- process_output %>% ungroup() %>% filter(!is.na(time_increment)) %>%
    distinct(time_increment) %>% pull()

  dat_to_plot <- process_output %>%
    mutate(filt_col = case_when(tolower(event_filter) == 'a' ~ 'Event A Only',
                                tolower(event_filter) == 'b' ~ 'Event B Only',
                                tolower(event_filter) == 'neither' ~ 'Neither Event',
                                tolower(event_filter) == 'both' ~ 'Both Events')) %>%
    filter(filt_col == stat_type)

  title <- dat_to_plot %>% ungroup() %>% distinct(filt_col) %>% pull()

  if(time_inc == 'year'){

    facet <- facet %>% append('stat_type') %>% unique()

    dat_to_plot <- dat_to_plot %>%
      unite(facet_col, !!!syms(facet), sep = '\n') %>%
      rename('ycol' = stat_ct,
             'denom' = total_pts)

    pp_qi <-  qic(data = dat_to_plot, x = time_start, y = ycol, chart = 'pp', facets = ~facet_col,
                  title = paste0('Control Chart: Proportion of Patients with ', title),
                  ylab = 'Proportion', xlab = 'Time',
                  show.grid = TRUE, n = denom)

    op_dat <- pp_qi$data

    new_pp <- ggplot(op_dat,aes(x,y)) +
      geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "lightgray",alpha = 0.4) +
      geom_line(colour = squba_colors_standard[[12]], size = .5) +
      geom_line(aes(x,cl)) +
      geom_point(colour = squba_colors_standard[[6]] , fill = squba_colors_standard[[6]], size = 1) +
      geom_point(data = subset(op_dat, y >= ucl), color = squba_colors_standard[[3]], size = 2) +
      geom_point(data = subset(op_dat, y <= lcl), color = squba_colors_standard[[3]], size = 2) +
      facet_wrap(~facet1, scales = 'free_y', ncol = 2) +
      ggtitle(label = paste0('Control Chart: Proportion of Patients with ', title)) +
      labs(x = 'Time',
           y = 'Proportion')+
      theme_minimal()

    new_pp[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                   'tooltip' = FALSE)

    output <- new_pp

  }else{

    anomalies <-
      plot_anomalies(.data=dat_to_plot,
                     .date_var=time_start,
                     .interactive=FALSE,
                     .title=paste0('Anomalies for ', title)) #%>%
      #layout(title = paste0('Anomalies for ', title))

    decomp <-
      plot_anomalies_decomp(.data=process_output,
                            .date_var=time_start,
                            .interactive=FALSE,
                            .title=paste0('Anomalies for ', title)) #%>%
      #layout(title = paste0('Anomalies for ', title))

    anomalies[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                      'tooltip' = FALSE)
    decomp[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                   'tooltip' = FALSE)

    output <- list(anomalies, decomp)

  }

  return(output)

}


#' *Multi Site, Anomaly Detection, Longitudinal*
#'
#' @param process_output the output from `prc_process`
#' @param event_filter the event type of interest for the analysis; can be either
#'                     `a`, `b`, `both`, or `neither`
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally generate a filtered visualization
#'
#' @return three graphs:
#'    1) line graph that shows the smoothed proportion of patients
#'    across time computation with the Euclidean distance associated with each line
#'    2) line graph that shows the raw proportion of patients
#'    across time computation with the Euclidean distance associated with each line
#'    3) a bar graph with the Euclidean distance value for each site, with the average
#'    proportion as the fill
#'
prc_ms_anom_la <- function(process_output,
                           event_filter,
                           large_n = FALSE,
                           large_n_sites = NULL){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(tolower(event_filter) %in% c('a', 'b', 'both', 'neither')){
    check_var <- NULL
  }else{cli::cli_abort('Invalid event_filter: please choose {.code A}, {.code B}, {.code Both}, or {.code Neither} (case insensitive)')}

  filt_op <- process_output %>%
    mutate(filt_col = case_when(tolower(event_filter) == 'a' ~ 'Event A Only',
                                tolower(event_filter) == 'b' ~ 'Event B Only',
                                tolower(event_filter) == 'neither' ~ 'Neither Event',
                                tolower(event_filter) == 'both' ~ 'Both Events')) %>%
    filter(filt_col == stat_type) %>%
    mutate(prop_col = prop_event)

  title <- filt_op %>% ungroup() %>% distinct(filt_col) %>% pull()

  allsites <-
    filt_op %>%
    select(time_start,stat_type,mean_allsiteprop) %>% distinct() %>%
    rename(prop_col=mean_allsiteprop) %>%
    mutate(site='all site average') %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Proportion: ",round(prop_col,3)),
           text_raw=paste0("Site: ", site,
                           "\n","Proportion: ",round(prop_col,3)))

  iqr_dat <- filt_op %>%
    select(time_start,stat_type,prop_col) %>% distinct() %>%
    group_by(time_start, stat_type) %>%
    summarise(q1 = stats::quantile(prop_col, 0.25),
              q3 = stats::quantile(prop_col, 0.75))


  dat_to_plot <-
    filt_op %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",round(prop_col,3),
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean))

  if(!large_n){
    p <- dat_to_plot %>%
      ggplot(aes(y = prop_col, x = time_start, color = site, group = site, text = text_smooth)) +
      geom_line(data=allsites, linewidth=1.1) +
      geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
      theme_minimal() +
      scale_color_squba() +
      labs(y = 'Proportion (Loess)',
           x = 'Time',
           title = paste0('Smoothed Proportion of ', title, ' Across Time'))

    q <- dat_to_plot %>%
      ggplot(aes(y = prop_col, x = time_start, color = site,
                 group=site, text=text_raw)) +
      geom_line(data=allsites,linewidth=1.1) +
      geom_line(linewidth=0.2) +
      theme_minimal() +
      scale_color_squba() +
      labs(x = 'Time',
           y = 'Proportion',
           title = paste0('Proportion of ', title, ' Across Time'))

    t <- dat_to_plot %>%
      distinct(site, dist_eucl_mean, site_loess) %>%
      group_by(site, dist_eucl_mean) %>%
      summarise(mean_site_loess = mean(site_loess)) %>%
      mutate(tooltip = paste0('Site: ', site,
                              '\nEuclidean Distance: ', dist_eucl_mean,
                              '\nAverage Loess Proportion: ', mean_site_loess)) %>%
      ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess, tooltip = tooltip)) +
      geom_segment(aes(x = site, xend = site, y = 0, yend = dist_eucl_mean), color = 'navy') +
      geom_point_interactive(aes(fill = mean_site_loess), shape = 21, size = 4) +
      coord_radial(r.axis.inside = FALSE, rotate.angle = TRUE) +
      guides(theta = guide_axis_theta(angle = 0)) +
      theme_minimal() +
      scale_fill_squba(palette = 'diverging', discrete = FALSE) +
      labs(fill = 'Avg. Proportion \n(Loess)',
           y ='Euclidean Distance',
           x = '',
           title = paste0('Euclidean Distance for ', title))

    p[['metadata']] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

    q[['metadata']] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

    t[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                              'tooltip' = TRUE)

    output <- list(p,
                   q,
                   t)
  }else{
    q <- ggplot(allsites, aes(x = time_start)) +
      geom_ribbon(data = iqr_dat, aes(ymin = q1, ymax = q3), alpha = 0.2) +
      geom_line(aes(y = prop_col, color = site, group = site), linewidth=1.1) +
      geom_point_interactive(aes(y = prop_col, color = site, group = site, tooltip=text_raw)) +
      theme_minimal() +
      #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
      scale_color_squba() +
      labs(x = 'Time',
           y = 'Proportion',
           title = paste0('Proportion of ', title, ' Across Time'),
           subtitle = 'Ribbon boundaries are IQR')

    if(is.null(large_n_sites)){

      t <- dat_to_plot %>%
        distinct(stat_type, dist_eucl_mean) %>%
        ggplot(aes(x = dist_eucl_mean, y = stat_type)) +
        geom_boxplot() +
        geom_point_interactive(color = 'gray',
                               alpha = 0.75, aes(tooltip = dist_eucl_mean)) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              legend.title = element_blank()) +
        scale_fill_squba(palette = 'diverging', discrete = FALSE) +
        labs(x ='Euclidean Distance',
             y = '',
             title = paste0('Distribution of Euclidean Distances'))

    }else{

      q <- q + geom_line(data = dat_to_plot %>% filter(site %in% large_n_sites),
                         aes(y = prop_col, color = site, group = site),
                         linewidth=0.2) +
        geom_point_interactive(data = dat_to_plot %>% filter(site %in% large_n_sites),
                               aes(y = prop_col, color = site, group = site, tooltip=text_raw))

      t <- dat_to_plot %>%
        distinct(stat_type,dist_eucl_mean) %>%
        ggplot(aes(x = dist_eucl_mean, y = stat_type)) +
        geom_boxplot() +
        geom_point_interactive(data = dat_to_plot %>% filter(site %in% large_n_sites),
                               aes(color = site, tooltip = dist_eucl_mean)) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              legend.title = element_blank()) +
        scale_fill_squba(palette = 'diverging', discrete = FALSE) +
        scale_color_squba() +
        labs(x ='Euclidean Distance',
             y = '',
             title = paste0('Distribution of Euclidean Distances'))
    }

    q[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                              'tooltip' = TRUE)
    t[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                              'tooltip' = TRUE)

    output <- q + t + plot_layout(ncol = 1, heights = c(5, 1))
  }

  return(output)

}
