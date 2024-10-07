
#' Patient Record Consistency Visualizations
#'
#' @param process_output the summary output from `prc_process`
#' @param output_function the name of the output function that should be executed; this is provided
#'                        in a console message after `prc_process` finishes executing
#' @param dist_from_stat the statistic from which distance should be measured for ms_exp_at
#'                       acceptable values are `mean` or `median`
#' @param event_filter the event type of interest for the analysis for ss_anom_at or ms_anom_at;
#'                     can be either `a`, `b`, `both`, or `neither`
#'
#' @return a graph visualizing the output from `prc_process`; see individual functions for
#'         more specific details
#'
#' @export
#'
prc_output <- function(process_output,
                       output_function,
                       dist_from_stat = 'mean',
                       event_filter){


  if(output_function == 'prc_ss_exp_nt'){

    prc_output <- prc_ss_exp_nt(process_output = process_output)

  }else if(output_function == 'prc_ms_exp_nt'){

    prc_output <- prc_ms_exp_nt(process_output = process_output)

  }else if(output_function == 'prc_ss_anom_nt'){

    prc_output <- prc_ss_anom_nt(process_output = process_output)

  }else if(output_function == 'prc_ms_anom_nt'){

    prc_output <- prc_ms_anom_nt(process_output = process_output)

  }else if(output_function == 'prc_ss_exp_at'){

    prc_output <- prc_ss_exp_at(process_output = process_output)

  }else if(output_function == 'prc_ms_exp_at'){

    prc_output <- prc_ms_exp_at(process_output = process_output,
                                dist_from_stat = dist_from_stat)

  }else if(output_function == 'prc_ss_anom_at'){

    prc_output <- prc_ss_anom_at(process_output = process_output,
                                 event_filter = event_filter)

  }else if(output_function == 'prc_ms_anom_at'){

    prc_output <- prc_ms_anom_at(process_output = process_output,
                                 event_filter = event_filter)

  }else{cli::cli_abort('Please enter a valid output function for this check type.')}

  return(prc_output)

}
