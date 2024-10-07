#' PRC Sample Event File
#'
#' A sample version of the file structure expected for the `prc_event_file`
#' parameter in the `prc_process` function. The user should recreate this file
#' and include their own domain definitions.
#'
#' Files referenced in the `codeset_name` column should be kept in the `file_subdirectory`
#' identified when `initialize_dq_session` is called.
#'
#' @format ## prc_event_file
#' A data frame with 2 rows and 7 columns
#' \describe{
#'   \item{event}{A or B; represents which of the two events should occur first and second in the sequence}
#'   \item{event_label}{An arbitrary string to label what the event is}
#'   \item{domain_tbl}{The name of the CDM table associated with the domain where concepts associated with the event can be found}
#'   \item{concept_field}{The name of the column in the domain table that contains the concepts of interest}
#'   \item{date_field}{The name of the column in the domain table that contains dates to be used for establishing the sequence of events.}
#'   \item{codeset_name}{The name of the codeset (found in the file_subdirectory) to be used to define the event}
#'   \item{filter_logic}{(optional) a string to be parsed as logic to filter the domain_tbl as needed to best represent the event}
#' }
#'
"prc_event_file"
