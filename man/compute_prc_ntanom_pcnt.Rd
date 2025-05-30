% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prc_process_subfuncs_pcornet.R
\name{compute_prc_ntanom_pcnt}
\alias{compute_prc_ntanom_pcnt}
\title{Compute Jaccard Index based on years of follow up for not over
time anomaly detection}
\usage{
compute_prc_ntanom_pcnt(
  cohort,
  site_col,
  grouped_list = "site",
  event_tbl = patientrecordconsistency::prc_event_file,
  grp_breaks = c(0, 1, 3, 8, 11, 15, 25, 50, 100)
)
}
\arguments{
\item{cohort}{table of cohort members with at least the site_col, \code{person_id}, \code{start_date}, and \code{end_date}}

\item{site_col}{the name of the column with the site names}

\item{grouped_list}{list of columns that should be used to group the table}

\item{event_tbl}{a table with the definitions for each event with the columns
\itemize{
\item \code{event} - A or B
\item \code{event_label} - a descriptive label for the event
\item \code{domain_tbl} - the default CDM table from which data is retrieved
\item \code{concept_field} - the field in the table where the codes of interest are stored
\item \code{date_field} - the date field to be used to establish the index & occurrence dates
\item \code{vocabulary_field} - (PCORnet only) The name of the column in the domain table where the vocabulary type is stored
\item \code{codeset_name} - the name of the codeset in the specs directory to define the variable of interest
\item \code{filter_logic} - a string indicating any filter logic that should be applied to establish the event
ex: an Hba1c > 6.5
}}

\item{grp_breaks}{a numeric vector that defines how to group different windows of follow up time}
}
\value{
a dataframe containing the jaccard index of the two user defined events within
each follow up window as definted by grp_breaks
}
\description{
Compute Jaccard Index based on years of follow up for not over
time anomaly detection
}
