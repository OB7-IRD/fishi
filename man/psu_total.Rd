% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psu_total.R
\name{psu_total}
\alias{psu_total}
\title{PSU Total}
\usage{
psu_total(
  dataframe,
  reported_year = NULL,
  selected_country = NULL,
  selected_ocean = NULL,
  selected_harbour = NULL,
  selected_variable = "trip"
)
}
\arguments{
\item{dataframe}{{\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the psu_total() function.}

\item{reported_year}{{\link[base]{integer}} expected. Write the wanted year of the report.}

\item{selected_country}{{\link[base]{integer}} expected. Country code to select the list of boat to count. If NULL give all the vessel for the given year.}

\item{selected_ocean}{{\link[base]{integer}} expected. Ocean code to select the list of boat to count. If NULL give all the vessel for the given year, works only for 'data_type' == 'observe'}

\item{selected_harbour}{{\link[base]{integer}} expected. Harbour code to select the list of boat to count. If NULL give all the vessel for the given year, works only for 'data_type' == 'observe'}

\item{selected_variable}{{\link[base]{character}} expected. Write the variable of the PSU. Can be 'trip', 'vessel' or 'well'. 'trip' by default.}
}
\value{
The function return a table.
}
\description{
Give the number of total psu for a given year.
}
\details{
The input dataframe frome sql must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
\itemize{
 \item{\code{  program}}
 \item{\code{  ocean_label}}
 \item{\code{  fleet}}
 \item{\code{  vessel_label}}
 \item{\code{  vessel_type_code}}
 \item{\code{  landing_date}}
 \item{\code{  country_code}}
 \item{\code{  vessel_well_number}}
 \item{\code{  arrival}}
 \item{\code{  port_arrival}}
}
\preformatted{
   program                          | ocean_label | fleet | vessel_label | vessel_type_code | landing_date | country_code | vessel_well_number | arrival   | port_arrival
   -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   AVDTH Atlantique 1999-2022 (IRD) | Atlantic    | FRA   | VIA AVENIR   | 6                | 2021-01-15   | 1            | 4T                 | 2021-01-1 | ABIDJAN
   AVDTH Atlantique 1999-2022 (IRD) | Atlantic    | FRA   | VIA AVENIR   | 6                | 2021-01-15   | 1            | 4T                 | 2021-01-1 | ABIDJAN
   AVDTH Atlantique 1999-2022 (IRD) | Atlantic    | FRA   | VIA AVENIR   | 6                | 2021-01-15   | 1            | 4T                 | 2021-01-1 | ABIDJAN
}
}
