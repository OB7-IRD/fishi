% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percentage_coverage.R
\name{percentage_coverage}
\alias{percentage_coverage}
\title{Percentage of well cover}
\usage{
percentage_coverage(
  dataframe,
  report_year,
  ocean = "Atlantic",
  vesseltype = "PS"
)
}
\arguments{
\item{dataframe}{{\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the bio_size_tuna() function.}

\item{report_year}{{\link[base]{integer}} expected. Year of the statistical report.}

\item{ocean}{{\link[base]{character}} expected. 'Atlantic' or 'Indian'. Atlantic by default.}

\item{vesseltype}{{\link[base]{character}} expected. 'SV', 'BB' and/or 'PS'. PS by default.}
}
\value{
The function return a table.
}
\description{
Percentage of well cover
}
\details{
The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
\preformatted{
   ocean_label  | flag   | vessel_type | arrival_port | arrival_year |well_with_tuna | well_with_sample
   -----------------------------------------------------------------------------------------------------
   Atlantic     | FRA    | SV          | ABIDJAN      | 2018         |37             | 13
   Atlantic     | FRA    | SV          | ABIDJAN      | 2017         |33             | 27
   Atlantic     | FRA    | BB          | DAKAR        | 2018         |32             | 18
}
}
