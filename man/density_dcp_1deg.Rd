% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density_dcp_1deg.R
\name{density_dcp_1deg}
\alias{density_dcp_1deg}
\title{Density of dcp 1 deg}
\usage{
density_dcp_1deg(dataframe, ocean = 1)
}
\arguments{
\item{dataframe}{{\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the density_dcp_1deg() function.}

\item{ocean}{{\link[base]{integer}} expected. Ocean code to choose the ocean, 1 by default.}
}
\value{
The function return ggplot R plot.
}
\description{
Map of the the density of deployments of dFADs 1 deg (Only for OA for the moment)
}
\details{
The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
\preformatted{
   count | center_geom_x | center_geom_y |  activity_date  | poly_geom
   ------------------------------------------------------------------------
    110  | 0.5           | 0.5           | 2013            | POLYGON ((0 0, 0 1, 1 1, 1 ...
    174  | 0.5           | 0.5           | 2014            | POLYGON ((0 0, 0 1, 1 1, 1 ...
    175  | 0.5           | 0.5           | 2023            | POLYGON ((0 0, 0 1, 1 1, 1 ...
}
}
