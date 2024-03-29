% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fishing_effort.R
\name{fishing_effort}
\alias{fishing_effort}
\title{Annual total number of fishing and searching days}
\usage{
fishing_effort(dataframe, graph_type = "plot", title = FALSE)
}
\arguments{
\item{dataframe}{{\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the fishing_effort() function.}

\item{graph_type}{{\link[base]{character}} expected. plot, plotly or table. Plot by default.}

\item{title}{TRUE or FALSE expected. False by default.}
}
\value{
The function return ggplot R plot.
}
\description{
Changes in nominal effort over time. Annual total number of fishing and searching days.
}
\details{
The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
\itemize{
 \item{\code{  activity_date}}
 \item{\code{  vessel_code}}
 \item{\code{  flag}}
 \item{\code{  fleet}}
 \item{\code{  vessel_label}}
 \item{\code{  landing_date}}
 \item{\code{  ocean_code}}
 \item{\code{  harbour_label}}
 \item{\code{  set_duration}}
 \item{\code{  vessel_type_code}}
 \item{\code{  total_hour_at_sea}}
 \item{\code{  total_hour_fished}}
}
Add these columns for an automatic title (optional):
\itemize{
 \item{\code{  country_code}}
}
}
