% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fishing_activity.R
\name{fishing_activity}
\alias{fishing_activity}
\title{Annual number of fishing sets}
\usage{
fishing_activity(dataframe, graph_type = "plot", title = FALSE)
}
\arguments{
\item{dataframe}{{\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the fishing_activity() function.}

\item{graph_type}{{\link[base]{character}} expected. plot, plotly or table. Plot by default.}

\item{title}{TRUE or FALSE expected. False by default.}
}
\value{
The function return ggplot R plot.
}
\description{
Fishing operations. Annual number of fishing sets on FOB-associated and free-swimming tuna schools.
}
\details{
The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
\itemize{
 \item{\code{  activity_date}}
 \item{\code{  school_code}}
 \item{\code{  positive_set}}
 \item{\code{  total_set}}
 \item{\code{  v_tpec}}
}
Add these columns for an automatic title (optional):
\itemize{
 \item{\code{  country_code}}
 \item{\code{  ocean_code}}
 \item{\code{  vessel_type_code}}
}
}
