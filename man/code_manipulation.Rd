% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_manipulation.R
\name{code_manipulation}
\alias{code_manipulation}
\title{Code manipulation}
\usage{
code_manipulation(data, referential, manipulation)
}
\arguments{
\item{data}{{\link[base]{vector}} or {\link[base]{factor}} expected. In practice you can provide all your data with duplicate values and let the function do the rest.}

\item{referential}{{\link[base]{character}} expected. Referential name. Take a look in the section below to see referential available.}

\item{manipulation}{{\link[base]{character}} expected. Type of output expected. You can choose between "legend", "modality" and "color".}
}
\value{
The function return  ggplot R object.
}
\description{
Code manipulation for ouput use in the graphic legend, color or modalities display.
}
\details{
So far, referential available are:
\itemize{
 \item{country: }{code, name and color of countries}
 \item{ocean: }{code, name and color of oceans}
 \item{vessel_simple_type: }{code, name and color of simplified vessel type}
 \item{specie: }{code, name and color of species}
}
}
