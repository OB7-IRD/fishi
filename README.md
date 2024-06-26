
<!-- README.md is generated from README.Rmd. Please edit that file and click on Knit button at the end. -->

# fishi R package <a href='https://ob7-ird.github.io/fishi'><img src='man/figures/logo.png' align="right" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fishi)](https://cran.r-project.org/package=fishi)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/OB7-IRD/fishi/workflows/R-CMD-check/badge.svg)](https://github.com/OB7-IRD/furdeb/actions)
<!-- badges: end -->

***FISHeries Indicators: exploratory and descriptive processes of data
associated to fisheries***

## Overview

The fishi package is designed to facilitate fisheries and marine science
research by providing a set of functions. It aims to simplify common
tasks in fisheries research and to improve the efficiency and
effectiveness of data analysis and interpretation in this field. It can
be used with csv/excel datasets and sql queries.

This version contains a large number of indicators for running the
statistical report
(<https://forge.ird.fr/marbec/ob7/statistical-report>) and the RShiny
(<https://forge.ird.fr/marbec/ob7/rshiny-fishi>).

## Installation

You can install the fishi package directly from GitHub using the
devtools package:

``` r
devtools::install_github("https://github.com/OB7-IRD/fishi",
                         INSTALL_opts=c("--no-multiarch"))
```

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of furdeb from GitHub.

``` r
devtools::install_github("https://github.com/OB7-IRD/fishi",
                         ref = "development",
                         INSTALL_opts=c("--no-multiarch"))
```

## Example Usage

Here is a basic example demonstrating some of the functionalities of the
fishi package.

### Data Loading

Load data from a CSV file:

``` yml
data <- read.csv("path_to_your_file.csv")
```

Or load data from a SQL database with DBI:

``` yml
db_connection <- DBI::dbConnect(RMySQL::MySQL(), 
                                dbname = "your_database_name", 
                                host = "your_host", 
                                user = "your_username", 
                                password = "your_password")

query <- "SELECT * FROM your_table"
data <- DBI::dbGetQuery(db_connection, query)
```

Or load data from a SQL database with furdeb:

``` yml
db_connection <- DBI::dbConnect(RMySQL::MySQL(), 
                                dbname = "your_database_name", 
                                host = "your_host", 
                                user = "your_username", 
                                password = "your_password")

# anchors informations
ocean <- as.integer(x = 1)
country <- as.integer(x = 1)
report_year <- as.integer(2022)
vessel_type = as.integer(x = c(4, 5, 6))

activity_vessel_type <- furdeb::data_extraction(type = "database", 
                                                file_path = system.file("sql",
                                                                        "balbaya_fishing_activity_vesseltype.sql",
                                                                        package = "fishi"),
                                                database_connection = balbaya_con, 
                                                anchor = list(time_period = time_period,
                                                              country = country,
                                                              vessel_type = vessel_type,
                                                              ocean = ocean))
```

### Vizualisation

``` yml
library(fishi)
fishing_capacity(dataframe = activity_vessel_type,
                 graph_type = "plot")
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on [GitHub issues
page](https://github.com/OB7-IRD/fishi/issues). This link is also
available if you have any questions and improvement propositions.
