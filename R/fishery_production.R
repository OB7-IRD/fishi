#' @name fishery_production
#' @title Fishery production
#' @description total fishery production. Catch by species of the French purse seine fishing fleet.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fishery_production() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification. 1 by default.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification. 1 by default.
#' @param fishing_type  {\link[base]{character}} expected. FSC, FOB or ALL.
#' @param graph_type {\link[base]{character}} expected. plot, plotly, table or percentage. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
#' @importFrom ggplot2 ggplot aes geom_area scale_fill_manual scale_y_continuous labs theme_bw
#' @importFrom plotly ggplotly
#' @importFrom graphics par plot axis lines abline legend
#' @importFrom tidyr pivot_longer
#' @importFrom codama r_type_checking
fishery_production <- function(data_connection,
                               time_period,
                               ocean,
                               country = as.integer(x = 1),
                               vessel_type = as.integer(x = 1),
                               fishing_type = "ALL",
                               graph_type = "plot",
                               title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  v_poids_capt <- NULL
  rf3 <- NULL
  ocean_name <- NULL
  gear <- NULL
  fleet <- NULL
  flag <- NULL
  school_type <- NULL
  yft <- NULL
  skj <- NULL
  bet <- NULL
  alb <- NULL
  oth <- NULL
  dsc <- NULL
  total_with_dsc <- NULL
  total <- NULL
  count <- NULL
  specie <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = data_connection,
                              type = "list",
                              length = 2L,
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = data_connection,
                                   type = "list",
                                   length = 2L,
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = time_period,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = time_period,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = ocean,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = ocean,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = country,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = country,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = vessel_type,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = vessel_type,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = fishing_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = fishing_type,
                                   type = "character",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    fishery_production_sql <- paste(readLines(con = system.file("sql",
                                                                "balbaya_fishery_production.sql",
                                                                package = "fishi")),
                                    collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  fishery_production_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                      sql  = fishery_production_sql,
                                                      time_period = DBI::SQL(paste(time_period,
                                                                                   collapse = ", ")),
                                                      country     = DBI::SQL(paste(country,
                                                                                   collapse = ", ")),
                                                      vessel_type = DBI::SQL(paste(vessel_type,
                                                                                   collapse = ", ")),
                                                      ocean = DBI::SQL(paste(ocean,
                                                                             collapse = ", ")))
  fishery_production_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                           statement = fishery_production_sql_final))
  # 3 - Data design ----
  # Add columns year, school type and species
  fishery_production_t1 <- fishery_production_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date),
                  school_type = dplyr::case_when(l4c_tban == "IND" ~ "free",
                                                 l4c_tban == "BL"  ~ "free",
                                                 l4c_tban == "BO"  ~ "log",
                                                 TRUE ~ "und"),
                  yft = dplyr::case_when(c_esp == 1 ~ v_poids_capt * rf3,
                                         TRUE ~ 0),
                  skj = dplyr::case_when(c_esp == 2 ~ v_poids_capt * rf3,
                                         TRUE ~ 0),
                  bet = dplyr::case_when(c_esp == 3 ~ v_poids_capt * rf3,
                                         TRUE ~ 0),
                  alb = dplyr::case_when(c_esp == 4 ~ v_poids_capt * rf3,
                                         TRUE ~ 0),
                  dsc = dplyr::case_when(c_esp == 8 | (c_esp >= 800 & c_esp <= 899) ~ v_poids_capt * rf3,
                                         TRUE ~ 0),
                  oth = dplyr::case_when(c_esp == 1 | c_esp == 2 | c_esp == 3 | c_esp == 4 | c_esp == 8 | (c_esp >= 800 & c_esp <= 899) ~ 0,
                                         TRUE ~ v_poids_capt * rf3),
                  total_with_dsc = v_poids_capt * rf3)
  # Sum columns species
  fishery_production_t2 <- fishery_production_t1 %>%
    dplyr::group_by(ocean_name,
                    year,
                    gear,
                    fleet,
                    flag,
                    school_type) %>%
    dplyr::summarise(yft = sum(yft, na.rm = TRUE),
                     skj = sum(skj, na.rm = TRUE),
                     bet = sum(bet, na.rm = TRUE),
                     alb = sum(alb, na.rm = TRUE),
                     dsc = sum(dsc, na.rm = TRUE),
                     oth = sum(oth, na.rm = TRUE),
                     total = sum(total_with_dsc, na.rm = TRUE) - sum(dsc, na.rm = TRUE),
                     .groups = "drop")
  if (fishing_type == "ALL") {
    ### Percentage of catches by species on all fishing mode
    # Group results by year
    table_catch_all <- fishery_production_t2 %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(yft = sum(yft, na.rm = TRUE),
                       skj = sum(skj, na.rm = TRUE),
                       bet = sum(bet, na.rm = TRUE),
                       alb = sum(alb, na.rm = TRUE),
                       oth = sum(oth, na.rm = TRUE),
                       total = sum(total, na.rm = TRUE),
                       .groups = "drop")
  } else if (fishing_type == "FSC") {
    ### Percentage of catches by species on FSC
    # Group results by year
    table_catch_all <- fishery_production_t2 %>%
      dplyr::filter(school_type == "free") %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(yft = sum(yft, na.rm = TRUE),
                       skj = sum(skj, na.rm = TRUE),
                       bet = sum(bet, na.rm = TRUE),
                       alb = sum(alb, na.rm = TRUE),
                       oth = sum(oth, na.rm = TRUE),
                       total = sum(total, na.rm = TRUE),
                       .groups = "drop")
  } else if (fishing_type == "FOB") {
    ### Percentage of catches by species on FOB
    # Group results by year
    table_catch_all <- fishery_production_t2 %>%
      dplyr::filter(school_type == "log") %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(yft = sum(yft, na.rm = TRUE),
                       skj = sum(skj, na.rm = TRUE),
                       bet = sum(bet, na.rm = TRUE),
                       alb = sum(alb, na.rm = TRUE),
                       oth = sum(oth, na.rm = TRUE),
                       total = sum(total, na.rm = TRUE),
                       .groups = "drop")
  }
  # 4 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data         = fishery_production_data$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = fishery_production_data$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = fishery_production_data$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  # 5 - Graphic design ----
  if (graph_type == "plot") {
    graphics::par(cex.axis = 1.4,
                  cex.lab = 1.4,
                  las = 2)
    if (title == TRUE) {
      plotrix::stackpoly(x = matrix(table_catch_all$year,
                                    nrow = length(table_catch_all$year),
                                    ncol = 3),
                         y = table_catch_all[, c("yft", "skj", "bet")] / 1000,
                         stack = TRUE,
                         cex.axis = 1.3,
                         cex.lab = 1.3,
                         xlab = "",
                         ylab = "Catch (x1000 t)",
                         main = paste0("Fishery production by ", fishing_type, " fishing mode. Catch by species of the ", country_legend, " ", vessel_type_legend, " fishing",
                                       "\n", "fleet during ", min(time_period), "-", max(time_period),", in the ", ocean_legend, " ocean."),
                         axis4 = FALSE,
                         col = c("khaki1",
                                 "firebrick2",
                                 "cornflowerblue"),
                         cex = 1.3,
                         ylim = c(0,
                                  max((table_catch_all$total * 1.02) / 1000,
                                      na.rm = TRUE)))
    } else {
      plotrix::stackpoly(x = matrix(table_catch_all$year,
                                    nrow = length(table_catch_all$year),
                                    ncol = 3),
                         y = table_catch_all[, c("yft", "skj", "bet")] / 1000,
                         stack = TRUE,
                         cex.axis = 1.3,
                         cex.lab = 1.3,
                         xlab = "",
                         ylab = "Catch (x1000 t)",
                         main = "",
                         axis4 = FALSE,
                         col = c("khaki1",
                                 "firebrick2",
                                 "cornflowerblue"),
                         cex = 1.3,
                         ylim = c(0,
                                  max((table_catch_all$total * 1.02) / 1000,
                                      na.rm = TRUE)))
    }
    graphics::abline(h = seq(20,
                             100,
                             20),
                     col = "lightgrey",
                     lty = 2)
    graphics::legend("topright",
                     legend = c("bet",
                                "skj",
                                "yft"),
                     bty = "n",
                     fill = c("khaki1",
                              "firebrick2",
                              "cornflowerblue"),
                     cex = 1.3)
    if (fishing_type == "FSC") {
      graphics::legend("topleft",
                       bty = "n",
                       legend = "(FSC)",
                       cex = 1.3)
    } else if (fishing_type == "FOB") {
      graphics::legend("topleft",
                       bty = "n",
                       legend = "(FOB)",
                       cex = 1.3)
    }
  }
  else if (graph_type == "plotly") {
    # pivot wider
    table_catch_3 <- table_catch_all %>%
      dplyr::select(1:4)
    data_pivot <- tidyr::pivot_longer(table_catch_3,
                                      cols = c(2:4),
                                      names_to = "specie",
                                      values_to = "count")
    # ggplot
    ggplot_table_catch <- ggplot2::ggplot(data_pivot, ggplot2::aes(x = year,
                                                                   y = count,
                                                                   fill = specie)) +
      ggplot2::geom_area() +
      ggplot2::scale_fill_manual(values = c("cornflowerblue", "firebrick2", "khaki1")) +
      ggplot2::scale_y_continuous(name = "Catch (x1000 t)") +
      ggplot2::theme_bw() +
      ggplot2::labs(fill = "")
    # plotly
    plotly_graph <- plotly::ggplotly(ggplot_table_catch)
    # Add a title
    if (title == TRUE) {
      plotly_graph <- plotly_graph %>%
        plotly::layout(title = list(text = paste0("Fishery production by ", fishing_type, " fishing mode. Catch by species of the ", country_legend, " ", vessel_type_legend, " fishing",
                                                 "\n", "fleet during ", min(time_period), "-", max(time_period),", in the ", ocean_legend, " ocean."),
                                    font = list(size = 17)),
                       margin = list(t = 120))

    }
    # Plot the plotly
    plotly_graph %>%
      plotly::layout(legend = list(orientation = "v",
                                   x = 0.9,
                                   y = 0.95))

  }
  else if (graph_type =="table") {
    table_catch_all <- round(table_catch_all, 0)
    table_catch_all <- table_catch_all %>%
      dplyr::summarise(Year = year,
                       YFT = yft,
                       SKJ = skj,
                       BET = bet,
                       ALB = alb,
                       OTH = oth,
                       TOTAL = total)
    as.data.frame(table_catch_all)
  }
  else if (graph_type =="percentage") {
    table_catch_all <- table_catch_all %>%
      dplyr::summarise(Year = year,
                       YFT = yft / total * 100,
                       SKJ = skj / total * 100,
                       BET = bet / total * 100,
                       ALB = alb / total * 100,
                       OTH = oth / total * 100,
                       TOTAL = total)
    table_catch_all <- round(table_catch_all, 1)
    table_catch_all$TOTAL <- round(table_catch_all$TOTAL, 0)
    as.data.frame(table_catch_all)
  }
}
