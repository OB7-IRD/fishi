#' @name fishery_production
#' @title Total fishery production
#' @description Total fishery production (catch by species).
#' Generates a figure for catches (x 1000 t) of the three main tunas: BET, SKJ and YFT.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the fishery_production() function.
#' @param fishing_type  {\link[base]{character}} expected. FSC, FOB or ALL. ALL by default.
#' @param graph_type {\link[base]{character}} expected. plot, plotly, table or percentage. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Referentials.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  - activity_date}}
#'  \item{\code{  - c_esp}}
#'  \item{\code{  - country_id}}
#'  \item{\code{  - flag}}
#'  \item{\code{  - fleet}}
#'  \item{\code{  - gear}}
#'  \item{\code{  - l4c_tban}}
#'  \item{\code{  - ocean_id}}
#'  \item{\code{  - ocean_name}}
#'  \item{\code{  - vessel_type_id}}
#'  \item{\code{  - v_poids_capt}}
#' }
#' @return The function return ggplot R plot.
#' @export
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
#' @importFrom ggplot2 ggplot aes geom_area scale_fill_manual scale_y_continuous labs theme_bw
#' @importFrom plotly ggplotly
#' @importFrom graphics par plot axis lines abline legend
#' @importFrom tidyr pivot_longer
#' @importFrom codama r_type_checking
fishery_production <- function(dataframe,
                               fishing_type = "ALL",
                               graph_type = "plot",
                               title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  v_poids_capt <- NULL
  ocean_name <- NULL
  gear <- NULL
  fleet <- NULL
  flag <- NULL
  school_type <- NULL
  YFT <- NULL
  SKJ <- NULL
  BET <- NULL
  ALB <- NULL
  OTH <- NULL
  DSC <- NULL
  total_with_DSC <- NULL
  total <- NULL
  count <- NULL
  specie <- NULL
  time_period <- NULL
  Catch <- NULL
  Species <- NULL
  # 1 - Arguments verification ----
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
  if (codama::r_type_checking(r_object = title,
                              type = "logical",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = title,
                                   type = "logical",
                                   output = "message"))
  }
  # 2 - Data design ----
  # Add columns year, school type and species
  fishery_production_t1 <- dataframe %>%
    dplyr::mutate(year = lubridate::year(x = activity_date),
                  school_type = dplyr::case_when(l4c_tban == "IND" ~ "free",
                                                 l4c_tban == "BL"  ~ "free",
                                                 l4c_tban == "BO"  ~ "log",
                                                 TRUE ~ "und"),
                  YFT = dplyr::case_when(c_esp == 1 ~ v_poids_capt,
                                         TRUE ~ 0),
                  SKJ = dplyr::case_when(c_esp == 2 ~ v_poids_capt,
                                         TRUE ~ 0),
                  BET = dplyr::case_when(c_esp == 3 ~ v_poids_capt,
                                         TRUE ~ 0),
                  ALB = dplyr::case_when(c_esp == 4 ~ v_poids_capt,
                                         TRUE ~ 0),
                  DSC = dplyr::case_when(c_esp == 8 | (c_esp >= 800 & c_esp <= 899) ~ v_poids_capt,
                                         TRUE ~ 0),
                  OTH = dplyr::case_when(c_esp == 1 | c_esp == 2 | c_esp == 3 | c_esp == 4 | c_esp == 8 | (c_esp >= 800 & c_esp <= 899) ~ 0,
                                         TRUE ~ v_poids_capt),
                  total_with_DSC = v_poids_capt)
  # Sum columns species
  fishery_production_t2 <- fishery_production_t1 %>%
    dplyr::group_by(ocean_name,
                    year,
                    gear,
                    fleet,
                    flag,
                    school_type) %>%
    dplyr::summarise(YFT = sum(YFT, na.rm = TRUE),
                     SKJ = sum(SKJ, na.rm = TRUE),
                     BET = sum(BET, na.rm = TRUE),
                     ALB = sum(ALB, na.rm = TRUE),
                     DSC = sum(DSC, na.rm = TRUE),
                     OTH = sum(OTH, na.rm = TRUE),
                     total = sum(total_with_DSC, na.rm = TRUE) - sum(DSC, na.rm = TRUE),
                     .groups = "drop")
  if (fishing_type == "ALL") {
    ### Percentage of catches by species on all fishing mode
    # Group results by year
    table_catch_all <- fishery_production_t2 %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(YFT = sum(YFT, na.rm = TRUE),
                       SKJ = sum(SKJ, na.rm = TRUE),
                       BET = sum(BET, na.rm = TRUE),
                       ALB = sum(ALB, na.rm = TRUE),
                       OTH = sum(OTH, na.rm = TRUE),
                       total = sum(total, na.rm = TRUE),
                       .groups = "drop")
  } else if (fishing_type == "FSC") {
    ### Percentage of catches by species on FSC
    # Group results by year
    table_catch_all <- fishery_production_t2 %>%
      dplyr::filter(school_type == "free") %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(YFT = sum(YFT, na.rm = TRUE),
                       SKJ = sum(SKJ, na.rm = TRUE),
                       BET = sum(BET, na.rm = TRUE),
                       ALB = sum(ALB, na.rm = TRUE),
                       OTH = sum(OTH, na.rm = TRUE),
                       total = sum(total, na.rm = TRUE),
                       .groups = "drop")
  } else if (fishing_type == "FOB") {
    ### Percentage of catches by species on FOB
    # Group results by year
    table_catch_all <- fishery_production_t2 %>%
      dplyr::filter(school_type == "log") %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(YFT = sum(YFT, na.rm = TRUE),
                       SKJ = sum(SKJ, na.rm = TRUE),
                       BET = sum(BET, na.rm = TRUE),
                       ALB = sum(ALB, na.rm = TRUE),
                       OTH = sum(OTH, na.rm = TRUE),
                       total = sum(total, na.rm = TRUE),
                       .groups = "drop")
  }
  # 3 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data         = dataframe$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = dataframe$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = dataframe$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  # time_period
  time_period <- c(unique(min(fishery_production_t1$year):max(fishery_production_t1$year)))
  # 4 - Graphic design ----
  if (graph_type == "plot") {
    graphics::par(cex.axis = 1.4,
                  cex.lab = 1.4,
                  las = 2)
    if (title == TRUE) {
      plotrix::stackpoly(x = matrix(table_catch_all$year,
                                    nrow = length(table_catch_all$year),
                                    ncol = 3),
                         y = table_catch_all[, c("YFT", "SKJ", "BET")] / 1000,
                         stack = TRUE,
                         cex.axis = 1.3,
                         cex.lab = 1.3,
                         xlab = "",
                         ylab = "Catch (x1000 t)",
                         main = paste0("Fishery production by ",
                                       fishing_type,
                                       " fishing mode. Catch by species of the ",
                                       country_legend,
                                       " ",
                                       vessel_type_legend,
                                       " fishing",
                                       "\n",
                                       "fleet during ",
                                       min(time_period),
                                       "-",
                                       max(time_period),
                                       ", in the ",
                                       ocean_legend,
                                       " ocean."),
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
                         y = table_catch_all[, c("YFT", "SKJ", "BET")] / 1000,
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
                     legend = c("BET",
                                "SKJ",
                                "YFT"),
                     bty = "n",
                     fill = c("cornflowerblue",
                              "firebrick2",
                              "khaki1"),
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
  } else if (graph_type == "ggplot") {
    table_long <- pivot_longer(table_catch_all,
                               cols = c("YFT",
                                        "SKJ",
                                        "BET"),
                               names_to = "Species",
                               values_to = "Catch")
    ggplot(table_long,
           aes(x = year,
               y = Catch / 1000,
               fill = Species)) +
      ggplot2::geom_area(position = "stack") +
      ggplot2::scale_fill_manual(values = c("YFT" = "khaki1",
                                            "SKJ" = "firebrick2",
                                            "BET" = "cornflowerblue")) +
      ggplot2::labs(x = "",
                    y = "Catch (x1000 t)",
                    title = "") +
      ggplot2::ylim(0, max((table_catch_all$total * 1.02) / 1000,
                           na.rm = TRUE)) +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  breaks = table_long$year) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1,
                                                         size = 12),
            legend.position = "top",
            legend.justification = "right",
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(size = 0.2,
                                                       color = "gray90")) +
      ggplot2::labs(fill = NULL)
  } else if (graph_type == "plotly") {
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
      ggplot2::scale_fill_manual(values = c("cornflowerblue",
                                            "firebrick2",
                                            "khaki1")) +
      ggplot2::scale_y_continuous(name = "Catch (x1000 t)") +
      ggplot2::theme_bw() +
      ggplot2::labs(fill = "")
    # plotly
    plotly_graph <- plotly::ggplotly(ggplot_table_catch)
    # Add a title
    if (title == TRUE) {
      plotly_graph <- plotly_graph %>%
        plotly::layout(title = list(text = paste0("Fishery production by ",
                                                  fishing_type,
                                                  " fishing mode. Catch by species of the ",
                                                  country_legend,
                                                  " ",
                                                  vessel_type_legend,
                                                  " fishing",
                                                  "\n",
                                                  "fleet during ",
                                                  min(time_period),
                                                  "-",
                                                  max(time_period),
                                                  ", in the ",
                                                  ocean_legend,
                                                  " ocean."),
                                    font = list(size = 17)),
                       margin = list(t = 120))

    }
    # Plot the plotly
    plotly_graph %>%
      plotly::layout(legend = list(orientation = "v",
                                   x = 0.9,
                                   y = 0.95))

  } else if (graph_type == "table") {
    table_catch_all <- round(table_catch_all, 0)
    table_catch_all <- table_catch_all %>%
      dplyr::summarise(Year = year,
                       YFT = YFT,
                       SKJ = SKJ,
                       BET = BET,
                       ALB = ALB,
                       OTH = OTH,
                       TOTAL = total)
    as.data.frame(table_catch_all)
  } else if (graph_type == "percentage") {
    table_catch_all <- table_catch_all %>%
      dplyr::summarise(Year = year,
                       YFT = YFT / total * 100,
                       SKJ = SKJ / total * 100,
                       BET = BET / total * 100,
                       ALB = ALB / total * 100,
                       OTH = OTH / total * 100,
                       TOTAL = total)
    table_catch_all <- round(table_catch_all, 1)
    table_catch_all$TOTAL <- round(table_catch_all$TOTAL, 0)
    as.data.frame(table_catch_all)
  }
}
