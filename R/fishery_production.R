#' @name fishery_production
#' @title Fishery production
#' @description Total fishery production. Catch by species of the French purse seine fishing fleet.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fishery_production function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param fishing_type  {\link[base]{character}} expected. FSC, FOB or TOTAL.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
fishery_production <- function(data_connection,
                               time_period,
                               country = as.integer(x = c(1,41)),
                               vessel_type = as.integer(x = 1),
                               fishing_type = "TOTAL"
) {
  # 0 - Global variables assignement ----
  ocean <- NULL
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
  TOTAL_WITH_DSC <- NULL
  TOTAL <- NULL
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    fishery_production_sql <- paste(readLines(con = system.file("sql",
                                                                "balbaya_fishery_production.sql",
                                                                package = "fishi")),
                                    collapse = "\n")
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
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # 3 - Data design ----
  fishery_production_t1 <- fishery_production_data %>%
    dplyr::mutate(school_type = dplyr::case_when(l4c_tban == "IND" ~ "free",
                                                 l4c_tban == "BL"  ~ "free",
                                                 l4c_tban == "BO"  ~ "log",
                                                 T ~ "und"),
                  YFT = dplyr::case_when(c_esp == 1 ~ v_poids_capt * rf3,
                                         T ~ 0),
                  SKJ = dplyr::case_when(c_esp == 2 ~ v_poids_capt * rf3,
                                         T ~ 0),
                  BET = dplyr::case_when(c_esp == 3 ~ v_poids_capt * rf3,
                                         T ~ 0),
                  ALB = dplyr::case_when(c_esp == 4 ~ v_poids_capt * rf3,
                                         T ~ 0),
                  DSC = dplyr::case_when(c_esp == 8 | (c_esp >=800 & c_esp <= 899) ~ v_poids_capt * rf3,
                                         T ~ 0),
                  OTH = dplyr::case_when(c_esp == 1 | c_esp == 2 | c_esp == 3 | c_esp == 4 | c_esp == 8 | (c_esp >=800 & c_esp <= 899) ~ 0,
                                         T ~ v_poids_capt * rf3))

  if (fishing_type == "TOTAL") {
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
                       TOTAL = sum(TOTAL_WITH_DSC, na.rm = TRUE) - sum(DSC, na.rm = TRUE),
                       .groups = "drop")

    table_catch_all <- fishery_production_t2 %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(YFT = sum(YFT, na.rm = TRUE),
                       SKJ = sum(SKJ, na.rm = TRUE),
                       BET = sum(BET, na.rm = TRUE),
                       ALB = sum(ALB, na.rm = TRUE),
                       OTH = sum(OTH, na.rm = TRUE),
                       TOTAL = sum(TOTAL, na.rm = TRUE),
                       .groups = "drop")
    # 4 - Legend design ----
    # 5 - Graphic design ----
    par(cex.axis = 1.4,
        cex.lab = 1.4,
        las = 1)
    plotrix::stackpoly(x = matrix(table_catch_all$year,
                                  nrow = length(table_catch_all$year),
                                  ncol=3),
                       y = table_catch_all[,c("YFT","SKJ","BET")]/1000,
                       stack = TRUE,
                       cex.axis = 1.3,
                       cex.lab = 1.3,
                       xlab = "",
                       ylab = "Catch (x1000 t)",
                       las = 1,
                       main = "",
                       axis4 = FALSE,
                       col=c("khaki1",
                             "firebrick2",
                             "cornflowerblue"),
                       cex = 1.3,
                       las = 1,
                       ylim = c(0,
                                max((table_catch_all$TOTAL*1.02)/1000,
                                    na.rm=T)))
    abline(h = seq(20,
                   100,
                   20),
           col = "lightgrey",
           lty = 2)
    legend("topright",
           legend = c("BET",
                      "SKJ",
                      "YFT"),
           bty ="n",
           fill =c("khaki1",
                   "firebrick2",
                   "cornflowerblue"),
           cex = 1.3)
  } else if (fishing_type == "FSC"){
    fishery_production_t1 <- subset(fishery_production_t1, school_type == "free")

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
                       TOTAL = sum(TOTAL_WITH_DSC, na.rm = TRUE) - sum(DSC, na.rm = TRUE),
                       .groups = "drop")

    table_catch_all <- fishery_production_t2 %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(YFT = sum(YFT, na.rm = TRUE),
                       SKJ = sum(SKJ, na.rm = TRUE),
                       BET = sum(BET, na.rm = TRUE),
                       ALB = sum(ALB, na.rm = TRUE),
                       OTH = sum(OTH, na.rm = TRUE),
                       TOTAL = sum(TOTAL, na.rm = TRUE),
                       .groups = "drop")


    # 4 - Legend design ----
    # 5 - Graphic design ----
    par(cex.axis = 1.4,
        cex.lab = 1.4,
        las = 1)
    plotrix::stackpoly(x = matrix(table_catch_all$year,
                                  nrow = length(table_catch_all$year),
                                  ncol=3),
                       y = table_catch_all[,c("YFT","SKJ","BET")]/1000,
                       stack = TRUE,
                       cex.axis = 1.3,
                       cex.lab = 1.3,
                       xlab = "",
                       ylab = "Catch (x1000 t)",
                       las = 1,
                       main = "",
                       axis4 = FALSE,
                       col=c("khaki1",
                             "firebrick2",
                             "cornflowerblue"),
                       cex = 1.3,
                       las = 1,
                       ylim = c(0,
                                max((table_catch_all$TOTAL*1.02)/1000,
                                    na.rm=T)))
    abline(h = seq(20,
                   100,
                   20),
           col = "lightgrey",
           lty = 2)
    legend("topright",
           legend = c("BET",
                      "SKJ",
                      "YFT"),
           bty ="n",
           fill =c("khaki1",
                   "firebrick2",
                   "cornflowerblue"),
           cex = 1.3)
    legend("topleft",
           bty = "n",
           legend = "(FSC)",
           cex =1.3)
  } else if (fishing_type == "FOB" ) {
    fishery_production_t1 <- subset(fishery_production_t1, school_type == "log")

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
                       TOTAL = sum(TOTAL_WITH_DSC, na.rm = TRUE) - sum(DSC, na.rm = TRUE),
                       .groups = "drop")

    table_catch_all <- fishery_production_t2 %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(YFT = sum(YFT, na.rm = TRUE),
                       SKJ = sum(SKJ, na.rm = TRUE),
                       BET = sum(BET, na.rm = TRUE),
                       ALB = sum(ALB, na.rm = TRUE),
                       OTH = sum(OTH, na.rm = TRUE),
                       TOTAL = sum(TOTAL, na.rm = TRUE),
                       .groups = "drop")


    # 4 - Legend design ----
    # 5 - Graphic design ----
    par(cex.axis = 1.4,
        cex.lab = 1.4,
        las = 1)
    plotrix::stackpoly(x = matrix(table_catch_all$year,
                                  nrow = length(table_catch_all$year),
                                  ncol=3),
                       y = table_catch_all[,c("YFT","SKJ","BET")]/1000,
                       stack = TRUE,
                       cex.axis = 1.3,
                       cex.lab = 1.3,
                       xlab = "",
                       ylab = "Catch (x1000 t)",
                       las = 1,
                       main = "",
                       axis4 = FALSE,
                       col=c("khaki1",
                             "firebrick2",
                             "cornflowerblue"),
                       cex = 1.3,
                       las = 1,
                       ylim = c(0,
                                max((table_catch_all$TOTAL*1.02)/1000,
                                    na.rm=T)))
    abline(h = seq(20,
                   100,
                   20),
           col = "lightgrey",
           lty = 2)
    legend("topright",
           legend = c("BET",
                      "SKJ",
                      "YFT"),
           bty ="n",
           fill =c("khaki1",
                   "firebrick2",
                   "cornflowerblue"),
           cex = 1.3)
    legend("topleft",
           bty = "n",
           legend = "(FOB)",
           cex =1.3)

  }


}
