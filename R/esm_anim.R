# library(ggplot2)
library(patchwork) # Needs to be added to Description still
library(animation)

#' Wrapper function to create animation across time.
#'
#' @param data A dataframe.
#' @param data_zoom A dataframe with the period selected for zoom.
#' @param var_date A string with the name of the date-variable for the x-axis.
#' @param interval A string with the name of the date-variable for the x-axis.
#' @param vars_event A vector with XXXXX.
#' @param vars_meas A vector with strings of variable names that determines order of x-axis.
#' @param vars_groups A vector of specific variable names that are in vars_meas.
#' @param lines A string with name of variable specifying the different groups which will be represented as different lines.
#' @param outcome A string with name of variable specifying the outcome.
#' @param vis_options A list with options for the graph.
#' The possibilities for the vis_options are:
#' "line = TRUE/FALSE"; whether or not a line should be drawn
#' "point = TRUE/FALSE"; whether or not points/dots should be drawn
#' "smooth = TRUE/FALSE"; whether or not smoothening line should be drawn.
#' "kernel = ..."; parameter to control the degree of smoothening (between 0 and 1).
#' "se_band = TRUE/FALSE"; whether or not confidence band should be drawn around smoothening line.
#' "axis_limits = ..."; vector with lower and upper limit of y-axis (e.g., c(0, 10))
#' @return A ggplot-object/graph.
#' @importFrom lubridate week day
#' @importFrom dplyr filter
#' @import animation

#' @export
esm_anim <- function(data = NULL,
                     var_date = NULL,
                     interval = NULL,
                     vars_event = NULL,
                     vars_meas = NULL,
                     vars_groups = NULL,
                     lines = NULL,
                     outcome = NULL,
                     vis_options = NULL)
{

      print(esm_ts_nw(data = data,
                data_zoom = filter(data, weekno_esmvis ==  2),
                var_date = var_date,
                vars_event = vars_event,
                vars_meas = vars_meas,
                vars_groups = vars_groups,
                lines = lines,
                outcome = outcome,
                vis_options = vis_options,
                interval = interval))


  if(interval == "week") {
    no_fig <- unique(data[["weekno_esmvis"]])
    no_fig <- no_fig[!is.na(no_fig)]
    data$interval_esm <-  data[["weekno_esmvis"]]
  } else if(interval == "day") {
    no_fig <- unique(data[["dayno_esmvis"]])
    no_fig <- no_fig[!is.na(no_fig)]
    data$interval_esm <-  data[["dayno_esmvis"]]
  } else if(interval == "all") {
    no_fig <- unique(data[["date_esmvis"]])
    no_fig <- no_fig[!is.na(no_fig)]
    data$interval_esm <-  data[["date_esmvis"]]
  }
  #print(paste(no_fig))
#
#   grain = 1.5
#   saveHTML({
#     #for (i in no_fig) {
#     for (i in 1:1) {
#
#       data_zoom <- filter(data, interval_esm ==  i)
#
#       print(
#         esm_ts_nw(data = data,
#                   data_zoom = data_zoom,
#                   var_date = var_date,
#                   vars_event = vars_event,
#                   vars_meas = vars_meas,
#                   vars_groups = vars_groups,
#                   lines = lines,
#                   outcome = outcome,
#                   vis_options = vis_options,
#                   interval = interval)
#       )
#       print(paste(round(i/max(no_fig), digits = 2) * 100, "% done", sep = ""))
#     }
#   }, interval = 2, htmlfile = paste("ESMvis_", Sys.time(), ".html", sep = ""),
#   ani.dev = function(...){png(res = 75 * grain, ...)},
#   ani.width = 1150 * grain, ani.height = 640 * grain, verbose = FALSE,
#   navigator = TRUE)
}
