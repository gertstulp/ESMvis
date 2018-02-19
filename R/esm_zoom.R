# library(ggplot2)
#library(patchwork) # Needs to be added to Description still


#' Wrapper function to create timeseries line plot and below a zoomed-in plot.
#'
#' @param data A dataframe.
#' @param data_zoom A dataframe with the period selected for zoom.
#' @param var_date A string with the name of the date-variable for the x-axis.
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
#' @param vars_events A vector with XXXXX.
#' @return A ggplot-object/graph.
#' @import ggplot2

#' @export
esm_zoom <- function(data = NULL,
                     data_zoom = NULL,
                     var_date = NULL,
                     lines = NULL,
                     outcome = NULL,
                     vis_options = NULL,
                     vars_events = NULL)
{

  overall_ts <- esm_ts(data, var_date = var_date, lines = "Name",
                       outcome = "Score", vis_options = vis_options)

  overall_ts <- overall_ts + annotate("rect", xmin = min(data_zoom[var_date]),
                              xmax = max(data_zoom[var_date]),
                              ymin = -Inf, ymax = Inf,
                              alpha = .2,
                              fill = "blue")

  zoom_ts <- esm_ts(data_zoom, var_date = var_date, lines = "Name",
                    outcome = "Score", vis_options = vis_options)

  #both_ts <- overall_ts + zoom_ts + plot_layout(ncol = 1)
  #both_ts
  zoom_ts # Patchwork not on CRAN YET problem.
}
