# library(ggplot2)
library(patchwork) # Needs to be added to Description still


#' Wrapper function to create animation across time.
#'
#' @param data A dataframe.
#' @param data_zoom A dataframe with the period selected for zoom.
#' @param var_date A string with the name of the date-variable for the x-axis.
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
#' @import ggplot2

#' @export
esm_ts_nw <- function(data = NULL,
                     data_zoom = NULL,
                     var_date = NULL,
                     vars_event = NULL,
                     vars_meas = NULL,
                     vars_groups = NULL,
                     lines = NULL,
                     outcome = NULL,
                     vis_options = NULL,
                     interval = NULL)
{


  overall_ts <- esm_ts(data, var_date = var_date, lines = "Name",
                       outcome = "Score", vis_options = vis_options)

  overall_ts <- overall_ts + annotate("rect", xmin = min(data_zoom[[var_date]]),
                                      xmax = max(data_zoom[[var_date]]),
                                      ymin = -Inf, ymax = Inf,
                                      alpha = .2,
                                      fill = "blue") +
    guides(colour= FALSE) + labs(title = "Timeline")

  zoom_ts <- esm_ts(data_zoom, var_date = var_date, lines = "Name",
                    outcome = "Score", vis_options = list(point = TRUE,
                                                          smooth = FALSE,
                                                          se_band = FALSE,
                                                          line = TRUE)) +
    theme(legend.position = "top") + labs(title = "Zoom")

  nw <- esm_nw(data_zoom, var_date = var_date, vars_meas = vars_meas,
               vars_groups = vars_groups,
               nodes = "Name",
               outcome = "Score", vis_options = vis_options,
               interval = interval)

  overall_ts + zoom_ts - nw + plot_layout(ncol = 1, heights = c(1, 3))

}
