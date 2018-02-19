# library(ggplot2)

#' Wrapper function to create barchart for each date / measurement.
#'
#' @param data A dataframe.
#' @param var_date A string with the name of the date-variable for the different facets.
#' @param var_meas A vector with strings of variable names that determines order of x-axis.
#' @param nodes A string with name of variable specifying the different groups which will be represented as different lines.
#' @param outcome A string with name of variable specifying the outcome.
#' @param vis_options A string with name of variable specifying the different groups which will be represented as different lines
#' The possibilities for the vis_options are:
#' "axis_limits = ..."; vector with lower and upper limit of y-axis (e.g., c(0, 10))
#' @return A ggplot-object/graph.
#' @import ggplot2

#' @export
esm_nw <- function(data = NULL,
                   var_date = NULL,
                   vars_meas = NULL,
                   nodes = NULL,
                   outcome = NULL,
                   vis_options = NULL)
{

  plot <- ggplot(data,
                 aes_string(x = nodes, y = outcome, fill = nodes)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    coord_flip() +
    scale_x_discrete(limits = vars_meas) +
    labs(x = "Time") +
    facet_wrap(as.formula(paste("~", var_date)))

  if ( !is.null(vis_options[["axis_limits"]]) ) {
    plot <- plot +
      scale_y_continuous(limits = vis_options[["axis_limits"]])
  }
  plot

}
