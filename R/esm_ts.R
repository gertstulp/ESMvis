# library(ggplot2)

#' Wrapper function to create timeseries line plot.
#'
#' @param data A dataframe.
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
#' @return A ggplot-object/graph.
#' @import ggplot2

#' @export
esm_ts <- function(data = NULL,
                   var_date = NULL,
                   lines = NULL,
                   outcome = NULL,
                   vis_options = NULL)
{

  plot <- ggplot(data,
                 aes_string(x = var_date, y = outcome, colour = lines)) +
    theme_minimal() +
    labs(x = "Time")

  # SHOW LAURA DEFAULT IS LINE
  if ( length(vis_options) == 0 ) {
    plot <- plot + geom_smooth(se = FALSE)
  } else if ( length(vis_options) != 0 ) {
    if ( !is.null(vis_options[["line"]]) && vis_options[["line"]] == TRUE ) {
      plot <- plot + geom_line()
    }
    if ( !is.null(vis_options[["point"]]) && vis_options[["point"]] == TRUE ) {
      plot <- plot + geom_point()
    }
    if ( !is.null(vis_options[["smooth"]]) &&
         vis_options[["smooth"]] == TRUE)  {
      if (is.null(vis_options[["kernel"]]) &&
          is.null(vis_options[["se_band"]]) ) {
        plot <- plot + geom_smooth()
      }
      if (!is.null(vis_options[["kernel"]]) &&
          !is.null(vis_options[["se_band"]]) ) {
        plot <- plot + geom_smooth(span = vis_options[["kernel"]],
                                   se = vis_options[["se_band"]])
      } else if (!is.null(vis_options[["kernel"]]) ) {
        plot <- plot + geom_smooth(span = vis_options[["kernel"]])
      } else if (!is.null(vis_options[["se_band"]]) ) {
        plot <- plot + geom_smooth(se = vis_options[["se_band"]])
      }
    }
    if ( !is.null(vis_options[["axis_limits"]]) ) {
      plot <- plot +
        scale_y_continuous(limits = vis_options[["axis_limits"]])
    }
  }
  plot

} # CAN BE DONE WITH &labs
