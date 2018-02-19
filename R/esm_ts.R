# library(ggplot2)

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
