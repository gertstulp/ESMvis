# library(ggplot2)

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
