# library(ggplot2)
#library(dplyr)
#' Wrapper function to create network for each date / measurement.
#'
#' @param data A dataframe.
#' @param var_date A string that will be passed to facet_wrap(as.formula(var_date)).
#' @param vars_meas A vector with strings of variable names that determines order of x-axis.
#' @param vars_groups A vector of specific variable names that are in vars_meas.
#' @param nodes A string with name of variable specifying the different groups which will be represented as different nodes.
#' @param outcome A string with name of variable specifying the outcome.
#' @param vis_options A string with name of variable specifying the different groups which will be represented as different lines
#' @param interval BLABLAA
#' @param vars_event LAALAL ALABELS
#' The possibilities for the vis_options are:
#' "axis_limits = ..."; vector with lower and upper limit of y-axis (e.g., c(0, 10))
#' @return A ggplot-object/graph.
#' @import ggplot2
#' @import dplyr left_join

#' @export
esm_nw <- function(data = NULL,
                   var_date = NULL,
                   vars_meas = NULL,
                   vars_groups = NULL,
                   interval = NULL,
                   nodes = NULL,
                   outcome = NULL,
                   vis_options = NULL,
                   vars_event = NULL)
{

  no_nodes <- length(vars_meas)
  if (no_nodes == 2) {
    node_df <- data.frame(Name = vars_meas,
                          x = c(0,0),
                          y = c(1, -1),
                          stringsAsFactors = FALSE)
  } else {
    node_df <- data.frame(Name = vars_meas,
                          x = sin(2 * pi * ((0:(no_nodes - 1))/no_nodes)),
                          y = cos(2 * pi * ((0:(no_nodes - 1))/no_nodes)),
                          stringsAsFactors = FALSE)
  }

  if (!is.null(vars_groups)) {
   node_df$node_colours <-  vars_groups
  }

  #print(str(node_df))

  data <- dplyr::left_join(data, node_df, by = "Name")
  #filter(data, !is.na(c(outcome)))
  data$abbr <- substr(data[[nodes]], 1, 4)
  #print(sum(is.na(data$Score)))
  plot <- ggplot(data,
                 aes_string(x = "x", y = "y")) +
    geom_point(size = 10, colour = "lightgrey") +
    scale_x_continuous(expand = c(0.20, 0)) +
    scale_y_continuous(expand = c(0.20, 0)) +
    coord_fixed() +
    facet_wrap(as.formula(paste("~", var_date))) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.key = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      strip.background = element_rect(fill = NA, colour = "grey50"),
      panel.border = element_blank(),
      panel.grid = element_blank()#,
      #panel.border = element_rect(fill = NA, color = "grey50")
    )

  if(!is.null(interval)) {
    if(interval == "week") {
      plot <- plot + facet_grid(
        as.formula(paste("ind_int_esmvis" , "~", "wday_esmvis")),
        drop = FALSE
      )
    } else if(interval == "day") {
      plot <- plot + facet_grid(
        as.formula(paste("ind_int_esmvis" , "~", "dayno_esmvis"))
      )
    }
  }


  #week_esmvis, day_esmvis, ind_int_esmvis

  if ( is.null(vars_groups) ) {
      plot <- plot + geom_point(aes_string(size = outcome, colour = nodes)) +
        #scale_size_continuous(limits = c(1, 10))
        scale_radius(range = c(1,10))# CHECK HOW THIS WORKS
  } else {
    names(vars_groups) <- vars_meas
    plot <- plot + geom_point(aes_string(size = outcome, colour = nodes)) +
      scale_colour_manual(values = vars_groups) +
      #scale_colour_identity() +
      scale_radius(range = c(1,10))
      #scale_size_continuous(limits = c(1, 10)) # CHECK HOW THIS WORKS
  }

  plot <- plot + geom_text(aes(label = abbr), colour = "white") +
    guides(colour = FALSE, size = FALSE)

  if ( !is.null(vis_options[["axis_limits"]]) ) {
    plot <- plot +
      scale_y_continuous(limits = vis_options[["axis_limits"]])
  }

  plot

}
#?scale_colour_manual
