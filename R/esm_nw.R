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
#' @importFrom dplyr left_join
#' @importFrom lubridate as_date

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

  # Create dataframe with layout of nodes in a circle
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

  # Combine coordinates nodes with data
  data <- dplyr::left_join(data, node_df, by = "Name")

  # Create abbreviations of node names
  data$abbr <- substr(data[[nodes]], 1, 4)

  # Create baseplot with grey background circles
  plot <- ggplot(data,
                 aes_string(x = "x", y = "y")) +
    geom_point(size = 10, colour = "lightgrey") +
    scale_x_continuous(expand = c(0.20, 0)) +
    scale_y_continuous(expand = c(0.20, 0)) +
    #coord_fixed() +
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

  # Create appropriate facetting grid
  if(!is.null(interval)) {
    if(interval == "week") {
      plot <- plot + facet_grid(
        as.formula(paste("ind_int_esmvis" , "~", "wday_esmvis")),
        drop = FALSE
      )
    } else if(interval == "day") {
      plot <- plot + facet_grid(
        as.formula(paste("as_date(date_esmvis)" , "~", "ind_int_esmvis"))
      )
    } # else if(interval == "all") {
#      plot <- plot + ggtitle(paste(data[["date_esmvis"]]))
#    }
  }

  #week_esmvis, day_esmvis, ind_int_esmvis

  # Add coloured circles depending on score
  plot <- plot +
    geom_point(aes_string(size = outcome, colour = nodes)) +
    scale_radius(range = c(1,10))

  # Add manual colouring scheme when provided
  if ( !is.na(vars_groups) ) {
    names(vars_groups) <- vars_meas
    plot <- plot + scale_colour_manual(values = vars_groups)
  }

  # Add variable label in circle
  plot <- plot +
    geom_text(aes(label = abbr), colour = "white") +
    guides(colour = FALSE, size = FALSE)

  # plot <- plot +
  #   geom_text(aes(label = abbr), colour = "white") +
  #   guides(colour = FALSE, size = FALSE)

  # THIS WORKS MUCH LESS WELL WITH NETWORKS
  # if ( !is.null(vis_options[["axis_limits"]]) ) {
  #   plot <- plot +
  #     scale_y_continuous(limits = vis_options[["axis_limits"]])
  # }

  if( !is.null(vars_event) ) {
    #print((data))

    data_label <- data %>%
      select(c("date_esmvis", "ind_int_esmvis",
               "wday_esmvis", vars_event[["score_event"]])) %>%
     filter( !is.na(data[vars_event[["score_event"]]])) %>%
      unique()
    #print(data_label)
    plot <- plot + geom_label(data = data_label,
      aes_string(x = 0, y = 0, label = '"PL"', alpha = vars_event[["score_event"]]),
      fill = "seagreen4") +
      guides(alpha = FALSE)


    #   vars_event = list(score_event = "plezierig"),
      # data=filter(df_ggplot$comb_df_lbl[[1]],
      #             colour!="black"),
      # aes(x = x, y = y, label=text,
      #     fill=colour, alpha=abs(alpha)),
      # hjust=0.5, vjust=0.5,
      # colour="white", size=6,
      # show.legend=FALSE, inherit.aes=FALSE) +
      # geom_label(
      #   data=filter(df_ggplot$comb_df_lbl[[1]], colour=="black"),
      #   aes(x = x, y = y, label=text),
      #   hjust=0.5, vjust=0.5,
      #   show.legend=FALSE, inherit.aes=FALSE)
  }






  plot

}
#?scale_colour_manual
