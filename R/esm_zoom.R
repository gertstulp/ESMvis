library(ggplot2)
library(patchwork)

esm_zoom <- function(data = NULL,
                     data_zoom = NULL,
                     date_var = NULL,
                     lines = NULL,
                   outcome = NULL,
                   vis_options = list(smooth = TRUE, point = FALSE,
                                      line = FALSE,
                                      kernel = NULL,
                                      se_band = TRUE,
                                      axis_limits = NULL),
                   vars_events = NULL)
{

  overall_ts <- esm_ts(data, date_var = date_var, lines = "Name",
                       outcome = "Score", vis_options = vis_options)

  overall_ts <- overall_ts + annotate("rect", xmin = min(data_zoom[date_var]),
                              xmax = max(data_zoom[date_var]),
                              ymin = -Inf, ymax = Inf,
                              alpha = .2,
                              fill = "blue")

  zoom_ts <- esm_ts(data_zoom, date_var = date_var, lines = "Name",
                    outcome = "Score", vis_options = vis_options)

  both_ts <- overall_ts + zoom_ts + plot_layout(ncol = 1)
  both_ts
}
