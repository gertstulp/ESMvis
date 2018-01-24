library(tidyr)
library(dplyr)

# ADD ID NUMBER IS IMPORTANT
esm_vis <- function(data = NULL,
                    date_var = NULL,
                    vars_meas = NULL,
                    vars_groups = NULL,
                    vars_events = NULL,
                    vars_descr = NULL,
                    type_vis = "timeseries", # what to do when you only want selected
                    vis_options = list(smooth = TRUE, point = FALSE,
                                       line = FALSE,
                                       kernel = 0.9,
                                       se_band = TRUE,
                                       axis_limits = NULL),
                    time_frame = "all",
                    sel_period = NULL,
                    sel_period_zoom = NULL,
                    show_events_ts = FALSE)
 {


  # CHECKS WHETHER INPUT IS CORRECT
  # check_data(data)
  # check_date(date_var)
  # check_meas(vars_meas)
  # check_groups(vars_groups)
  # check_events(vars_events)
  # check_descr(vars_descr)

data_process <- data_processing(data,
  date_var,
  vars_meas,
  vars_groups,
  vars_events,
  vars_descr,
  type_vis,
  time_frame,
  sel_period,
  sel_period_zoom,
  show_events_ts)

data_ts <- data_process[["data_l"]]

  if(type_vis == "timeseries") {
    esm_ts(data_ts, date_var = date_var, lines = "Name",
           outcome = "Score", vis_options = vis_options)
  } else if(type_vis == "zoom") {
    data_zoom <- data_process[["data_zoom"]]
    esm_zoom(data_ts, data_zoom, date_var = date_var, lines = "Name",
           outcome = "Score", vis_options = vis_options)
  } else if(type_vis == "barchart") {
    esm_bc(data_ts, date_var = date_var, vars_meas, bars = "Name",
           outcome = "Score", vis_options = vis_options)
  } else { # Error message weird format. FIX
    stop("You haven't selected correct type of visualisation in the
          argument 'type_vis = ...'. Please choose from: 'timeseries',
          'zoom', 'network', or 'barchart'")
  }

}
