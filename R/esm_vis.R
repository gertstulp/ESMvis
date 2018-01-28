library(tidyr)
library(dplyr)

esm_vis <- function(data = NULL,
                    var_date = NULL,
                    format_date = NULL,
                    vars_meas = NULL,
                    vars_groups = NULL,
                    vars_event = NULL,
                    vars_descr = NULL,
                    ID = NULL,
                    type_vis = "timeseries", # what to do when you only want selected
                    vis_options = NULL,
                    time_frame = "all",
                    sel_period = NULL,
                    sel_period_zoom = NULL)
 {

data_process <- data_processing(
  data,
  var_date,
  format_date,
  vars_meas,
  vars_groups,
  vars_event,
  vars_descr,
  ID,
  type_vis,
  time_frame,
  sel_period,
  sel_period_zoom)

data_ts <- data_process[["data_l"]]

# SHOW LAURA!!
  if (type_vis == "timeseries") {
    esm_ts(data_ts, var_date = "date_esmvis", lines = "Name",
           outcome = "Score", vis_options = vis_options)
  } else if (type_vis == "zoom") {
    data_zoom <- data_process[["data_zoom"]]
    esm_zoom(data_ts, data_zoom, var_date = "date_esmvis", lines = "Name",
           outcome = "Score", vis_options = vis_options)
  } else if (type_vis == "barchart") {
    esm_bc(data_ts, var_date = "date_esmvis", vars_meas, bars = "Name",
           outcome = "Score", vis_options = vis_options)
  }
}
