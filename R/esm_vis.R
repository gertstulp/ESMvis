#' Wrapper function to create desired visualisation.
#'
#' @param data A dataframe.
#' @param var_date A string with the name of the date-variable.
#' @param format_date A string with the format of the date (e.g., "ymd", "ymd_HM").
#' @param vars_meas A vector of variable names that need to be visualised.
#' @param vars_groups A vector of specific variable names that are in vars_meas.
#' @param vars_event A vector of names of variables that describe events.
#' @param vars_descr A vector of names of variables that describe events.
#' @param ID A list that must contain the elements ID_var and ID; ID_var must be a string with a variable name, and ID must be a string of the unique identifier.
#' @param type_vis The type of visualisation required; the options are "timeseries" (default), "zoom", "barchart", and "network".
#' @param vis_options A list with visualisation options. See the functions esm_ts, esm_bc, and esm_nw for further help.
#' @param time_frame A vector with the first and last measurement.
#' @param sel_period A vector with the first and last measurement for selecting a specific period.
#' @param sel_period_zoom A vector with the first and last measurement for selecting a specific period in the "zoom" graph.
#' @return A ggplot-object/graph.

#' @export
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
  } else if (type_vis == "network") {
    esm_nw(data_ts, var_date = "date_esmvis", vars_meas, vars_groups,
           nodes = "Name", outcome = "Score", vis_options = vis_options)
  }
}
