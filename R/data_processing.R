data_processing <- function(data = NULL,
                            date_var = NULL,
                            vars_meas = NULL,
                            vars_groups = NULL,
                            vars_events = NULL,
                            vars_descr = NULL,
                            type_vis = "timeseries",
                            time_frame = "all",
                            sel_period = NULL,
                            sel_period_zoom = NULL,
                            show_events_ts = FALSE)
{

  if(is.null(data) || !is.data.frame(data)) {
    stop("Please provide a dataframe with the argument 'data = ...'.
         Perhaps use data.frame(...)")
  }

  # Filter
  if( !is.null(sel_period) ) {
    if( sel_period[1] < min(data[date_var], na.rm = TRUE) ) {
      stop("Specified first date not within data")
    } else  if( sel_period[2] > max(data[date_var], na.rm = TRUE) ) {
      stop("Specified last date not within data")
    } else if(sel_period[1] > sel_period[2] ) {
      stop("Last date earlier than first date!")
    } else {
      data <- filter(data,
                     data[date_var] >= sel_period[1] &
                     data[date_var] <= sel_period[2])
    }
  }

  # From wide to long
  data_l <- gather(data, vars_meas,
                   key = "Name", value = "Score")

  if(type_vis == "zoom") {
    if( !is.null(sel_period_zoom) ) {
      if( sel_period_zoom[1] < min(data_l[date_var], na.rm = TRUE) ) {
        stop("Specified first date not within data")
      } else if( sel_period_zoom[2] > max(data_l[date_var], na.rm = TRUE) ) {
        stop("Specified last date not within data")
      } else if(sel_period_zoom[1] > sel_period_zoom[2] ) {
        stop("Last date earlier than first date!")
      } else {
        data_zoom <- filter(data_l,
                       data_l[date_var] >= sel_period_zoom[1] &
                       data_l[date_var] <= sel_period_zoom[2])
      }
    }
    return(list(data_l = data_l,
                data_zoom = data_zoom))
  } else {
    return(list(data_l = data_l))
  }

}
