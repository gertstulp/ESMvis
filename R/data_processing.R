library(tidyr)
library(dplyr)
library(lubridate)

data_processing <- function(data = NULL,
                            var_date = NULL,
                            format_date = NULL,
                            vars_meas = NULL,
                            vars_groups = NULL,
                            vars_event = NULL,
                            vars_descr = NULL,
                            ID = NULL,
                            type_vis = "timeseries",
                            time_frame = "all",
                            sel_period = NULL,
                            sel_period_zoom = NULL,
                            show_events_ts = FALSE)
{

  # Checking all arguments ---------------------------
  if (is.null(data) || !is.data.frame(data)) {
    stop("Please provide a dataframe with the argument 'data = ...'.
         Perhaps use data.frame(...)")
  }

  if ( is.null(var_date) ) {
    warning("No date variable specified. Using rownumber as date.
            Use argument 'var_date = ...' to specify")
    data$date_esmvis <- 1:nrow(data)
  } else if ( inherits(data[[var_date]],
                       c("Date", "POSIXlt", "POSIXct", "is.POSIXt")) ) {
    data$date_esmvis <- data[[var_date]]
  } else {
    if ( is.null(format_date) ) {
      warning("No dateformat specified. Using rownumber as date.
              Use argument 'format_date = ...' to specify")
      data$date_esmvis <- 1:nrow(data)
    } else {
      data$date_esmvis <- parse_date_time(data[[var_date]], format_date)
    }
  }

  if ( is.null(vars_meas) ) {
    stop("No variables specified for visualisation.
         Use argument 'vars_meas = ...' to specify.")
  } else if ( !all(vars_meas %in% colnames(data)) ) {
    stop("Not all variables specified in 'vars_meas = ...' exist!")
  } else if ( !any(apply(data[vars_meas], 2, is.numeric)) ) {
    stop("Not all variables specified in 'vars_meas = ...' are numeric!")
  }

  if ( !is.null(vars_groups) ) {
    if ( !all(vars_groups %in% colnames(data)) ) {
      stop("Not all variables specified in 'vars_groups = ...' exist!")
    } else if ( !all(vars_groups %in% vars_meas) ) {
      stop("Not all grouping variables specified in 'vars_groups = ...'
           are specified in 'vars_meas = ...'")
    }
  }

  # check_events(vars_events)
  # check_descr(vars_descr)

  if ( length(vars_event) != 0 ) {
    if (length(vars_event) == 2) {
      if(is.null(vars_event[["score_event"]]) ||
         is.null(vars_event[["text_event"]]) ) {
        stop("One of the variables in 'vars_event = ...' is NULL")
      } else if ( !all(c(vars_event[["score_event"]], vars_event[["text_event"]]
                  %in% colnames(data)) ) ) {
        stop("Not all variables  in 'vars_event = ...' exist!")
      } else if ( !is.numeric(data[[vars_event[["score_event"]]]]) ) {
        stop("Variable in 'score_event = ...' not numerical!")
      }
    } else {
      if( !is.null(vars_event[["score_event"]]) ) {
        if ( !(vars_event[["score_event"]] %in% colnames(data)) ) {
          stop("Variable in 'score_event = ...' does not exist!")
        } else if ( !is.numeric(data[[vars_event[["score_event"]]]]) ) {
          stop("Variable in 'score_event = ...' not numerical!")
        }
      } else if( !is.null(vars_event[["text_event"]]) ) {
        if ( !(vars_event[["text_event"]] %in% colnames(data)) ) {
          stop("Variable in 'text_event = ...' does not exist!")
        }
      } else {
        stop("Variable in 'vars_event = ...' is NULL")
      }
    }
  }


  if ( length(ID) != 0 ) {
    if ( length(ID) == 2 ) {
      if ( !is.null(ID[["var_ID"]]) && !is.null(ID[["ID"]]) ) {
        if ( !all(ID[["var_ID"]] %in% colnames(data)) ) {
          stop("Not all variables in 'ID = ...' exist!")
        } else if ( !all(ID[["ID"]] %in% data[[ID[["var_ID"]]]]) ) {
          stop("ID specified not found in data.")
        }
      } else if ( !is.null(ID[["var_ID"]]) && is.null(ID[["ID"]]) ) {
        stop("No ID-number/person specified.
               Use 'list(var_ID = ..., ID = ...)' for specification.")
      } else if ( is.null(ID[["var_ID"]]) && !is.null(ID[["ID"]]) ) {
        stop("ID/person specified, but not the name of the ID-variable.
             Use 'list(var_ID = ..., ID = ...)' for specification.")
      }
    } else {
      stop("Please specify both ID-variable and ID.
             Use 'list(var_ID = ..., ID = ...)' for specification.")
    }
  }

  # Filter
  if ( !is.null(sel_period) ) {
    if ( sel_period[1] < min(data[date_var], na.rm = TRUE) ) {
      stop("Specified first date not within data")
    } else if ( sel_period[2] > max(data[date_var], na.rm = TRUE) ) {
      stop("Specified last date not within data")
    } else if (sel_period[1] > sel_period[2] ) {
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
