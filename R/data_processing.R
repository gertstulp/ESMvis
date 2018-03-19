# library(tidyr)
# library(dplyr)
# library(lubridate)

#' Process data before visualisation.
#'
#' @param data A dataframe.
#' @param var_date A string with the name of the date-variable.
#' @param format_date A string with the format of the date (e.g., "ymd", "ymd_HM").
#' @param vars_meas A vector of variable names that need to be visualised.
#' @param vars_groups A vector of specific variable names that are in vars_meas.
#' @param vars_event A vector of names of variables that describe events.
#' @param vars_descr A vector of names of variables that describe events.
#' @param interval XXXXXXXX Can be week or day
#' @param ID A list that must contain the elements ID_var and ID; ID_var must be a string with a variable name, and ID must be a string of the unique identifier.
#' @param type_vis The type of visualisation required; the options are "timeseries" (default), "zoom", "barchart", and "network".
#' @param time_frame A vector with the first and last measurement.
#' @param sel_period A vector with the first and last measurement for selecting a specific period.
#' @param sel_period_zoom A vector with the first and last measurement for selecting a specific period in the "zoom" graph.
#' @return A list with one or two dataframes for visualisation.
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' @importFrom lubridate parse_date_time week wday

#' @export
data_processing <- function(data = NULL,
                            var_date = NULL,
                            format_date = NULL,
                            vars_meas = NULL,
                            vars_groups = NULL,
                            vars_event = NULL,
                            vars_descr = NULL,
                            interval = "week",
                            ID = NULL,
                            type_vis = "timeseries",
                            time_frame = "all",
                            sel_period = NULL,
                            sel_period_zoom = NULL)
{

  # Checking all arguments ---------------------------
  if (is.null(data) || !is.data.frame(data)) {
    stop("Please provide a dataframe with the argument 'data = ...'.
         Perhaps use data.frame(...)")
  }
# COULD ALSO ADD WEEK HERE
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
      data$date_esmvis <- lubridate::parse_date_time(data[[var_date]],
                                                     format_date)
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

  ### CHECKING INTERVAL MAKE FUNCTIONS



  # PAS DEZE AAN
#  if ( !is.null(vars_groups) ) {
#    if ( !all(vars_groups %in% colnames(data)) ) {
#      stop("Not all variables specified in 'vars_groups = ...' exist!")
#    } else if ( !all(vars_groups %in% vars_meas) ) {
#      stop("Not all grouping variables specified in 'vars_groups = ...'
#           are specified in 'vars_meas = ...'")
#    }
#  }

  # check_events(vars_events)
  # check_descr(vars_descr)

  # if ( length(vars_event) != 0 ) {
  #   if (length(vars_event) == 2) {
  #     if(is.null(vars_event[["score_event"]]) ||
  #        is.null(vars_event[["text_event"]]) ) {
  #       stop("One of the variables in 'vars_event = ...' is NULL")
  #     } else if ( !all(c(vars_event[["score_event"]], vars_event[["text_event"]]
  #                 %in% colnames(data)) ) ) {
  #       stop("Not all variables  in 'vars_event = ...' exist!")
  #     } else if ( !is.numeric(data[[vars_event[["score_event"]]]]) ) {
  #       stop("Variable in 'score_event = ...' not numerical!")
  #     }
  #   } else {
  #     if( !is.null(vars_event[["score_event"]]) ) {
  #       if ( !(vars_event[["score_event"]] %in% colnames(data)) ) {
  #         stop("Variable in 'score_event = ...' does not exist!")
  #       } else if ( !is.numeric(data[[vars_event[["score_event"]]]]) ) {
  #         stop("Variable in 'score_event = ...' not numerical!")
  #       }
  #     } else if( !is.null(vars_event[["text_event"]]) ) {
  #       if ( !(vars_event[["text_event"]] %in% colnames(data)) ) {
  #         stop("Variable in 'text_event = ...' does not exist!")
  #       }
  #     } else {
  #       stop("Variable in 'vars_event = ...' is NULL")
  #     }
  #   }
  # }


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
      } else if ( is.null(ID[["var_ID"]]) && is.null(ID[["ID"]]) ) {
        stop("Both ID and var_ID are NULL.
             Use 'list(var_ID = ..., ID = ...)' for specification.")
      }
    } else {
      stop("Please specify both ID-variable and ID.
             Use 'list(var_ID = ..., ID = ...)' for specification.")
    }
  }

  if ( length(ID) == 2 ) {
    data <- dplyr::filter(data,
                          data[ID[["var_ID"]]] ==  ID[["ID"]])
  }


  # ADD WARNINGS
  library(tidyverse)
  if ( !is.null(interval) ) {
    if (inherits(data[["date_esmvis"]],
                 c("Date", "POSIXlt", "POSIXct", "is.POSIXt"))) {
        data$week_esmvis <- lubridate::week(data[["date_esmvis"]])
        data$day_esmvis <- lubridate::wday(data[["date_esmvis"]],
                                           label = TRUE, abbr = TRUE,
                                           week_start = 1)
        data <- data %>%
          arrange(date_esmvis) %>%
          group_by(day_esmvis) %>%
          mutate(ind_int_esmvis = row_number(day_esmvis)) %>%
          ungroup()
    } else if( is.numeric(data[[var_date]]) ) {
      data$week_esmvis <- (( data[["date_esmvis"]] - 1 ) %/% 7) + 1
      data$day_esmvis <- data[["date_esmvis"]]
      data <- data %>%
        arrange(week_esmvis, day_esmvis) %>%
        group_by(week_esmvis, day_esmvis) %>%
        mutate(ind_int_esmvis = row_number(day_esmvis)) %>%
        ungroup()
    }
  }


  if ( !(type_vis %in% c("timeseries", "zoom", "network",
                         "barchart", "combined", "animation")) ) {
    # Error message weird format. FIX
    stop("You haven't selected a correct type of visualisation in the
         argument 'type_vis = ...'. Please choose from: 'timeseries',
         'zoom', 'network', or 'barchart'")
  }

  if ( !is.null(sel_period) ) {
    if ( sel_period[1] < min(data[var_date], na.rm = TRUE) ) {
      stop("First date specified in 'sel_period = ...'
           not within (selected) data")
    } else if ( sel_period[2] > max(data[var_date], na.rm = TRUE) ) {
      stop("First last specified in 'sel_period = ...'
           not within (selected) data")
    } else if (sel_period[1] > sel_period[2] ) {
      stop("Last date before first date in 'sel_period = ...'!")
    }
  }

  if ( !is.null(sel_period) ) {
    data <- dplyr::filter(data,
                          data[var_date] >= sel_period[1] &
                          data[var_date] <= sel_period[2])
  }

  if ( !is.null(sel_period_zoom) ) {
    if ( !(type_vis %in% c("zoom", "combined")) ) {
      warning("Did you forget 'type_vis = 'zoom'")
    }
    if ( is.null(sel_period) ) {
      if ( sel_period_zoom[1] < min(data[var_date], na.rm = TRUE) ) {
        stop("Specified first date not within (selected) data
             selected with 'sel_period_zoom = ...'")
      } else if ( sel_period_zoom[2] > max(data[var_date], na.rm = TRUE) ) {
        stop("Specified last date not within (selected) data
             selected with 'sel_period_zoom = ...'")
      } else if (sel_period_zoom[1] > sel_period_zoom[2] ) {
        stop("Last date before first date
             selected with 'sel_period_zoom = ...'!")
      }
    } else {
      if ( sel_period_zoom[1] < sel_period[1] ) {
        stop("Specified last date in 'sel_period_zoom = ...'not within period
             selected with 'sel_period = ...' ")
      } else if ( sel_period_zoom[2] > sel_period[2] ) {
        stop("Specified first date in 'sel_period_zoom = ...'not within period
             selected with 'sel_period = ...' ")
      } else if (sel_period_zoom[1] > sel_period_zoom[2] ) {
        stop("Last date before first date
             selected with 'sel_period_zoom = ...'!")
      }
    }
  }

  # From wide to long
  data_l <- tidyr::gather(data, vars_meas,
                          key = "Name", value = "Score")

  if ( type_vis %in% c("zoom", "combined") && !is.null(sel_period_zoom) ) {
    data_zoom <- dplyr::filter(data_l,
                              data_l[var_date] >= sel_period_zoom[1] &
                              data_l[var_date] <= sel_period_zoom[2])
    return(list(data_l = data_l,
                data_zoom = data_zoom))
  } else {
    return(list(data_l = data_l))
  }

}
