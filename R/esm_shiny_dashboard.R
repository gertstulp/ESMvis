library("gridExtra")
library("ggplot2")
library("scales")
library("stringr")
library("lubridate")
library("shiny")
library("plotly")
library("readr")
library("haven")
library("readxl")
library("RColorBrewer")
library("shinycssloaders")
library("Hmisc")
library("shinydashboard")

esm_shiny <- function( input_df = NA, df_label = NA ) {

  ui <- dashboardPage(

    dashboardHeader(title = "ESMvis"),

    dashboardSidebar(

      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        #menuItem("Widgets", tabName = "widgets", icon = icon("th")),
        selectInput("timevar", "Select time variable:",
                    choices = c(" ", names(input_df))),
        selectInput("variables", "Select variable(s):",
                    choices = c(" ", names(input_df)), multiple = TRUE),
        selectInput(inputId = "Type",
                    label = "Type of visualization:",
                    choices = c("timeseries", "timeseries + zoom", "network",
                                "barchart"),
                    selected = "timeseries"),
        selectInput(inputId = "Time",
                    label = "Time unit:",
                    choices = c("Per measurement", "Per day", "Per week",
                                "Per day and week", "Select measurements",
                                "All measurements"),
                    selected = "All measurements"),
        uiOutput("dates")
      )
    ), # Close dashboardsidebar

    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                  box(withSpinner(plotOutput("out_timeseries"), type=4),
                      width = 9, height = 700),

                  box(
                    title = "Controls",
                    conditionalPanel(
                      condition = "input.Type=='timeseries'",
                      checkboxGroupInput("geoms", "What to visualise:",
                                         c("Smoothening" = "smooth",
                                           "Points" = "point",
                                           "Lines" = "line"),
                                         selected = c("smooth")),
                      conditionalPanel(
                        condition = "'smooth' %in% input.geoms",
                        sliderInput("kernel", "Degree smoothening:", min = 0.01,
                                    max = 1, value = 0.50),
                        checkboxInput("se_band", "Show error band", FALSE)
                      ),
                      sliderInput("axis_limits", "Limits y-axis:", 0, 100, c(0, 100))
                    ),
                    checkboxInput("change_clrs", "Change colours", FALSE),
                    uiOutput("colours"),
                    width = 3
                  )
                )
        ),

        # Second tab content
        tabItem(tabName = "widgets",
                h2("Widgets tab content")
        )
      )
    )

    ) # Close dashboardPage

  server <- function(input, output, session) {

    #####################################
    ### GET DATES FROM PROCESSED DATA ###
    #####################################

    output$colours <- renderUI({
      if(is.null(input$variables) | input$change_clrs == FALSE)
        return()

      textInput("group_clrs", "Provide colours",
                value = paste(
                  hue_pal()(length(input$variables)), collapse = ","))
    })

    output$dates <- renderUI({
      # If missing input, return to avoid error later in function
      if(is.null(input$variables) | input$timevar == " ")
        return()

      temp <- data_processing(input_df,
                              var_date = input$timevar,
                              vars_meas = input$variables)$data_l

      if(input$Time == "Per measurement") {
        selectInput("meas", "Select measurement", choices = temp$date_esmvis)
      } else if(input$Time == "Per day") {
        selectInput("day", "Select day", choices = temp$dayno_esmvis)
      } else if(input$Time == "Per week") {
        selectInput("week", "Select week", choices = temp$weekno_esmvis)
      }
      # Fix a select period here.
      # else if(input$Time == "Select measurements") {
      #   selectInput("sel_date", "Select date", choices = temp$date_esmvis),
      #   sliderInput("sel_meas", "Select # measurements",
      #                min = 1, max = 100, value = 25)
      # }
    })

    #####################################
    ###### READ IN / GET DATA ###########
    #####################################

    colours_man <- reactive({
      if(is.null(input$variables) | input$change_clrs == FALSE)
        return()

      as.vector(unlist(strsplit(input$group_clrs, "[,]")))
    })

    period_selected <- reactive({
      if( !is.null(input$meas) ) {
        date_minmax <- input$meas
        class_date <- class(input_df[[input$timevar]])

        if(input$Time == "Per measurement" ) {
          if ( class_date %in% c("POSIXlt", "POSIXct", "is.POSIXt")) {
            date_minmax <- ymd_hms(date_minmax)
          } else if( class_date == "Date" ) {
            date_minmax <- as_date(date_minmax)
          } else {
            date_minmax <- as.numeric(date_minmax)
          }
          c(date_minmax, date_minmax)
        } else if(input$Time == "Per day") {

          if ( class_date %in% c("POSIXlt", "POSIXct", "is.POSIXt")) {
            date_minmax <- ymd_hms(date_minmax)
          } else if( class_date == "Date" ) {
            date_minmax <- as_date(date_minmax)
          } else {
            date_minmax <- as.numeric(date_minmax)
          }
          c(date_minmax, date_minmax)

        } else if(input$Time == "Per week") {
          date_minmax <- input$meas
          class_date <- class(input_df[[input$timevar]])
          if ( class_date %in% c("POSIXlt", "POSIXct", "is.POSIXt")) {
            date_minmax <- ymd_hms(date_minmax)
          } else if( class_date == "Date" ) {
            date_minmax <- as_date(date_minmax)
          } else {
            date_minmax <- as.numeric(date_minmax)
          }
          c(date_minmax, date_minmax)
        } else {
          NULL
        }
      }

    })

    graph <- reactive({

      if(!is.null(input$variables) & input$timevar != " " ) {
        esm_vis(input_df,
                var_date = input$timevar,
                vars_meas = input$variables,
                type_vis = input$Type,
                vars_groups = (if(input$change_clrs) colours_man() else NA),
                #ID = list(var_ID = "id", ID = "10720"), # DELETE LATER
                sel_period = period_selected(),
                vis_options = list(point = ("point" %in% input$geoms),
                                  smooth = ("smooth" %in% input$geoms),
                                  line = ("line" %in% input$geoms),
                                  se_band = input$se_band,
                                  kernel = input$kernel,
                                  axis_limits = input$axis_limits)
                )
      }
      else {
        stop("Please select a time variable and a variable to visualise first")
      }
    })

    #####################################
    ####### CREATE GRAPHS    ###########
    #####################################

    timeseries <- reactive({
      output_list <- list(graphs_ts = graph())
    })


    #####################################
    ###### GRAPHICAL/TABLE OUTPUT #######
    #####################################

    output$out_timeseries <- renderPlot(
      {
        timeseries()$graphs_ts
      })

    #####################################
    ########## DOWNLOAD PLOT ###########
    #####################################

    # End R-session when browser closed
    session$onSessionEnded(stopApp)
  }
  shinyApp(ui, server)
}
# esm_shiny(esm_data1)
esm_shiny(data_massi1)
