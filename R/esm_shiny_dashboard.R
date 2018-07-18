library("gridExtra")
library("ggplot2")
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
        # conditionalPanel(
        #   condition = "input.Time=='Per measurement'",
        #   selectInput("meas", "Select measurement", choices = "")
        # ),
        # conditionalPanel(
        #   condition = "input.Time=='Per day'",
        #   selectInput("day", "Select day", choices = "")
        # ),
        # conditionalPanel(
        #   condition = "input.Time == 'Per week' ||
        #   input.Type == 'Per day and week'",
        #   selectInput("week", "Select week", choices = "")
        # ),
        # conditionalPanel(
        #   condition = "input.Time == 'Select measurements'",
        #   selectInput("sel_date", "Select date", choices = ""),
        #   sliderInput("sel_meas", "Select # measurements",
        #               min = 1, max = 100, value = 25)
        # )
      )

    ), # Close dashboardsidebar

    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                  box(withSpinner(plotOutput("out_timeseries"), type=4),
                      width = 9),

                  box(
                    title = "Controls",
                    sliderInput("slider", "Number of observations:", 1, 100, 50),
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

    period_selected <- reactive({

      if(input$Time == "Per measurement" & !is.null(input$meas)) {
        date_min <- input$meas
        class(date_min) <- class(input_df[[input$timevar]])
        c(date_min, date_min)
        # if (inherits(input_df[[input$timevar]],
        #              c("Date", "POSIXlt", "POSIXct", "is.POSIXt"))) {
        #   c(as_datetime(input$meas), as_datetime(input$meas))
        # } else if( is.numeric(input_df[[input$timevar]]) ) {
        #   c(input$meas, input$meas)
        # }
      } else if(input$Time == "Per day") {
        NULL
      } else if(input$Time == "Per week") {
        NULL
      } else {
        NULL
      }

    })

    graph <- reactive({

      if(!is.null(input$variables) & input$timevar != " " ) {
        esm_vis(input_df,
                var_date = input$timevar,
                vars_meas = input$variables,
                type_vis = input$Type,
                #ID = list(var_ID = "id", ID = "10720"), # DELETE LATER
                sel_period = period_selected()
                )

      }
      else {
        stop("Please select a time variable and a variable to visualise first")
      }
    })

    # df_shiny <- reactive({
      # size width/heigth in pixels
      # max_width = 1150
      # size_axs = max_width/7.2
      #
      # if(input$Type=="Per measurement") {
      #   df_nodes <- input_df$comb_df[[1]] %>%
      #     filter(date_ymd_hms==ymd_hms(input$meas))
      #   df_labels <- input_df$comb_df_lbl[[1]] %>%
      #     filter(date_ymd_hms==ymd_hms(input$meas))
      #   df_comments <- input_df$comb_df_comm[[1]] %>%
      #     filter(date_ymd_hms==ymd_hms(input$meas))
      #
      #   #no_h_facets = 1
      #   #no_v_facets = 1
      #   height_network=2*size_axs
      #   width_network=2*size_axs
      #
      # } else if(input$Type=="Per day") {
      #   df_nodes <- input_df$comb_df[[1]] %>%
      #     filter(date_ymd==ymd(input$day))
      #   df_labels <- input_df$comb_df_lbl[[1]] %>%
      #     filter(date_ymd==ymd(input$day))
      #   df_comments <- input_df$comb_df_comm[[1]] %>%
      #     filter(date_ymd==ymd(input$day))
      #
      #   #no_h_facets = nrow(df_comments)
      #   #no_v_facets = 1
      #
      #   height_network=2*size_axs
      #   width_network=3*2*size_axs
      # } else if(input$Type=="Per week" | input$Type=="Per day and week") {
      #   df_nodes <- input_df$comb_df[[1]] %>%
      #     filter(week_rank == input$week)
      #   df_labels <- input_df$comb_df_lbl[[1]] %>%
      #     filter(week_rank == input$week)
      #   df_comments <- input_df$comb_df_comm[[1]] %>%
      #     filter(week_rank == input$week)
      #
      #   #no_h_facets = 7
      #   #no_v_facets = ifelse(input$Type=="Per week", ceiling(nrow(df_comments)/7), 3)
      #   height_network=ifelse(input$Type=="Per week",
      #                         (ceiling(nrow(df_comments)/7)*size_axs),
      #                         3*size_axs)
      #   width_network=7*size_axs
      # } else if(input$Type=="Select measurements") {
      #   index_startdate <- which(get_choices()$date_ymd_hms==ymd_hms(input$sel_date))
      #   end_date <- get_choices()$date_ymd_hms[index_startdate + as.numeric(input$sel_meas)-1]
      #
      #   df_nodes <- input_df$comb_df[[1]] %>%
      #     filter(date_ymd_hms>=ymd_hms(input$sel_date) &
      #            date_ymd_hms<=ymd_hms(end_date))
      #   df_labels <- input_df$comb_df_lbl[[1]] %>%
      #     filter(date_ymd_hms>=ymd_hms(input$sel_date) &
      #              date_ymd_hms<=ymd_hms(end_date))
      #   df_comments <- input_df$comb_df_comm[[1]] %>%
      #     filter(date_ymd_hms>=ymd_hms(input$sel_date) &
      #              date_ymd_hms<=ymd_hms(end_date))
      #
      #   #no_h_facets = 7
      #   #no_v_facets = nrow(df_comments)/7
      #   height_network=(nrow(df_comments)/7)*size_axs
      #   width_network=7*size_axs
      # } else if(input$Type=="All measurements") {
      #   df_nodes <- input_df$comb_df[[1]]
      #   df_labels <- input_df$comb_df_lbl[[1]]
      #   df_comments <- input_df$comb_df_comm[[1]]
      #
      #   # calculate appropriate dimensions for many measurements
      #   size_small <- max_width/ceiling(sqrt(nrow(df_comments)))
      #
      #   height_network=1400
      #   width_network=1400
      #   #height_network=ceiling(sqrt(nrow(df_comments)))*size_small
      #   #width_network=ceiling(sqrt(nrow(df_comments)))*size_small
      # }
      #
      # first_date <- min(df_comments$date_ymd_hms)
      # if (min(df_comments$date_ymd_hms)==max(df_comments$date_ymd_hms)) {
      #   last_date <- min(df_comments$date_ymd_hms) + minutes(300)
      # } else {
      #   last_date <- max(df_comments$date_ymd_hms)
      # }
      #
      #
      # if(input$ts_style=="All combined" |
      #    input$ts_style=="Neg+Pos+Other") {
      #   #no_v_facets_ts = 3
      #   height_ts = 2*size_axs
      # } else if(input$ts_style=="Neg+Pos") {
      #   #no_v_facets_ts = 2
      #   height_ts = size_axs
      # } else if(input$ts_style=="Neg+Pos+Other+Events") {
      #   #no_v_facets_ts = 4
      #   height_ts = 2*size_axs
      # } else {
      #   #no_v_facets_ts = 1
      #   height_ts = size_axs
      # }
      #
      # df_list <- list(df_nodes = df_nodes,
      #                 #df_labels = df_labels,
      #                 #df_comments = df_comments,
      #                 timeline = filter(input_df$comb_df[[1]], x==xend&y==yend),
      #                 timeline_events = input_df$comb_df_lbl[[1]],
      #                 first_date = first_date,
      #                 last_date = last_date,
      #                 #max_width = max_width,
      #                 #size_axs = size_axs,
      #                 #height_network = height_network,
      #                 #width_network = width_network,
      #                 #height_ts = height_ts
      #                 )
      #
      # return(df_list)
    # })

    #####################################
    ####### CREATE GRAPHS    ###########
    #####################################

    # network <- reactive({
    #
    #   df_nodes <- df_shiny()$df_nodes
    #   df_labels <- df_shiny()$df_labels
    #   df_comments <- df_shiny()$df_comments
    #
    #   a <- ggplot(df_nodes, aes(x = x, y = y, xend = xend, yend = yend)) +
    #     scale_x_continuous(limits=c(-0.1, 1.1)) +
    #     scale_y_continuous(limits=c(-0.1, 1.1)) +
    #     coord_fixed() +
    #     theme_blank() +
    #     theme(strip.background = element_rect(fill=NA))
    #
    #   if(input$edges) a <- a + geom_edges()
    #
    #   if(input$nodes) {
    #     a <- a + geom_point(size=22, colour="lightgrey") +
    #       geom_point(aes(colour=colour, size=value), show.legend = FALSE) +
    #       geom_nodetext(aes(label = vertex.names),
    #                     fontface = "bold", colour="white") +
    #     scale_colour_manual(values=c("white" = "white", "red" = "#CC79A7", "green" = "#56B4E9", "grey" = "lightgray")) +
    #     scale_radius(range = c(1,22))
    #   }
    #
    #   if(input$labels) {
    #     a <- a + geom_label(data=df_labels[dim(df_labels)[1]:1,],
    #                         aes(x = x, y = y, label=text,
    #                             fill=colour,
    #                             alpha=abs(value)),
    #                         colour="white",
    #                         size=3,
    #                         show.legend=FALSE, inherit.aes=FALSE) +
    #       scale_fill_manual(values=c("red" = "red", "black" = "black", "green" = "seagreen4")) +
    #       scale_alpha_continuous(range=c(0.4,1))
    #   }
    #
    #   if(input$comments) {
    #     a <- a + geom_label(data=df_comments,
    #                         aes(x = x, y = y, label=text_cut),
    #                         fill="white",
    #                         colour="black",
    #                         size=3,
    #                         show.legend=FALSE, inherit.aes=FALSE)
    #   }
    #
    #   if(input$Type=="Per measurement") {
    #     a <- a + ggtitle(paste("Date:", df_shiny()$first_date))
    #   } else if(input$Type=="Per day") {
    #     a <- a + facet_wrap(~ day_period, drop=FALSE) +
    #       ggtitle(paste("Date:", df_shiny()$first_date))
    #   } else if(input$Type=="Per week") {
    #     a <- a + facet_wrap(~ date_ymd_hms, ncol=7) +
    #       ggtitle(paste("Date:", df_shiny()$first_date, "--", df_shiny()$last_date))
    #   } else if(input$Type=="Per day and week") {
    #     a <- a + facet_grid(day_period ~ day_label, drop=FALSE) +
    #       ggtitle(paste("Date:", df_shiny()$first_date, "--", df_shiny()$last_date))
    #   } else if(input$Type=="Select measurements") {
    #     a <- a + facet_wrap(~ date_ymd_hms, ncol=7) +
    #       ggtitle(paste("Date:", df_shiny()$first_date, "--", df_shiny()$last_date))
    #   } else if(input$Type=="All measurements") {
    #     a <- a + facet_wrap(~ date_ymd_hms) +
    #       ggtitle(paste("Date:", df_shiny()$first_date, "--", df_shiny()$last_date))
    #   }
    #
    #   a
    #   })

    timeseries <- reactive({
      # timeline_df <- df_shiny()$timeline
      #
      # p0 <- ggplot(timeline_df,
      #              aes(x = date_ymd_hms, y=value, colour=vertex.names)) +
      #   theme_bw() +
      #   labs(x="Datum", y="Score", colour=NULL) +
      #   scale_x_datetime(date_breaks = "1 month",
      #                    #date_minor_breaks = "1 week",
      #                    date_labels = "%b") +
      #   theme(legend.key.height=unit(0.35, "cm"),
      #         strip.background = element_rect(fill=NA)) +
      #   annotate("rect", xmin=df_shiny()$first_date,
      #            xmax=df_shiny()$last_date,
      #            ymin=-Inf, ymax=Inf,
      #            alpha=0.3, fill="blue")
      #
      # if(input$datapoints) p0 <- p0 + geom_point()
      #
      # if(input$line) p0 <- p0 + geom_line()
      #
      # if(input$smoothening) p0 <- p0 + geom_smooth(se=FALSE,
      #                                              span = input$smooth,
      #                                              method = 'loess')
      #
      #
      #
      #
      # if(input$ts_style!="All combined" &
      #           input$ts_style!="Events" ) {
      #   timeline_df <- timeline_df %>% mutate(
      #     facets = case_when(
      #       vertex.names %in% c("ctrl","somb","intr","aln","afspr","vlg") ~ "Negatieve emoties",
      #       vertex.names %in% c("lks", "kan","hui","nut","geno") ~ "Positieve emoties",
      #       vertex.names %in% c("eet", "aanm", "verm") ~ "Overige emoties"
      #     )
      #   )
      #   timeline_df_split <- split(timeline_df,f = timeline_df$facets)
      #   p0 <- p0 + facet_grid(facets~.)
      #   p1 <- p0 %+%
      #     timeline_df_split$'Negatieve emoties' +
      #     scale_colour_brewer(palette="Set1")
      #
      #   p2 <- p0 %+%
      #     timeline_df_split$'Positieve emoties' +
      #     scale_colour_brewer(palette="Set2")
      #
      #   p3 <- p0 %+%
      #     timeline_df_split$'Overige emoties' +
      #     scale_colour_brewer(palette="Set3")
      # }
      #
      # if(input$ts_style=="Events" |
      #    input$ts_style=="Neg+Pos+Other+Events" ) {
      #   df_events <- df_shiny()$timeline_events %>%
      #     filter(
      #       vertex.names %in% c("plez", "onpl", "slp")) %>%
      #     mutate(facets = "Events")
      #
      #   p4 <- p0 %+%
      #     filter(df_events, value>0) +
      #     scale_colour_brewer(palette="Accent") +
      #     geom_rug(data=filter(df_events, value==0),
      #              aes(x = date_ymd_hms, y=value, colour=vertex.names))
      # }
      #
      # if(input$ts_style=="All combined") {
      #   output_grobs <- arrangeGrob(p0)
      #   output_graph <- p0
      # } else if(input$ts_style=="Neg+Pos+Other") {
      #   output_grobs <- arrangeGrob(p1,p2,p3, ncol=2)
      #   output_graph <- grid.arrange(p1,p2,p3, ncol=2)
      # } else if(input$ts_style=="Neg+Pos") {
      #   output_grobs <- arrangeGrob(p1,p2, ncol=2)
      #   output_graph <- grid.arrange(p1,p2, ncol=2)
      # } else if(input$ts_style=="Neg") {
      #   output_grobs <- arrangeGrob(p1)
      #   output_graph <- p1
      # } else if(input$ts_style=="Pos") {
      #   output_grobs <- arrangeGrob(p2)
      #   output_graph <- p2
      # } else if(input$ts_style=="Other") {
      #   output_grobs <- arrangeGrob(p3)
      #   output_graph <- p3
      # } else if(input$ts_style=="Events") {
      #   output_grobs <- arrangeGrob(p4)
      #   output_graph <- p4
      # } else if(input$ts_style=="Neg+Pos+Other+Events") {
      #   output_grobs <- arrangeGrob(p1,p2,p3,p4, ncol=2)
      #   output_graph <- grid.arrange(p1,p2,p3,p4, ncol=2)
      # }
      #
      # output_list <- list(grobs_ts = output_grobs,
      #                     graphs_ts = output_graph)


      # output_graph <- ggplot(ToothGrowth, aes(x = supp, y = len)) +
      #   geom_boxplot() +
      #   labs(x = paste(is.vector(input$variables)), y = paste(length(input$variables)),
      #        title = paste(input$variables, collapse = ", "))
      #
      # output_graph <- ggplot(input_df, aes(x = "", y = pleasure)) +
      #   geom_boxplot()
      #
      # output_graph <- ggplot(data()$data_l,
      #                        aes(x = Name, y = Score)) +
      #   geom_boxplot()

      #warning("bla outputlist")
      output_list <- list(graphs_ts = graph())
    })


    #####################################
    ###### GRAPHICAL/TABLE OUTPUT #######
    #####################################

    # max_width <- reactive ({
    #   df_shiny()$max_width
    # })
    #
    # size_axs <- reactive ({
    #   df_shiny()$size_axs
    # })
    #
    # width <- reactive ({
    #   df_shiny()$width_network
    # })
    #
    # height <- reactive ({
    #   df_shiny()$height_network
    # })
    #
    # height_ts <- reactive ({
    #   df_shiny()$height_ts
    # })
    #
    # height_comb <- reactive ({
    #   df_shiny()$height_ts + df_shiny()$height_network
    # })
    #
    # output$out_network <- renderPlot(width = width,
    #                                 height = height,
    #                                 {
    #                                   network()
    #                                 })
    #
    # output$out_timeseries <- renderPlot(width = max_width,
    #                                 height = height_ts,
    #                                 {
    #                                   timeseries()$graphs_ts
    #                                 })


    output$out_timeseries <- renderPlot(
      {
        timeseries()$graphs_ts
      })

    # output$out_plotly <- renderPlotly({
    #   ggplotly(timeseries()$graphs_ts)
    # })

    # output$out_combined <- renderPlot(width=width,
    #                                   height=height_comb, {
    #   g1 <- timeseries()$grobs_ts
    #   g2 <- arrangeGrob(network())
    #   grid.arrange(grobs = list(g1, g2), nrow=2, heights=c(height_ts(),height()))
    # })

    #####################################
    ########## DOWNLOAD PLOT ###########
    #####################################

    # output$download_plot_PDF_network <- downloadHandler(
    #   filename <- function() {
    #     paste("Figure_esmviz_", Sys.time(), ".pdf", sep = "")
    #   },
    #   content <- function(file) {
    #     ggsave(file, network(), width = (width()/size_axs()*2.75),
    #            height = (height()/size_axs()*2.75), units = "in")
    #   },
    #   contentType = "application/pdf" # MIME type of the image
    # )
    #
    # output$download_plot_PDF_ts <- downloadHandler(
    #   filename <- function() {
    #     paste("Figure_esmviz_", Sys.time(), ".pdf", sep = "")
    #   },
    #   content <- function(file) {
    #     grid.arrange(grobs = list(g1, g2), nrow=2, heights=c(height_ts(),height()))
    #
    #     ggsave(file, timeseries()$grobs_ts, width = (width()/size_axs()*2.75),
    #            height = (height()/size_axs()*2.75), units = "in")
    #   },
    #   contentType = "application/pdf" # MIME type of the image
    # )
    #
    # output$download_plot_PDF_comb <- downloadHandler(
    #   filename <- function() {
    #     paste("Figure_esmviz_", Sys.time(), ".pdf", sep = "")
    #   },
    #   content <- function(file) {
    #     g1 <- timeseries()$grobs_ts
    #     g2 <- arrangeGrob(network())
    #
    #     out <- arrangeGrob(grobs = list(g1, g2), nrow=2, heights=c(height_ts(),height()))
    #
    #     ggsave(file, out, width = (width()/size_axs()*2.75),
    #            height = (height_comb()/size_axs()*2.75), units = "in")
    #   },
    #   contentType = "application/pdf" # MIME type of the image
    # )

    # End R-session when browser closed
    session$onSessionEnded(stopApp)
  }
  shinyApp(ui, server)
}
# esm_shiny(esm_data1)
esm_shiny(data_massi1)
