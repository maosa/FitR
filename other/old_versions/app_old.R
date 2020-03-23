library(data.table)
# library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(DataCombine)
library(miscTools)
library(grDevices)
library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(DT)
library(rhandsontable)
library(shinyjs)
library(shinycssloaders)
library(timevis)

setwd("/Users/andreasmaos/Desktop/FitR")

path <- "/Users/andreasmaos/Desktop/FitR/data.csv"

run <- "/Users/andreasmaos/Desktop/FitR/run.csv"

plan <- "/Users/andreasmaos/Desktop/FitR/plan.csv"

calendar <- "/Users/andreasmaos/Desktop/FitR/calendar/calendar.csv"

workouts <- c("Chest", "Back", "Legs", "Compound", "Running", "Rest", "Other")

jscode <- "shinyjs.closeWindow = function() { window.close(); }"


######################################################################################
# ----------------------------------------UI---------------------------------------- #
######################################################################################

ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage(title = tags$h4(tags$strong("FitR")),
                           
                           ########################################################
                           ######################### HOME #########################
                           ########################################################
                           
                           tabPanel(
                             # "Home",
                             icon = icon("calendar", lib = "font-awesome", class = "fa-2x"),
                             
                             tags$br(),
                             
                             ############### TIMELINE ###############
                             
                             fluidRow(
                               column(3,
                                      tags$h4(tags$strong("PLAN BUILDER")))
                             ), # close fluidRow
                             
                             fluidRow(
                               column(12,
                                      timevisOutput("timeline") %>%
                                        withSpinner(type = 7, color = "#000000"))
                             ), # close fluidRow
                             
                             fluidRow(
                               column(2,
                                      selectInput(inputId = "plan_workout",
                                                  label = NULL,
                                                  choices = workouts)),
                               
                               conditionalPanel(
                                 condition = "input.plan_workout == \"Other\"",
                                 column(2, 
                                        textInput(inputId = "plan_other",
                                                  label = NULL,
                                                  value = "Specify"))
                               ),
                               
                               column(1,
                                      actionButton(inputId = "plan_add",
                                                   label = NULL,
                                                   icon = icon("plus"))),
                               
                               column(1,
                                      actionButton(inputId = "plan_remove",
                                                   label = NULL,
                                                   icon = icon("minus"))),
                               
                               column(1,
                                      actionButton(inputId = "center",
                                                   label = NULL,
                                                   icon = icon("arrows-h"))),
                               
                               column(1,
                                      actionButton(inputId = "calendar_sync",
                                                   label = NULL,
                                                   icon = icon("retweet"))),
                               
                               column(1,
                                      actionButton(inputId = "backup",
                                                   label = NULL,
                                                   icon = icon("download"))),
                               
                               column(1,
                                      useShinyjs(),
                                      extendShinyjs(text = jscode, functions = c("closeWindow")),
                                      actionButton(inputId = "close",
                                                   label = NULL,
                                                   icon = icon("power-off")))
                             ), # close fluidRow
                             
                             tags$hr(),
                             
                             ############### WORKOUT LOG ###############
                             
                             fluidRow(
                               column(3,
                                      tags$h4(tags$strong("FITNESS LOG")))
                             ), # close fluidRow
                             
                             ##### SIDE BAR #####
                             
                             sidebarLayout(
                               sidebarPanel(width = 4,
                                            dateInput(inputId = "date",
                                                      label = "Date",
                                                      format = "DD-dd-mm-yyyy",
                                                      weekstart = 1),
                                            
                                            selectInput(inputId = "workout",
                                                        label = "Workouts",
                                                        choices = workouts),
                                            
                                            conditionalPanel(
                                              condition = "input.workout == \"Other\"",
                                              textInput(inputId = "other",
                                                               label = NULL,
                                                               value = "Specify other workout")
                                            ),
                                            
                                            conditionalPanel(
                                              condition = "input.workout == \"Running\"",
                                              numericInput(inputId = "distance",
                                                           label = "Running distance (km)",
                                                           value = NULL,
                                                           min = 0)
                                            ),
                                            
                                            numericInput(inputId = "duration",
                                                         label = "Duration (mins)",
                                                         value = 35,
                                                         min = 0),
                                            
                                            fluidRow(
                                              column(5,
                                                     actionButton(inputId = "remove",
                                                                  label = NULL,
                                                                  icon = icon("minus"),
                                                                  width = "100%")),
                                              
                                              column(5,
                                                     offset = 2,
                                                     actionButton(inputId = "add",
                                                                  label = NULL,
                                                                  icon = icon("plus"),
                                                                  width = "100%"))
                                            ) # close fluidRow
                               ),
                               
                               ##### MAIN PANEL #####
                               
                               mainPanel(width = 8,
                                         DTOutput("log") %>%
                                           withSpinner(type = 7, color = "#000000")
                               ) # close mainPanel
                             ) # close sidebarLayout
                           ), # close tabPanel
                           
                           ###########################################################
                           ######################### RUNNING #########################
                           ###########################################################
                           
                           tabPanel(
                             # "Running",
                             # icon = icon("walking", lib = "font-awesome", class = "fa-2x"),
                             icon = icon("heartbeat", lib = "font-awesome", class = "fa-2x"),
                             
                             tags$br(),
                             
                             fluidRow(
                               column(12,
                                      DTOutput("run") %>%
                                        withSpinner(type = 7, color = "#000000"))
                             ), # close fluidRow
                             
                             tags$hr(),
                             
                             fluidRow(
                               column(12,
                                      plotOutput("runstats", height = "500px") %>%
                                        withSpinner(type = 7, color = "#000000"))
                             )
                           ), # close tab
                           
                           ##############################################################
                           ######################### STATISTICS #########################
                           ##############################################################
                           
                           tabPanel(
                             # "Statistics",
                             icon = icon("bar-chart-o", lib = "font-awesome", class = "fa-2x"),
                             
                             tags$br(),
                             
                             fluidRow(
                               column(4,
                                      dateRangeInput(inputId = "period",
                                                     label = "Select period",
                                                     start = as.Date(head(fread(path)$Date, 1)),
                                                     end = as.Date(tail(fread(path)$Date, 1)),
                                                     format = "yyyy-mm-dd",
                                                     startview = "year",
                                                     weekstart = 1,
                                                     separator = "to")),
                               
                               column(1,
                                      tags$br(),
                                      actionButton(inputId = "reset_period",
                                                   label = NULL,
                                                   icon = icon("play")))
                             ),
                             
                             fluidRow(
                               column(12,
                                      DTOutput("stats") %>%
                                        withSpinner(type = 7, color = "#000000"))
                             ), # close fluidRow
                             
                             tags$hr(),
                             
                             plotOutput(outputId = "duration_graph", height = "500px") %>%
                               withSpinner(type = 7, color = "#000000"),
                             
                             tags$hr(),
                             
                             fluidRow(
                               column(6,
                                      plotlyOutput(outputId = "piechartnum") %>%
                                        withSpinner(type = 7, color = "#000000")),
                               
                               column(6,
                                      plotlyOutput(outputId = "piecharttime") %>%
                                        withSpinner(type = 7, color = "#000000"))
                             ) # close fluidRow
                           ) # close tabPanel
                ) # close navbarPage
) # close fluidPage



######################################################################################
# --------------------------------------SERVER-------------------------------------- #
######################################################################################

server <- function(input, output) {
  
  observeEvent(input$close, {     # <<<<<----- Exit app
    js$closeWindow()
    stopApp()
  })
  
  
  observeEvent(input$backup, {
    withProgress(message = "Backing up data...", {
      system("cp /Users/andreasmaos/Desktop/FitR/data.csv /Users/andreasmaos/Desktop/FitR/backups/data_backup.csv")
      system("cp /Users/andreasmaos/Desktop/FitR/run.csv /Users/andreasmaos/Desktop/FitR/backups/run_backup.csv")
      system("cp /Users/andreasmaos/Desktop/FitR/plan.csv /Users/andreasmaos/Desktop/FitR/backups/plan_backup.csv")
      system("cp /Users/andreasmaos/Desktop/FitR/calendar/calendar.csv /Users/andreasmaos/Desktop/FitR/backups/calendar_backup.csv")
    })
  })
  
  
  ############################# TIMELINE #############################
  
  observeEvent(input$plan_add, {
    if (input$plan_workout == "Other") {
      write.table(data.frame(
        id = ((nrow(fread(plan))) + 1),
        content = input$plan_other,
        start = as.character((as.Date(tail(fread(plan)$start, 1)) + 1))
        # style = c("color: white; background-color: black; border-color: grey")
      ),
      file = plan,
      append = TRUE,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE)
    } # close if statement
    
    else {
      write.table(data.frame(
        id = ((nrow(fread(plan))) + 1),
        content = input$plan_workout,
        start = as.character((as.Date(tail(fread(plan)$start, 1)) + 1))
        # style = c("color: white; background-color: black; border-color: grey")
      ),
      file = plan,
      append = TRUE,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE)
    } # close else statement
  })
  
  
  observeEvent(input$plan_remove, {
    write.table(fread(plan)[-nrow(fread(plan)), ],
                file = plan,
                sep = ",",
                row.names = FALSE,
                col.names = TRUE,
                quote = FALSE)
  })
  
  
  timeline_data <- eventReactive(c(input$plan_add, input$plan_remove), {
    read.csv(plan)
  })
  
  
  output$timeline <- renderTimevis({
    timevis(
      data = timeline_data(),
      fit = FALSE,
      showZoom = FALSE,
      zoomFactor = 0.25) %>%
      setWindow(Sys.Date() - 1, Sys.Date() + 6)
  })
  
  
  observeEvent(input$center, {
    centerTime("timeline", Sys.Date() + 3)
  })
  
  
  observeEvent(input$calendar_sync, {       ############### <<<<<----- create .ics file
    write.table(data.frame(
      starttime = gsub("-", "", as.character(fread(plan)$start)),
      summary = fread(plan)$content),
      file = calendar,
      sep = "\t",
      row.names = FALSE,
      quote = FALSE)
    
    df <- read.csv("/Users/andreasmaos/Desktop/FitR/calendar/calendar.csv",
                   sep = "\t",
                   stringsAsFactors = FALSE)
    
    ics_header <- readLines("/Users/andreasmaos/Desktop/FitR/calendar/template_header.ics", warn = FALSE)
    ics_body <- readLines("/Users/andreasmaos/Desktop/FitR/calendar/template_body.ics", warn = FALSE)
    ics_footer <- readLines("/Users/andreasmaos/Desktop/FitR/calendar/template_footer.ics", warn = FALSE)
    ics_events <- ""
    
    for(i in 1:nrow(df)) {
      ics_body <- str_replace(ics_body, "DTSTART:.*", paste0("DTSTART:", df$starttime[i]))
      # ics_body <- str_replace(ics_body, "DTEND:.*", paste0("DTEND:", df$endtime[i]))
      # create unique identifier
      ics_body <- str_replace(ics_body, "UID:.*", paste0("UID:", paste0(df$starttime[i], df$endtime[i])))
      ics_body <- str_replace(ics_body, "SUMMARY:.*", paste0("SUMMARY:", df$summary[i]))
      ics_events <- append(ics_events, ics_body)
    }
    
    # combine template parts to one vector
    ics_events <- append(ics_header, ics_events)
    ics_events <- append(ics_events, ics_footer)
    
    write(ics_events, file = "/Users/andreasmaos/Desktop/FitR/calendar/Workouts.ics")
    
    system("open /Users/andreasmaos/Desktop/FitR/calendar/Workouts.ics")
  })
  
  
  
  
  ############################# WORKOUT LOG #############################
  
  observeEvent(input$add, {
    if (input$workout == "Other") {
      write.table(data.frame(
        No = ((nrow(fread(path))) + 1),
        Date = as.character(input$date),
        Workout = input$other,
        Duration = as.numeric(input$duration)),     # <----- Specify other workout
        file = path,
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE)
    }
    
    else if (input$workout == "Rest") {
      write.table(data.frame(
        No = ((nrow(fread(path))) + 1),
        Date = as.character(input$date),     # <----- Rest
        Workout = "Rest",
        Duration = 0),
        file = path,
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE)
    }
    
    else if (input$workout == "Running") {
      write.table(data.frame(
        No = ((nrow(fread(path))) + 1),
        Date = as.character(input$date),
        Workout = (input$workout),
        Duration = as.numeric(input$duration)),     # <----- Write in Schedule tab
        file = path,
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE)
      
      write.table(data.frame(
        No = ((nrow(fread(run))) + 1),
        Date = as.character(input$date),
        Distance = as.numeric(input$distance),
        Duration = as.numeric(input$duration),
        Pace = as.numeric(
          round((floor(input$duration/input$distance)) + ((((input$duration/input$distance) - (floor(input$duration/input$distance)))*60)/100), 2)
        ) # close as.numeric
      ),     # <----- Write in Running tab
      file = run,
      append = TRUE,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE)
    }
    
    else {
      write.table(data.frame(
        No = ((nrow(fread(path))) + 1),
        Date = as.character(input$date),
        Workout = input$workout,
        Duration = input$duration),
        file = path,
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE)
    }
  }) # close observeEvent
  
  
  observeEvent(input$remove, {
    if (fread(path)[nrow(fread(path)), 3] == "Running") {
      write.table(fread(path)[-nrow(fread(path)), ],
                  file = path,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE)
      
      write.table(fread(run)[-nrow(fread(run)), ],
                  file = run,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE)
    }
    
    else {
      write.table(fread(path)[-nrow(fread(path)), ],
                  file = path,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE)
    }
  }) # close observeEvent
  
  
  table <- eventReactive(c(input$add, input$remove), {
    fread(path)
  })
  
  
  table_run <- eventReactive(c(input$add, input$remove), {
    fread(run)
  })
  
  
  output$log <- DT::renderDT({
    datatable(table(), rownames = FALSE,
              colnames = c("#", "Date", "Workout", "Duration (mins)"),
              options = list(
                dom = "tp",
                order = list(0, "desc"),
                pageLength = 7,
                columnDefs = list(list(className = "dt-center",
                                       targets = c(0, 1, 2, 3))))) %>%
      formatDate("Date", "toDateString") %>%
      formatStyle("No", fontWeight = "bold") %>%
      formatStyle(columns = "No",
                  valueColumns = "Workout",
                  backgroundColor = styleEqual(
                    c(unique(grep("Chest",fread(path)$Workout, value = TRUE)),
                      unique(grep("Back",fread(path)$Workout, value = TRUE)),
                      unique(grep("Pull",fread(path)$Workout, value = TRUE)),
                      unique(grep("Legs",fread(path)$Workout, value = TRUE)),
                      unique(grep("Texas",fread(path)$Workout, value = TRUE)),
                      "Compound",
                      "Running",
                      "Rest",
                      unique(grep("Cardio",fread(path)$Workout, value = TRUE)),
                      unique(grep("Multisport",fread(path)$Workout, value = TRUE)),
                      unique(grep("Shoulders",fread(path)$Workout, value = TRUE)),
                      "Other"),
                    c(replicate(19, "indianred"),
                      replicate(16, "navy"),
                      replicate(5, "navy"),
                      replicate(3, "gold"),
                      replicate(3, "lavender"),
                      "lavender",
                      "mediumturquoise",
                      "black",
                      replicate(7, "mediumturquoise"),
                      replicate(4, "mistyrose"),
                      "mistyrose",
                      "mistyrose")
                    ))
  }) # close renderDT
  
  
  ################################ RUNNING ################################
  
  
  ###################### TABLE ######################
  output$run <- DT::renderDT({
    datatable(table_run(),
              rownames = FALSE,
              colnames = c("#", "Date", "Distance (km)", "Duration (mins)", "Pace (mins/km)"),
              options = list(
                dom = "tp",
                order = list(0, "desc"),
                pageLength = 5,
                columnDefs = list(list(className = "dt-center",
                                       targets = c(0, 1, 2, 3, 4))))) %>%
      formatDate("Date", "toDateString") %>%
      formatStyle("No", fontWeight = "bold")
  })
  
  ###################### GRAPH ######################
  running_stats <- eventReactive(c(input$add, input$remove), {
    run_graph <- data.frame(
      run_num <- fread(run)$No,
      dist <- fread(run)$Distance
    )
    
    
    ggplot(run_graph, aes(run_num, dist)) +
      geom_line(color = "mediumturquoise") + 
      # geom_point(size = 1, color = "turquoise") +
      labs(title = "Distance per run",
           x = "Run",
           y = "Distance (km)") +
      scale_x_continuous(breaks = seq(0, nrow(fread(run)), 5)) +
      scale_y_continuous(breaks = seq((round(((min(fread(run)$Distance)) - 1), 0)),
                                      max(fread(run)$Distance), 1)) +
      geom_hline(aes(yintercept = mean(fread(run)$Distance)),
                 linetype = "dashed",
                 color = "grey10") +
      geom_text(aes(0, (mean(fread(run)$Distance)),
                    label = round(mean(fread(run)$Distance), 2),
                    vjust = -1)) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14, face = "bold"),
            axis.line = element_line(color = "black", linetype = "solid"),
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.ticks = element_line(color = "black", size = 0.60),
            axis.ticks.length = unit(0.20, "cm"),
            panel.background = element_rect(fill = "snow2"),
            panel.grid.major = element_line(colour = "snow1",
                                            size = 0.60),
            panel.grid.minor = element_line(colour = "snow1",
                                            size = 0.30,
                                            linetype = "solid")
      )
    
  }) # close eventReactive
  
  output$runstats <- renderPlot({
    running_stats()
  })
  
  ################################ STATISTICS ################################
  
  observeEvent(input$reset_period, {
    reset("period")
  })
  
  statistics_table <- eventReactive(c(input$add, input$remove, input$period), {
    
    if (input$period[1] == input$period[2]) {
      showNotification("No stats for a single day. Please refer to the workout log for info", type = "error")
      data.frame(n = NA, u = NA, l = NA, l = NA)
    } # close if statement
    
    else if (input$period[2] > as.Date(tail(fread(path)$Date, 1))) {
      showNotification("Umm...are you trying to see into the future?", type = "error")
      data.frame(n = NA, u = NA, l = NA, l = NA)
    } # close else if statement
    
    else if (input$period[1] > input$period[2]) {
      showNotification("Did you get the dates mixed up?", type = "error")
      data.frame(n = NA, u = NA, l = NA, l = NA)
    } # close else if statement
    
    else if (Sys.Date() == input$period[2] & as.numeric(Sys.Date() - as.Date(fread(path)$Date[1]) + 1) == nrow(fread(path))) {
      data.frame(
        avg_mins_per_day <- round(sum(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]])/nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]), 1),
        avg_hours_per_week <- round((sum(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]])/60)/(nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])/7), 1),
        rest_days <- length(which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]] == 0)),
        rest_days_percent <- round((length(which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]] == 0)) * 100)/nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]), 1)
      )
    } # close else if statement
    
    else if (Sys.Date() == input$period[2] & as.numeric(Sys.Date() - as.Date(fread(path)$Date[1]) + 1) != nrow(fread(path))) {
      data.frame(
        avg_mins_per_day <- round(sum(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ][[4]])/nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ]), 1),
        avg_hours_per_week <- round((sum(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ][[4]])/60)/(nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])/7), 1),
        rest_days <- length(which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ][[4]] == 0)),
        rest_days_percent <- round((length(which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ][[4]] == 0)) * 100)/nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ]), 1)
      )
    } # close else if statement
    
    else {
      data.frame(
        avg_mins_per_day <- round(mean(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]]), 1),
        avg_hours_per_week <- round((sum(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]])/60)/(nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])/7), 1),
        rest_days <- length(which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]] == 0)),
        rest_days_percent <- round((length(which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]] == 0)) * 100)/nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]), 1)
      )
    } # close else statement
  }) # close eventReactive
  
  
  output$stats <- DT::renderDT({
    datatable(statistics_table(),
                rownames = FALSE,
                colnames = c("Avgerage time/day (mins)",
                             "Avgerage time/week (hours)",
                             "Rest days",
                             "Rest (%)"),
                options = list(
                  dom = "t",
                  bSort = FALSE,
                  columnDefs = list(list(className = "dt-center",
                                         targets = c(0, 1, 2, 3)))
                )
      )
  }) # close renderDT
  
  
  #################### DURATION GRAPH ###################
  
  duration <- eventReactive(c(input$add, input$remove, input$period), {
    
    if (Sys.Date() == input$period[2] & as.numeric(Sys.Date() - as.Date(fread(path)$Date[1]) + 1) == nrow(fread(path))) {
      duration_graph <- data.frame(
        workout_id <- fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration != 0), ]$No,
        duration <- fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration != 0), ]$Duration
      )
      
      ggplot(duration_graph, aes(workout_id, duration)) +
        geom_line(color = "black") + 
        labs(title = paste0("Duration per workout (period: ",
                            input$period[1],
                            " to ",
                            input$period[2],
                            ")"),
             x = "Workout",
             y = "Duration (mins)") +
        scale_x_continuous(breaks = waiver(),
                           limits = c(min(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$No),
                                      max(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$No))) +
        scale_y_continuous(breaks = waiver(),
                           limits = c(min(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration),
                                      max(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration))) +
        geom_hline(aes(yintercept = mean(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]])),
                   linetype = "dashed",
                   color = "navyblue") +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 14, face = "bold"),
              axis.line = element_line(color = "black", linetype = "solid"),
              axis.text = element_text(size = 12, face = "bold", colour = "black"),
              axis.ticks = element_line(color = "black", size = 0.60),
              axis.ticks.length = unit(0.20, "cm"),
              panel.background = element_rect(fill = "snow2"),
              panel.grid.major = element_line(colour = "snow1",
                                              size = 0.60),
              panel.grid.minor = element_line(colour = "snow1",
                                              size = 0.30,
                                              linetype = "solid")
        )
    } # close if statement
    
    else if (Sys.Date() == input$period[2] & as.numeric(Sys.Date() - as.Date(fread(path)$Date[1]) + 1) != nrow(fread(path))) {
      duration_graph <- data.frame(
        workout_id <- fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ][which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ]$Duration != 0), ]$No,
        duration <- fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ][which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ]$Duration != 0), ]$Duration
      )
      
      ggplot(duration_graph, aes(workout_id, duration)) +
        geom_line(color = "black") + 
        labs(title = paste0("Duration per workout (period: ",
                            input$period[1],
                            " to ",
                            input$period[2],
                            ")"),
             x = "Workout",
             y = "Duration (mins)") +
        scale_x_continuous(breaks = waiver(),
                           limits = c(min(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ]$No),
                                      max(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ]$No))) +
        scale_y_continuous(breaks = waiver(),
                           limits = c(min(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ]$Duration),
                                      max(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ]$Duration))) +
        geom_hline(aes(yintercept = mean(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ][[4]])),
                   linetype = "dashed",
                   color = "navyblue") +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 14, face = "bold"),
              axis.line = element_line(color = "black", linetype = "solid"),
              axis.text = element_text(size = 12, face = "bold", colour = "black"),
              axis.ticks = element_line(color = "black", size = 0.60),
              axis.ticks.length = unit(0.20, "cm"),
              panel.background = element_rect(fill = "snow2"),
              panel.grid.major = element_line(colour = "snow1",
                                              size = 0.60),
              panel.grid.minor = element_line(colour = "snow1",
                                              size = 0.30,
                                              linetype = "solid")
        )
    } # close else if statement
    
    else {
      duration_graph <- data.frame(
        workout_id <- fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration != 0), ]$No,
        duration <- fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration != 0), ]$Duration
      )
      
      ggplot(duration_graph, aes(workout_id, duration)) +
        geom_line(color = "black") + 
        labs(title = paste0("Duration per workout (period: ",
                            input$period[1],
                            " to ",
                            input$period[2],
                            ")"),
             x = "Workout",
             y = "Duration (mins)") +
        scale_x_continuous(breaks = waiver(),
                           limits = c(min(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$No),
                                      max(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$No))) +
        scale_y_continuous(breaks = waiver(),
                           limits = c(min(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration),
                                      max(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration))) +
        geom_hline(aes(yintercept = mean(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]])),
                   linetype = "dashed",
                   color = "navyblue") +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 14, face = "bold"),
              axis.line = element_line(color = "black", linetype = "solid"),
              axis.text = element_text(size = 12, face = "bold", colour = "black"),
              axis.ticks = element_line(color = "black", size = 0.60),
              axis.ticks.length = unit(0.20, "cm"),
              panel.background = element_rect(fill = "snow2"),
              panel.grid.major = element_line(colour = "snow1",
                                              size = 0.60),
              panel.grid.minor = element_line(colour = "snow1",
                                              size = 0.30,
                                              linetype = "solid")
        )
    } # close else statement
  }) # close eventReactive
  
  
  output$duration_graph <- renderPlot({
    duration()
  })
  
  
  #################### PIE CHART NUM ####################
  
  pie_chart_num <- eventReactive(c(input$add, input$remove, input$period), {
    
    if (Sys.Date() == input$period[2] & as.numeric(Sys.Date() - as.Date(fread(path)$Date[1]) + 1) == nrow(fread(path))) {
      chest <- fread(path)[c(grep("Chest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ]
      back <- unique(rbind(data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Back", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ]),
                           data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Pull", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])))
      legs <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Legs", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      compound <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Compound", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      rest <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Rest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      
      pie1 <- data.frame(
        Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
        num = c(nrow(chest), nrow(back), nrow(legs), nrow(compound),
                nrow(fread(run)),
                (nrow(fread(path)))-nrow(chest)-nrow(back)-nrow(legs)-nrow(compound)-nrow(fread(run))-nrow(rest))
      )
      
      plot_ly(pie1,
              labels = ~Workout,
              values = ~num,
              type = 'pie',
              textposition = "inside",
              textinfo = "percent",
              insidetextfont = list(color = "gray50"),
              hoverinfo = "text",
              text = ~paste0(Workout, ": ", num, " workouts"),
              marker = list(colors = c("indianred", "navy", "gold", "lavender",
                                       "mediumturquoise", "mistyrose"),
                            line = list(color = "#FFFFFF", width = 1)),
              showlegend = FALSE) %>%
        layout(title = "Percentage of workouts by type",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    } # close if statement
    
    else if (Sys.Date() == input$period[2] & as.numeric(Sys.Date() - as.Date(fread(path)$Date[1]) + 1) != nrow(fread(path))) {
      chest <- fread(path)[c(grep("Chest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ]
      back <- unique(rbind(data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Back", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ]),
                           data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Pull", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ])))
      legs <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Legs", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ])
      compound <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Compound", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ])
      rest <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Rest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ])
      
      pie1 <- data.frame(
        Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
        num = c(nrow(chest), nrow(back), nrow(legs), nrow(compound),
                nrow(fread(run)),
                (nrow(fread(path)))-nrow(chest)-nrow(back)-nrow(legs)-nrow(compound)-nrow(fread(run))-nrow(rest))
      )
      
      plot_ly(pie1,
              labels = ~Workout,
              values = ~num,
              type = 'pie',
              textposition = "inside",
              textinfo = "percent",
              insidetextfont = list(color = "gray50"),
              hoverinfo = "text",
              text = ~paste0(Workout, ": ", num, " workouts"),
              marker = list(colors = c("indianred", "navy", "gold", "lavender",
                                       "mediumturquoise", "mistyrose"),
                            line = list(color = "#FFFFFF", width = 1)),
              showlegend = FALSE) %>%
        layout(title = "Percentage of workouts by type",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    } # close else if statement
    
    else {
      chest <- fread(path)[c(grep("Chest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ]
      back <- unique(rbind(data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Back", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ]),
                           data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Pull", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])))
      legs <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Legs", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      compound <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Compound", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      rest <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Rest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      
      pie1 <- data.frame(
        Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
        num = c(nrow(chest), nrow(back), nrow(legs), nrow(compound),
                nrow(fread(run)),
                (nrow(fread(path)))-nrow(chest)-nrow(back)-nrow(legs)-nrow(compound)-nrow(fread(run))-nrow(rest))
      )
      
      plot_ly(pie1,
              labels = ~Workout,
              values = ~num,
              type = 'pie',
              textposition = "inside",
              textinfo = "percent",
              insidetextfont = list(color = "gray50"),
              hoverinfo = "text",
              text = ~paste0(Workout, ": ", num, " workouts"),
              marker = list(colors = c("indianred", "navy", "gold", "lavender",
                                       "mediumturquoise", "mistyrose"),
                            line = list(color = "#FFFFFF", width = 1)),
              showlegend = FALSE) %>%
        layout(title = "Percentage of workouts by type",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    } # close else statement
  }) # close eventReactive
  
  
  output$piechartnum <- renderPlotly({
    pie_chart_num()
  })
  
  #################### PIE CHART TIME ####################
  
  pie_chart_time <- eventReactive(c(input$add, input$remove, input$period), {
    
    if (Sys.Date() == input$period[2] & as.numeric(Sys.Date() - as.Date(fread(path)$Date[1]) + 1) == nrow(fread(path))) {
      chest <- fread(path)[c(grep("Chest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ]
      back <- unique(rbind(data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Back", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ]),
                           data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Pull", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])))
      legs <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Legs", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      compound <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Compound", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      rest <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Rest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      
      pie2 <- data.frame(
        Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
        time = c(sum(chest$Duration), sum(back$Duration), sum(legs$Duration),
                 sum(compound$Duration), sum(fread(run)$Duration), 
                 (sum(fread(path)$Duration)-sum(chest$Duration)-sum(back$Duration)-sum(legs$Duration)-sum(compound$Duration)-sum(fread(run)$Duration)))
      )
      
      plot_ly(pie2,
              labels = ~Workout,
              values = ~time,
              type = 'pie',
              textposition = "inside",
              textinfo = "percent",
              insidetextfont = list(color = "gray50"),
              hoverinfo = "text",
              text = ~paste0(Workout, ": ", round(time/60, 2), " hours"),
              marker = list(colors = c("indianred", "navy", "gold", "lavender",
                                       "mediumturquoise", "mistyrose"),
                            line = list(color = "#FFFFFF", width = 1)),
              showlegend = FALSE) %>%
        layout(title = "Percentage of workouts by duration",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    } # close if statement
    
    else if (Sys.Date() == input$period[2] & as.numeric(Sys.Date() - as.Date(fread(path)$Date[1]) + 1) != nrow(fread(path))) {
      chest <- fread(path)[c(grep("Chest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ]
      back <- unique(rbind(data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Back", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ]),
                           data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Pull", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ])))
      legs <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Legs", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ])
      compound <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Compound", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ])
      rest <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])[c(grep("Rest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(as.character(as.Date(input$period[2]) - 1), fread(path)$Date)), ])$Workout)), ])
      
      pie2 <- data.frame(
        Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
        time = c(sum(chest$Duration), sum(back$Duration), sum(legs$Duration),
                 sum(compound$Duration), sum(fread(run)$Duration), 
                 (sum(fread(path)$Duration)-sum(chest$Duration)-sum(back$Duration)-sum(legs$Duration)-sum(compound$Duration)-sum(fread(run)$Duration)))
      )
      
      plot_ly(pie2,
              labels = ~Workout,
              values = ~time,
              type = 'pie',
              textposition = "inside",
              textinfo = "percent",
              insidetextfont = list(color = "gray50"),
              hoverinfo = "text",
              text = ~paste0(Workout, ": ", round(time/60, 2), " hours"),
              marker = list(colors = c("indianred", "navy", "gold", "lavender",
                                       "mediumturquoise", "mistyrose"),
                            line = list(color = "#FFFFFF", width = 1)),
              showlegend = FALSE) %>%
        layout(title = "Percentage of workouts by duration",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    } # close else if statement
    
    else {
      chest <- fread(path)[c(grep("Chest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ]
      back <- unique(rbind(data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Back", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ]),
                           data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Pull", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])))
      legs <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Legs", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      compound <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Compound", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      rest <- data.frame((fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])[c(grep("Rest", (fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])$Workout)), ])
      
      pie2 <- data.frame(
        Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
        time = c(sum(chest$Duration), sum(back$Duration), sum(legs$Duration),
                 sum(compound$Duration), sum(fread(run)$Duration), 
                 (sum(fread(path)$Duration)-sum(chest$Duration)-sum(back$Duration)-sum(legs$Duration)-sum(compound$Duration)-sum(fread(run)$Duration)))
      )
      
      plot_ly(pie2,
              labels = ~Workout,
              values = ~time,
              type = 'pie',
              textposition = "inside",
              textinfo = "percent",
              insidetextfont = list(color = "gray50"),
              hoverinfo = "text",
              text = ~paste0(Workout, ": ", round(time/60, 2), " hours"),
              marker = list(colors = c("indianred", "navy", "gold", "lavender",
                                       "mediumturquoise", "mistyrose"),
                            line = list(color = "#FFFFFF", width = 1)),
              showlegend = FALSE) %>%
        layout(title = "Percentage of workouts by duration",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    } # close else statement
  }) # close pie_chart_time
  
  output$piecharttime <- renderPlotly({
    pie_chart_time()
  })
  
} # close server

######################################################################################
######################################################################################
######################################################################################

shinyApp(ui = ui, server = server)


