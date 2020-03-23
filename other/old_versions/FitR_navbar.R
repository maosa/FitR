##### required packages

packages <- c("data.table", "dplyr", "stringr", "ggplot2", "DataCombine", "miscTools",
              "grDevices", "shiny", "shinydashboard", "plotly", "shinythemes",
              "DT", "rhandsontable", "shinyjs", "shinycssloaders", "timevis")

##### check if package is installed and if not, install it

for (package in packages) {
  if (!package %in% rownames(installed.packages())) {
    install.packages(package)
  }
  
  else {
    NULL
  }
}

##### load required packages

library(data.table)
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

##### check if required directories/files exist and if not, create them

if (!dir.exists(paste0(Sys.getenv("HOME"), "/FitR"))) {
  dir.create(paste0(Sys.getenv("HOME"), "/FitR"))
} else {
  NULL
}

if (!dir.exists(paste0(Sys.getenv("HOME"), "/FitR/calendar"))) {
  dir.create(paste0(Sys.getenv("HOME"), "/FitR/calendar"))
} else {
  NULL
}

if (!dir.exists(paste0(Sys.getenv("HOME"), "/FitR/backups"))) {
  dir.create(paste0(Sys.getenv("HOME"), "/FitR/backups"))
} else {
  NULL
}

if (!file.exists(paste0(Sys.getenv("HOME"), "/FitR/data.csv")) & 
    !file.exists(paste0(Sys.getenv("HOME"), "/FitR/run.csv")) &
    !file.exists(paste0(Sys.getenv("HOME"), "/FitR/plan.csv")) &
    !file.exists(paste0(Sys.getenv("HOME"), "/FitR/calendar.csv"))) {
  ##### create csv files
  write.table(data.frame("No", "Date", "Workout", "Duration"),
              file = paste0(Sys.getenv("HOME"), "/FitR/data.csv"),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              quote = FALSE)
  
  write.table(data.frame("No", "Date", "Distance", "Duration", "Pace"),
              file = paste0(Sys.getenv("HOME"), "/FitR/run.csv"),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              quote = FALSE)
  
  write.table(data.frame("id", "content", "start"),
              file = paste0(Sys.getenv("HOME"), "/FitR/plan.csv"),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              quote = FALSE)
  
  write.table(data.frame("starttime", "summary"),
              file = paste0(Sys.getenv("HOME"), "/FitR/calendar/calendar.csv"),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              quote = FALSE)
} else {
  NULL
}

##### create required .ics files

if (!file.exists(paste0(Sys.getenv("HOME"), "/FitR/calendar/template_header.ics")) & 
    !file.exists(paste0(Sys.getenv("HOME"), "/FitR/calendar/template_body.ics")) &
    !file.exists(paste0(Sys.getenv("HOME"), "/FitR/calendar/template_footer.ics"))) {
  
  cat("BEGIN:VCALENDAR\nVERSION:2.0\nCALSCALE:GREGORIAN",
      file = paste0(Sys.getenv("HOME"), "/FitR/calendar/template_header.ics"))
  cat("BEGIN:VEVENT\nDTSTART:20060912T060000Z\nUID:461092315540@example.com\nSUMMARY:new2\nEND:VEVENT\n",
      file = paste0(Sys.getenv("HOME"), "/FitR/calendar/template_body.ics"))
  cat("END:VCALENDAR",
      file = paste0(Sys.getenv("HOME"), "/FitR/calendar/template_footer.ics"))
  
} else {
  NULL
}


setwd(paste0(Sys.getenv("HOME"), "/FitR"))

path <- paste0(Sys.getenv("HOME"), "/FitR/data.csv")

run <- paste0(Sys.getenv("HOME"), "/FitR/run.csv")

plan <- paste0(Sys.getenv("HOME"), "/FitR/plan.csv")

calendar <- paste0(Sys.getenv("HOME"), "/FitR/calendar/calendar.csv")

workouts <- c("Chest", "Back", "Legs", "Compound", "Running", "Rest", "Other")

jscode <- "shinyjs.closeWindow = function() { window.close(); }"


######################################################################################
######################################################################################
# ----------------------------------------UI---------------------------------------- #
######################################################################################
######################################################################################

ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage(title = tags$h4(tags$strong("FitR")),
                           id = "fitr_navbar",
                           
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
######################################################################################
# --------------------------------------SERVER-------------------------------------- #
######################################################################################
######################################################################################


server <- function(input, output) {
  
  observeEvent(input$close, {     # <<<<<----- Exit app
    js$closeWindow()
    stopApp()
  })
  
  
  observeEvent(input$backup, {
    withProgress(message = "Backing up data...", {
      system("cp ~/FitR/data.csv ~/FitR/backups/data_backup.csv")
      system("cp ~/FitR/run.csv ~/FitR/backups/run_backup.csv")
      system("cp ~/FitR/plan.csv ~/FitR/backups/plan_backup.csv")
      system("cp ~/FitR/calendar/calendar.csv ~/FitR/backups/calendar_backup.csv")
    })
  })
  
  
  ############################# TIMELINE #############################

  observeEvent(input$plan_add, {
    
    if (nrow(fread(plan)) == 0) {
      
      if (input$plan_workout == "Other") {
        write.table(data.frame(
          id = ((nrow(fread(plan))) + 1),
          content = input$plan_other,
          start = as.character(Sys.Date())
        ),
        file = plan,
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE)
      } # close nested if statement
      
      else {
        write.table(data.frame(
          id = ((nrow(fread(plan))) + 1),
          content = input$plan_workout,
          start = as.character(Sys.Date())
        ),
        file = plan,
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE)
      } # close nested else statement
      
    } # close if statement
    
    else {
      
      if (input$plan_workout == "Other") {
        write.table(data.frame(
          id = ((nrow(fread(plan))) + 1),
          content = input$plan_other,
          start = as.character((as.Date(tail(fread(plan)$start, 1)) + 1))
        ),
        file = plan,
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE)
      } # close nested if statement
      
      else {
        write.table(data.frame(
          id = ((nrow(fread(plan))) + 1),
          content = input$plan_workout,
          start = as.character((as.Date(tail(fread(plan)$start, 1)) + 1))
        ),
        file = plan,
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE)
      } # close nested else statement
    } # close else statement
  }) # close observeEvent - plan_add
  
  
  observeEvent(input$plan_remove, {
    if (nrow(fread(path)) == 0) {
      showNotification("There are no planned workouts to delete", type = "error")
    } # close if statement
    
    else {
      write.table(fread(plan)[-nrow(fread(plan)), ],
                  file = plan,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE)
    } # close else statement
  })
  
  
  timeline_data <- eventReactive(c(input$plan_add, input$plan_remove), {
    fread(plan)
  })
  
  
  output$timeline <- renderTimevis({
    validate(need(nrow(timeline_data()) != 0, message = "No data available"))
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
    
    df <- read.csv(paste0(Sys.getenv("HOME"), "/FitR/calendar/calendar.csv"),
                   sep = "\t",
                   stringsAsFactors = FALSE)
    
    ics_header <- readLines(paste0(Sys.getenv("HOME"), "/FitR/calendar/template_header.ics"), warn = FALSE)
    ics_body <- readLines(paste0(Sys.getenv("HOME"), "/FitR/calendar/template_body.ics"), warn = FALSE)
    ics_footer <- readLines(paste0(Sys.getenv("HOME"), "/FitR/calendar/template_footer.ics"), warn = FALSE)
    ics_events <- ""
    
    for(i in 1:nrow(df)) {
      ics_body <- str_replace(ics_body, "DTSTART:.*", paste0("DTSTART:", df$starttime[i]))
      # create unique identifier
      ics_body <- str_replace(ics_body, "UID:.*", paste0("UID:", paste0(df$starttime[i], df$endtime[i])))
      ics_body <- str_replace(ics_body, "SUMMARY:.*", paste0("SUMMARY:", df$summary[i]))
      ics_events <- append(ics_events, ics_body)
    }
    
    # combine template parts to one vector
    ics_events <- append(ics_header, ics_events)
    ics_events <- append(ics_events, ics_footer)
    
    write(ics_events, file = paste0(Sys.getenv("HOME"), "/FitR/calendar/Workouts.ics"))
    
    system("open ~/FitR/calendar/Workouts.ics")
  })
  
  
  
  
  ############################# WORKOUT LOG #############################
  
  observeEvent(input$add, {
    if (input$workout == "Other") {
      write.table(data.frame(
        No = ((nrow(fread(path))) + 1),
        Date = as.character(input$date),
        Workout = input$other,
        Duration = as.numeric(input$duration)),
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
        Date = as.character(input$date),
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
        Duration = as.numeric(input$duration)),
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
      ),
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
    if (nrow(fread(path)) == 0) {
      showNotification("There are no logged workouts to delete", type = "error")
    } # close if statement
    
    else {
      
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
      } # close nested if statement
      
      else {
        write.table(fread(path)[-nrow(fread(path)), ],
                    file = path,
                    sep = ",",
                    row.names = FALSE,
                    col.names = TRUE,
                    quote = FALSE)
      } # close nested else statement
      
    } # close else statement
  }) # close observeEvent
  
  
  table <- eventReactive(c(input$add, input$remove), {
    fread(path)
  })
  
  
  table_run <- eventReactive(c(input$add, input$remove), {
    fread(run)
  })
  
  
  output$log <- DT::renderDT({
    validate(need(nrow(table()) != 0, message = "No data available"))
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
                    c("Chest", "Back", "Legs", "Compound",
                      "Running", "Rest", "Other"),
                    c("indianred", "navy", "gold", "lavender",
                      "mediumturquoise", "black", "mistyrose"))
                  )
  }) # close renderDT
  
  
  ################################ RUNNING ################################
  
  running_stats <- eventReactive(c(input$add, input$remove), {
    run_graph <- data.frame(
      run_num <- fread(run)$No,
      dist <- fread(run)$Distance
    )
  
  ###################### TABLE ######################
  
  output$run <- DT::renderDT({
    validate(need(nrow(table_run()) != 0, message = "No data available"))
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
    validate(need(nrow(table_run()) != 0, message = "No data available"))
    running_stats()
  })
  
  ################################ STATISTICS ################################
  
  observeEvent(input$reset_period, {
    reset("period")
  })
  
  statistics_table <- eventReactive(c(input$add, input$remove, input$period), {
    
    if (input$period[1] == input$period[2]) {
      showNotification("Stats for a single day not available. Please refer to the workout log.", type = "error")
      data.frame(n = NA, u = NA, l = NA, l = NA)
    } # close if statement
    
    else if (input$period[2] > as.Date(tail(fread(path)$Date, 1))) {
      showNotification("Future stats are not available?", type = "error")
      data.frame(n = NA, u = NA, l = NA, l = NA)
    } # close else if statement
    
    else if (input$period[1] > input$period[2]) {
      showNotification("Did you get the dates mixed up?", type = "error")
      data.frame(n = NA, u = NA, l = NA, l = NA)
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
    validate(need(nrow(table()) != 0, message = "No data available"))
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
  })
  

  #################### DURATION GRAPH ###################
  
  duration <- eventReactive(c(input$add, input$remove, input$period), {
    
    if (input$period[1] == input$period[2]) {
      ggplot()
    } # close if statement
    
    else if (input$period[2] > as.Date(tail(fread(path)$Date, 1))) {
      ggplot()
    } # close else if statement
    
    else if (input$period[1] > input$period[2]) {
      ggplot()
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
    validate(need(nrow(table()) != 0, message = "No data available"))
    duration()
  })
  
  
  #################### PIE CHART NUM ####################
  
  pie_chart_num <- eventReactive(c(input$add, input$remove, input$period), {
    
    if (input$period[1] == input$period[2]) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # close if statement
    
    else if (input$period[2] > as.Date(tail(fread(path)$Date, 1))) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # close else if statement
    
    else if (input$period[1] > input$period[2]) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
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
    validate(need(nrow(table()) != 0, message = "No data available"))
    pie_chart_num()
  })
  
  #################### PIE CHART TIME ####################
  
  pie_chart_time <- eventReactive(c(input$add, input$remove, input$period), {
    
    if (input$period[1] == input$period[2]) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # close if statement
    
    else if (input$period[2] > as.Date(tail(fread(path)$Date, 1))) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # close else if statement
    
    else if (input$period[1] > input$period[2]) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
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
    validate(need(nrow(table()) != 0, message = "No data available"))
    pie_chart_time()
  })
  
} # close server

######################################################################################
######################################################################################

shinyApp(ui = ui, server = server)

