packages <- c("data.table", "dplyr", "stringr", "ggplot2", "DataCombine", "miscTools",
              "grDevices", "shiny", "shinyBS", "shinydashboard", "plotly", "shinythemes",
              "DT", "rhandsontable", "shinyjs", "shinycssloaders", "timevis")

for (package in packages) {
  if (!package %in% rownames(installed.packages())) {
    install.packages(package)
  } # if
} # loop

##### load required packages

suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(ggplot2))
suppressMessages(library(DataCombine))
suppressMessages(library(miscTools))
suppressMessages(library(grDevices))
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyBS))
suppressMessages(library(plotly))
suppressMessages(library(shinythemes))
suppressMessages(library(DT))
suppressMessages(library(rhandsontable))
suppressMessages(library(shinyjs))
suppressMessages(library(shinycssloaders))
suppressMessages(library(timevis))

##### check if required directories/files exist and if not, create them

if (!dir.exists(paste0(Sys.getenv("HOME"), "/fitr"))) {
  dir.create(paste0(Sys.getenv("HOME"), "/fitr"))
}

if (!dir.exists(paste0(Sys.getenv("HOME"), "/fitr/calendar"))) {
  dir.create(paste0(Sys.getenv("HOME"), "/fitr/calendar"))
}

if (!dir.exists(paste0(Sys.getenv("HOME"), "/fitr/backups"))) {
  dir.create(paste0(Sys.getenv("HOME"), "/fitr/backups"))
}

if (!file.exists(paste0(Sys.getenv("HOME"), "/fitr/data.csv")) & 
    !file.exists(paste0(Sys.getenv("HOME"), "/fitr/run.csv")) &
    !file.exists(paste0(Sys.getenv("HOME"), "/fitr/plan.csv")) &
    !file.exists(paste0(Sys.getenv("HOME"), "/fitr/calendar/calendar.csv"))) {
  ##### create csv files
  write.table(data.frame("No", "Date", "Workout", "Duration"),
              file = paste0(Sys.getenv("HOME"), "/fitr/data.csv"),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              quote = FALSE)
  
  write.table(data.frame("No", "Date", "Distance", "Duration", "Pace"),
              file = paste0(Sys.getenv("HOME"), "/fitr/run.csv"),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              quote = FALSE)
  
  write.table(data.frame("id", "content", "start"),
              file = paste0(Sys.getenv("HOME"), "/fitr/plan.csv"),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              quote = FALSE)
  
  write.table(data.frame("starttime", "summary"),
              file = paste0(Sys.getenv("HOME"), "/fitr/calendar/calendar.csv"),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",",
              quote = FALSE)
}

##### create required .ics files

if (!file.exists(paste0(Sys.getenv("HOME"), "/fitr/calendar/template_header.ics")) & 
    !file.exists(paste0(Sys.getenv("HOME"), "/fitr/calendar/template_body.ics")) &
    !file.exists(paste0(Sys.getenv("HOME"), "/fitr/calendar/template_footer.ics"))) {
  
  cat("BEGIN:VCALENDAR\nVERSION:2.0\nCALSCALE:GREGORIAN",
      file = paste0(Sys.getenv("HOME"), "/fitr/calendar/template_header.ics"))
  cat("BEGIN:VEVENT\nDTSTART:20060912T060000Z\nUID:461092315540@example.com\nSUMMARY:new2\nEND:VEVENT\n",
      file = paste0(Sys.getenv("HOME"), "/fitr/calendar/template_body.ics"))
  cat("END:VCALENDAR",
      file = paste0(Sys.getenv("HOME"), "/fitr/calendar/template_footer.ics"))
  
}

setwd(paste0(Sys.getenv("HOME"), "/fitr"))

path <- paste0(Sys.getenv("HOME"), "/fitr/data.csv")

run <- paste0(Sys.getenv("HOME"), "/fitr/run.csv")

plan <- paste0(Sys.getenv("HOME"), "/fitr/plan.csv")

calendar <- paste0(Sys.getenv("HOME"), "/fitr/calendar/calendar.csv")

workouts <- c("Chest", "Back", "Legs", "Compound", "Running", "Rest", "Other")

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

cat("\n\t##############\n\n\tWELCOME!\n\tLET's GET FIT!\n\n\t##############\n\n")

######################################################################################
######################################################################################
# ----------------------------------------UI---------------------------------------- #
######################################################################################
######################################################################################

ui <- dashboardPage(skin = "black", title = "FitR - Your fitness tracker",
                    
                    dashboardHeader(title = tags$strong("FitR")),
                    
                    ################################################## DASHBOARD SIDEBAR ######################################
                    
                    dashboardSidebar(
                      sidebarMenu(id = "sidebar",
                                  
                                  menuItem("Home", tabName = "home", startExpanded = TRUE, icon = icon("calendar")),
                                  
                                  menuItem("Running", tabName = "running", startExpanded = TRUE, icon = icon("heartbeat")),
                                  
                                  menuItem("Statistics", tabName = "statistics", startExpanded = TRUE, icon = icon("bar-chart-o"))
                      ) # sidebarMenu
                    ), # dashboardSidebar
                    
                    ##################################################################################################
                    ######################################### DASHBOARD MAIN BODY ####################################
                    ##################################################################################################
                    
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName = "home",
                                
                                fluidRow(
                                  box(title = tags$strong("Plan Builder"),
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = 12,
                                      
                                      fluidRow(
                                        column(12,timevisOutput("timeline") %>%
                                                 withSpinner(type = 7, color = "#000000"))),
                                      
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
                                                           value = "Specify"))),
                                        
                                        column(1,
                                               actionButton(inputId = "plan_add",
                                                            label = NULL,
                                                            width = "100%",
                                                            style = "color: white; background-color: black;",
                                                            icon = icon("plus"))),
                                        
                                        bsPopover(id = "plan_add",
                                                  title = NULL,
                                                  content = "Plan a workout",
                                                  placement = "bottom",
                                                  trigger = "hover"),
                                        
                                        column(1,
                                               actionButton(inputId = "plan_remove",
                                                            label = NULL,
                                                            width = "100%",
                                                            style = "color: white; background-color: black;",
                                                            icon = icon("minus"))),
                                        
                                        bsPopover(id = "plan_remove",
                                                  title = NULL,
                                                  content = "Remove planned workout",
                                                  placement = "bottom",
                                                  trigger = "hover"),
                                        
                                        column(1,
                                               actionButton(inputId = "center",
                                                            label = NULL,
                                                            width = "100%",
                                                            style = "color: white; background-color: black;",
                                                            icon = icon("arrows-h"))),
                                        
                                        bsPopover(id = "center",
                                                  title = NULL,
                                                  content = "Center timeline",
                                                  placement = "bottom",
                                                  trigger = "hover"),
                                        
                                        column(1,
                                               actionButton(inputId = "calendar_sync",
                                                            label = NULL,
                                                            width = "100%",
                                                            style = "color: white; background-color: black;",
                                                            icon = icon("retweet"))),
                                        
                                        bsPopover(id = "calendar_sync",
                                                  title = NULL,
                                                  content = "Sync with calendar",
                                                  placement = "bottom",
                                                  trigger = "hover"),
                                        
                                        column(1,
                                               actionButton(inputId = "backup",
                                                            label = NULL,
                                                            width = "100%",
                                                            style = "color: white; background-color: black;",
                                                            icon = icon("download"))),
                                        
                                        bsPopover(id = "backup",
                                                  title = NULL,
                                                  content = "Backup data",
                                                  placement = "bottom",
                                                  trigger = "hover"),
                                        
                                        column(1,
                                               useShinyjs(),
                                               extendShinyjs(text = jscode, functions = c("closeWindow")),
                                               actionButton(inputId = "close",
                                                            label = NULL,
                                                            width = "100%",
                                                            style = "color: white; background-color: black;",
                                                            icon = icon("power-off"))))
                                  ) # box 1
                                ), # fluidRow - box 1
                                
                                fluidRow(
                                  
                                  box(title = tags$strong("Fitness Log"),
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = 12,
                                      
                                      sidebarLayout(
                                        sidebarPanel(width = 4,
                                                     
                                                     uiOutput("dateUI"),
                                                     
                                                     selectInput(inputId = "workout",
                                                                 label = "Workouts",
                                                                 choices = workouts),
                                                     
                                                     conditionalPanel(
                                                       condition = "input.workout == \"Other\"",
                                                       textInput(inputId = "other",
                                                                 label = NULL,
                                                                 value = "Specify")),
                                                     
                                                     conditionalPanel(
                                                       condition = "input.workout == \"Running\"",
                                                       numericInput(inputId = "distance",
                                                                    label = "Running distance (km)",
                                                                    value = NULL,
                                                                    min = 0)),
                                                     
                                                     numericInput(inputId = "duration",
                                                                  label = "Duration (mins)",
                                                                  value = 35,
                                                                  min = 0),
                                                     
                                                     fluidRow(
                                                       column(5,
                                                              actionButton(inputId = "remove",
                                                                           label = NULL,
                                                                           width = "100%",
                                                                           style = "color: white; background-color: black;",
                                                                           icon = icon("minus"))),
                                                       
                                                       bsPopover(id = "remove",
                                                                 title = NULL,
                                                                 content = "Remove recorded workout",
                                                                 placement = "bottom",
                                                                 trigger = "hover"),
                                                       
                                                       column(5,
                                                              offset = 2,
                                                              actionButton(inputId = "add",
                                                                           label = NULL,
                                                                           width = "100%",
                                                                           style = "color: white; background-color: black;",
                                                                           icon = icon("plus"))),
                                                       
                                                       bsPopover(id = "add",
                                                                 title = NULL,
                                                                 content = "Record new workout",
                                                                 placement = "bottom",
                                                                 trigger = "hover"))
                                        ), # close sidebarPanel
                                        
                                        mainPanel(width = 8,
                                                  column(12,
                                                         DTOutput("log") %>%
                                                           withSpinner(type = 7, color = "#000000"))
                                        ) # close mainPanel
                                      ) # close sidebarLayout
                                  ) # close box 2
                                ) # close fluidRow - box 2
                        ), # home tab
                        
                        tabItem(tabName = "running",
                                
                                tags$br(),
                                
                                fluidRow(
                                  column(12,
                                         DTOutput("run") %>%
                                           withSpinner(type = 7, color = "#000000"))),
                                
                                tags$br(),
                                
                                fluidRow(
                                  box(title = tags$strong("Distance per run"),
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = 12,
                                      
                                      fluidRow(
                                        column(12,
                                               plotOutput("runstats", height = "500px") %>%
                                                 withSpinner(type = 7, color = "#000000")))
                                  ) # box 3
                                ) # fluidRow - box 3
                        ), # running tab
                        
                        tabItem(tabName = "statistics",
                                
                                tags$br(),
                                
                                fluidRow(
                                  column(4, uiOutput("periodUI")),
                                  
                                  column(1,
                                         tags$br(),
                                         actionButton(inputId = "reset_period",
                                                      label = NULL,
                                                      width = "100%",
                                                      style = "color: white; background-color: black;",
                                                      icon = icon("play"))),
                                  
                                  bsPopover(id = "reset_period",
                                            title = NULL,
                                            content = "Reset time period",
                                            placement = "right",
                                            trigger = "hover")),
                                
                                fluidRow(
                                  column(12,
                                         DTOutput("stats") %>%
                                           withSpinner(type = 7, color = "#000000"))),
                                
                                tags$br(),
                                
                                fluidRow(
                                  box(title = tags$strong("Duration per workout"),
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = 12,
                                      fluidRow(
                                        column(12,
                                               plotOutput(outputId = "duration_graph", height = "500px") %>%
                                                 withSpinner(type = 7, color = "#000000")))
                                  ) # box 4
                                ), # fluidRow - box 4
                                
                                tags$br(),
                                
                                fluidRow(
                                  box(title = NULL,
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = 12,
                                      fluidRow(
                                        column(6,
                                               plotlyOutput(outputId = "piechartnum") %>%
                                                 withSpinner(type = 7, color = "#000000")),
                                        
                                        column(6,
                                               plotlyOutput(outputId = "piecharttime") %>%
                                                 withSpinner(type = 7, color = "#000000"))
                                      ) # fluidRow
                                  ) # box 5
                                ) # fluidRow - box 5
                        ) # statistics tab
                      ) # tabItems
                    ) # dashboardBody
) # dashboardPage

######################################################################################
######################################################################################
# --------------------------------------SERVER-------------------------------------- #
######################################################################################
######################################################################################


server <- function(input, output, session) {
  
  
  values <- reactiveValues()
  
  values$timeline_data <- fread(plan)
  values$table <- fread(path)
  values$table_run <- fread(run)
  
  
  session$onSessionEnded(function() {
    gc()
    cat("\n\t#######\n\n\tSEE YA!\n\n\t#######\n\n")
    stopApp()
  })
  
  
  observeEvent(input$close, {
    js$closeWindow()
    cat("\n\t#######\n\n\tSEE YA!\n\n\t#######\n\n")
    stopApp()
  })
  
  
  observeEvent(input$backup, {
    
    withProgress(message = "Backing up data...", {
      system("cp ~/fitr/data.csv ~/fitr/backups/data_backup.csv")
      system("cp ~/fitr/run.csv ~/fitr/backups/run_backup.csv")
      system("cp ~/fitr/plan.csv ~/fitr/backups/plan_backup.csv")
      system("cp ~/fitr/calendar/calendar.csv ~/fitr/backups/calendar_backup.csv")
    }) # withProgress
    
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
      } # nested if
      
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
      } # nested else
    } # if nrow(plan) == 0
    
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
      } # nested else
    } # else
    
    values$timeline_data <- fread(plan)
    
  }) # observeEvent - plan_add
  
  
  observeEvent(input$plan_remove, {
    
    if (nrow(fread(plan)) == 0) {
      showNotification("There are no planned workouts to delete", type = "error")
    } # if
    
    else {
      write.table(fread(plan)[-nrow(fread(plan)), ],
                  file = plan,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE)
    } # else
    
    values$timeline_data <- fread(plan)
    
  }) # observeEvent - plan_remove
  
  
  output$timeline <- renderTimevis({
    
    validate(need(nrow(values$timeline_data) != 0, message = "No data available"))
    
    timevis(
      data = values$timeline_data,
      fit = FALSE,
      showZoom = FALSE,
      zoomFactor = 0.25) %>%
      setWindow(Sys.Date() - 1, Sys.Date() + 6)
  })
  
  
  observeEvent(input$center, {
    centerTime("timeline", Sys.Date() + 3)
  })
  
  
  observeEvent(input$calendar_sync, {
    write.table(data.frame(
      starttime = gsub("-", "", as.character(fread(plan)$start)),
      summary = fread(plan)$content),
      file = calendar,
      sep = "\t",
      row.names = FALSE,
      quote = FALSE)
    
    df <- read.csv(paste0(Sys.getenv("HOME"), "/fitr/calendar/calendar.csv"),
                   sep = "\t",
                   stringsAsFactors = FALSE)
    
    ics_header <- readLines(paste0(Sys.getenv("HOME"), "/fitr/calendar/template_header.ics"), warn = FALSE)
    ics_body <- readLines(paste0(Sys.getenv("HOME"), "/fitr/calendar/template_body.ics"), warn = FALSE)
    ics_footer <- readLines(paste0(Sys.getenv("HOME"), "/fitr/calendar/template_footer.ics"), warn = FALSE)
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
    
    write(ics_events, file = paste0(Sys.getenv("HOME"), "/fitr/calendar/Workouts.ics"))
    
    system("open ~/fitr/calendar/Workouts.ics")
  })
  
  
  ############################# WORKOUT LOG #############################
  
  output$dateUI <- renderUI({
    
    validate(need(values$table, message = FALSE))
    
    dateInput(inputId = "date",
              label = "Date",
              width = "100%",
              value = as.Date(tail(fread(path)$Date, 1)) + 1,
              format = "DD-dd-mm-yyyy",
              weekstart = 1)
    
  })
  
  
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
    } # if Other
    
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
    } # else if Rest
    
    else if (input$workout == "Running") {
      write.table(data.frame(
        No = ((nrow(fread(path))) + 1),
        Date = as.character(input$date),
        Workout = input$workout,
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
        ) # as.numeric
      ),
      file = run,
      append = TRUE,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE)
    } # else if Running
    
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
    } # else
    
    values$table <- fread(path)
    
    values$table_run <- fread(run)
    
  }) # observeEvent
  
  
  observeEvent(input$remove, {
    if (nrow(fread(path)) == 0) {
      showNotification("There are no logged workouts to delete", type = "error")
    } # if
    
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
      } # nested if
      
      else {
        write.table(fread(path)[-nrow(fread(path)), ],
                    file = path,
                    sep = ",",
                    row.names = FALSE,
                    col.names = TRUE,
                    quote = FALSE)
      } # nested else
    } # else
    
    values$table <- fread(path)
    
    values$table_run <- fread(run)
    
  }) # observeEvent
  
  
  output$log <- DT::renderDT({
    
    validate(need(nrow(values$table) != 0, message = "No data available"))
    
    datatable(values$table,
              rownames = FALSE,
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
                  # backgroundColor = styleEqual(
                  #   c("Chest", "Back", "Legs", "Compound",
                  #     "Running", "Rest", "Other"),
                  #   c("indianred", "navy", "gold", "lavender",
                  #     "mediumturquoise", "black", "mistyrose")),
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
                      "Other",
                      "Bouldering"),
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
                      "mistyrose",
                      "mistyrose")
                    ) #styleEqual
                  ) #formatStyle
  }) # renderDT
  
  
  ################################ RUNNING ################################
  
  output$run <- DT::renderDT({
    
    validate(need(nrow(values$table_run) != 0, message = "No data available"))
    
    datatable(values$table_run,
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
  
  
  running_stats <- eventReactive(c(input$add, input$remove, input$sidebar), {
    
    run_graph <- data.frame(
      run_num <- fread(run)$No,
      dist <- fread(run)$Distance
    )
    
    ggplot(run_graph, aes(run_num, dist)) +
      geom_line(color = "mediumturquoise") + 
      labs(x = "Run",
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
      ) # theme
  }) # eventReactive
  
  
  output$runstats <- renderPlot({
    validate(need(nrow(values$table_run) != 0, message = "No data available"))
    running_stats()
  })
  
  
  ################################ STATISTICS ################################
  
  output$periodUI <- renderUI({
    
    validate(need(input$sidebar, message = FALSE))
    validate(need(nrow(values$table) != 0, message = "No data available"))
    
    dateRangeInput(inputId = "period",
                   label = "Select period",
                   start = as.Date(head(values$table$Date, 1)),
                   end = as.Date(tail(values$table$Date, 1)),
                   min = as.Date(head(values$table$Date, 1)),
                   format = "yyyy-mm-dd",
                   startview = "year",
                   weekstart = 1,
                   separator = "to")
  }) # renderUI
  
  
  observeEvent(input$reset_period, {
    reset("period")
  })
  
  
  statistics_table <- eventReactive(c(input$add, input$remove, input$period, input$sidebar), {
    
    if (input$period[1] == input$period[2]) {
      showNotification("Stats for a single day not available. Please refer to the workout log.", type = "error")
      data.frame(n = NA, u = NA, l = NA, l = NA)
    } # if
    
    else if (input$period[2] > as.Date(tail(fread(path)$Date, 1))) {
      showNotification("Future stats are not available!", type = "error")
      data.frame(n = NA, u = NA, l = NA, l = NA)
    } # else if
    
    else if (input$period[1] > input$period[2]) {
      showNotification("Did you get the dates mixed up?", type = "error")
      data.frame(n = NA, u = NA, l = NA, l = NA)
    } # else if
    
    else {
      data.frame(
        total_entries <- nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]),
        workout_days <- length(which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]] != 0)),
        rest_days <- length(which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]] == 0)),
        rest_days_percent <- round((length(which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]] == 0)) * 100)/nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]), 1),
        avg_mins_per_day <- round(mean(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]]), 1),
        avg_hours_per_week <- round((sum(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][[4]])/60)/(nrow(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ])/7), 1)
      ) # data.frame
    } # else
  }) # eventReactive
  
  
  output$stats <- DT::renderDT({
    
    validate(need(input$period, message = FALSE))
    validate(need(nrow(values$table) != 0, message = "No data available"))
    
    datatable(statistics_table(),
              rownames = FALSE,
              colnames = c("Total entries",
                           "Workouts tracked",
                           "Rest days",
                           "Rest (%)",
                           "Average workout time/day (mins)",
                           "Average workout time/week (hours)"),
              options = list(
                dom = "t",
                bSort = FALSE,
                columnDefs = list(list(className = "dt-center",
                                       targets = c(0, 1, 2, 3, 4, 5)))
              )
    ) # datatable
  })
  
  
  #################### DURATION GRAPH ###################
  
  duration <- eventReactive(c(input$add, input$remove, input$period, input$sidebar), {
    
    if (input$period[1] == input$period[2]) {
      ggplot()
    } # if
    
    else if (input$period[2] > as.Date(tail(fread(path)$Date, 1))) {
      ggplot()
    } # else if
    
    else if (input$period[1] > input$period[2]) {
      ggplot()
    } # else if
    
    else {
      duration_graph <- data.frame(
        workout_id <- fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration != 0), ]$No,
        duration <- fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ][which(fread(path)[c(grep(input$period[1], fread(path)$Date):grep(input$period[2], fread(path)$Date)), ]$Duration != 0), ]$Duration
      )
      
      ggplot(duration_graph, aes(workout_id, duration)) +
        geom_line(color = "black") + 
        labs(x = "Workout",
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
        ) # theme
    } # else
  }) # eventReactive
  
  
  output$duration_graph <- renderPlot({
    validate(need(input$period, message = FALSE))
    validate(need(nrow(values$table) != 0, message = "No data available"))
    duration()
  })
  
  
  #################### PIE CHART NUM ####################
  
  pie_chart_num <- eventReactive(c(input$add, input$remove, input$period, input$sidebar), {
    
    if (input$period[1] == input$period[2]) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # if
    
    else if (input$period[2] > as.Date(tail(fread(path)$Date, 1))) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # else if
    
    else if (input$period[1] > input$period[2]) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # else if
    
    else {
      
      data1 <- fread(path) %>%
        dplyr::filter(fread(path)$Date >= input$period[1] & fread(path)$Date <= input$period[2])
      
      chest1 <- data.frame(data1[grep("Chest", data1$Workout), ])
      
      back1 <- unique(rbind(data.frame(data1[grep("Back", data1$Workout), ]),
                            data.frame(data1[grep("Pull", data1$Workout), ])))
      
      legs1 <- data.frame(data1[grep("Legs", data1$Workout), ])
      
      compound1 <- data.frame(data1[grep("Compound", data1$Workout), ])
      
      run1 <- data.frame(data1[grep("Running", data1$Workout), ])
      
      rest1 <- data.frame(data1[grep("Rest", data1$Workout), ])
      
      
      pie1 <- data.frame(
        Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
        num = c(nrow(chest1),
                nrow(back1),
                nrow(legs1),
                nrow(compound1),
                nrow(run1),
                nrow(data1)-nrow(chest1)-nrow(back1)-nrow(legs1)-nrow(compound1)-nrow(run1)-nrow(rest1))
      ) # data.frame
      
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
      
    } # else
  }) # eventReactive
  
  
  output$piechartnum <- renderPlotly({
    validate(need(input$period, message = FALSE))
    validate(need(nrow(values$table) != 0, message = "No data available"))
    pie_chart_num()
  })
  
  
  #################### PIE CHART TIME ####################
  
  pie_chart_time <- eventReactive(c(input$add, input$remove, input$period, input$sidebar), {
    
    if (input$period[1] == input$period[2]) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # if
    
    else if (input$period[2] > as.Date(tail(fread(path)$Date, 1))) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # else if
    
    else if (input$period[1] > input$period[2]) {
      plot_ly(data = data.frame(n = NA, u = NA), type = "pie")
    } # else if
    
    else {
      
      data2 <- fread(path) %>%
        dplyr::filter(fread(path)$Date >= input$period[1] & fread(path)$Date <= input$period[2])
      
      chest2 <- data.frame(data2[grep("Chest", data2$Workout), ])
      
      back2 <- unique(rbind(data.frame(data2[grep("Back", data2$Workout), ]),
                            data.frame(data2[grep("Pull", data2$Workout), ])))
      
      legs2 <- data.frame(data2[grep("Legs", data2$Workout), ])
      
      compound2 <- data.frame(data2[grep("Compound", data2$Workout), ])
      
      run2 <- data.frame(data2[grep("Running", data2$Workout), ])
      
      rest2 <- data.frame(data2[grep("Rest", data2$Workout), ])
      
      
      pie2 <- data.frame(
        Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
        time = c(sum(chest2$Duration),
                 sum(back2$Duration),
                 sum(legs2$Duration),
                 sum(compound2$Duration),
                 sum(run2$Duration), 
                 (sum(data2$Duration)-sum(chest2$Duration)-sum(back2$Duration)-sum(legs2$Duration)-sum(compound2$Duration)-sum(run2$Duration)))
      ) # data.frame
      
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
      
    } # else
  }) # pie_chart_time
  
  
  output$piecharttime <- renderPlotly({
    validate(need(input$period, message = FALSE))
    validate(need(nrow(values$table) != 0, message = "No data available"))
    pie_chart_time()
  })
  
  
} # server

######################################################################################
######################################################################################

shinyApp(ui = ui, server = server, options = list(port = 5000))
