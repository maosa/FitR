library(shiny)
library(shinydashboard)
library(shinythemes)
library(Matrix)
library(dplyr)
library(ggplot2)
library(mclust)
library(DT)
library(rhandsontable)
library(DataCombine)
library(miscTools)
library(grDevices)
library(shinyjs)

setwd("/Users/andreasmaos/Desktop/MuscleApp")

path <- "/Users/andreasmaos/Desktop/MuscleApp/data.csv"

run <- "/Users/andreasmaos/Desktop/MuscleApp/run.csv"

workouts <- c("Chest", "Back", "Legs", "Compound", "Running", "Rest", "Other")

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# ---------------------------------------------------------------------------------- #
# ----------------------------------------UI---------------------------------------- #
# ---------------------------------------------------------------------------------- #
ui <- fluidPage(theme = shinytheme("spacelab"),
                
                fluidRow(
                  column(3,
                         tags$h1(tags$strong("MuscleApp")),
                         tags$h6(tags$em("Developer: Andreas Maos"))),
                  
                  # column(4,
                  #        tags$br(),
                  #        tags$img(src = "dumbbell.jp2", width = 150, height = 75)),
                  
                  column(2,
                         offset = 3,
                         tags$br(),
                         tags$br(),
                         useShinyjs(),
                         extendShinyjs(text = jscode, functions = c("closeWindow")),
                         actionButton("close", "Close app"))
                  
                  # column(2,
                  #        tags$img(src = "kettlebell.jp2", width = 82.5, height = 97.5)),
                  # 
                  # column(2,
                  #        tags$img(src = "heart.jp2", width = 105, height = 90))
                ), # close fluidRow
                
                tags$hr(),
                
                tabsetPanel(
                  
                  #-------------------->>>>>>>>>> SCHEDULE <<<<<<<<<<--------------------#
                  
                  tabPanel(
                    # "Workout log",
                    icon = icon("calendar", lib = "font-awesome", class = "fa-2x"),
                    
                    tags$br(),
                    #---------->>>>>>>>>> Side bar <<<<<<<<<<----------#
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   dateInput(inputId = "date",
                                             label = "Date",
                                             format = "DD-dd-mm-yyyy",
                                             weekstart = 1),
                                   
                                   selectInput(inputId = "workout",
                                               label = "Workouts",
                                               choices = workouts),
                                   
                                   # textInput(inputId = "other",
                                   #           label = NULL,
                                   #           value = "Specify other workout"),
                                   
                                   numericInput(inputId = "distance",
                                                label = "Running distance (km)",
                                                value = NULL,
                                                min = 0),
                                   
                                   numericInput(inputId = "duration",
                                                label = "Duration (mins)",
                                                value = 35,
                                                min = 0),
                                   
                                   actionButton(inputId = "add",
                                                label = "Add new entry"),
                                   
                                   tags$br(),
                                   tags$br(),
                                   
                                   actionButton(inputId = "remove",
                                                label = "Remove last entry")
                      ),
                      
                      #---------->>>>>>>>>> Main panel <<<<<<<<<<----------#
                      mainPanel(
                        DTOutput("log")
                      ) # close mainPanel
                    ) # close sidebarLayout
                  ), # close tabPanel
                  
                  #-------------------->>>>>>>>>> RUNNING <<<<<<<<<<--------------------#
                  
                  tabPanel(
                    # "Running",
                    # icon = icon("walking", lib = "font-awesome", class = "fa-2x"),
                    icon = icon("heartbeat", lib = "font-awesome", class = "fa-2x"),
                    
                    tags$br(),
                    
                    fluidRow(
                      column(12,
                             DTOutput("run"))
                    ), # close fluidRow
                    
                    tags$hr(),
                    
                    fluidRow(
                      column(12,
                             plotOutput("runstats",
                                        height = "500px"))
                    )
                  ), # close tab
                  
                  #-------------------->>>>>>>>>> STATISTICS <<<<<<<<<<--------------------#
                  
                  tabPanel(
                    # "Statistics",
                    icon = icon("bar-chart-o", lib = "font-awesome", class = "fa-2x"),
                    
                    tags$br(),
                    
                    fluidRow(
                      column(12,
                             DTOutput("stats"))
                    ), # close fluidRow
                    
                    tags$hr(),
                    
                    plotOutput(outputId = "piechartnum"),
                    
                    tags$hr(),
                    
                    plotOutput(outputId = "piecharttime")
                    
                  ) # close tabPanel
                ) # close tabsetPanel
) # close fluidPage

# ---------------------------------------------------------------------------------- #
# --------------------------------------SERVER-------------------------------------- #
# ---------------------------------------------------------------------------------- #

server <- function(input, output) {
  
  observeEvent(input$close, {     # <<<<<----- Closes window and app
    js$closeWindow()
    stopApp()
  })
  
  data <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
  run_data <- read.csv(run, header = TRUE, stringsAsFactors = FALSE)
  
  # ------------------------->>>>> SCHEDULE TAB <<<<<------------------------- #
  
  observeEvent(input$workout, {
    if (input$workout == "Other") {
      insertUI(
        selector = "#workout",
        where = "afterEnd",
        ui = textInput(inputId = "other",
                       label = NULL,
                       value = "Specify other workout"),
        immediate = TRUE)
    }
    
    else {
      removeUI(
        selector = "div:has(> #other)",
        immediate = TRUE)
    }
  }) # close observeEvent
  
  observeEvent(input$add, {
    if (input$workout == "Other") {
      write.table(data.frame(
        No = ((nrow(read.csv(path))) + 1),
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
        No = ((nrow(read.csv(path))) + 1),
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
        No = ((nrow(read.csv(path))) + 1),
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
        No = ((nrow(read.csv(run))) + 1),
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
        No = ((nrow(read.csv(path))) + 1),
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
    if (read.csv(path)[nrow(read.csv(path)), 3] == "Running") {
      write.table(read.csv(path, header = TRUE,
                           stringsAsFactors = FALSE)[-nrow(read.csv(path)), ],
                  file = path,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE)
      
      write.table(read.csv(run, header = TRUE,
                           stringsAsFactors = FALSE)[-nrow(read.csv(run)), ],
                  file = run,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE)
    }
    
    else {
      write.table(read.csv(path, header = TRUE,
                           stringsAsFactors = FALSE)[-nrow(read.csv(path)), ],
                  file = path,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE,
                  quote = FALSE)
    }
  }) # close observeEvent
  
  
  table <- eventReactive(c(input$add, input$remove), {
    read.csv(path, header = TRUE, stringsAsFactors = FALSE)
  })
  
  table_run <- eventReactive(c(input$add, input$remove), {
    read.csv(run, header = TRUE, stringsAsFactors = FALSE)
  })
  
  
  output$log <- DT::renderDT({
    datatable(table(), rownames = FALSE,
              colnames = c("#", "Date", "Workout", "Duration (mins)"),
              # caption = "Workout log",
              options = list(
                # ---->>>> black table headrer  
                # initComplete = JS(
                #   "function(settings, json) {",
                #   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                #   "}"),
                dom = "tp",
                order = list(0, "desc"),
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
                      "mediumturquoise", "black", "mistyrose")))
  }) # close renderDT
  
  
  # ------------------------->>>>> RUNNING TAB <<<<<------------------------- #
  
  
  # --------------->>>>> TABLE <<<<<--------------- # 
  output$run <- DT::renderDT({
    datatable(table_run(),
              rownames = FALSE,
              colnames = c("#", "Date", "Distance (km)", "Duration (mins)", "Pace (mins/km)"),
              options = list(
                dom = "tp",
                order = list(0, "desc"),
                columnDefs = list(list(className = "dt-center",
                                       targets = c(0, 1, 2, 3, 4))))) %>%
      
      formatDate("Date", "toDateString") %>%
      
      formatStyle("No", fontWeight = "bold")
  })
  
  # --------------->>>>> GRAPH <<<<<--------------- #
  running_stats <- eventReactive(c(input$add, input$remove), {
    run_graph <- data.frame(
      run_num <- read.csv(run)$No,
      dist <- read.csv(run)$Distance
    )
    
    ggplot(run_graph, aes(run_num, dist)) +
      geom_line(color = "mediumturquoise") + 
      geom_point(size = 1, color = "turquoise") +
      labs(title = "Distance per run",
           x = "Run",
           y = "Distance (km)") +
      scale_x_continuous(breaks = seq(0, nrow(read.csv(run)), 5)) +
      scale_y_continuous(breaks = seq((round(((min(read.csv(run)$Distance)) - 1), 0)),
                                      max(read.csv(run)$Distance), 1)) +
      geom_hline(aes(yintercept = mean(read.csv(run)$Distance)),
                 linetype = "dashed",
                 color = "grey10") +
      geom_text(aes(0, (mean(read.csv(run)$Distance)),
                    label = round(mean(read.csv(run)$Distance), 2),
                    vjust = -1)) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14, face = "bold"),
            axis.line = element_line(color = "grey30", linetype = "solid"),
            axis.text = element_text(size = 12, face = "bold", colour = "grey30"),
            axis.ticks = element_line(color = "grey30", size = 0.60),
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
  
  # ------------------------->>>>> STATISTICS TAB <<<<<------------------------- #
  
  statistics_table <- eventReactive(c(input$add, input$remove), {
    data.frame(
      avg_mins_per_day <- round((sum(read.csv(path)[[4]])/(nrow(read.csv(path)))), 2),
      avg_hours_per_week <- round((sum(read.csv(path)[[4]])/((nrow(read.csv(path)))/7))/60, 2),
      rest_days <- length(which(read.csv(path)[[4]] == 0)),
      rest_days_percent <- round((length(grep("Rest", read.csv(path)$Workout))*100/nrow(read.csv(path))), 1)
    )
  })
  
  
  output$stats <- DT::renderDT({
    datatable(statistics_table(),
              rownames = FALSE,
              colnames = c("Avg time per day (mins)",
                           "Avg time per week (hours)",
                           "Total rest days",
                           "Proportion of rest days (%)"),
              options = list(
                dom = "t",
                bSort = FALSE,
                columnDefs = list(list(className = "dt-center",
                                       targets = c(0, 1, 2, 3)))
              )
    )
  })
  
  # ---------->>>>> pie_chart_num <<<<<---------- #
  
  pie_chart_num <- eventReactive(c(input$add, input$remove), {
    
    chest <- data.frame(read.csv(path)[c(grep("Chest", (read.csv(path))$Workout)), ])
    back <- data.frame(read.csv(path)[c(grep("Back", (read.csv(path))$Workout)), ])
    legs <- data.frame(read.csv(path)[c(grep("Legs", (read.csv(path))$Workout)), ])
    compound <- data.frame(read.csv(path)[c(grep("Compound", (read.csv(path))$Workout)), ])
    rest <- data.frame(read.csv(path)[c(grep("Rest", (read.csv(path))$Workout)), ])
    
    pie1 <- data.frame(
      Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
      num = c(nrow(chest), nrow(back), nrow(legs), nrow(compound),
              nrow(read.csv(run)),
              (nrow(read.csv(path)))-nrow(chest)-nrow(back)-nrow(legs)-nrow(compound)-nrow(read.csv(run))-nrow(rest))
    )
    
    Key_1 <- paste0(c(nrow(chest), nrow(back), nrow(legs), nrow(compound),
                      nrow(read.csv(run)),
                      nrow(read.csv(path))-nrow(chest)-nrow(back)-nrow(legs)-nrow(compound)-nrow(read.csv(run))-nrow(rest)),
                    " ",
                    c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
                    " (",
                    c(round((nrow(chest)*100/(nrow(read.csv(path)) - nrow(rest))), 1),
                      round((nrow(back)*100/(nrow(read.csv(path)) - nrow(rest))), 1),
                      round((nrow(legs)*100/(nrow(read.csv(path)) - nrow(rest))), 1),
                      round((nrow(compound)*100/(nrow(read.csv(path)) - nrow(rest))), 1),
                      round((nrow(read.csv(run))*100/(nrow(read.csv(path)) - nrow(rest))), 1),
                      round(((nrow(read.csv(path)))-nrow(chest)-nrow(back)-nrow(legs)-nrow(compound)-nrow(read.csv(run))-nrow(rest))*100/(nrow(read.csv(path)) - nrow(rest)), 1)),
                    "%)")
    
    ggplot(pie1, aes(x = "", y = num, fill = Key_1)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y", start = 0) +
      scale_fill_manual(values = c("navy", "indianred", "lavender", "gold",
                                   "mistyrose", "mediumturquoise")) +
      labs(title = "Number of workouts by type") +
      theme_void() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold")
      )
    
  }) # close pie_chart_num
  
  output$piechartnum <- renderPlot({
    pie_chart_num()
  })
  
  # ---------->>>>> pie_chart_time <<<<<---------- #
  
  pie_chart_time <- eventReactive(c(input$add, input$remove), {
    
    chest <- data.frame(read.csv(path)[c(grep("Chest", (read.csv(path))$Workout)), ])
    back <- data.frame(read.csv(path)[c(grep("Back", (read.csv(path))$Workout)), ])
    legs <- data.frame(read.csv(path)[c(grep("Legs", (read.csv(path))$Workout)), ])
    compound <- data.frame(read.csv(path)[c(grep("Compound", (read.csv(path))$Workout)), ])
    rest <- data.frame(read.csv(path)[c(grep("Rest", (read.csv(path))$Workout)), ])
    
    pie2 <- data.frame(
      Workout = c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
      time = c(sum(chest$Duration), sum(back$Duration), sum(legs$Duration),
               sum(compound$Duration), sum(read.csv(run)$Duration), 
               (sum(read.csv(path)$Duration)-sum(chest$Duration)-sum(back$Duration)-sum(legs$Duration)-sum(compound$Duration)-sum(read.csv(run)$Duration)))
    )
    
    Key_2 <- paste0(c("Chest", "Back", "Legs", "Compound", "Running", "Other"),
                    " (",
                    c(round((sum(chest$Duration)*100/sum(read.csv(path)$Duration)), 1),
                      round((sum(back$Duration)*100/sum(read.csv(path)$Duration)), 1),
                      round((sum(legs$Duration)*100/sum(read.csv(path)$Duration)), 1),
                      round((sum(compound$Duration)*100/sum(read.csv(path)$Duration)), 1),
                      round((sum(read.csv(run)$Duration)*100/sum(read.csv(path)$Duration)), 1),
                      round((sum(read.csv(path)$Duration)-sum(chest$Duration)-sum(back$Duration)-sum(legs$Duration)-sum(compound$Duration)-sum(read.csv(run)$Duration))*100/sum(read.csv(path)$Duration), 1)
                    ), "%)")
    
    ggplot(pie2, aes(x = "", y = time, fill = Key_2)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y", start = 0) +
      scale_fill_manual(values = c("navy", "indianred", "lavender", "gold",
                                   "mistyrose", "mediumturquoise")) +
      labs(title = "Percentage of workouts by duration") +
      theme_void() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold")
      )
    
  }) # close pie_chart_time
  
  output$piecharttime <- renderPlot({
    pie_chart_time()
  })
  
} # close server

# ---------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------- #

shinyApp(ui = ui,server = server, options = list(port = 6051))
