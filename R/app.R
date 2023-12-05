#' My shiny app
#' @export
app<-function(...){

  library(shiny)
  library(shinythemes)
  library(DT)
  library(tidyverse)
  library(ggsoccer)
  library(data.table)
  library(BasketballAnalyzeR)
  library(gganimate)
  library(ggrepel)

  latest <- "Pass"
  data("competition")
  data("df1")
  data("df2")
  data("df3")
  data("df4")
  data("df5")
  # for(i in 1:50){
  #   data(i)
  # }
  # do.call('use_data', list(as.name(name), overwrite = TRUE))
  # new_names_files <- updateNameFiles(paste0("df",1:3199),"team")

  ui <- navbarPage(theme = shinytheme('flatly'),id = "foot",
                   tags$div(tags$img(src="StatsBomb_R_Hex.svg", width = 108, height = 108,
                                     style="float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px")),
                   tabPanel("Visualisation",
                            fluidRow(
                              column(9,wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",plotOutput("plot1", height = 500, click="plot_click1"))),
                              column(3,
                                     fluidRow(column(12,
                                                     # Error Message Appearance
                                                     tags$head(tags$style(HTML(".shiny-output-error-validation { font-style: italic; font-size: 125%; }"))),
                                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 250px;",
                                                               p(tags$em("Cliquer sur un point de la figure pour obtenir des informations.", style = "font-size: 70%; font-family:Helvetica; color:#4c4c4c"),
                                                                 style = "text-align:left; margin-bottom: 15px;"),
                                                               htmlOutput("par_plr_click")))),
                                     fluidRow(column(12,
                                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                                               selectInput("dataset", label = "Choix du match", choices = paste0("df",1:5) ,width = '100%'),
                                                               # fileInput(input$dataset, "salut", accept = ".rda"),
                                                               p(tags$b("Minutes :", style = "font-size: 102%")),
                                                               fluidRow(column(5, align = "center",
                                                                               selectInput(inputId = "debut",
                                                                                           label = NULL,
                                                                                           choices = seq(0, 90, 1),
                                                                                           selected = 0)),
                                                                        column(2, style = "margin-top: 7px", align = "center", p("à")),
                                                                        column(5, align = "center",
                                                                               selectInput(inputId = "fin",
                                                                                           label = NULL,
                                                                                           choices = seq(0, 90, 1),
                                                                                           selected = 90))),
                                                               radioButtons("periode", "Période du match :",
                                                                            choices = c("Tout le match","1ère période", "2nd période"),
                                                                            selected = "Tout le match"),
                                                               selectInput(inputId = "choix_plot",
                                                                           label = "Choix du graphique",
                                                                           choices = c("Tirs par équipe"),
                                                                           selected = 0))


                                     ))
                              ))),
                   tabPanel("Data Events",
                            # sidebarPanel(
                            #   # selectInput("column_count", "Number of Columns", 2:4, 2),
                            #   # submitButton("Go")),)
                            fluidRow(
                              column(9,wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                                 # selectInput(inputId = "events",
                                                 #             label = NULL,
                                                 #             choices = sort(unique(tbl["type"])),
                                                 #             selected = latest),
                                                 DTOutput("table"))),
                              column(3,
                                     fluidRow(column(12,
                                                     # tags$head(tags$style(HTML(".shiny-output-error-validation { font-style: italic; font-size: 125%; }"))),
                                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 250px;",
                                                               p(tags$em("Choisissez un jeu de données pour avoir plus d'informations.", style = "font-size: 100%; font-family:Helvetica; color:#4c4c4c"),
                                                                 style = "text-align:left; margin-bottom: 15px;"),selectInput("dataset2", label = "Dataset", choices = paste0("df",1:5) ,width = '100%'))), # new_names_files #`data/events`
                                              fluidRow(column(12,
                                                              wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 450px; margin-left: 12px; margin-right: 12px;",
                                                                        p(tags$b("Nombre de lignes:", style = "font-size: 102%")),
                                                                        p(tags$b("Nombre de colonnes:", style = "font-size: 102%")),
                                                                        htmlOutput("nrow_col")

                                                              )))

                                     ))
                            )
                   ),
                   tabPanel("Competitions",
                            DTOutput("comp")
                   )
  )
  server <- function(input, output, session) {

    # # yo <- `data/events`[which(new_names_files == input$dataset)[1]]
    tbl <- reactive({
      supCol_by_names(get(input$dataset2),c("id","index","related_events")) # supCol_by_names(get(input$dataset),c("id","index","related_events"))
    #   data = remove_columns_by_name(data,c("id","index","related_events"))
    })
    tbl2 <- renderDT(datatable(tbl()))
    # tbl2 <- remove_columns_by_name(jsonlite::fromJSON(paste0("../Code/",deparse(substitute(data/events)),"/",input$dataset)),c("id","index","related_events"))
    #
    output$table <- renderDT(datatable(tbl(), # remove_columns_by_name(jsonlite::fromJSON(paste0("../Code/",deparse(substitute(data/events)),"/",input$dataset)),c("id","index","related_events"))
                                       filter = "top",
                                       options = list(
                                         language = list(search = 'Filter:'),
                                         list(lengthChange = T),
                                         info = F,
                                         paging = F,
                                         searching = T,
                                         stripeClasses = F,
                                         lengthChange = F,
                                         scrollY = "505px",
                                         scrollCollapse = T,
                                         columnDefs = list(list(targets = 8, visible = F))
                                       )
    )

    )
    # Graphique onglet Vizulatisation
    # output$plot1 <- renderPlot({
    #   data = supCol_by_names(get(input$dataset),c("id","index","related_events"))
    #   # shot_pitch(tbl(),c(input$debut,input$fin))
    #   if(input$periode == "Tout le match"){
    #     shot_pitch(data,c(input$debut,input$fin))}
    #   else if(input$periode == "1ère période"){
    #     shot_pitch(data,c(0,45))}
    #   else{shot_pitch(data,c(45,90))}
    # })

    output$plot1 <- renderPlot({
      data = supCol_by_names(get(input$dataset),c("id","index","related_events"))
      radar_plot(data)
    })


    output$par_plr_click <- renderPrint({input$plot_click1})# renderPrint({input$plot_click1}) # input$plot_click1

    output$comp <- renderDT(competition)

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server,...)
}
