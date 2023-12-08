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
  library(plotly)
  library(ggimage)
  library(plotly)
  library(ggthemes)

  data("competition")
  names_df = c()
  for(df in paste0("df",1:5)){
    data(df)
    assign(df, supCol_by_names(get(df),c("id","index","related_events")))
    names_df = c(names_df,updateNameFiles(get(df),"team"))
  }

  # for(i in 1:50){
  #   data(i)
  # }
  # do.call('use_data', list(as.name(name), overwrite = TRUE))
  # new_names_files <- updateNameFiles(paste0("df",1:3199),"team")

  ui <- navbarPage(theme = shinytheme('flatly'),id = "foot",
                   tags$div(tags$img(src="assets/StatsBomb_R_Hex.svg", width = 108, height = 108,
                                     style="float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px")),
                   tabPanel("Match",
                            mainPanel(textOutput("texte_Match1")),
                            fluidRow(
                              column(9,wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                                 plotOutput("plot", height = 500),
                                                 DTOutput("table_lineup")),
                              ),
                              column(3,
                                     fluidRow(
                                       column(12,
                                              wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 250px;",
                                                        selectInput("dataset3",label = "Choix du match", choices = names_df ,width = '100%')
                                              )
                                        ),
                                       column(12,
                                              wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 450px;",
                                                        plotOutput("funnel",height = 200)

                            )))))),
                   tabPanel("Visualisation",
                            fluidRow(
                              column(9,wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",plotOutput("plot1", height = 500, click="plot_click1"))
                                     # conditionalPanel(
                                     #   condition = "input.choix_plot == 'Heatmap par joueur'",
                                     #   selectInput("liste_player", "Liste joueurs",
                                     #               choices = c(get(input$dataset)$tactics$lineup[[1]]$player$name,get(input$dataset)$tactics$lineup[[2]]$player$name)))
                                     ),
                              column(3,
                                     fluidRow(column(12,
                                                     tags$head(tags$style(HTML(".shiny-output-error-validation { font-style: italic; font-size: 125%; }"))),
                                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 250px;",
                                                               p(tags$em("Cliquer sur un point de la figure pour obtenir des informations.", style = "font-size: 70%; font-family:Helvetica; color:#4c4c4c"),
                                                                 style = "text-align:left; margin-bottom: 15px;"),
                                                               htmlOutput("par_plr_click")))),
                                     fluidRow(column(12,
                                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 450px;",
                                                               selectInput("dataset", label = "Choix du match", choices = paste0("df",1:5) ,width = '100%'),
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
                                                                           choices = c("Tirs par équipe","Contribution attaquants",
                                                                                       "Heatmap par joueur"),
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

    # Onglet Match
    output$test_Match1 <- renderText({
      "Match opposant Barcelone à Deportivo Alavés"
    })

    tbl <- reactive({
      data(input$dataset2)
      supCol_by_names(get(input$dataset2),c("id","index","related_events"))})

    output$table <- renderDT(datatable(tbl(),
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

    # output$plot1 <- renderPlot({
    #   data = supCol_by_names(get(input$dataset),c("id","index","related_events"))
    #   radar_plot(data)
    # })


    output$plot1 <- renderPlot({
      data(input$dataset)
      data = supCol_by_names(get(input$dataset),c("id","index","related_events"))
      if(input$choix_plot == "Contribution attaquants"){
        radar_plot(data)
      }else if(input$choix_plot == "Tirs par équipe"){
        if(input$periode == "Tout le match"){
          shot_pitch(data,c(input$debut,input$fin))}
        else if(input$periode == "1ère période"){
          shot_pitch(data,c(0,45))}
        else{shot_pitch(data,c(45,90))}
      }else if(input$choix_plot == "Heatmap par joueur"){
        heatmap_player(data,"Lionel Andrés Messi Cuccittini")
      }
    })


    output$par_plr_click <- renderPrint({input$plot_click1})# renderPrint({input$plot_click1}) # input$plot_click1

    output$comp <- renderDT(competition)
    # dom = cbind(get(input$dataset)$tactics$lineup[[1]][3],get(input$dataset)$tactics$lineup[[1]][1]$player$name),
    # colnames(dom) = c("Numéro","Joueur"),
    # ext = cbind(get(input$dataset)$tactics$lineup[[2]][3],get(input$dataset)$tactics$lineup[[2]][1]$player$name),
    # colnames(ext) = c("Numéro","Joueur"),

    tbl2 <- reactive({
      dom = cbind(get(paste0("df",which(names_df == input$dataset3)))$tactics$lineup[[1]][3],get(paste0("df",which(names_df == input$dataset3)))$tactics$lineup[[1]][1]$player$name)
    })
    # tbl3 <- reactive({
    #   ext = cbind(get(input$dataset)$tactics$lineup[[2]][3],get(input$dataset)$tactics$lineup[[2]][1]$player$name)
    # })
    output$table_lineup <- renderDT(datatable(tbl2(),
                                              caption = "Equipe domcile",
                                              ))

    # data_lineup <- reactive({
    #   data(input$data)
    #   supCol_by_names(get(input$data),c("id","index","related_events"))})

    output$plot <- renderPlot({
      data = supCol_by_names(get(paste0("df",which(names_df == input$dataset3))),c("id","index","related_events"))
      get_lineups(data)
      })

    output$funnel <- renderPlot({
      data = supCol_by_names(get(paste0("df",which(names_df == input$dataset3))),c("id","index","related_events"))
      funnel_plot(data)
      })

      # datatable(position_player(df$tactics[1,1],df$tactics$lineup[[1]],1), caption = paste0("Equipe",get(input$dataset)$team$name[1])))
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server,...)
}
