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
  library(emoji)
  library(shinyjs)

  data("competition")
  data("df1")
  data("df2")
  data("df3")
  data("df4")
  data("df5")
  names_df = c()
  for(df in paste0("df",1:5)){
    data(df)
    assign(df, supCol_by_names(get(df),c("id","index","related_events")))
    names_df = c(names_df,updateNameFiles(get(df),"team"))
  }

  ui <- navbarPage(theme = shinytheme('flatly'),id = "foot",
                   tags$div(tags$img(src="assets/StatsBomb_R_Hex.svg", width = 108, height = 108,
                                     style="float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px")),
                   tabPanel("Match",
                            fluidRow(
                              column(9,
                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                     fluidRow(column(12,p(tags$b("Composition des équipes", style = "font-size: 150%")),align = "center")),# fluidRow(column(12,textOutput("texte_Match1"),align = "center")),
                                     fluidRow(
                                       column(3,
                                              DTOutput("table_lineup")),
                                       column(6,
                                              plotOutput("plot", height = 500)),
                                       column(3,
                                              DTOutput("table_lineup2"))
                                     ))
                              ),
                              column(3,
                                     fluidRow(
                                       column(12,
                                              wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 250px;",
                                                        selectInput("dataset3",label = "Choix du match", choices = names_df ,width = '100%'),
                                                        fluidRow(column(12,p(tags$b("Score final :", style = "font-size: 102%")),
                                                        textOutput("score"))),
                                                        textOutput("buteursOutput")
                                              )
                                        ),
                                       column(12,
                                              wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 450px;",
                                                        fluidRow(column(12,p(tags$b("Statistiques du match", style = "font-size: 100%")))),
                                                        plotOutput("funnel",height = 300)

                            )))))),
                   tabPanel("Visualisation",
                            fluidRow(
                              column(9,wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                                 uiOutput("titre_plot_viz"),# fluidRow(column(12,p(tags$b(textOutput("titre_plot_viz"), style = "font-size: 150%")),align = "center")),
                                                 br(),
                                                 plotOutput("plot1", height = 500, click="plot_click1"))),
                              column(3,
                                     fluidRow(column(12,
                                                     tags$head(tags$style(HTML(".shiny-output-error-validation { font-style: italic; font-size: 125%; }"))),
                                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 250px;",
                                                               p(tags$em("Cliquer sur un point de la figure pour obtenir des informations.", style = "font-size: 70%; font-family:Helvetica; color:#4c4c4c"),
                                                                 style = "text-align:left; margin-bottom: 15px;"),
                                                               htmlOutput("par_plr_click")))),
                                     fluidRow(column(12,
                                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 450px;",
                                                               selectInput("dataset", label = "Choix du match", choices = names_df) ,width = '100%',
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
                                                                           selected = 0),
                                                               uiOutput("joueur")
                                                               # selectInput("joueurInput", "Sélectionnez un joueur"),choices = unique(df1$player$name)[-1],selected = unique(df1$player$name)[2])
                                     )))
                              ))),
                   tabPanel("Data Events",
                            fluidRow(
                              column(9,wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                                 # selectInput(inputId = "events",
                                                 #             label = NULL,
                                                 #             choices = sort(unique(tbl["type"])),
                                                 #             selected = latest),
                                                 uiOutput("colSelction"),
                                                 # selectInput("colSelected",label = "Selection colonnes", choices = names(df1), multiple = TRUE),
                                                 DTOutput("table"))),
                              column(3,
                                     fluidRow(column(12,
                                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 250px;",
                                                               p(tags$em("Choisissez un jeu de données pour avoir plus d'informations.", style = "font-size: 100%; font-family:Helvetica; color:#4c4c4c"),
                                                                 style = "text-align:left; margin-bottom: 15px;"),selectInput("dataset2", label = "Dataset", choices = names_df ,width = '100%'))), # new_names_files #`data/events`
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
                            fluidRow(
                            column(12,
                                   wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 800px;",
                                   DTOutput("comp")))
                   )
  ))
  server <- function(input, output, session) {

    # Onglet Match
    output$texte_Match1 <- renderText({
      match = input$dataset3
      match = paste0("<b style='font-size: 18px;'>",match,"</b>")
      HTML(match)
    })

    output$score <- renderText({
      df = supCol_by_names(get(paste0("df",which(names_df == input$dataset3))),c("id","index","related_events"))
      buteur = get_score(df)
      n_but1 = nrow(buteur[buteur$Equipe == df$team[1],])
      n_but2 = nrow(buteur[buteur$Equipe == df$team[2],])
      res = paste0(n_but1," - ",n_but2)
      return(res)
    })

    output$buteursOutput <- renderText({
      df = supCol_by_names(get(paste0("df",which(names_df == input$dataset3))),c("id","index","related_events"))
      buteur = get_score(df)
      n_but1 = nrow(buteur[buteur$Equipe == df$team[1]])
      n_but2 = nrow(buteur[buteur$Equipe == df$team[2]])
      buteurs_list <- lapply(1:nrow(buteur), function(i) {
        paste0("⚽"  ,buteur[i,3],
          " (", buteur[i,1], "')\n"
        )
      })
      buteurs_html <- paste(buteurs_list, collapse = "")
      return(buteurs_html)
      })

    # Onglet Vizualisation

    output$titre_plot_viz <- renderUI({
      if(input$choix_plot == "Tirs par équipe"){
        fluidRow(column(12,p(tags$b("Répartition des tirs par équipe", style = "font-size: 150%")),align = "center"))
      }else if(input$choix_plot == "Contribution attaquants"){
        fluidRow(column(12,p(tags$b("Contribution des attaquants du match", style = "font-size: 150%")),align = "center"))
      }else if(input$choix_plot == "Heatmap par joueur"){
        fluidRow(column(12,p(tags$b("Zones d'activités du joueur dans le match", style = "font-size: 150%")),align = "center"))
      }
    })

    output$par_plr_click <- renderPrint({

      plr_info_str <- function(click_df) {

        details <- if(nrow(click_df) == 0) {
          p(tags$b("Minute: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), br(),
            tags$b("Joueur: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), br(),
            tags$b("Équipe: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), br(),
            tags$b("Issue du tir: ", style = "font-size: 85%; font-family:Helvetica; color:#000000")
            )
          }
        else if(nrow(click_df) >= 1) {
          p(tags$b("Minute: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), tags$em(click_df$Minute), br(),
            tags$b("Joueur: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), tags$em(click_df$Joueur), br(),
            tags$b("Équipe: ", style = "font-size: 108%; font-family:Helvetica; color:#000000"), tags$em(click_df$Equipe), br(),
            tags$b("Issue du tir: ", style = "font-size: 85%; font-family:Helvetica; color:#000000"), tags$em(click_df$Issue)
            )
          }
        else { NA }

        return(details)
      }

      df = supCol_by_names(get(paste0("df",which(names_df == input$dataset))),c("id","index","related_events"))
      df = df %>%
            filter(type == "Shot")
      x = sapply(df$location, `[`, 1)
      y = sapply(df$location, `[`, 2)
      df = cbind(df[,c("minute","player","team")],df$shot$outcome,x,y)
      colnames(df) = c("Minute","Joueur","Equipe","Issue","x_coord","y_coord")
      plr_info_str(nearPoints(df,
                              coordinfo = input$plot_click1,
                              xvar = "x_coord",
                              yvar = "y_coord",
                              threshold = 3,
                              maxpoints = 3,
                              addDist = T) %>%
                     arrange(dist_) %>%
                     slice(1))
    })

    tbl <- reactive({
      # df = supCol_by_names(get(input$dataset2),c("id","index","related_events"))
      # colSelected <- input$colSelected
      # df[-c(1:2),c("period","timestamp","type")]
      df = supCol_by_names(get(paste0("df",which(names_df == input$dataset2))),c("id","index","related_events"))
      df[-c(1:2),]
      # [-c(1:2),c("timestamp","type","team","duration","player","location")]
      })

    output$table <- renderDT({
      # data(input$dataset2)
      # df = supCol_by_names(get(input$dataset2),c("id","index","related_events"))
      # colSelected <- input$colSelected
      # cat("Colonnes sélectionnées :", colSelected, "\n")  # Message de débogage
      # df = df[-c(1:2),c("period","timestamp","type",colSelected)]
      datatable(tbl(),
          filter = "none",
          rownames = FALSE,
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
            columnDefs = list(list(targets = 11, visible = F))))})

    output$joueur <- renderUI({
      dom = cbind(get(paste0("df",which(names_df == input$dataset)))$tactics$lineup[[1]][3],
                  get(paste0("df",which(names_df == input$dataset)))$tactics$lineup[[1]][1]$player$name)
      ext = cbind(get(paste0("df",which(names_df == input$dataset)))$tactics$lineup[[2]][3],
                  get(paste0("df",which(names_df == input$dataset)))$tactics$lineup[[2]][1]$player$name)
      data = cbind(dom,ext)
      colnames(data) = c("Numéros", "Joueurs")
      selectInput("Joueur", "Choix du joueur", choices = data$Joueurs)# unique(data$player)[-1])
    })

    output$plot1 <- renderPlot({
      data = supCol_by_names(get(paste0("df",which(names_df == input$dataset))),c("id","index","related_events"))
      if(input$choix_plot == "Contribution attaquants"){
        radar_plot(data)
      }else if(input$choix_plot == "Tirs par équipe"){
        if(input$periode == "Tout le match"){
          shot_pitch(data,c(input$debut,input$fin))}
        else if(input$periode == "1ère période"){
          shot_pitch(data,c(0,45))}
        else{shot_pitch(data,c(45,90))}
      }else if(input$choix_plot == "Heatmap par joueur"){
        # updateSelectInput(session, "joueurInput", choices = unique(data$player)[-1],selected = unique(data$player)[2])
        heatmap_player(data,input$Joueur) # ,"Lionel Andrés Messi Cuccittini")
      }
    })


    output$comp <- renderDT(
      datatable(competition,
                rownames = FALSE,
                filter = "none",
                options = list(
                  list(lengthChange = T),
                  info = F,
                  paging = F,
                  searching = F,
                  stripeClasses = F,
                  lengthChange = F,
                  scrollY = "505px",
                  scrollCollapse = T,
                  columnDefs = list(list(targets = 11, visible = F)))))

    tbl2 <- reactive({
      dom = cbind(get(paste0("df",which(names_df == input$dataset3)))$tactics$lineup[[1]][3],get(paste0("df",which(names_df == input$dataset3)))$tactics$lineup[[1]][1]$player$name)
      colnames(dom) = c("Numéros", "Joueurs")
      dom
      })

    tbl3 <- reactive({
      ext = cbind(get(paste0("df",which(names_df == input$dataset3)))$tactics$lineup[[2]][3],get(paste0("df",which(names_df == input$dataset3)))$tactics$lineup[[2]][1]$player$name)
      colnames(ext) = c("Numéros", "Joueurs")
      ext
      })
    output$table_lineup <- renderDT(datatable(tbl2(),
                                              caption = "Equipe domcile",
                                              rownames = FALSE,
                                              options = list(
                                                pageLength = 11,
                                                stripe = FALSE,
                                                dom = 't'),
                                              callback = JS('table.on("init.dt", function() {
                                                $(".dataTables_wrapper").css({
                                                  "border": "4px solid #000000",
                                                  "font-family": "Arial, sans-serif",
                                                  "font-size": "12px",
                                                  "font-weight": "bold",
                                                  "color": "#000000",
                                                  "text-align": "center"
                                                });
                                              })')
                                              ))

    output$table_lineup2 <- renderDT(datatable(tbl3(),
                                              caption = "Equipe extérieur",
                                              rownames = FALSE,
                                              options = list(
                                                pageLength = 11,
                                                stripe = FALSE,
                                                dom = 't'),
                                              callback = JS('table.on("init.dt", function() {
                                                $(".dataTables_wrapper").css({
                                                  "border": "4px solid #000000",
                                                  "font-family": "Arial, sans-serif",
                                                  "font-size": "12px",
                                                  "font-weight": "bold",
                                                  "color": "#000000",
                                                  "text-align": "center"
                                                });
                                              })')
    ))

    output$plot <- renderPlot({
      data = supCol_by_names(get(paste0("df",which(names_df == input$dataset3))),c("id","index","related_events"))
      get_lineups(data)
      })

    output$funnel <- renderPlot({
      data = supCol_by_names(get(paste0("df",which(names_df == input$dataset3))),c("id","index","related_events"))
      funnel_plot(data)
      })

    output$ColSelection <- renderUI({
      data = supCol_by_names(get(paste0("df",which(names_df == input$dataset2))),c("id","index","related_events"))
      selectInput("multiSelection", "Selection colonnes", choices = colnames(data), multiple = TRUE)
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server,...)
}
