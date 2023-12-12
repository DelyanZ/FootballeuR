#' Display lineup on a soccer pitch
#'
#' @param df A dataframe
#'
#' @return The lineups
#' @export
#'
#' @examples get_lineups(df)
get_lineups = function(df){
  data_lineup = df$tactics[1:2,]
  domicile = df$tactics$lineup[[1]]
  exterieur = df$tactics$lineup[[2]]
  domicile = position_player(data_lineup[1,1], domicile, 1)
  exterieur = position_player(data_lineup[2,1], exterieur, 2)
  plot1 <- ggplot() +
    annotate_pitch(colour = "white",
                   fill = "#3ab54a",
                   dimensions = pitch_statsbomb) +
    coord_cartesian(xlim = c(0, 120), ylim = c(0,80))+
    theme_pitch() +
    theme(panel.background = element_rect(fill = "#3ab54a")) +
    geom_image(data=rbind(domicile[,3:5],exterieur[,3:5]),
               aes(x=x,y=y,image=c(rep("inst/assets/jersey.png",11),rep("inst/assets/jersey2.png",11))), size=.1) +
    geom_text(
      data = rbind(domicile[,3:5],exterieur[,3:5]),
      aes(x = x, y = y, label = jersey_number),
      size = 3.5,
      vjust = 0.25,
      hjust = 0.5
    ) +
    labs(title=paste0("Composition d'équipe : ",df$team[1], " VS ", df$team[2]),
         subtitle="La Liga 2017/2018",
         caption="Data Source: StatsBomb")
  plot1
}

#' Get the coordinates of players according the formation
#'
#' @param formation A string
#'
#' @return A dataframe with 2 columns x and y
#' @export
#'
#' @examples formation_coord("442")
formation_coord = function(formation){
  if(formation == "442"){
    x = c(3, 15, 15, 15, 15, 35, 35, 35, 35, 55, 55)
    y = c(40, 10, 30, 50, 70, 10, 30, 50, 70, 30, 50)
  }else if(formation == "451"){
    x = c(3, 20, 20, 20, 20, 30, 40, 40, 40, 40, 50)
    y = c(40, 70, 50, 30, 10, 40, 70, 50, 30, 10, 40)
  }else if(formation == "41212"){
    x = c(3, 15, 15, 15, 15, 25, 35, 35, 45, 55, 55)
    y = c(40, 10, 30, 50, 70, 40, 30, 50, 40, 30, 50)
  }else if(formation == "433"){
    x = c(3, 15, 15, 15, 15, 35, 35, 35, 50, 55, 50)
    y = c(40, 10, 30, 50, 70, 20, 40, 60, 10,70,40)
  }else if(formation == "352"){
    x = c(3, 15, 15, 15, 25, 25, 35, 45, 45, 55, 55)
    y = c(40, 20, 40, 60, 10, 70, 40, 30, 50, 30, 50)
  }
  return(cbind(x,y))
}

#' Get the data lineups and coordinates
#'
#' @param formation A string
#' @param data_lineup Data lineup
#' @param team_side A value (1 or 2)
#'
#' @return Dataframe with lineups and coordinates
#' @export
#'
#' @examples position_player("442",data_lineup,1)
position_player = function(formation, data_lineup,team_side){
  if(team_side == 1){
    data_lineup = cbind(data_lineup,formation_coord(formation))
  }else if(team_side == 2){
    coord = formation_coord(formation)
    coord[,1] = 120 - coord[,1]
    data_lineup = cbind(data_lineup,coord)
  }
  return(data_lineup)
}
