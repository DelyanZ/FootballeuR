#' Funnel plot
#'
#' @param df A dataframe
#'
#' @return The funnel plot
#' @export
#'
#' @examples funnel_plot(df)
funnel_plot = function(df){

  # Récuperer la possessions par équipe
  possessions = df %>%
    filter(type == "Carry" | type == "Pass") %>%
    select(team,duration) %>%
    group_by(team) %>%
    summarise(total = sum(duration))
  possessions$nb = (possessions$total/sum(possessions$total)) *100
  possessions = possessions[,-2]

  # Récupérer les tirs par équipe
  tir = df %>%
    filter(type == "Shot") %>%
    select(team) %>%
    group_by(team) %>%
    summarise(nb = length(team))

  # Récupérer les tirs cadrés par équipe
  tir_cadres = df %>%
    filter(type == "Shot")
  tir_cadres = aggregate(tir_cadres$team,by=list(tir_cadres$team,tir_cadres$shot$outcome), FUN=length)
  tir_cadres = tir_cadres %>%
    filter(Group.2 == "Saved" | Group.2 == "Goal")
  tir_cadres = tir_cadres[,-2]
  tir_cadres = aggregate(tir_cadres$x,by=list(tir_cadres$Group.1), FUN=sum)
  tir_cadres = check_missing(tir_cadres[,1],df,tir_cadres)
  colnames(tir_cadres) = c("team", "nb")

  # Récupérer les tirs non-cadrés par équipe
  tir_n_cadres = data.frame(tir$team,(tir$nb-tir_cadres$nb))
  tir_n_cadres = check_missing(tir_n_cadres[,1],df,tir_n_cadres)
  colnames(tir_n_cadres) = c("team","nb")

  # Récupérer les tirs bloqués par équipe
  tir_bloque = df %>%
    filter(type == "Shot")
  tir_bloque = aggregate(tir_bloque$team,by=list(tir_bloque$team,tir_bloque$shot$outcome), FUN=length)
  tir_bloque = tir_bloque %>%
    filter(Group.2 == "Blocked")
  tir_bloque = tir_bloque[,-2]
  tir_bloque = check_missing(tir_bloque[,1],df,tir_bloque)
  colnames(tir_bloque) = c("team", "nb")

  # Récupérer les coups francs par équipe
  cf = df %>%
    filter(type == "Shot" | type == "Pass")
  cf = cf[which(cf$shot$type =="Free Kick" | cf$pass$type == "Free Kick"),"team"]
  cf = aggregate(cf,by=list(cf), FUN=length)
  cf = check_missing(cf[,1],df,cf)
  colnames(cf) = c("team", "nb")

  # Récupérer les corners par équipe
  corner = df[which(df$pass$type == "Corner"),"team"]
  corner = aggregate(corner,by=list(corner), FUN=length)
  hors_jeu = check_missing(corner[,1],df,corner)
  colnames(corner) = c("team", "nb")

  # Récupérer les hors-jeu par équipe
  hors_jeu = df[which(df$pass$outcome == "Pass Offside"),"team"]
  hors_jeu = aggregate(hors_jeu,by=list(hors_jeu), FUN=length)
  hors_jeu = check_missing(hors_jeu[,1],df,hors_jeu)
  colnames(hors_jeu) = c("team", "nb")

  # Récupérer les sauvetages du gardien par équipe
  GK_save = df[which(df$shot$outcome == "Saved"),"team"]
  GK_save = aggregate(GK_save,by=list(GK_save), FUN=length)
  GK_save = check_missing(GK_save$Group.1,df,GK_save)
  Gk_save = check_missing(GK_save[,1],df,GK_save)
  colnames(GK_save) = c("team", "nb")

  # Contruction du dataframe utilisé pour contruire le graphique
  label = c(rep("Tirs au but",2),rep("Tirs cadrés",2),rep("Tirs non cadrés",2),
            rep("Tirs bloqués",2),rep("Coup Francs",2),rep("Corners",2),rep("Hors-Jeu",2),rep("Sauvetages du gardien",2))
  funnel = rbind(tir,tir_cadres,tir_n_cadres,tir_bloque,cf,corner,hors_jeu,GK_save)
  funnel = cbind(funnel,label)

  funnel$nb = funnel$nb * 10^5
  funnel[funnel$team == funnel$team[1],]["nb"] = -funnel[funnel$team == funnel$team[1],]["nb"]

  # Construction du graphique
  brks <- seq(-10000000, 10000000, 2500000)
  lbls = as.character(c(seq(100, 0, -25), seq(25, 100, 25)))
  couleurs_team = c("Barcelona" = "blue1", "Deportivo Alavés" = "red")

  ggplot(funnel, aes(x = label, y = nb, fill = team)) +
    geom_bar(stat = "identity", width = .6) +
    scale_y_continuous(breaks = brks,
                       labels = lbls) +
    labs(title = "",x="",y="") +
    coord_flip() +  # Flip axes
    theme_tufte() +
    theme(
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 12)
      ) +
    scale_fill_manual(values = couleurs_team)
}

#' Check is NA and replace by 0
#'
#' @param colonne A column
#' @param data A dataframe
#' @param data2 An other dataframe
#'
#' @return A dataframe
#' @export
#'
#' @examples check_missing(myColumns,myDF,myDF2)
check_missing = function(colonne, data,data2){
  if(length(unique(colonne)) == 1){
    if(data$team[1] == colonne){
      data2 = rbind(data2, c(data$team[2],0))
    }else
      data2 = rbind(data2, c(data$team[1],0))
  }
  data2[,2] = as.integer(data2[,2])
  return(data2)
}
