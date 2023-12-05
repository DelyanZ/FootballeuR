#' Radar plot on player contribution
#'
#' @param df A dataframe
#'
#' @return A radar plot
#' @export
#'
#' @examples radar_plot(myDF)
radar_plot <- function(df){

  library(BasketballAnalyzeR)

  ## XGA par attaquant
  attaquants = cbind(df[c("player","position")],df$shot$statsbomb_xg)
  attaquants["df$shot$statsbomb_xg"][is.na(attaquants["df$shot$statsbomb_xg"])] <- 0

  attaquants = attaquants %>%
    filter(position %like% "Forward" | position %like% "Wing") %>%
    select(player,`df$shot$statsbomb_xg`)

  attaquants <- aggregate(attaquants$`df$shot$statsbomb_xg`, by=list(attaquants$player), FUN=sum)

  ## Dribbles réussis par attaquant
  dribble = cbind(df[c("type","position","player")],df$pass$outcome)
  dribble = dribble %>% filter(type == "Dribble" & (position %like% "Forward" | position %like% "Wing") & dribble$`df$dribble$outcome` == "Complete")
  dribble = aggregate(dribble$`df$dribble$outcome`, by=list(dribble$player), FUN=length)


  ## Passes réussi par attaquant
  pas = cbind(df[c("type","position","player")],df$pass$outcome)
  pas = pas %>% filter(type =="Pass" & (position %like% "Forward" | position %like% "Wing") & is.na(pas$`df$pass$outcome`))
  pas = aggregate(pas$`df$pass$outcome`, by=list(pas$player), FUN=length)

  ## Tirs par attaquant

  tirs = df[c("type","position","player")]
  tirs = tirs %>% filter(type == "Shot" & (position %like% "Forward" | position %like% "Wing"))
  tirs = aggregate(tirs$player,by=list(tirs$player), FUN=length)

  ## Tirs cadres par attaquant
  but = cbind(df[c("type","position","player")],df$shot$outcome)
  but = but %>% filter(type == "Shot" & (position %like% "Forward" | position %like% "Wing") & (but$`df$shot$outcome` == "Blocked" | but$`df$shot$outcome` == "Saved"))
  but = aggregate(but$player,by=list(but$player), FUN=length)


  ## Passes clés par attaquants
  key_pass = cbind(df[c("type","position","player")],df$pass$shot_assist,df$pass$goal_assist)
  key_pass = key_pass %>% filter(type == "Pass" & (position %like% "Forward" | position %like% "Wing") & (key_pass$`df$pass$goal_assist` == T | key_pass$`df$pass$shot_assist` == T))
  key_pass = aggregate(key_pass$player,by=list(key_pass$player), FUN=length)

  ## Liste attaquants qui ont joué
  att = df[c("player", "position")]
  att = df %>%
    filter(position %like% "Forward" | position %like% "Wing") %>%
    select(player)
  att = data.frame(unique(att$player))
  colnames(att) = "Group.1"

  df2 <- merge(x=att,y=attaquants,
               by="Group.1", all.x=TRUE)
  df3 <- merge(x=df2,y=dribble,
               by="Group.1",all.x=TRUE)
  df4 <- merge(x=df3,y=pas,
               by="Group.1",all.x=TRUE)
  df5 <- merge(x=df4,y=tirs,
               by="Group.1",all.x=TRUE)
  df6 <- merge(x=df5,y=but,
               by="Group.1",all.x=TRUE)
  df7 <- merge(x=df6,y=key_pass,
               by="Group.1",all.x=TRUE)
  colnames(df7) = c("Joueurs", "xGA", "dribbles", "passes", "tirs", "but", "pcles")

  df7[is.na(df7)] <- 0

  df7[,-1] <- mutate_all(df7[,-1], function(x) as.numeric(as.character(x)))
  p <- radialprofile(data=df7[,-1], title=df7[,1], std=T)
}
