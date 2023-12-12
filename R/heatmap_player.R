#' Heatmap on soccer pitch to describe activity by player
#'
#' @param df A Dataframe
#' @param name_player The name of the player among all present in the game
#'
#' @return An heatmap on a soccer pitch
#' @export
#'
#' @examples heatmap_player(df,"Lionel Andrés Messi Cuccittini")
heatmap_player = function(df,name_player){
  player_data = df %>%
    filter(player == name_player)
  x = sapply(player_data$location, `[`, 1)
  y = sapply(player_data$location, `[`, 2)

  ggplot(player_data) +
    annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
    geom_density2d_filled(aes(x, y), alpha=0.4, contour_var='ndensity') +
    scale_x_continuous(c(0, 120)) +
    scale_y_continuous(c(0, 80)) +
    labs(title=paste0("Activité de ",name_player),
         subtitle= paste(df$team[1]," vs ",df$team[2])) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill='#021e3f', color='#021e3f'),
      panel.background = element_rect(fill='#021e3f', color='#021e3f'),
      plot.title = element_text(hjust=0.5, vjust=0, size=14),
      plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
      text = element_text(family="Geneva", color='white'),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "none"
  )
}
