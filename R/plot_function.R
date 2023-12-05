#' Function to display shots of a game
#'
#' @param tble The dataframe
#' @param time A vector with begin and end time
#'
#' @return A soccer pitch with points represen by shots
#' @export
#'
#' @examples shot_pitch(df,c(0,90))
#' @examples shot_pitch(df,c(0,45))
#' @examples shot_pitch(df,c(45,90))
shot_pitch = function(tble,time){
  data = tble[with(tble,tble$type =="Shot"),]
  data = data %>% filter(minute >= time[1], minute <= time[2])
  x = sapply(data$location, `[`, 1)
  y = sapply(data$location, `[`, 2)
  ggplot(data) +
    annotate_pitch(colour = "white",
                   fill = "#3ab54a",
                   dimensions = pitch_statsbomb) +
    geom_point(aes(x = x, y = y, fill = possession_team),
               shape = 21,
               size = 4) +
    coord_cartesian(xlim = c(0, 120), ylim = c(0,80))+
    theme_pitch() +
    theme(panel.background = element_rect(fill = "#3ab54a"))
}
