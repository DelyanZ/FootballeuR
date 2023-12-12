#' Get a dataframe of all scoreurs of a single game
#'
#' @param df A dataframe
#'
#' @return A dataframe
#' @export
#'
#' @examples get_score(df)
get_score = function(df){
  but = cbind(df[c("minute","team","player")],df$shot$outcome)
  but = but %>% filter(but$`df$shot$outcome` == "Goal")
  but = but[,-4]
  colnames(but) = c("Temps","Equipe","Joueur")
  return(but)
}
