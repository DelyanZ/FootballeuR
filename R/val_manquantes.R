#' Count all missing data in a list
#'
#' @param l A list
#'
#' @return A frequency
#' @export
#'
#' @examples missing_data_list(myList)
missing_data_list = function(l){
  compteur = 0
  for(i in seq_along(l)){
    if(is.na(l[i])){compteur = compteur + 1}
  }
  return(compteur/length(l))
}

#' Count missing data by columns
#'
#' @param df A dataframe
#'
#' @return Nothing
#' @export
#'
#' @examples missing_data(myDF)
missing_data = function(df){
  for(i in 1:ncol(df)){
    if(is.data.frame(df[[i]])){missing_data(df[[i]])}
    else if(is.list(df[[i]])){pourcentage <- missing_data_list(df[[i]])}
    else{
      pourcentage = sum(is.na(df[[i]]))/nrow(df)
    }
    print(names(df[i]))
    print(pourcentage)
  }
}



