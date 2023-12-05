# Dans ce script, nous allons tenter de changer le type des varaibles en vu de faciliter leur analyse.
#' Change variable type in a nested dataframe
#'
#' @param col The columns to change of the nested dataframe
#' @param name_col A string of the name of the column to change
#'
#' @return The columns with a new type
#' @export
#'
#' @examples change_type(data[i],"time")
change_type <- function(col,name_col){
  type <- typeof(col)
  if(length(unique(col)) == length(col)){return(invisible(NULL))}
  if(type == "numeric" | type == "double"){return(invisible(NULL))}
  if(type == "integer"){
    if(length(unique(col)) > 10){col = as.numeric(col);return(col)}
    else{
      # Demander l'aide de l'utlisateur
      type = readline(paste0("Quel type souhaitez vous pour la variable ",name_col, " : "))
      # paste0("Quel type souhaitez vous pour la variable ",names(test[1]), "/n",
      #        "Sachant qu'elle contient : ", length(unique(test[1])),"modalités.", "/n",
      #        "Alors ? ")
      col = change_type_user(type,col)
      return(col)
    }
  }
  if(type == "character"){
    if(length(unique(col)) <= 50){col = as.factor(col); return(col)}
  }
  # Si nous sommes encore la, cela signifie que nous ne rentrons dans aucun des cas ci-dessus
  # Nous allons alors demander à l'tulisateur de trancher
  type = readline(paste0("Quel type souhaitez vous pour la variable ",name_col, " : "))
  col = change_type_user(type,col)
  return(col)
}

#' Change type according the input of the user
#'
#' @param type The type desired by the user
#' @param col The columns to apply the new type
#'
#' @return The same columns with a new type
#' @export
#'
#' @examples change_type_ser("int",data[i])
#' @examples change_type_ser("date",data[i])
change_type_user = function(type,col){
    col = switch(type,
             "int" = as.integer(col),
             "chr" = as.character(col),
             "factor" = as.factor(col),
             "date" = as.Date(col),
             "hms" = as_hms(col),
             "double" = as.double(col),
             "num" = as.numeric(col),
             "date-heure" = as.POSIXct.Date(col))
    return(col)
}

#' Function to range a list
#'
#' @param l A list
#'
#' @return The same list with type changed
#' @export
#'
#' @examples range_list(l)
range_list <- function(l){
  for(i in seq_along(l)){
    if(is.data.frame(l[[i]])){l[[i]] <- change_type_df(l[[i]])}
    else if(is.list(l[[i]])){l[[i]] <- range_list(l[[i]])}
    else{l[[i]] <- change_type(l[[i]])}
  }
  return(l)
}

#' Change the type of all columns in the nested dataframe
#'
#' @param df Nested dataframe to change variables types
#'
#' @return Nested dataframe with types variables changed
#' @export
#'
#' @examples change_type_df(data)
change_type_df <- function(df){
  for(i in 1:ncol(df)){
    # Il y a tout d'abord 2 conditions à respecter
    if(is.data.frame(df[[i]])){df[[i]] <- change_type_df(df[[i]])}
    else if(is.list(df[[i]])){df[[i]] <- range_list(df[[i]])}
    else{
      df[,i] <- change_type(df[[i]],names(df[i]))
    }
  }
  return(df)
}


