#' Delete columns of dataframe according their names
#'
#' @param df The dataframe to update.
#' @param col_names A vector of string which contains the names of columns to drop
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' test <- jsonlite::fromJSON("../data-raw/15946.json")
#' remove_columns_by_name(test,c("id","index","related_events"))
supCol_by_names <- function(df, col_names) {
  if (is.data.frame(df)) {
    # Supprimer les colonnes par nom
    df <- df[, !(names(df) %like% col_names | names(df) %in% col_names)]

    # Parcourir les sous-dataframes récursivement
    for (col in names(df)) {
      if(class(df[[col]]) == "list"){
        df[[col]] <- supCol(df[[col]],col_names)
      }else{
        df[[col]] <- supCol_by_names(df[[col]], col_names)
      }
    }
  }
  return(df)
}

supCol <- function(df_list, col_names) {
  if (is.list(df_list) & !is.data.frame(df_list)) {
    for (i in seq_along(df_list)) {
      if(is.data.frame(df_list[[i]])){
        df_list[[i]] <- supCol(df_list[[i]], col_names)
      }
    }
  } else if (is.data.frame(df_list)) {
    for (coln in colnames(df_list)) {
      if(is.data.frame(df_list[[coln]])){
        df_list[[coln]] <- supCol(df_list[[coln]], col_names)
      }
      for(col_name in col_names){
        if (col_name %in% coln) {
          df_list[[coln]] <- NULL
        }
      }
    }
  }
  return(df_list)
}
