#' Update names of few dataframes
#'
#' @param DFs A dataframe
#' @param colonne A columns of the dataframe
#'
#' @return A name
#' @export
#'
#' @examples updateNameFiles(df1,"team")
updateNameFiles = function(df,col){
  return(paste0(unique(df[col])[[1]][1]," vs ",unique(df[col])[[1]][2]))
}
