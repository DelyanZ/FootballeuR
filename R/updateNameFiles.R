#' Update names of few dataframes
#'
#' @param DFs A list of dataframes
#' @param colonne A columns of the dataframe
#'
#' @return A list of the names of the dataframes
#' @export
#'
#' @examples updateNameFiles(`../data`,"team")
updateNameFiles = function(DFs,col){
  names_files = list()
  for(df in DFs){
    names_files = append(names_files,paste0(unique(df[col])[1]," vs ",unique(df[col])[2]))
    return(names_files)
  }
}
