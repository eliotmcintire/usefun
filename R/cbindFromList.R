#' @title cbindFromList: column binds a list of data frames or data.tables, returning a merged table with unique columns
#'
#' @param lst List of data.tables or data.frames to column bind
#'
#' @importFrom data.table data.table
#'
#' @return Returns a merged data.table
#' @export
#' @author Tati Micheletti
#'
#' @rdname cbindFromList

cbindFromList <- function(lst){
  bindedList <- do.call(cbind, args = lst)
  bindedList <- data.table::data.table(bindedList)
  bindedList[, which(duplicated(names(bindedList))) := NULL]
  return(bindedList)
}
