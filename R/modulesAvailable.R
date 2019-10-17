# hexSticker helpers
#' Function to show the modules that have stickers available
#'
#' @return This function returns a vector of all modules that have stickers available
#'
#' @author Tati Micheletti
#' @export
#' @importFrom data.table data.table
#' @importFrom reproducible prepInputs
#'
#' @rdname modulesAvailable

modulesAvailable <- function(){
  moduleTable <- suppressMessages(prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/stickers/moduleTable.csv",
                            fun = "data.table::fread", destinationPath = tempdir()))
  unique(moduleTable[,module])
}

