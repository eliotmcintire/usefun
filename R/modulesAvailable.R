# hsxSticker helpers

modulesAvailable <- function(){
  library("reproducible")
  moduleTable <- suppressMessages(prepInputs(url = "https://github.com/tati-micheletti/host/raw/master/stickers/moduleTable.csv", 
                            fun = "data.table::fread", destinationPath = tempdir()))
  unique(moduleTable[,module])
}

