moduleSticker <- function(moduleTable = NULL, moduleName, 
                          directory = getwd(), 
                          useCache = NULL, savedSticker = NULL,
                          ...){
  library("hexSticker")
  library("showtext")
  library("sysfonts")
  
  if (is.null(moduleName))
    stop("You have to specify the name of the module you want to generate the sticker for.
         If you don't know the available modules, please call `modulesAvailable()`")
  
  if (is.null(moduleTable))
    moduleTable <- prepInputs(targetFile = "moduleTable.csv",
                              url = "https://github.com/tati-micheletti/host/raw/master/stickers/moduleTable.csv", 
                              fun = "data.table::fread", useCache = useCache)
  moduleTable <- moduleTable[module == moduleName, ]
  font_add_google("Exo")
  
  ## Automatically use showtext to render text for future devices
  showtext_auto()
  
  imageURL <- paste0("https://github.com/tati-micheletti/host/raw/master/images/", 
                     moduleTable[parameter == "imageName", value], ".png")
  if (is.null(savedSticker)){
    savedSticker <- file.path(directory, paste0("stickers/", 
                                                moduleTable[parameter == "imageName", value],
                                                "_hex.png")) 
  } else {
    savedSticker <- file.path(directory, paste0("stickers/", savedSticker,"_hex.png"))
  }
  
  packageName <- ifelse(moduleName == "fireSense + SCFM", 
                        "fireSense\n+ SCFM", 
                        ifelse(moduleName == "caribouLambda", 
                               "caribouL",
                               unique(moduleTable[, module])))
  
  sticker(imageURL, package = packageName,
          h_color = moduleTable[parameter == "h_color", value], 
          h_fill = moduleTable[parameter == "h_fill", value],
          p_color = "white", 
          p_family = "Exo", 
          p_size = as.numeric(moduleTable[parameter == "p_size", value]), 
          p_x = as.numeric(moduleTable[parameter == "p_x", value]), 
          p_y = as.numeric(moduleTable[parameter == "p_y", value]),
          s_x = as.numeric(moduleTable[parameter == "s_x", value]), 
          s_y = as.numeric(moduleTable[parameter == "s_y", value]), 
          s_width = as.numeric(moduleTable[parameter == "s_width", value]), 
          s_height = as.numeric(moduleTable[parameter == "s_height", value]),
          filename = savedSticker, 
          fontface = "bold", 
          dpi = 600,
          lineheight = 0.13
          )
  return(message("Your sticker has been saved: ", savedSticker))
}
