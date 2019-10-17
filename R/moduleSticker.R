# hexSticker helpers
#' Function to create stikers for SpaDES modules
#'
#' @param moduleTable Table with the information to generate the sticker (i.e. figure name, height, colors, etc)
#' @param moduleName charcter string of the module's name.
#' @param directory charachter string of the directory where the sticker should be saved
#' @param useCache logical. Should the process of downloading and loading the table be cached?
#' @param savedSticker Character string. Name of the file to e saved. If not provided,
#'                     saves with the name of the figure. Useful if more than one stickers
#'                     have the same figure
#' @param ... Other parameters for ggplot2 (i.e. fontface, lineheight)
#'
#' @return This function returns the location where the sticker is available
#'
#' @author Tati Micheletti
#' @export
#' @importFrom hexSticker sticker
#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_add_google
#' @importFrom reproducible Cache
#'
#' @rdname moduleSticker


moduleSticker <- function(moduleTable = NULL, moduleName,
                          directory = getwd(),
                          useCache = NULL, savedSticker = NULL,
                          ...){

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
                               "caribou",
                               unique(moduleTable[, module])))
  stick <- sticker(imageURL, package = packageName,
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

  # if (moduleName == "caribouLambda"){ # Gave up! Not working...
  #   lambdaStick <- stick + geom_pkgname("~lambda", x = 1.5, y = 1.55,
  #                                       family = "sans", parse = TRUE,
  #                                       size = 40)
  #   # hexSticker::sticker_dev()
  #   png(filename = savedSticker, res = 300)
  #   print(lambdaStick)
  #   dev.off()
  # }
  return(message("Your sticker has been saved: ", savedSticker))
}
