#' Plots of forest age
#'
#' @param dataPath character. Path to data
#' @param typeSim character. Which simulation is it? i.e. 'LandR_SCFM' | 'LandR.CS_fS'
#' @param addCaribousuitability logical. Should the plot show which forest ages are better or worse for Caribou in a color coded way?
#' @param overwrite logical.
#'
#' @return plot
#'
#' @author Tati Micheletti
#' @export
#' @importFrom ggplot2 geom_line ggplot ggtitle geom_ribbon theme
#' @importFrom data.table data.table rbindlist getDTthreads setDTthreads
#' @importFrom SpaDES.core paddedFloatToChar
#' @importFrom googledrive drive_upload
#' @importFrom LandR sppColors vegTypeMapGenerator
#' @importFrom quickPlot clearPlot
#' @importFrom raster writeRaster
#' @importFrom SpaDES.tools rasterizeReduced
#'
#' @include substrBoth.R
#'
#' @rdname forestAgePlot

forestAgePlot <- function(dataPath, typeSim,
                          addCaribousuitability = FALSE,
                          overwrite = FALSE){

  fileName <- file.path(dataPath, paste0("forestAgePlot", typeSim, ".png"))
  if (all(file.exists(fileName), !isTRUE(overwrite))){
    message("Plot exist and overwrite is FALSE. Returning plot path")
    return(fileName)
  }

  cohorDataList <- bringObjectTS(path = dataPath, rastersNamePattern = "cohortData")
  pixelGroupList <- bringObjectTS(path = dataPath, rastersNamePattern = "pixelGroupMap")

  # MAX AGE
  maxAge <- data.table::rbindlist(lapply(X = names(cohorDataList), function(index){
    cohort <- cohorDataList[[index]]
    pixelGroup <- pixelGroupList[[index]]
    a <- cohort[, list(maxAge = max(age, na.rm = TRUE)), by = "pixelGroup"]
    r <- rasterizeReduced(a, pixelGroup, "maxAge", "pixelGroup")
    return(list(meanAge = mean(r[], na.rm = TRUE),
                minAge = min(r[], na.rm = TRUE),
                maxAge = max(r[], na.rm = TRUE),
                medianAge = median(r[], na.rm = TRUE),
                years = as.numeric(substrBoth(strng = index,
                                                      howManyCharacters = 4,
                                                      fromEnd = TRUE))))
  }))
  oldBurn <- ifelse(addCaribousuitability, "red", "white")
  recentBurn <- ifelse(addCaribousuitability, "yellow", "white")
  noBurn <- ifelse(addCaribousuitability, "green", "white")
  agePlot <- ggplot2::ggplot(data = maxAge, aes(x = years)) +
    geom_ribbon(aes(ymin = 40, ymax = 60), alpha = 0.3, fill = oldBurn) + # Old burn
    geom_ribbon(aes(ymin = 0, ymax = 40), alpha = 0.3, fill = recentBurn) + # Recent burn
    geom_ribbon(aes(ymin = 60, ymax = maxAge), alpha = 0.3, fill = noBurn) + # No burn
    geom_line(aes(y = meanAge), size = 1.2) +
    geom_line(aes(y = medianAge), size = 1.2, linetype = "dashed") +
    ggtitle(paste0("Forest Age - ", typeSim)) +
    theme(legend.position = "bottom")
  ggsave(fileName, plot = agePlot)
return(agePlot)
}
