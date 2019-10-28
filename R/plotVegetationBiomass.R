#' Plots Leading Vegetation Type using cohortData and pixelGroupMap
#'
#' @param years numeric. Years available/intended to be used for the giphy
#' @param dataPath character. Path to data
#' @param typeSim character. Which simulation is it? i.e. 'LandR_SCFM' | 'LandR.CS_fS'
#' @param saveRAS logical. Save the raster for posterior use?
#'
#' @return plot
#'
#' @author Tati Micheletti
#' @export
#' @importFrom ggplot2 geom_line ggplot ggtitle
#' @importFrom data.table data.table rbindlist getDTthreads setDTthreads
#' @importFrom SpaDES.core paddedFloatToChar
#' @importFrom googledrive drive_upload
#' @importFrom LandR sppColors vegTypeMapGenerator
#' @importFrom quickPlot clearPlot
#' @importFrom raster writeRaster
#' 
#' @include bringObjectTS.R
#'
#' @rdname plotLeadingVegetationType

plotVegetationBiomass <- function(years = c(2001, 2100), 
                                  dataPath, 
                                  typeSim, 
                                  colNA = "grey85", saveRAS = TRUE){
  
  folderPath <- dataPath
  
  cohorDataList <- bringObjectTS(path = folderPath, rastersNamePattern = "cohortData")
  pixelGroupList <- bringObjectTS(path = folderPath, rastersNamePattern = "pixelGroupMap")
  
  # BIOMASS ~~~~~~~~~~~~~~~~
  maxBiomassPlot <- lapply(X = c(1:length(cohorDataList)), function(index){
    cohort <- cohorDataList[[index]]
    pixelGroup <- pixelGroupList[[index]]
    a <- cohort[, list(sumBio = sum(B, na.rm = TRUE)), by = "pixelGroup"]
    r <- SpaDES.tools::rasterizeReduced(a, pixelGroup, "sumBio", "pixelGroup")
    return(r)
  })
  names(maxBiomassPlot) <- paste0("biomassYear", years)
  if (saveRAS){
    lapply(1:length(maxBiomassPlot), function(index){
      writeRaster(x = maxBiomassPlot[[index]], filename = paste0(folderPath, "RAS", names(maxBiomassPlot)[index]), format = "GTiff")
    })
  }
  rng = range(c(getValues(maxBiomassPlot[[1]]), getValues(maxBiomassPlot[[2]])), na.rm = TRUE)
  # brks <- seq(min(rng), max(rng)/10, by = (max(rng)/10-min(rng))/10) # Looks like the problem of the cohort that had 10x more biomass is gone...
  brks <- c(seq(min(rng), max(rng), by = 1000),max(rng))
  nb <- length(brks)-1
  cols <- rev(heat.colors(nb))
  parSetup <- par()
  invisible(on.exit(par(parSetup)))
  if (length(years < 4)){
    par(mfrow=c(1,length(years)))
  } else {
    if (all(length(years > 3), length(years < 7))){
      par(mfrow=c(length(years)/2,length(years)))
    }
  }
  plot(maxBiomassPlot[[1]], breaks = brks, col = cols, lab.breaks = brks, 
       main = paste0('Max biomass ', names(maxBiomassPlot)[[1]], " - ", typeSim), colNA = colNA)
  plot(maxBiomassPlot[[2]], breaks = brks, col = cols, lab.breaks = brks, 
       main = paste0('Max biomass ', names(maxBiomassPlot)[[2]], " - ", typeSim), colNA = colNA)
  p <- recordPlot()
  return(p)
}