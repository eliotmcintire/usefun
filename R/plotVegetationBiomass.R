#' Plots vegetation biomass Type using cohortData and pixelGroupMap
#'
#' @param years numeric. Years available/intended to be used for the giphy
#' @param dataPath character. Path to data
#' @param typeSim character. Which simulation is it? i.e. 'LandR_SCFM' | 'LandR.CS_fS'
#' @param saveRAS logical. Save the raster for posterior use?
#' @param overwrite logical.
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
#' @rdname plotVegetationBiomass

plotVegetationBiomass <- function(years = c(2011, 2100),
                                  dataPath,
                                  typeSim,
                                  colNA = "grey85",
                                  saveRAS = TRUE,
                                  overwrite = FALSE){
  if (!isTRUE(overwrite)){
    fileName <- usefun::grepMulti(x = list.files(dataPath, full.names = TRUE), patterns = c("RAS_biomassYear", ".tif")) #[ FIX ] It won't make the "missing" leading years...
    if (length(fileName) != 0){
      fileName <- fileName[!fileName %in% usefun::grepMulti(x = fileName, patterns = c("aux"))]
      message("Rasters exist and overwrite is FALSE. Returning")
      stk <- raster::stack(lapply(X = fileName, FUN = raster::raster))
      return(stk)
    }
  }
# browser() # just commented out 09DEC19 :: why was it here...? because the function isnt working properly. Apparently it is...? Feb 9th 2020
  cohorDataList <- lapply(years, FUN = function(y){
  tbl <- bringObjectTS(path = dataPath, rastersNamePattern = c("cohortData", y))
    return(tbl[[1]])
    })
  names(cohorDataList) <- paste0("Year", years)
  pixelGroupList <- lapply(years, FUN = function(y){
    tbl <- bringObjectTS(path = dataPath, rastersNamePattern = c("pixelGroupMap", y))
    return(tbl[[1]])
  })
  names(pixelGroupList) <- paste0("Year", years)
# browser() # just commented out 09DEC19 :: why was it here...?
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
      writeRaster(x = maxBiomassPlot[[index]], filename = file.path(dataPath, paste0("RAS_",
                                                                                       names(maxBiomassPlot)[index], ".tif")),
                                                                    format = "GTiff", overwrite = TRUE)
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
  
  png(filename = file.path(dataPath, paste0("biomassVegetation", typeSim, ".png")), height = 600, width = 900)
  quickPlot::clearPlot()
  plot(maxBiomassPlot[[1]], breaks = brks, col = cols, lab.breaks = brks,
       main = paste0('Max biomass ', names(maxBiomassPlot)[[1]], " - ", typeSim), colNA = colNA)
  plot(maxBiomassPlot[[2]], breaks = brks, col = cols, lab.breaks = brks,
       main = paste0('Max biomass ', names(maxBiomassPlot)[[2]], " - ", typeSim), colNA = colNA)

  shouldPlot <- FALSE #TODO MAKE IT A PARAMETER. SET TO FALSE FOR SERVER
  if(shouldPlot){
p <- recordPlot()
  dev.off()    
      return(p)
} else {
      dev.off()
  return(NULL)
  }  
}
