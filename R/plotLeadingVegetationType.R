#' Plots Leading Vegetation Type using cohortData and pixelGroupMap
#'
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
#' @importFrom raster writeRaster stack raster
#'
#' @include bringObjectTS.R
#' @include grepMulti.R
#'
#' @rdname plotLeadingVegetationType

plotLeadingVegetationType <- function(dataPath,
                                      typeSim,
                                      colNA = "grey85",
                                      saveRAS = TRUE,
                                      overwrite = FALSE){
if (!isTRUE(overwrite)){
  fileName <- usefun::grepMulti(x = list.files(dataPath, full.names = TRUE), patterns = c("RAS_LeadingTypeYear", ".tif")) #[ FIX ] It won't make the "missing" leading years...
  if (length(fileName) != 0){
    fileName <- fileName[!fileName %in% usefun::grepMulti(x = fileName, patterns = c("aux"))]
    message("Rasters exist and overwrite is FALSE. Returning")
    stk <- raster::stack(lapply(X = fileName, FUN = raster::raster))
    return(stk)
   }
}

  cohorDataList <- bringObjectTS(path = dataPath, rastersNamePattern = "cohortData")
  pixelGroupList <- bringObjectTS(path = dataPath, rastersNamePattern = "pixelGroupMap")

  sppEquivCol <- "NWT"
  data("sppEquivalencies_CA", package = "LandR")
  sppEquivalencies_CA[, NWT := c(Abie_Bal = "Abie_Bal",
                                 Betu_Pap = "Betu_Pap",
                                 Lari_Lar = "Lari_Lar",
                                 Pice_Gla = "Pice_Gla",
                                 Pice_Mar = "Pice_Mar",
                                 Pinu_Ban = "Pinu_Ban",
                                 Popu_Tre = "Popu_Tre")[Boreal]]

  sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(NWT)]
  sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA$NWT
  sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, sppEquivCol = sppEquivCol,
                                   palette = "Set1")
  mixed <- structure("#D0FB84", names = "Mixed")
  sppColorVect[length(sppColorVect)+1] <- mixed
  attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"

  # LEADING TYPE ~~~~~~~~~~~~~~

  leadingSpecies <- lapply(X = names(cohorDataList), function(index){
    cohort <- cohorDataList[[index]]
    pixelGroup <- pixelGroupList[[index]]
    r <- LandR::vegTypeMapGenerator(x = cohort, pixelGroupMap = pixelGroup,
                                    vegLeadingProportion = 0.8, mixedType = 2, sppEquiv = sppEquivalencies_CA,
                                    sppEquivCol = "NWT", colors = sppColorVect, pixelGroupColName = "pixelGroup",
                                    doAssertion = options("LandR.assertions" = FALSE))
    return(r)
  })
  names(leadingSpecies) <- paste0("LeadingType", names(cohorDataList))
  if (saveRAS){
    lapply(1:length(leadingSpecies), function(index){
      fileName <- file.path(dataPath, paste0("RAS_", names(leadingSpecies)[index], ".tif"))
      writeRaster(x = leadingSpecies[[index]], filename = fileName,
                  format = "GTiff", overwrite = TRUE)
    })
  }
  # library("quickPlot")
  # quickPlot::clearPlot()
  # for (index in seq_along(leadingSpecies))
  #   quickPlot::Plot(leadingSpecies[[index]], title = names(leadingSpecies)[[index]])
  png(filename = file.path(dataPath, paste0("leadingVegetation", typeSim, ".png")), height = 600, width = 900)
    quickPlot::clearPlot()
  quickPlot::Plot(leadingSpecies[[1]], title = paste0(names(leadingSpecies)[[1]], " - ", typeSim))
  quickPlot::Plot(leadingSpecies[[length(leadingSpecies)]],
                  title = paste0(names(leadingSpecies)[[length(leadingSpecies)]], " - ", typeSim)) # Shortcut for the current vs. future landscapes.
  # Couldn't get raster plot to work. Might be easier to make a ggplot
  p <- recordPlot()
  dev.off()
  return(p)
}
