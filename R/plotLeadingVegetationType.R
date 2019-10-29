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

plotLeadingVegetationType <- function(years = c(2011, 2100),
                                      dataPath,
                                      typeSim,
                                      colNA = "grey85",
                                      saveRAS = TRUE){
  folderPath <- dataPath

  cohorDataList <- bringObjectTS(path = folderPath, rastersNamePattern = "cohortData")
  pixelGroupList <- bringObjectTS(path = folderPath, rastersNamePattern = "pixelGroupMap")

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
      writeRaster(x = leadingSpecies[[index]], filename = file.path(folderPath, paste0("RAS_", names(leadingSpecies)[index])),
                  format = "GTiff", overwrite = TRUE)
    })
  }
  # library("quickPlot")
  # quickPlot::clearPlot()
  # for (index in seq_along(leadingSpecies))
  #   quickPlot::Plot(leadingSpecies[[index]], title = names(leadingSpecies)[[index]])
clearPlot()
  quickPlot::Plot(leadingSpecies[[1]], title = paste0(names(leadingSpecies)[[1]], " - ", typeSim))
  quickPlot::Plot(leadingSpecies[[length(leadingSpecies)]],
                  title = paste0(names(leadingSpecies)[[length(leadingSpecies)]], " - ", typeSim)) # Shortcut for the current vs. future landscapes.
  # Couldn't get raster plot to work. Might be easier to make a ggplot
  p <- recordPlot()
  return(p)
}
