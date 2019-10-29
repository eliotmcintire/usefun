#' Plots biomass per species: proportional or absolute, and total or just overstory
#'
#' @param dataPath character. Path to data
#' @param typeSim character. Which typeSimation is it? i.e. 'LandR_SCFM' | 'LandR.CS_fS'
#' @param columnsType logical. Should the plot be continuous (lines) or columns?
#' @param proportional logical. Should the plot be of the proportional biomass?
#' @param overstory logical. Should the plot be of the overstory biomass?
#' @param overwrite logical.
#' @param maxVal numeric. Max value for y axis. Passing this ensures that both overstory
#'               and all biomass plots are comparable Default to 1e10.
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
#' @include bringObjectTS.R
#'
#' @rdname totalBiomassPerSpecies

totalBiomassPerSpecies <- function(dataPath,
                                   typeSim,
                                   proportional = FALSE,
                                   columnsType = FALSE,
                                   overstory = FALSE,
                                   overwrite = FALSE,
                                   maxVal = 2e10){
  prop <- NULL
  overS <- NULL
  if (isTRUE(proportional)) prop <- "_Prop"
  if (isTRUE(overstory)) overS <- "_Overstory"

  if (!isTRUE(overwrite)){
  pat <- c("biomassMapStack_", typeSim, prop, overS)
    fileName <- usefun::grepMulti(x = list.files(dataPath, full.names = TRUE), patterns = pat) #[ FIX ] It won't make the "missing" leading years...
    if (!is.null(fileName)){
      message("Plots exist and overwrite is FALSE. Returning paths")
      return(fileName)
    }
  }

  if (proportional)
    maxVal <- 100

  cohortDataList <- bringObjectTS(path = dataPath, rastersNamePattern = "cohortData")
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
  biomassBySpecies <- rbindlist(lapply(X = names(cohortDataList), FUN = function(yr){
    cohort <- cohortDataList[[yr]]
    pixelGroup <- pixelGroupList[[yr]]
    if (NROW(cohort[duplicated(cohort)]) != 0)
      cohort <- cohort[!duplicated(cohort)]
    pixelCohortData <- LandR::addNoPixel2CohortData(cohort, pixelGroup)
    pixelCohortData[, B := as.double(B)]
    thisPeriod <- pixelCohortData[, list(year = as.numeric(substrBoth(strng = yr,
                                                                      howManyCharacters = 4,
                                                                      fromEnd = TRUE)),
                                         BiomassBySpecies = sum(B*noPixels, na.rm = TRUE)),
                                  by = .(speciesCode)]

    # For proportional
    if (all(!isTRUE(overstory), isTRUE(proportional))) {
      # stop("This still need to be debug. Not working") # [ FIX ]
      thisPeriod$propBiomassBySpecies <- 100 * (thisPeriod$BiomassBySpecies / sum(thisPeriod$BiomassBySpecies))
    }
    if (overstory) {
      pixelCohortData[, bWeightedAge := floor(sum(age*B)/sum(B)/10)*10, .(pixelGroup)]
      overstory <- pixelCohortData[age >= bWeightedAge, .(overstoryBiomass = sum(B * noPixels)), .(speciesCode)]
      thisPeriod <- thisPeriod[overstory, on = 'speciesCode']
      if (isTRUE(proportional)) {
        # stop("This still need to be debug. Not working") # [ FIX ]
        thisPeriod$overstoryBiomassProp <- 100 * (thisPeriod$overstoryBiomass / sum(thisPeriod$overstoryBiomass))
      }
    }
    return(thisPeriod)
  })
)

  if (isTRUE(proportional)){
    if (isTRUE(overstory)){
      y <- biomassBySpecies$overstoryBiomassProp # Propor = TRUE, Overst = TRUE
    } else {
      y <- biomassBySpecies$propBiomassBySpecies  # Propor = TRUE, Overst = FALSE
    }
  } else {
    if (isTRUE(overstory)){
      y <- biomassBySpecies$overstoryBiomass # Propor = FALSE, Overst = TRUE
    } else {
      y <- biomassBySpecies$BiomassBySpecies  # Propor = FALSE, Overst = FALSE
    }
  }

  png(filename = file.path(dataPath, paste0("biomassMapStack_", typeSim, prop, overS, ".png")), height = 600, width = 900)
  library("ggplot2")

  if (columnsType){
    plot2 <- ggplot(data = biomassBySpecies, aes(x = year, y = y,
                                                 fill = speciesCode), position = "fill") +
      geom_col(aes(y = y)) +
      scale_fill_viridis_d() +
      labs(x = "Year", y = "Total Biomass", title = paste0("Total biomass by species\n",
                                                     "across pixels - ", typeSim, " ", overS)) +
      theme_bw() +
      theme(legend.text = element_text(size = 20), legend.title = element_blank(),
            text = element_text(size=20),
            axis.text.x = element_text(size = 20),
            title = element_text(size = 22)) +
      ylim(0, maxVal)
    quickPlot::clearPlot()
    print(plot2)
    dev.off()

  } else {
    plot2 <- ggplot(data = biomassBySpecies, aes(x = year, y = y,
                                                 fill = speciesCode, group = speciesCode)) +
      geom_area(position = "stack") +
      scale_fill_manual(values = sppColorVect) +
      labs(x = "Year", y = "Total Biomass", title = paste0("Total biomass by species\n",
                                              "across pixels - ", typeSim, " ", overS)) +
      theme(legend.text = element_text(size = 16), legend.title = element_blank(),
            text = element_text(size=16),
            axis.text.x = element_text(size = 16)) +
      ylim(0, maxVal)
    quickPlot::clearPlot()
    quickPlot::Plot(plot2, new = TRUE)
    dev.off()
  }

return(plot2)
}
