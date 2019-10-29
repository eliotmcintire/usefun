#' Plots summary of burns
#'
#' @param years numeric. Years available/intended to be used for the giphy
#' @param dataPath character. Path to data
#' @param typeSim character. Which simulation is it? i.e. 'LandR_SCFM' | 'LandR.CS_fS'
#' @param saveRAS logical. Save the raster for posterior use?
#' @param overwrite logical. Default FALSE
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
#' @rdname plotBurnSummary

plotBurnSummary <- function(dataPath,
                            typeSim,
                            lastYear,
                            theObject = NULL,
                            overwrite = FALSE){

  fileName <- file.path(dataPath, paste0("burnSummary", typeSim, ".png"))
  if (all(file.exists(fileName), !isTRUE(overwrite))){
    message("Plot exist and overwrite is FALSE. Returning plot path")
    return(fileName)
  }


parSetup <- par()
invisible(on.exit(par(parSetup)))
par(mfrow=c(2, 1))

# FIRE
if (!is.null(theObject)){
  burnSumm <- theObject
} else {
  burnSumm <- readRDS(file.path(dataPath, paste0("burnSummary_year", lastYear,".rds")))
}

areaB <- burnSumm[, sumAB := sum(areaBurned), by = year]
areaB <- data.table(year = areaB$year, val = areaB$sumAB, var = "area_burned")
areaB <- unique(areaB)

tend <-lm(val ~ year, data = areaB)
require(stats)
coeff <- coefficients(tend)

# N fires
nFires <- burnSumm[, Nfires := length(N), by = year]
nFires <- data.table(year = nFires$year, val = nFires$Nfires, var = "number_fires")
nFires <- unique(nFires)
tendF <-lm(val ~ year, data = nFires)
require(stats)
coeffF <- coefficients(tendF)

# New facet label names for dose variable
replacementNames <- c(paste0("Area burned: ", paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))),
                      paste0("No fires: ", paste0("y = ", round(coeffF[2],1), "*x ", round(coeffF[1],1))))
names(replacementNames) <- c("area_burned", "number_fires")

dt <- rbind(areaB, nFires)
library("ggplot2")
p <- ggplot(data = dt, aes(x = year, y = val, )) +
     geom_point() +
     stat_smooth(method = "lm", aes(fill = var, color = var)) +
  facet_grid(var ~ ., scales = "free_y", labeller = labeller(var = replacementNames)) +
  theme(axis.title.y.left = element_blank(),
        legend.position = "none",
        strip.text.y = element_text(size = 12, face = "bold"))
ggsave(fileName, plot = p)

return(fileName)
}
