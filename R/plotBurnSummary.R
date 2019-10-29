#' Plots summary of burns
#'
#' @param years numeric. Years available/intended to be used for the giphy
#' @param folderData character. Path to data
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
#' @rdname plotBurnSummary

plotBurnSummary <- function(folderData,
                            typeSim,
                            lastYear,
                            theObject = NULL){

parSetup <- par()
invisible(on.exit(par(parSetup)))
par(mfrow=c(2, 1))

# FIRE
if (!is.null(theObject)){
  burnSumm <- theObject
} else {
  burnSumm <- readRDS(file.path(folderData, paste0("burnSummary_year", lastYear,".rds")))
}
areaB <- burnSumm[, sumAB := sum(areaBurned), by = year]
areaB <- data.table(YEAR = areaB$year, AREABURNED = areaB$sumAB)
areaB <- unique(areaB)
tend <-lm(AREABURNED ~ YEAR, data = areaB)
require(stats)
coeff <- coefficients(tend)

# equation of the line :
eq <- paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# plot
plot(areaB, main = paste0(eq, " - ", typeSim))
abline(tend, col="blue")

# N fires
nFires <- burnSumm[, Nfires := length(N), by = year]
nFires <- data.table(YEAR = nFires$year, NUMBFIRES = nFires$Nfires)
nFires <- unique(nFires)
tendF <-lm(NUMBFIRES ~ YEAR, data = nFires)
require(stats)
coeffF <- coefficients(tendF)

# equation of the line :
eqF <- paste0("y = ", round(coeffF[2],1), "*x ", round(coeffF[1],1))
# plot
plot(nFires, main = paste0(eqF, " - ", typeSim))
abline(tendF, col="red")

p <- recordPlot()

return(p)
}
