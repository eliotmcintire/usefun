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
#' @importFrom ggplot2 geom_line ggplot ggtitle facet_grid labeller
#' @importFrom data.table data.table rbindlist getDTthreads setDTthreads
#' @importFrom SpaDES.core paddedFloatToChar
#' @importFrom googledrive drive_upload
#' @importFrom LandR sppColors vegTypeMapGenerator
#' @importFrom quickPlot clearPlot
#' @importFrom raster writeRaster
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob
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
areaB <- areaB[, val := val/1000] # Doing this so I can plot the axis with mostly the same limits. Needs to be informed in the captions!!
# Could eventually implement something as: https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/

tend <-lm(val ~ year, data = areaB)
require(stats)
coeff <- coefficients(tend)
Fstats <- summary(tend)$fstatistic
names(Fstats) <- NULL
pValueA <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, " \n(significant)", " \n(non-significant)")

# N fires
nFires <- burnSumm[, Nfires := length(N), by = year]
nFires <- data.table(year = nFires$year, val = nFires$Nfires, var = "number_fires")
nFires <- unique(nFires)
tendF <-lm(val ~ year, data = nFires)
require(stats)
coeffF <- coefficients(tendF)

# 1. See if the trend in fires is statistically significant
Fstats <- summary(tendF)$fstatistic
names(Fstats) <- NULL
pValueF <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, " \n(significant)", " \n(non-significant)")

coefXA <- round(coeff[2],1)
coefYA <- round(coeff[1],1)
coefXF <- round(coeffF[2],1)
coefYF <- round(coeffF[1],1)

# New facet label names for dose variable
replacementNames <- c(paste0("Area burned: ",
                             "y = ", ifelse(coefXA < 10000, coefXA, formatC(coefXA, format = "e", digits = 2)),
                             "x + ", ifelse(coefYA < 10000, coefYA, formatC(coefYA, format = "e", digits = 2)), pValueA),
                      paste0("No fires: ",
                             "y = ", ifelse(coefXF < 10000, coefXF, formatC(coefXF, format = "e", digits = 2)),
                             "x + ", ifelse(coefYF < 10000, coefYF, formatC(coefYF, format = "e", digits = 2)), pValueF))
names(replacementNames) <- c("area_burned", "number_fires")

dt <- rbind(areaB, nFires)
library("ggplot2")
p1 <- ggplot(data = dt[var == "area_burned",], aes(x = year, y = val)) +
  geom_point() +
  stat_smooth(method = "lm", color = "darkred", fill = "red") +
  facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
  theme(legend.position = "none",
        strip.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm")) +
  coord_cartesian(ylim = c(100, 1800)) +
  labs(y = "ha x 10^3")
p2 <- ggplot(data = dt[var == "number_fires",], aes(x = year, y = val, colour = "blue")) +
  geom_point(colour = "black") +
  stat_smooth(method = "lm", fill = "blue", color = "darkblue") +
  facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
  theme(legend.position = "none",
        strip.text.y = element_text(size = 10, face = "bold"),
        plot.margin = unit(c(-0.01, 0.2, 0.2, 0.2), "cm")) +
  coord_cartesian(ylim = c(100, 1800)) +
  ylab(label = "no. of fires")

p <- gridExtra::grid.arrange(p1, p2, ncol=1,
                             top = grid::textGrob(typeSim, gp = gpar(fontsize = 12)))

ggsave(fileName, plot = p)

return(list(fileLocation = fileName, model = list(areaBurned = tend, noFires = tendF)))
}
