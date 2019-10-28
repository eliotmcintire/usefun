#' Creates a GIF of biomass change
#'
#' @param years numeric. Years available/intended to be used for the giphy
#' @param dataPath character. Path to data
#' @param uploadTo character. Google drive folder id. (i.e. "1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH")
#'
#' @return list of plots
#'
#' @author Tati Micheletti
#' @export
#' @importFrom ggplot2 geom_line ggplot ggtitle
#' @importFrom data.table data.table rbindlist getDTthreads setDTthreads
#' @importFrom SpaDES.core paddedFloatToChar
#' @importFrom googledrive drive_upload
#'
#' @rdname biomassPerSpeciesYearGIF

biomassPerSpeciesYearGIF <- function(dataPath, years = NULL, uploadTo){

  # Collect info on all years creating a table, then plot the results
if (is.null(years)){
  ysrName <- paddedFloatToChar(seq(0,100, by = 10), padL = 3)
} else {
  ysrName <- years
}
  cohortDataComplete <- rbindlist(lapply(X = ysrName, FUN = function(yr){
    tryCatch({
      simBM <- readRDS(file.path(dataPath, paste0("cohortData_year", yr,".rds")))
      simBM$year <- as.numeric(yr)
      return(simBM)
    }, error = function(e){
      message("There is no data associated to year ", yr, ". Returning NULL")
      return(NULL)
    })
  }))

  td <- getDTthreads()
  setDTthreads(1)
  on.exit(setDTthreads(td))
  std.err <- function(x, ...) sd(x, ...)/sqrt(length(x))
  cohortDataComplete[, BiomassBySpecies := sum(B, na.rm = TRUE),
                                by = .(speciesCode, pixelGroup, year)]
  cohortDataComplete[, c("medianBiomass", "totalBiomass", "meanBiomass", "seBiomass", "sdBiomass") :=
                       list(median(as.numeric(BiomassBySpecies), na.rm = TRUE),
                            sum(as.numeric(BiomassBySpecies), na.rm = TRUE),
                            mean(as.numeric(BiomassBySpecies), na.rm = TRUE),
                            std.err(as.numeric(BiomassBySpecies), na.rm = TRUE),
                            sd(as.numeric(BiomassBySpecies), na.rm = TRUE)),
                                      by = .(speciesCode, year)]

  cols = c("speciesCode", "year", "totalBiomass", "medianBiomass",
           "meanBiomass", "seBiomass", "sdBiomass")
  dt <- cohortDataComplete[, ..cols]
  BiomassPerSpecies <- unique(dt)

  #PLOT
  dt <- as.data.frame(BiomassPerSpecies)
  library("ggplot2")
  meanBiomassPerSpecies <- ggplot(dt, aes(x = year, y = meanBiomass, group = speciesCode)) +
    geom_line(size=1.2, aes(color = speciesCode)) +
    ggtitle(label = "Mean biomass per species per year")
    # geom_errorbar(aes(ymin = meanBiomass - sdBiomass,
    #                   ymax = meanBiomass + sdBiomass),
    #               width=.2,
    #               position=position_dodge(.9))

  medianBiomassPerSpecies <- ggplot(dt, aes(x = year, y = medianBiomass, group = speciesCode)) +
    geom_line(size=1.2, aes(color = speciesCode)) +
    ggtitle(label = "Median biomass per species per year")

  totalBiomassPerSpecies <- ggplot(dt, aes(x = year, y = totalBiomass, group = speciesCode)) +
    geom_line(size=1.2, aes(color = speciesCode)) +
    ggtitle(label = "Total biomass per species per year")

  p <- lapply(X = c("totalBiomassPerSpecies", "medianBiomassPerSpecies", "meanBiomassPerSpecies"),
         function(plotting){
    gifName <- file.path(dataPath, paste0(plotting,
                                          toupper(format(Sys.time(), "%d%b%y")),".png"))
    png(gifName,
        width = 700, height = 480)
    print(get(plotting))
    dev.off()
    if (is.character(uploadTo)){
      googledrive::drive_upload(gifName,
                                path = as_id(uploadTo))
    }

  })
  return(p)
}
