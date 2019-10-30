#' Bootstraps rasters for testing significancy on comparable rasters of different species or scenarios
#'
#' @param years numeric. Years to compare. Currently this function only compares 2 years.
#' @param dataPath character. Path to raster data.
#' @param shp character or shapefile. If you wish to calculate these metrics for separate areas.
#'            Needs to match the rasters. Default is NULL (i.e. the whole raster is only one area)
#' @param sampleSize numeric or "auto" (default). What is the sample size (i.e. number of pixels)
#'                   we want to use on the bootstrapping? If "auto" it calculates internally
#'                   Cohen's D And Hedges G Effect Size.
#' @param n numeric. Default is 100. How many iterations (random selection of `sampleSize` pixels) should be done?
#'
#' @return list of significant species or scenarios with indication of increasing or decreasing
#'
#' @author Tati Micheletti
#' @export
#' @importFrom crayon yellow green
#' @importFrom data.table data.table rbindlist
#' @importFrom reproducible prepInputs Cache
#' @importFrom stats wilcox.test t.test
#' @importFrom pryr where
#' @importFrom effsize cohen.d
#' @importFrom raster getValues raster
#'
#' @include helpersBirds.R
#' @include grepMulti.R
#' @include substrBoth.R
#' @include cbindFromList.R
#'
#' @rdname bootstrapPercentChanges

bootstrapPercentChanges <- function(dataPath,
                                    years = c(2001, 2100),
                                    sampleSize = "auto",
                                    n = 100,
                                    shp = NULL){

  if (class(shp) == "character"){
    studyArea <- Cache(prepStudyAreaForBirds, studyArea = shp,
                       dataPath = reproducible::checkPath(file.path(dataPath, "birdRTMEdehzhieRAS"),
                                                          create = TRUE))
  } else {
    studyArea <- shp
  }
  fullTable <- lapply(1:n, function(repetition){
    message(crayon::yellow("Starting calculateSignificantChangesInBirds for repetition ", repetition, " TIME: ", Sys.time()))
    t <- Sys.time
    changesTable <- .calculateSignificantChangesInBirds(dataPath = dataPath, years = years,
                                                       sampleSize = sampleSize, studyArea = studyArea,
                                                       repetition = repetition)
    message(crayon::green("FINISHED calculateSignificantChangesInBirds for repetition ", repetition, " ELAPSED: ", Sys.time() - t))
    t <- Sys.time()
    percentChange <- .calculatePercentageChanges(changesTable = changesTable, column = "result")
    return(list(changesTable = changesTable, percentChange = percentChange))
  })
  changesTableList <- lapply(fullTable, '[[',"changesTable")
  constantSpecies <- .whichSpeciesChange(changesTable = changesTableList)
  percChange <- data.table::rbindlist(lapply(fullTable, '[[',"percentChange"))
  if (!is.null(studyArea)){
    dt <- data.table::rbindlist(lapply(unique(percChange[["location"]]), function(locality){
      percChangeLoc <- percChange[location == locality,]
      dt <- .calcICPercentChange(percChange = percChangeLoc)
      dt[["location"]] <- locality
      return(dt)
    })
    )
  } else {
    dt <- .calcICPercentChange(percChange = percChange)
  }
  return(list(constantSpecies = constantSpecies, tableIC = dt))
}
