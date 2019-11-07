#' Calculates the mean value of rasters through time
#'
#' @param ras RasterStack. Time series used to calculate the mean value through time
#' @param scenario character. Which scenario are you running ie. `LandR.CS_fS`
#'            Needs to match the rasters. Default is NULL (i.e. the whole raster is only one area)
#' @param initialTime numeric. Format of the first year of analysis.
#' @return table with average, SD and CI95%
#'
#' @author Tati Micheletti
#' @export
#' @importFrom data.table data.table rbindlist
#'
#' @include substrBoth.R
#'
#' @rdname meanValuesTime

meanValuesTime <- function(ras,
                           scenario,
                           initialTime){
  # scenario == "noCS" | "CS"
browser()
  if (is(ras, "RasterStack")){
    fullTable <- lapply(names(ras), FUN = function(year){
      modTable <- lapply(ras[[year]], FUN = function(mod){
        if (is(mod, "list")){
          meanAndUnc <- lapply(mod, function(eachRas){
            average <- median(eachRas[], na.rm = TRUE)
            rasType <- ifelse(grepl(names(eachRas), pattern = "Uncertain"), "SD", "AVERAGE")
            yr <- as.numeric(substrBoth(string = names(eachRas), howManyCharacters = nchar(initialTime)))
            dt <- data.table::data.table(average = average, year = yr, scenario = scenario)
            return(dt)
          })
          dt <- meanAndUnc[[1]]
          dt$SD <-  meanAndUnc[[2]][["average"]]
          dt$IC <- dt$SD*1.96
          return(dt)
        } else {
          average <- median(mod[], na.rm = TRUE)
          rasType <- ifelse(grepl(names(mod), pattern = "Uncertain"), "SD", "AVERAGE")
          yr <- as.numeric(substrBoth(strng = names(mod), howManyCharacters = nchar(initialTime[1]), fromEnd = TRUE))
          dt <- data.table::data.table(average = average, year = yr, scenario = scenario, rasType = rasType)
        return(dt)
      }
        })
      return(rbindlist(modTable))
    })
    fullTable <- rbindlist(fullTable)
    return(fullTable)
  } else {
    fullTable <- lapply(ras, FUN = function(year){
      modTable <- lapply(year, FUN = function(mod){
        meanAndUnc <- lapply(mod, function(eachRas){
          average <- median(eachRas[], na.rm = TRUE)
          rasType <- ifelse(grepl(names(eachRas), pattern = "Uncertain"), "SD", "AVERAGE")
          yr <- as.numeric(substrBoth(string = names(eachRas), howManyCharacters = nchar(initialTime)))
          dt <- data.table::data.table(average = average, year = yr, scenario = scenario)
          return(dt)
        })
        dt <- meanAndUnc[[1]]
        dt$SD <-  meanAndUnc[[2]][["average"]]
        dt$IC <- dt$SD*1.96
        return(dt)
      })
      return(rbindlist(modTable))
    })
    fullTable <- rbindlist(fullTable)
    return(fullTable)
  }
}
