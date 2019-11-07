#' Uses a set of rules to calculate how many non-NA pixels in a raster follow those rules,
#' usig a shapefile to subset those of interest
#'
#' @param ras rasterLayer to calculate the n of pixels that follow a given rule
#' @param rule character string of rule to determine which pixels should be computed
#' @param shp shapefile indicating the different areas to subset the pixels to consider in the calculation
#' @param pol numeric. Representation of the polygon of interest in `pol` for the calculation
#'
#' @return A list of the percent disturbance, total pixels not NA and how many pixels are disturbed according to the ruls
#'
#' @author Tati Micheletti
#' @export
#' @importFrom raster getValues
#' @rdname calculatePixelsInaRule

calculatePixelsInaRule <- function(ras,
                                 rule,
                                 pol,
                                 shp, ...){
  dots <- list(...)
  e <- environment()
  lapply(X = seq_along(dots), FUN = function(each){
    assign(x = names(dots)[each], value = dots[[each]], envir = e)
  })
  vals <- raster::getValues(x = ras)
  shp[is.na(shp)] <- -1
  polValues <- vals[shp[] == as.numeric(pol)] # Here we might have problems sometimes. Returns 0 character table. We only have NA's in the numeric pool!
  totPixelsNotNA <- sum(!is.na(polValues))
  isRecentDisturbance <- !is.na(polValues) & eval(parse(text = paste0("polValues", rule)))
  cummDisturbance <- sum(isRecentDisturbance, na.rm = TRUE)
  percentDisturbance <- 100*(cummDisturbance/totPixelsNotNA)
  if (is.na(percentDisturbance)) browser()
  return(list(percentDisturbance = percentDisturbance,
              isDisturbance = isRecentDisturbance,
              totPixelsNotNA = totPixelsNotNA))
}
