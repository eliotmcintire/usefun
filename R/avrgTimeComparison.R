#' Compare two or more scenarios for averages through time
#'
#' @param ... List or tables of results coming from usefun::meanValuesTime to be compared.
#' @param folderID character. Google folder id to upload to.
#'                 Only needs to be provided if `upload == TRUE`. Default is `NULL``
#' @param upload logical. Should the raster be updated to googledrive? Only works if the
#'               raster exists OR is set to be written.
#' @param outputFolder character. Path to the folder where it should be saved.
#' @param comparisonID character. Name to indentify the comparison (for file name)
#'
#' @return list of significant species or scenarios with indication of increasing or decreasing
#'
#' @author Tati Micheletti
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_ribbon theme_bw
#' @importFrom data.table data.table rbindlist
#' @importFrom grDevices png
#' @importFrom googledrive drive_upload as_id
#'
#' @rdname avrgTimeComparison

avrgTimeComparison <- function(...,
                               upload,
                               outputFolder,
                               comparisonID,
                               folderID){ # Caribou RSF average through time. Can take up to 2 comparisons for now

  if (is.null(comparisonID))
    comparisonID <- "generic"

  browser()
  dots <- list(...) # List/individual data.tables (the latter if outside of modules) of results coming from sim$averageInTime
  dt <- rbindlist(dots)
  p <- ggplot(data = dt, aes(x = year, y = average, ymin = (average - IC), ymax = (average + IC), group = scenario)) +
    geom_line(aes(color = scenario)) +
    geom_ribbon(aes(fill = scenario), alpha = 0.5) +
    theme_bw()
  pngFig <- file.path(outputFolder, "average", comparisonID,"Comparison.png")
  png(pngFig, width = 700, height = 480)
  print(p)
  dev.off()
  if(upload)
    googledrive::drive_upload(pngFig,
                              path = googledrive::as_id(folderID))
  return(p)
}
