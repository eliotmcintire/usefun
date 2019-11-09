#' Compare two or more scenarios for averages through time
#'
#' @param ... List or tables of results coming from usefun::meanValuesTime to be compared.
#' @param folderID character. Google folder id to upload to.
#'                 Only needs to be provided if `upload == TRUE`. Default is `NULL``
#' @param upload logical. Should the raster be updated to googledrive? Only works if the
#'               raster exists OR is set to be written.
#' @param outputFolder character. Path to the folder where it should be saved.
#' @param comparisonID character. Name to indentify the comparison (for file name)
#' @param plotCI logical. Should the plot have confidence interval?
#'
#' @return list of significant species or scenarios with indication of increasing or decreasing
#'
#' @author Tati Micheletti
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_ribbon theme_bw aes facet_grid
#' @importFrom data.table data.table rbindlist
#' @importFrom grDevices png
#' @importFrom googledrive drive_upload as_id
#'
#' @rdname avrgTimeComparison

avrgTimeComparison <- function(...,
                               upload,
                               outputFolder,
                               comparisonID,
                               folderID,
                               plotCI = TRUE){ # Caribou RSF average through time. Can take up to 2 comparisons for now

  if (is.null(comparisonID))
    comparisonID <- "generic"

  dots <- list(...) # List/individual data.tables (the latter if outside of modules) of results coming from sim$averageInTime
  depth <- function(this,thisdepth=0){
    if(!is.list(this)){
      return(thisdepth)
    }else{
      return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))
    }
  }
  while (depth(dots) != 2){
    dots <- unlist(dots, recursive = FALSE)
  }
  dots <- lapply(1:length(dots), function(index){
    dots[[index]]$grouping <- names(dots)[index]
    return(dots[[index]])
  })
  dt <- rbindlist(dots)
  dtAverage <- subset(x = dt, rasType == "AVERAGE")
  dtSd <- subset(x = dt, rasType == "SD")
  if (NROW(dtSd) != 0){
    dtSd[, IC := average*1.96]
    dtSd <- dtSd[, c("year", "scenario", "IC")]
    dt <- merge(dtAverage, dtSd)
  }
  p <- ggplot(data = dt, aes(x = year, y = average,
                             group = scenario))
 if (plotCI & NROW(dtSd) != 0) {
    p <- p +  geom_ribbon(aes(fill = scenario, ymin = (average - IC), ymax = (average + IC)), alpha = 0.5)
}
    p <- p + geom_line(aes(color = scenario)) +
      theme_bw()
  if (length(unique(dt$grouping)) != 1){
    p <- p + facet_grid(grouping ~ ., scales = "free_y") +
      theme(legend.position = "bottom")
  }

  pngFig <- file.path(outputFolder, paste0("average", comparisonID,"Comparison.png"))
  png(pngFig, width = 700, height = 480)
  print(p)
  dev.off()
  if(upload){
    googledrive::drive_upload(pngFig,
                              path = googledrive::as_id(folderID))
    message("averageComparison was saved in", ": https://drive.google.com/drive/u/0/folders/", folderID)
  }
  return(p)
}
