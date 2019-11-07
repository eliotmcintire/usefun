

avrgTimeComparison <- function(..., 
                               upload, 
                               outputFolder, 
                               comparisonID,
                               folderID){ # Caribou RSF average through time. Can take up to 2 comparisons for now
  
  if (is.null(comparisonID))
    comparisonID <- "generic"
  
  browser()
  dots <- list(...) # List/individual data.tables (trhe latter if outside of modules) of results coming from sim$averageInTime
  dt <- rbindlist(dots)
  library("ggplot2")
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