#' ECCC's RSF plot style
#'
#' @param ras RasterLayer. Layer to generate 10 binned green to red (caribou RSF) map.
#' @param folderID character. Google folder id to upload to.
#'                 Only needs to be provided if `upload == TRUE`. Default is `NULL``
#' @param rasName character. Name of the raster to be saved.
#' @param outputFolder character. Path to the folder where it should be saved.
#' @param writeReclasRas logical. Default is `FALSE`. Should the raster be written to disk?
#' @param upload logical. Should the raster be updated to googledrive? Only works if the
#'               raster exists OR is set to be written.
#'
#' @return Plots that have similat binning system to ECCC's 2011 Caribou RSF
#'
#' @author Tati Micheletti
#' @export
#' @importFrom crayon green red
#' @importFrom raster getValues raster reclassify writeRaster nlayers
#' @importFrom rasterVis gplot
#' @importFrom googledrive drive_upload as_id
#' @importFrom ggplot2 geom_raster scale_fill_manual coord_equal labs element_blank theme
#' @importFrom grDevices colorRampPalette
#'
#' @rdname RSFplot

RSFplot <- function(ras,
                    upload = FALSE,
                    writeReclasRas = FALSE,
                    outputFolder = tempdir(),
                    rasName,
                    folderID = NULL){
  if (is(ras, "RasterStack")){
    vals <- raster::getValues(ras[[1]])
  } else {
    vals <- raster::getValues(ras)
  }
  getBin <- function(vals){
    (max(vals, na.rm = TRUE)-min(vals, na.rm = TRUE))/10
  }
  bin <- getBin(vals)
  mn <- min(vals, na.rm = TRUE)
  m <- matrix(c(mn-1, mn+bin, 1,
                mn+bin, mn+bin*2, 2,
                mn+bin*2, mn+bin*3, 3,
                mn+bin*3, mn+bin*4, 4,
                mn+bin*4, mn+bin*5, 5,
                mn+bin*5, mn+bin*6, 6,
                mn+bin*6, mn+bin*7, 7,
                mn+bin*7, mn+bin*8, 8,
                mn+bin*8, mn+bin*9, 9,
                mn+bin*9, mn+bin*10, 10), ncol = 3, byrow = TRUE)

  r <- reclassify(ras, m)

  greenRed<-colorRampPalette(c("darkgreen","yellow","red"))
  colsGR <- greenRed(10)
stk <-  lapply(1:nlayers(x = r), function(lay){
    y <- usefun::substrBoth(strng = names(r[[lay]]),
                       howManyCharacters = 4,
                       fromEnd = TRUE)
    rasNameFinal <- file.path(outputFolder, paste0(rasName,"_",
                                                   y, ".tif"))
    library("ggplot2")
    p <- gplot(r[[lay]]) +
      geom_raster(aes(fill = factor(value))) +
      scale_fill_manual(values = colsGR, aesthetics = "fill") +
      coord_equal() +
      labs(fill = paste0("RSF ", y)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    print(p)
    if (writeReclasRas)
      writeRaster(r[[lay]], filename = rasNameFinal, format = "GTiff",
                  overwrite = TRUE)

    if(upload){
      if (is.null(folderID)) stop("Please provide folderID when upload == TRUE")
      googledrive::drive_upload(file.path(outputFolder, rasNameFinal),
                                path = googledrive::as_id(folderID))
    }
    message("Finished RSF like plot for ", rasName, " year ", y)
    return(r[[lay]])
    })
message(crayon::green("Finished RSF like plots for all scenarios and years"))
  return(stk)
}


