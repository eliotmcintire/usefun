#' classifyWetlands classifies wetlands (really!) using the wetlands layer set as input and a either LCC05 or LCC2010
#'
#' @param LCC numeric. 2005 (250m resolution) or 2010 (30m resolution) landcover  rasters.
#'
#' @param wetLayerInput Which wetland should be used as input (raster with projection).
#'                      It was originally designed to work with the DUCKS Unlimited Waterland
#'                      layer (30m) but can work with any waterlayers that have the following code:
#'                      possibleLakes == 0
#'                      water bodies == 1
#'                      wetlands == 2
#'                      uplands > 2
#'
#'  @param pathData Where the layers are stored and/or should be saved to
#'
#'  @param studyArea If the layer should be cropped and masked after classification. Optional.
#'
#'  @param RasterToMatch raster to match the new layer after classification to. Optional.
#'
#'
#' @return As with \code{\link[archivist]{cache}}, returns the value of the
#' function call or the cached version (i.e., the result from a previous call
#' to this same cached function with identical arguments).
#'
#' @author Tati Micheletti
#' @export
#' @importFrom LandR prepInputsLCC
#' @importFrom raster raster projectRaster extract
#' @importFrom data.table data.table
#' @importFrom reproducible prepInputs postProcess Require
#' @rdname classifyWetlands

classifyWetlands <- function(LCC,
                             wetLayerInput,
                             pathData,
                             studyArea = NULL,
                             RasterToMatch = NULL){
  Require("LandR")
  Require("raster")

  # Load LCC layer
  rasLCC <- LandR::prepInputsLCC(year = LCC, destinationPath = pathData,
                                 studyArea = studyArea, filename2 = paste0("LCC", LCC),
                                 format = "GTiff", overwrite = TRUE)
  if (as.character(crs(rasLCC))!=as.character(crs(wetLayerInput))){
    rasLCC <- raster::projectRaster(from = rasLCC, crs = crs(wetLayerInput))
  }
  # get xy of all pixels in DUCKS that are 1, 2 or 3+
  possibleLakes <- which(values(wetLayerInput)==0) ###
  watIndex <- which(values(wetLayerInput)==1)
  wetIndex <- which(values(wetLayerInput)==2)
  upIndex <- which(values(wetLayerInput)>2)

  # extract the pixel numbers of these xy from LCC05.
  lakes <- xyFromCell(wetLayerInput, possibleLakes) ###
  wetLocations <- xyFromCell(wetLayerInput, wetIndex)
  watLocations <- xyFromCell(wetLayerInput, watIndex)
  upLocations <- xyFromCell(wetLayerInput, upIndex)
  lcc05Lakes <- as.data.table(raster::extract(rasLCC, lakes, cellnumbers = TRUE)) ###
  lcc05Wetlands <- as.data.table(raster::extract(rasLCC, wetLocations, cellnumbers = TRUE))
  lcc05Water <- as.data.table(raster::extract(rasLCC, watLocations, cellnumbers = TRUE))
  lcc05Uplands <- as.data.table(raster::extract(rasLCC, upLocations, cellnumbers = TRUE))

  # TM (FEB 24th): This below only makes sense if the original wetLayerInput raster is the original 30m resolution.
  # This way we would have more than 1 pixel ID in the LCC represented by more than 1 pixel in the DUCKS layer...
  # Using the default one for the NWT, we are never going to have more than 1 pixel here
  # # Calculate how many times each pixel index exists
  countLake <- lcc05Lakes[, .N, by = cells]
  countWet <- lcc05Wetlands[, .N, by = cells]
  countWat <- lcc05Water[, .N, by = cells]
  countUp <- lcc05Uplands[, .N, by = cells]

  # TM (FEB 24th): Now this looks weird/wrong. we can only fit 8^2 (64) 30m DUCKS pixels in 250m resolution
  # LCC05. This means that the 50% is not 50, but 64/2.
  # I should calculate the 50% based on both rasters resolution
  # This is how many pixels make for 50% of the total number of pixels. If the resolutions
  # are the same, this will be 0.5. If < 1, then the default is to round up to 1
  fiftyRule <- ceiling((round(unique(res(rasLCC))/unique(res(wetLayerInput)), 0)^2)/2)

  # If more than 50% of the pixels in the LCC are classified in , that pixel index in LCC05 is actually a wetland
  lccLakeIndex <- countLake[N >= fiftyRule, cells]
  lccWetIndex <- countWet[N >= fiftyRule, cells]
  lccWatIndex <- countWat[N >= fiftyRule, cells]
  lccUpIndex <- countUp[N >= fiftyRule, cells]

  # Generate and return the mask layer
  lccWetLayer <- rasLCC
  lccWetLayer[!is.na(lccWetLayer)] <- -1

  # Lakes and water
  lccWetLayer[lccLakeIndex] <- 1
  lccWetLayer[lccWatIndex] <- 1

  # Mask it with RTM
if (exists("RasterToMatch")){
  prepRTM <- reproducible::postProcess(RasterToMatch, rasterToMatch = lccWetLayer, filename2 = NULL)
} else
  prepRTM <- NULL

  lccWetLayer <- reproducible::postProcess(lccWetLayer, rasterToMatch = prepRTM, maskWithRTM = TRUE, filename2 = NULL)
  lccWetLayer[lccWetLayer == 0] <- NA

  # Do uplands and wetlands
  lccWetLayer[lccWetIndex] <- 2
  lccWetLayer[lccUpIndex] <- 3
  lccWetLayer[lccWetLayer == -1] <- NA

  storage.mode(lccWetLayer[]) <- "integer"

  return(lccWetLayer)
}
