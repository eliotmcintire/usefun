#' extractDisturbanceFast extracts disturbance of anthropogenic and fire
#'
#' @param ageMap RasterLayer. Map with forest age.
#' @param caribouShapefile Shapefile with polygons for which we want to calculate lambda for the caribou demographic model.
#' @param recoveryTime numeric. Recovery time in years that the forest needs to support Caribou. Default = 40.
#' @param anthropogenicLayer Anthropogenic disturbance (raster) layer. Currently, road density layer used for both RSF and demographic models.
#' @param waterRaster Raster layer indicating water bodies.
#' @param rasterToMatch RasterLayer template for these layers to match.
#'
#' @return A list of the anthropogenic and fire disturbances as percent (0-100)
#'
#' @author Tati Micheletti
#' @export
#' @include calculatePixelsInaRule.R
#' @importFrom crayon red
#' @importFrom raster raster stack
#' @importFrom reproducible postProcess
#' @rdname extractDisturbanceFast
#'
extractDisturbanceFast <- function(ageMap,
                               caribouShapefile,
                               recoveryTime = 40,
                               anthropogenicLayer = NULL,
                               waterRaster,
                               rasterToMatch){
  # Make sure that the layers align. This will only happen once until the anthropogenic layer becomes dynamic.
  areStackable <- TRUE
tryCatch({
  invisible(raster::stack(waterRaster, anthropogenicLayer, ageMap, rasterToMatch))
}, error = function(e){
  areStackable <- FALSE
})
  if (!areStackable){
    waterRaster <- reproducible::postProcess(x = waterRaster,
                                             rasterToMatch = rasterToMatch,
                                             destinationPath = tempdir(),
                                             filename2 = NULL)

    ageMap <- reproducible::postProcess(x = ageMap,
                                             rasterToMatch = rasterToMatch,
                                             destinationPath = tempdir(),
                                             filename2 = NULL)
    if (!is.null(anthropogenicLayer)){
      anthropogenicLayer <- reproducible::postProcess(x = anthropogenicLayer,
                                                      rasterToMatch = rasterToMatch,
                                                      maskWithRTM = TRUE,
                                                      destinationPath = tempdir(),
                                                      filename2 = NULL)
    }
    areStackable <- TRUE
    tryCatch({
      invisible(raster::stack(waterRaster, anthropogenicLayer, ageMap, rasterToMatch))
    }, error = function(e){
      areStackable <- FALSE
    })
    if (!areStackable) stop("Something went wrong with the layers for extraction of disturbance (caribouPopGrowthModel). Please debug.")
  }
    # Check for anthropogenic layer and make sure it aligns with the others
    if (!is.null(anthropogenicLayer)){

    # Convert NA background of lineDensity/anthropogenic disturbance to 0, while water stays as NA
    backgroundWithoutWater <- rasterToMatch
    backgroundWithoutWater[waterRaster == 1] <- NA
    backgroundWithoutWater[!is.na(backgroundWithoutWater)] <- 0

    # Fix anthropogenic layer that is NA/1 only. Add zeros to all pixels that are water, basically (which is where we can't build anything)
    backgroundWithoutWater[anthropogenicLayer == 1] <- 1
    anthropogenicLayer <- backgroundWithoutWater
  } else {
    message(crayon::red("anthropogenicLayer is NULL. The prediction will assume anthropogenic disturbances do not exist"))
    anthropogenicLayer <- rasterToMatch
    anthropogenicLayer[anthropogenicLayer == 1] <- 0
    }

  # Extract the caribou shapefile values by fasterizing it. Way faster than raster::extract
    caribouShapefile <- reproducible::postProcess(x = caribouShapefile, rasterToMatch = ageMap,
                                                    destinationPath = tempdir(), filename2 = NULL)
    caribouShapefileSF <- sf::st_as_sf(caribouShapefile)
      nm <- if (!is.null(caribouShapefile$NAME)) "NAME" else "Name"
    caribouShapefileSF$ID <- as.numeric(seq(1:length(caribouShapefileSF[[nm]])))
    caribouShapefileRas <- fasterize::fasterize(sf = caribouShapefileSF, raster = ageMap, field = "ID")

    # Extract Fire
    listExtr <- lapply(X = caribouShapefileSF$ID, FUN = function(pol){
      # For fire:
      # From the age map calculate for each polygon the total amount of pixels that had fires
      # over the total number of pixels "available" to burn (non-NA, as NA can be cities/water/etc).
      # Then multiply by 100 to work with %.
      # We are calculating percent disturbance ONLY for those pixels that are forest/within BCR 6 NWT
          percentFire <- calculatePixelsInaRule(ras = ageMap,
                                              rule = "<= recoveryTime", # Need to be a character string of the rule
                                              pol = pol,
                                              shp = caribouShapefileRas,
                                              recoveryTime = recoveryTime)
          if (percentFire$percentDisturbance < 0 | percentFire$percentDisturbance > 100 | is.na(percentFire$percentDisturbance)){
            print("Something went wrong with the fire distubance calculation. Value is either negative or above 100%. Please debug.")
            browser() # Check pol
          }  # Data sanity check
          # ~~~~~~~~~~~~~~~~~~~
      # For anthropogenic:
      # From the anthropo layer we calculate for each polygon the total amount of pixels that had disturbances
      # over the total number of pixels "available" to have it (non-NA when na is JUST WATER).
      # Then multiply by 100 to work with %.
          percentAnthopo <- calculatePixelsInaRule(ras = anthropogenicLayer,
                                                    rule = "> 0", # Need to be a character string of the rule
                                                    pol = pol,
                                                    shp = caribouShapefileRas)
          if (percentAnthopo$percentDisturbance < 0 | percentAnthopo$percentDisturbance > 100 | is.na(percentAnthopo$percentDisturbance)){
            print("Something went wrong with the anthropogenic distubance calculation. Value is either negative or above 100%. Please debug.")
            browser()
          }
            # ~~~~~~~~~~~~~~~~~~~
      # For total disturbance:
      # Need to overlay the disturbances and extract the total
          isDistrubance <- percentFire$isDisturbance | percentAnthopo$isDisturbance
          totPixelsNotNADist <- max(percentFire$totPixelsNotNA, percentAnthopo$totPixelsNotNA) # Total number of pixels that
          # can have any type of disturbance, so need to maximize (i.e. I might not be able to have fire, but
          # I sure could have anthropogenic)
          cummDist <- sum(isDistrubance, na.rm = TRUE)
          percentCumm <- 100*(cummDist/totPixelsNotNADist)
          if (percentCumm > 100){
            message(crayon::red(paste0("Total disturbance for polygon ", crayon::cyan(caribouShapefileSF[[nm]][pol]), " presented ", crayon::cyan(paste0(round(percentCumm, 0),"%")),
" pixels disturbed. \nThis might need some digging... For now, converting to 100%")))
            percentCumm <- 100
          }  # Data sanity check
          if (percentCumm < 0 ){  # Data sanity check
              print("Something went wrong with the total distubance calculation. Value is either negative or above 100%. Please debug.")
              browser()
          }

              # Making the data.frame
          df <- data.frame(DH_Fire = percentFire$percentDisturbance,
                           DH_Anthro = percentAnthopo$percentDisturbance,
                           DH_Total = percentCumm)
          return(df)
    })
    #Naming both fire and anthro disturbances
    names(listExtr) <- caribouShapefile[[nm]]
  return(listExtr)
}
