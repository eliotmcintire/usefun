#' @title
#' Preparing study area based on BCR and Canadian or American provinces or states.
#'
#' @description
#' Downloads, reprojects, crops and masks to speficic areas in canada such as:
#' BCR6, random areas, provinces and territories, or any of the last in the
#' BCR6.
#'
#' @param bcr               Numeric. Bird Conservation Region in North America that you want to crop for.
#'                          Default is `NULL`. If `NULL``, it returns only the shapefile of the province.
#' @param province          Character string. Province or territory. Default is `NULL`. If null, returns
#'                          the map of ??.
#' @param country           Character string. 3 letter ISO for a specific country. The complete list of
#'                          countries can be seen by calling `raster::getData('ISO3')
#'
#' @param ...               Arguments to be passed to `prepInputs` or `Cache` (i.e. targetFile,
#'                          cacheId, destinationPath, overwrite, etc.).
#' @details If you provide a bcr that is outside of a province area, the object returned is `NULL`.
#'
#' @author Tati Micheletti
#' @export
#' @importFrom magrittr %>%
#' @importFrom reproducible prepInputs postProcess
#' @importFrom raster getData subset
#' @importFrom crayon yellow
#' @rdname provinceBCRStudyArea
#'

provinceBCRStudyArea <- function(bcr = NULL, province = NULL, country, ...) {
  dots <- list(...)
  if (is.null(country) | length(country) != 1){
    stop("Please choose one country: 'USA' for United States of America, 'CAN' for Canada, etc.
         The complete list of countries can be seen by calling `raster::getData('ISO3')`")
  }
  if (is.null(bcr) && is.null(province)){
    message(crayon::yellow("BCR and province are NULL. Returning the map of ", country))
    cntry <- raster::getData(name = "GADM", download = TRUE, country = country, level = 1)
    return(cntry)
  } else {
    if (is.null(bcr) && !is.null(province)){
      message(crayon::yellow("bcr is NULL. Returning the map of ", country, " for ", paste(province, collapse = "; ")))
      provs <- raster::getData(name = "GADM", download = TRUE, country = country, level = 1)  %>%
        raster::subset(NAME_1 %in% province)
      if (length(provs) == 0) stop("The province(s) ", paste(province, collapse = "; "), " doesn't(don't) exist in ", country)
      return(provs)
    } else {
      if (!is.null(bcr) && is.null(province)){
        message(crayon::yellow("province is NULL. Returning the map of BCR ", bcr, " for ", country))
        dots$url <- "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip"
        BCRshp <- do.call(reproducible::prepInputs, dots) %>%
          raster::subset(BCR %in% bcr)
        if (length(BCRshp) == 0) stop("The BCR ", bcr, " doesn't exist in ", country)
        return(BCRshp)
      } else {
        message(crayon::yellow("Both BCR and province provided. Returning the map of BCR ", bcr, " for ",
                               paste(province, collapse = "; "), " in ", country))
        provs <- raster::getData(name = "GADM", download = TRUE, country = country, level = 1)  %>%
          raster::subset(NAME_1 %in% province)
        if (length(provs) == 0) stop("The province(s) ", paste(province, collapse = "; "), " doesn't(don't) exist in ", country)
        dots$url <- "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip"
        BCRshp <- do.call(reproducible::preProcess, dots)
        dots$x <- raster::shapefile(BCRshp$targetFilePath)
        dots$studyArea <- provs
        BCRshp <- do.call(reproducible::postProcess, dots) %>%
          raster::subset(BCR %in% bcr)
        if (length(BCRshp) == 0) stop("The BCR ", BCR, " doesn't exist in ", country)
        return(BCRshp)
      }
    }
  }
  }
