prepareClimateLayers <- function(pathInputs = NULL,
                                 variables = NULL,
                                 years = NULL,
                                 GDriveFolder = NULL,
                                 climateFilePath = NULL,
                                 fileResolution = NULL,
                                 authEmail = NULL,
                                 RCP = NULL, # 45
                                 climateModel = NULL, # CanESM2
                                 ensemble = NULL, # r11i1p1
                                 rasterToMatch = NULL,
                                 studyArea = NULL,
                                 model = NULL, # 'birds', 'fireSense'. If you wanna provide other variables, don't use birds or fireSense here.
                                 doughtMonths = 4:9, # months for fireSense to calculate MonthDoughtCode (MDC)
                                 returnCalculatedLayersForFireSense = FALSE){ # If TRUE, it returns the calculated MDC layers already, not the original stack with Tmax and PPT
# TODO metadata!

  # File names': projection_resolution_year
  # This function returns a list of all years, with each year being the local path for the raster stack that contains all variables
  library("usefun")
  library("raster")
  library("googledrive")
  library("reproducible")

    googledrive::drive_auth(email = authEmail)
  # 1. Make sure it has all defaults
    if (doughtMonths != 4:9){
      stop("Drought calculation for months other than April to June is not yet supported") # TODO
    }
    if (is.null(model)){
    stop("Please provide the model for which you are creating the variables (i.e. 'birds', 'fireSense', 'NA')")
    }
    if (is.null(rasterToMatch)){
      message("rasterToMatch is NULL, no post processing will happen")
    }
    if (is.null(ensemble)){
      ensemble <- "r11i1p1"
      message("ensemble is NULL, using default ", ensemble)
    }
    if (is.null(RCP)){
    RCP <- paste0("RCP", 45)
    message("RCP is NULL, using default ", RCP)
    }
    if (is.null(climateModel)){
      climateModel <- "CanESM2"
      message("climateModel is NULL, using default ", climateModel)
    }
  if (is.null(GDriveFolder)){
    message("GDriveFolder is NULL, using default folder (https://drive.google.com/open?id=1Ww_GYtxB9ZALGjjJnML_F4Mgl8UR-zl9).
            Ignore this message if you have already the file locally.")
    GDriveFolder <- "https://drive.google.com/open?id=1Ww_GYtxB9ZALGjjJnML_F4Mgl8UR-zl9"
  }
  if (is.null(pathInputs)){
    message(paste0("pathInputs is NULL, using default temp folder: ", tempdir()))
    pathInputs <- tempdir()
  }
  if (is.null(climateFilePath)){
    climateFilePath <- "https://drive.google.com/open?id=1wcgytGJmfZGaapZZ9M9blfGa-45eLVWE"
    threeArcMin <- TRUE
    message("This function uses CanESM2 RCP45 resolution 3ArcMinute (~3 x 5Km).", "\nFile downloaded from climateFilePath URL: ", climateFilePath,").",
            "\nIf another layer is intended, please provide the URL to the file.", "\n(This function does not download the files from ClimateNA)")
  }
  if (is.null(fileResolution)){ # For naming purposes mostly. Should only be passed if user is also passing the path to the complete file
    if (threeArcMin){
      fileResolution <- "3ArcMin"
      message("fileResolution is NULL. Using the original")
    } else {
      stop("Please inform the resolution in fileResolution when passing a climate archive file path")
    }
  }
  if (is.null(years)){
    years <- 2011:2100
      message("Years is NULL. Using the original time series (2011-2100)")
  }
  if (all(is.null(variables), !model %in% c("birds", "fireSense"))){
    variables <- c("AHM", "bFFP", "CMD01", "CMD02", "CMD03", "CMD04", "CMD05",
    "CMD06", "CMD07", "CMD08", "CMD09", "CMD10", "CMD11", "CMD12",
    "CMD", "CMD_at", "CMD_sm", "CMD_sp", "CMD_wt", "DD_0_01", "DD_0_02",
    "DD_0_03", "DD_0_04", "DD_0_05", "DD_0_06", "DD_0_07", "DD_0_08",
    "DD_0_09", "DD_0_10", "DD_0_11", "DD_0_12", "DD_0", "DD_0_at",
    "DD_0_sm", "DD_0_sp", "DD_0_wt", "DD_18_01", "DD18_01", "DD_18_02",
    "DD18_02", "DD_18_03", "DD18_03", "DD_18_04", "DD18_04", "DD_18_05",
    "DD18_05", "DD_18_06", "DD18_06", "DD_18_07", "DD18_07", "DD_18_08",
    "DD18_08", "DD_18_09", "DD18_09", "DD_18_10", "DD18_10", "DD_18_11",
    "DD18_11", "DD_18_12", "DD18_12", "DD_18", "DD18", "DD_18_at",
    "DD18_at", "DD_18_sm", "DD18_sm", "DD_18_sp", "DD18_sp", "DD_18_wt",
    "DD18_wt", "DD5_01", "DD5_02", "DD5_03", "DD5_04", "DD5_05",
    "DD5_06", "DD5_07", "DD5_08", "DD5_09", "DD5_10", "DD5_11", "DD5_12",
    "DD5", "DD5_at", "DD5_sm", "DD5_sp", "DD5_wt", "eFFP", "EMT",
    "Eref01", "Eref02", "Eref03", "Eref04", "Eref05", "Eref06", "Eref07",
    "Eref08", "Eref09", "Eref10", "Eref11", "Eref12", "Eref", "Eref_at",
    "Eref_sm", "Eref_sp", "Eref_wt", "EXT", "FFP", "MAP", "MAR",
    "MAT", "MCMT", "MSP", "MWMT", "NFFD01", "NFFD02", "NFFD03", "NFFD04",
    "NFFD05", "NFFD06", "NFFD07", "NFFD08", "NFFD09", "NFFD10", "NFFD11",
    "NFFD12", "NFFD", "NFFD_at", "NFFD_sm", "NFFD_sp", "NFFD_wt",
    "PAS01", "PAS02", "PAS03", "PAS04", "PAS05", "PAS06", "PAS07",
    "PAS08", "PAS09", "PAS10", "PAS11", "PAS12", "PAS", "PAS_at",
    "PAS_sm", "PAS_sp", "PAS_wt", "PPT01", "PPT02", "PPT03", "PPT04",
    "PPT05", "PPT06", "PPT07", "PPT08", "PPT09", "PPT10", "PPT11",
    "PPT12", "PPT_at", "PPT_sm", "PPT_sp", "PPT_wt", "Rad01", "Rad02",
    "Rad03", "Rad04", "Rad05", "Rad06", "Rad07", "Rad08", "Rad09",
    "Rad10", "Rad11", "Rad12", "Rad_at", "Rad_sm", "Rad_sp", "Rad_wt",
    "RH01", "RH02", "RH03", "RH04", "RH05", "RH06", "RH07", "RH08",
    "RH09", "RH10", "RH11", "RH12", "RH", "RH_at", "RH_sm", "RH_sp",
    "RH_wt", "SHM", "Tave01", "Tave02", "Tave03", "Tave04", "Tave05",
    "Tave06", "Tave07", "Tave08", "Tave09", "Tave10", "Tave11", "Tave12",
    "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", "TD", "Tmax01", "Tmax02",
    "Tmax03", "Tmax04", "Tmax05", "Tmax06", "Tmax07", "Tmax08", "Tmax09",
    "Tmax10", "Tmax11", "Tmax12", "Tmax_at", "Tmax_sm", "Tmax_sp",
    "Tmax_wt", "Tmin01", "Tmin02", "Tmin03", "Tmin04", "Tmin05",
    "Tmin06", "Tmin07", "Tmin08", "Tmin09", "Tmin10", "Tmin11", "Tmin12",
    "Tmin_at", "Tmin_sm", "Tmin_sp", "Tmin_wt")
    message(crayon::red(paste0("variables is NULL, using all variables available in the climate layers. ",
                   "ATTENTION! This might be very slow!", " Total number of variables loaded: ", length(variables))))
  }
    if (model == "birds"){
      variables <- c("AHM", "bFFP", "CMD", "DD_0", "DD_18",
                     "DD18", "DD5", "eFFP", "EMT", "EXT", "FFP",
                     "MAP", "MAT", "MCMT", "MSP", "MWMT", "NFFD",
                     "PAS", "PPT_sm", "PPT_wt", "SHM", "Tave_sm",
                     "Tave_wt", "TD")
    }
    if (model == "fireSense"){
      variables <- c(paste0("Tmax0", doughtMonths), paste0("PPT0", doughtMonths))
      #PPT, Tmax
    }

  # 2. Check if we have the years chosen (we should lapply through years)

yearsList <- lapply(X = years, FUN = function(y){
  if (all(model == "fireSense", isTRUE(returnCalculatedLayersForFireSense))){
    fileName <- file.path(pathInputs, paste0(paste(climateModel, RCP, ensemble,
                                                   fileResolution, model, "Calc", y, sep = "_"), ".grd"))
  } else {
    fileName <- file.path(pathInputs, paste0(paste(climateModel, RCP, ensemble, fileResolution, model, y, sep = "_"), ".grd"))
    dType <- "INT4S"
  }
  if (file.exists(fileName)) {
    message(crayon::green(paste0(fileName, " exists. Returning the raster stack")))
    # A. If we have the year, return
    return(raster::stack(fileName))
  } else {
    # B. If we don't have the year LOCALLY, see if we have in the cloud
    message(crayon::yellow(paste0(fileName, " does not exist locally. Checking the cloud... ")))
    filesInFolder <- googledrive::drive_ls(path = googledrive::as_id(GDriveFolder), recursive = FALSE)
    if (paste0(basename2(tools::file_path_sans_ext(fileName)), ".zip") %in% filesInFolder$name){
      rw <- which(filesInFolder$name == paste0(basename2(tools::file_path_sans_ext(fileName)), ".zip"))
      # googledrive::drive_download(file = as_id(filesInFolder$id[rw]),
      #                             path = file.path(pathInputs, paste0(basename2(tools::file_path_sans_ext(fileName)), ".zip")))
      reproducible::preProcess(url = paste0("https://drive.google.com/open?id=", filesInFolder$id[rw]),
                               alseExtract = "similar",
                               archive = paste0(basename2(tools::file_path_sans_ext(fileName)), ".zip"),
                               destinationPath = pathInputs) # Currently not really working. Giving error:
      # Error in grepl(archive, pattern = destinationPathUser) :
      #   object 'destinationPathUser' not found
      message(crayon::green(paste0(fileName, " now exists locally. Returning the raster stack with the following variables: ", variables)))
      return(raster::stack(fileName))
    } else {
      # B1. If we don't have it in the cloud, (use the years in file name), make it from the original layer.
      message(crayon::yellow(paste0(fileName, " does not exist locally nor in the cloud. Creating layers... ")))
      fullDatasetName <- googledrive::drive_get(as_id(climateFilePath))$name
      if (!file.exists(file.path(pathInputs, fullDatasetName))){
        message(crayon::red(paste0(fullDatasetName, " does not exist in your pathInputs (", pathInputs,
                                   "). Downloading, unzipping and creating layers... This might take a few hours")))
        reproducible::preProcess(url = climateFilePath,
                                 filename2 = fullDatasetName,
                                 destinationPath = pathInputs) # Currently not working well. Downloads, but doesn't unzip. Needs to be implemented in prepInputs
        # TEMPORARY SYSTEM CALL WITH THE OPTION x - this assumes you have '7za'
        system(paste0("7za x ", file.path(pathInputs, fullDatasetName))) # ==> This hasn't been tested with the full file path. Just guessing it works... done by hand
      }
      datasetsPath <- file.path(pathInputs, tools::file_path_sans_ext(fullDatasetName))
      folders <- setdiff(list.dirs(path = datasetsPath), datasetsPath) # excluding original folder from the variable
      currentYearsFolder <- usefun::grepMulti(x = folders, patterns = c(climateModel, RCP, ensemble, y))
      currentYearFiles <- list.files(currentYearsFolder)
      filesToLoad <- paste0(variables, ".asc")
      variablesStack <- raster::stack(lapply(X = filesToLoad, FUN = function(variable){
        ras <- raster::raster(x = file.path(currentYearsFolder, variable))
        crs(ras) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
        if (any(!is.null(rasterToMatch), !is.null(studyArea)))
          ras <- reproducible::postProcess(x = ras, studyArea = studyArea, rasterToMatch = rasterToMatch,
                                           filename2 = NULL)
        return(ras)
      })
      )
      # For fireSense, do the calculations already'
      if (all(model == "fireSense", isTRUE(returnCalculatedLayersForFireSense))){
        # Day length adjustement L_f in Drought Code (taken from Van Wagner 1987)
        L_f <- function(month){
          c('4' = 0.9,
            '5' = 3.8,
            '6' = 5.8,
            '7' = 6.4,
            '8' = 5.0,
            '9' = 2.4)[[as.character(month)]] # TODO [ FIX ] Update for all months, check latitude problem. Ideally, bring original table in here.
        }

        nDays <- function(month){
          c('4' = 30,
            '5' = 31,
            '6' = 30,
            '7' = 31,
            '8' = 31,
            '9' = 30)[[as.character(month)]]
        }

        MDC06 <- reproducible::Cache(calc, climateLayers, fun = function(x){
          MDC_0 <- 0
          for (month in doughtMonths){
            PPT <- x[[paste0("PPT0", month)]]
            Tmax <- x[[paste0("Tmax0", month)]]
            MDC_m <- pmax(MDC_0 + .25 * nDays(month) * (.36 * Tmax + L_f(month)) -
                            400 * log(1 + 3.937 * .83 * PPT / (800 * exp(-MDC_0/400))) +
                            .25 * nDays(month) * (.36 * Tmax + L_f(month)),0)
            MDC_0 <- pmax((MDC_0 + MDC_m) / 2, 0)
          }
          return(MDC_0)
        })
        variablesStack <- MDC06
        dType <- reproducible::assessDataType(variablesStack)
      }
      writeRaster(variablesStack, filename = fileName, datatype = dType)
      variablesStack <- raster::stack(fileName)
      filesToUpload <- usefun::grepMulti(x = list.files(dirname(fileName), full.names = TRUE),
                                         patterns = basename(tools::file_path_sans_ext(fileName)))
      zip(zipfile = tools::file_path_sans_ext(fileName), files = filesToUpload)
      googledrive::drive_upload(media = paste0(tools::file_path_sans_ext(fileName), ".zip"),
                                path = googledrive::as_id(GDriveFolder))
      return(raster::stack(fileName))
    }
  }
})
names(yearsList) <- paste0("year", years)
return(yearsList)
}
