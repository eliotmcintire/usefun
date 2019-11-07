# helpers for bootstrapPercentChanges

.calculateSignificantChangesInBirds <- function(dataPath, years, species = NULL, pixelBased = TRUE,
                                               sampleSize = "auto", redFactorTimes = 15,
                                               studyArea = NULL, repetition = NULL){ # limited to 2 years!
  if (is.null(species)){
    spFiles <- grepMulti(x = list.files(dataPath), patterns = c(years[1],".tif"))
    splittedAllNames <- strsplit(x = spFiles, split = "predicted") # including anything else i.e. diversity rasters
    splittedAllNames2 <- splittedAllNames[!splittedAllNames %in% usefun::grepMulti(x = splittedAllNames, patterns = paste0("_", years[1]))]
    splitted <- unlist(lapply(splittedAllNames, function(birdFilename){
      if (length(birdFilename)==1) return(NULL)
      return(birdFilename[length(birdFilename)])
    }))
    species <- substrBoth(strng = splitted, howManyCharacters = 4, fromEnd = FALSE)
  }
  if (length(years)>2) stop("Currently this function only compares 2 years")
  tableOfChanges <- data.table::rbindlist(lapply(species, function(bird){
    birdInyears <- lapply(years, function(y){
      rasPath <- grepMulti(x = list.files(dataPath, recursive = TRUE, full.names = TRUE), patterns = c(bird, y))
      if (length(rasPath)==0){
        stop("The raster file for ", bird, " for year ", y, " wasn't found. Maybe not all species were run for all years? ",
             "You can pass specific species to be ran using the argument 'species'")
      }
      rasValue <- data.table::data.table(raster::getValues(raster::raster(rasPath)))
      return(rasValue)
    })
    names(birdInyears) <- years
    dtForTest <- usefun::cbindFromList(birdInyears)
    if (!is.null(studyArea)){
      # rasPath <- usefun::grepMulti(x = list.files(dataPath, recursive = TRUE, full.names = TRUE), patterns = c(bird)) # JUST A TEMPLATE!
      # # location <- reproducible::Cache(prepStudyAreaForBirds, studyArea = studyArea, dataPath = dataPath, RTMpath = rasPath[1],
      # #                                 userTags = c("object:location", "purpose:Edehzhie"))
      dtForTest <- cbind(dtForTest, data.table::data.table(location = studyArea))
      uniqueLocations <- unique(dtForTest$location)[!is.na(unique(dtForTest$location))]
      byLocationModelList <- lapply(uniqueLocations, function(locality){
        dtForTestloc <- dtForTest[location == locality,]
        t <- Sys.time()
        mod <- .calculatePvalueOfRasters(dtForTest = dtForTestloc,
                                        pixelBased = pixelBased, sampleSize = sampleSize,
                                        species = species,
                                        redFactorTimes = redFactorTimes,  bird = bird)
        message(crayon::white("Finished calculations for ", bird,
                              " (locality ", locality, " repetition ", repetition,") ",
                              "Elapsed Time: ", Sys.time() - t))
        return(mod)
      })
      names(byLocationModelList) <- uniqueLocations
      dt <- data.table::rbindlist(lapply(uniqueLocations, function(loc){
        direction <- ifelse(byLocationModelList[[loc]]$p.value>=0.05,"no change",
                            ifelse(byLocationModelList[[loc]]$mean1 < byLocationModelList[[loc]]$mean2,
                                   "increased","decreased"))
        dt <- data.table::data.table(species = bird, tTest = byLocationModelList$p.value,
                                     result = direction, location = loc)
      }))
    } else {
      r <- .calculatePvalueOfRasters(dtForTest = dtForTest,
                                    pixelBased = pixelBased, sampleSize = sampleSize,
                                    species = species,
                                    redFactorTimes = redFactorTimes, bird = bird)
      direction <- ifelse(r$p.value>=0.05,"no change",
                          ifelse(r$mean1 < r$mean2, "increased","decreased"))
      dt <- data.table::data.table(species = bird, tTest = r$p.value, result = direction)
    }
    return(dt)
  })
  )
  return(tableOfChanges)
}

.calculatePercentageChanges <- function(changesTable, column){
  if (is(changesTable, "list")){
    changesTable <- rbindlist(lapply(X = names(changesTable), FUN = function(simulation){
      tb <- changesTable[[simulation]]
      tb$simulation <- simulation
      return(tb)
    })
    )
  }
  if ("location" %in% names(changesTable)){
    if ("simulation" %in% names(changesTable)){
      stop("You have provided both locations and different simulations/scenarios for this analysis. This is still not supported.")
    }
    dt <- data.table::rbindlist(lapply(unique(changesTable$location), function(locality){
      changesVector <- table(changesTable[location == locality, ..column,])
      dt <- data.table::data.table(direction = names(changesVector),
                                   value = as.numeric(changesVector),
                                   percent = 100*(as.numeric(changesVector)/NROW(changesTable[location == locality, ..column,])),
                                   location = locality)
    }))
  } else {
    if ("simulation" %in% names(changesTable)){
      dt <- data.table::rbindlist(lapply(unique(changesTable$simulation), function(scenario){
        changesVector <- table(changesTable[simulation == scenario, ..column,])
        dt <- data.table::data.table(direction = names(changesVector),
                                     value = as.numeric(changesVector),
                                     percent = 100*(as.numeric(changesVector)/NROW(changesTable[simulation == scenario, ..column,])),
                                     simulation = scenario)
      }))
    } else {
      changesVector <- table(changesTable[, ..column])
      dt <- data.table::data.table(direction = names(changesVector),
                                   value = as.numeric(changesVector),
                                   percent = 100*(as.numeric(changesVector)/NROW(changesTable)))
    }
  }
  return(dt)
}

.whichSpeciesChange <- function(changesTable){
  if (!is(changesTable, "list"))
    stop("changesTable needs to be a list, even if of only one element")
  listOfChanges <- lapply(1:length(changesTable), function(repetition){
    tb <- changesTable[[repetition]]
    if ("location" %in% names(tb)){
      uniqueLocations <- unique(tb$location)
      dt  <- lapply(uniqueLocations, function(locality){
        dt <- .whichSpeciesChangeDT(tb = tb[location == locality])
        return(dt)
      })
      names(dt) <- paste0("location", uniqueLocations)
    } else {
      dt <- .whichSpeciesChangeDT(tb = tb)
    }
    return(dt)
  })
  names(listOfChanges) <- paste0("repetition", (1:length(changesTable)))
  # Reorder the three elements, needs to make a reduce for each
  if (is(changesTable, "list")){
    if ("location" %in% names(changesTable[[1]])){
      tb <- changesTable[[1]]
      uniqueLocations <- unique(tb$location)
      allLocations <- lapply(uniqueLocations, function(locality) {
        listOfChangesLocal <- lapply(listOfChanges, '[[', paste0("location", locality))
        increased <- lapply(listOfChangesLocal, '[[', "increased")
        decreased <- lapply(listOfChangesLocal, '[[', "decreased")
        noChange <- lapply(listOfChangesLocal, '[[', "noChange")

        increasedConsistent <- Reduce(intersect, increased)
        decreasedConsistent <- Reduce(intersect, decreased)
        noChangeConsistent <- Reduce(intersect, noChange)

        return(list(increased = increasedConsistent,
                    decreased = decreasedConsistent,
                    noChange = noChangeConsistent))
      })
      names(allLocations) <- paste0("location", uniqueLocations)
      return(allLocations)
    }
  } else {
    increased <- lapply(listOfChanges, '[[', "increased")
    decreased <- lapply(listOfChanges, '[[', "decreased")
    noChange <- lapply(listOfChanges, '[[', "noChange")

    increasedConsistent <- Reduce(intersect, increased)
    decreasedConsistent <- Reduce(intersect, decreased)
    noChangeConsistent <- Reduce(intersect, noChange)

    return(list(increased = increasedConsistent, decreased = decreasedConsistent, noChange = noChangeConsistent))
  }
}

.whichSpeciesChangeDT <- function(tb){
  if (!is(tb, "list")){
    inc <- tb[result == "increased", species]
    dec <- tb[result == "decreased", species]
    noChange <- tb[result == "no change", species]
    dt <- list(increased = inc, decreased = dec, noChange = noChange)
  } else {
    dt  <- lapply(names(tb), function(simul){
      tbS <- tb[[simul]]
      inc <- tbS[result == "increased", species]
      dec <- tbS[result == "decreased", species]
      noChange <- tbS[result == "no change", species]
      dt <- list(increased = inc, decreased = dec, noChange = noChange)
      return(dt)
    })
    names(dt) <- names(tb)
  return(dt)
  }
}

.calculatePvalueOfRasters <- function(dtForTest, pixelBased = FALSE,
                                      sampleSize, species, redFactorTimes,
                                      bird){
  if (pixelBased){
    if (!is.null(sampleSize)){
      if (all(sampleSize == "auto", bird == species[[1]])){ # NEEDS TO HAPPEN ONLY FOR THE FIRST BIRD SPECIES AND STAY
        # COMPUTE IDEAL SIZE SAMPLE FOR SAMPLING USING Cohen's D And Hedges G Effect Size
        treatment <- dtForTest[[2]][!is.na(dtForTest[[2]])]
        control <- dtForTest[[1]][!is.na(dtForTest[[1]])]

        ## data and factor
        cohen <- cohen.d(treatment, control, paired = TRUE,
                         conf.level = 0.01, hedges.correction = TRUE)
        limit <- 0.2
        # Effect size	d	Reference
        # Very small	0.01	Sawilowsky, 2009
        # Small	      0.20	Cohen, 1988
        # Medium	    0.50	Cohen, 1988
        # Large	      0.80	Cohen, 1988
        # Very large  1.20	Sawilowsky, 2009
        # Huge	      2.0	Sawilowsky, 2009
        vectorSize <- length(treatment)
        message(crayon::yellow("Calculating ideal sample size using Cohen's D and Hedges'g effect size statistics..."))
        while (cohen$estimate < limit){
          newSample <- sample(x = 1:NROW(treatment),
                              size = vectorSize,
                              replace = FALSE)
          cohen <- cohen.d(treatment[newSample], control[newSample], paired = TRUE,
                           conf.level = 0.01, hedges.correction = TRUE)
          vectorSize <- vectorSize - round(vectorSize/redFactorTimes, 0)
        }
        message(crayon::green(paste0("Ideal sample size calculated as ",
                                     length(sampleSize), "\n with sample size effect calcutated as")))
        print(cohen)
        stop(paste0("Please pass the ideal size calculated", length(sampleSize)," to the module's parameter 'sampleSize' and ",
                    "restart the simulation (i.e. by using 'mySimOut <- restartSpades()'). ",
                    "The current version still doesn't have this process automated"))
      }
      # SAMPLE FROM TABLE AND RUN THE TEST
      dtForTest$ID <- 1:NROW(dtForTest)
      env <- environment()
      if (!exists("sbsetID")){
        if (tryCatch({
          pryr::where(name = "sbsetID", env = env)
          return(FALSE)},
          error = function(e) return(TRUE))){
          assign(x = "sbsetID", value = sample(x = dtForTest[!is.na(dtForTest)[ID]]$ID,
                                               size = sampleSize, replace = FALSE),
                 envir = env)
        } else {
          sbsetID <- get("sbsetID", envir = pryr::where(name = "sbsetID", env = env))
        }
      }
      dtForTest <- dtForTest[sbsetID,]
    }
    test <- wilcox.test(x = dtForTest[[1]], y = dtForTest[[2]],
                        paired = TRUE, alternative = "two.sided")
    p.value <- test$p.value
    mean1 <- mean(dtForTest[[1]], na.rm = TRUE)
    mean2 <- mean(dtForTest[[2]], na.rm = TRUE)
  } else {
    mean1 <- mean(dtForTest[[1]], na.rm = TRUE)
    sd1 <- sd(dtForTest[[1]], na.rm = TRUE)
    sampleSize1 <- sum(!is.na(dtForTest[[1]]))
    mean2 <- mean(dtForTest[[2]], na.rm = TRUE)
    sd2 <- sd(dtForTest[[2]], na.rm = TRUE)
    sampleSize2 <- sum(!is.na(dtForTest[[2]]))
    test <- .tTestMeansSD(mean1 = mean1, mean2 = mean2, sd1 = sd1, sd2 = sd2,
                         sampleSize1 = sampleSize1, sampleSize2 = sampleSize2)

    p.value <- test["pValue"]
  }
  return(list(mean1 = mean1, mean2 = mean2, p.value = p.value))
}

.prepStudyAreaForBirds <- function(studyArea, dataPath){
  if (is(studyArea, "character")){
    studyArea <- prepInputs(url = studyArea, targetFile = "birdRTMEdehzhie.tif",
                            destinationPath = dataPath,
                            userTags = "birdRTMEdehzhie", filename2 = "birdRTMEdehzhie")
  }
  location <- raster::getValues(studyArea)
  location[location == 0] <- NA
  return(location)
}

.tTestMeansSD <- function(mean1, mean2, sd1, sd2,
                         sampleSize1, sampleSize2, m0 = 0,
                         equal.variance = FALSE){
  # FROM Macro: https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
  # mean1, mean2: the sample means
  # sd1, sd2: the sample standard deviations
  # sampleSize1, sampleSize2: the same sizes
  # m0: the null value for the difference in means to be tested for. Default is 0.
  # equal.variance: whether or not to assume equal variance. Default is FALSE.

  if(equal.variance == FALSE ){
    se <- sqrt( (sd1^2/sampleSize1) + (sd2^2/sampleSize2) )
    # welch-satterthwaite df
    df <- ( (sd1^2/sampleSize1 + sd2^2/sampleSize2)^2 )/( (sd1^2/sampleSize1)^2/(sampleSize1-1) + (sd2^2/sampleSize2)^2/(sampleSize2-1) )
  } else {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/sampleSize1 + 1/sampleSize2) * ((sampleSize1-1)*sd1^2 + (sampleSize2-1)*sd2^2)/(sampleSize1+sampleSize2-2) )
    df <- sampleSize1 + sampleSize2 - 2
  }
  t <- (mean1 - mean2 - m0)/se
  dat <- c(mean1-mean2, se, t, 2*pt(-abs(t),df))
  names(dat) <- c("DifferenceOfMeans", "stdError", "t", "pValue")
  return(dat)
}

.calcICPercentChange <- function(percChange){
  increasedIC <- tryCatch({
    t.test(percChange[direction == "increased", value], conf.level = 0.95)$conf.int},
    error = function(e) return(NA))
  decreasedIC <- tryCatch({
    t.test(percChange[direction == "decreased", value], conf.level = 0.95)$conf.int},
    error = function(e) return(NA))
  noChangeIC <- tryCatch({
    t.test(percChange[direction == "no change", value], conf.level = 0.95)$conf.int},
    error = function(e) return(NA))
  dt <- data.table::data.table(direction = c("increased", "decreased", "noChange"),
                               lower95 = c(increasedIC[1], decreasedIC[1], noChangeIC[1]),
                               upper95 = c(increasedIC[2], decreasedIC[2], noChangeIC[2]))
  return(dt)
}
