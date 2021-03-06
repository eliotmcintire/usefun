% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotVegetationBiomass.R
\name{plotVegetationBiomass}
\alias{plotVegetationBiomass}
\title{Plots vegetation biomass Type using cohortData and pixelGroupMap}
\usage{
plotVegetationBiomass(
  years = c(2011, 2100),
  dataPath,
  typeSim,
  colNA = "grey85",
  saveRAS = TRUE,
  overwrite = FALSE
)
}
\arguments{
\item{years}{numeric. Years available/intended to be used for the giphy}

\item{dataPath}{character. Path to data}

\item{typeSim}{character. Which simulation is it? i.e. 'LandR_SCFM' | 'LandR.CS_fS'}

\item{saveRAS}{logical. Save the raster for posterior use?}

\item{overwrite}{logical.}
}
\value{
plot
}
\description{
Plots vegetation biomass Type using cohortData and pixelGroupMap
}
\author{
Tati Micheletti
}
