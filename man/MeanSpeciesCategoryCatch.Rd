% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpeciesCategoryCatch.R
\name{MeanSpeciesCategoryCatch}
\alias{MeanSpeciesCategoryCatch}
\title{Mean species category catch over Stations in each AcousticPSU}
\usage{
MeanSpeciesCategoryCatch(
  SpeciesCategoryCatchData,
  SumSpeciesCategoryCatchData,
  LayerDefinition = c("FunctionParameter", "FunctionInput", "PreDefined"),
  LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"),
  Resolution = double(),
  LayerTable = data.table::data.table(),
  BioticLayer = NULL,
  SurveyDefinition = c("FunctionParameter", "FunctionInput"),
  SurveyDefinitionMethod = c("AllStrata", "Table"),
  SurveyTable = data.table::data.table(),
  Survey = NULL,
  PSUDefinition = c("FunctionParameter", "FunctionInput"),
  PSUDefinitionMethod = c("StationToPSU", "None"),
  StratumPolygon = NULL,
  BioticPSU = NULL
)
}
\arguments{
\item{SpeciesCategoryCatchData}{The \code{\link{SpeciesCategoryCatchData}} data.}

\item{SumSpeciesCategoryCatchData}{The \code{\link{SumSpeciesCategoryCatchData}} data.}

\item{LayerDefinition}{The method to use for defining the Layers, one of \code{FunctionParameter} to define the Layers on the fly in this function, \code{FunctionInput} to import Layer process data from a previously run process using the function \code{DefineBioticLayer}, or \code{PreDefined} whihc requires \code{SumLengthDistributionData} as input.}

\item{LayerDefinitionMethod}{See \code{\link{DefineBioticLayer}}}

\item{Resolution}{Numeric: A single numeric giving the thickness of the layers.}

\item{LayerTable}{A table of Layer name, MinLayerDepth in meters and MaxLayerDepth in meters, defining the Layers.}

\item{BioticLayer}{The \code{\link{BioticLayer}} process data.}

\item{SurveyDefinition}{The method to use for defining the Survey, one of \code{FunctionParameter} to define the Survey on the fly in this function, or \code{FunctionInput} to import Survey process data from a previously run process by the input \code{Survey}.}

\item{SurveyDefinitionMethod}{See \code{\link{DefineSurvey}}}

\item{SurveyTable}{A table of the two columns Stratum and Survey.}

\item{Survey}{The \code{\link{Survey}} process data.}

\item{PSUDefinition}{The method to use for defining the PSUs, one of \code{FunctionParameter} to define the PSUs on the fly in this function, or \code{FunctionInput} to import PSU process data from a previously run process by \code{BioticPSU}.}

\item{PSUDefinitionMethod}{See \code{\link{DefineBioticPSU}}}

\item{StratumPolygon}{The \code{\link{StratumPolygon}} process data.}

\item{BioticPSU}{The \code{\link{BioticPSU}} process data.}
}
\value{
An \code{\link{MeanSpeciesCategoryCatchData}} object.
}
\description{
This function averages \code{link{SpeciesCategoryCatchData}} data horizontally, weighted by the effective towed distance.
}
