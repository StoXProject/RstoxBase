% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Acoustic.R
\name{MeanNASC}
\alias{MeanNASC}
\title{Mean NASC}
\usage{
MeanNASC(
  NASCData,
  SumNASCData,
  LayerDefinition = c("FunctionParameter", "FunctionInput", "PreDefined"),
  LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"),
  Resolution = double(),
  LayerTable = data.table::data.table(),
  AcousticLayer = NULL,
  SurveyDefinition = c("FunctionParameter", "FunctionInput"),
  SurveyDefinitionMethod = c("AllStrata", "Table"),
  SurveyTable = data.table::data.table(),
  Survey = NULL,
  PSUDefinition = c("FunctionParameter", "FunctionInput"),
  PSUDefinitionMethod = c("EDSUToPSU"),
  StratumPolygon = NULL,
  AcousticPSU = NULL
)
}
\arguments{
\item{NASCData}{The \code{\link{NASCData}} data.}

\item{SumNASCData}{The \code{\link{SumNASCData}} data.}

\item{LayerDefinition}{The method to use for defining the Layers, one of \code{FunctionParameter} to define the Layers on the fly in this function, \code{FunctionInput} to import Layer process data from a previously run process using the function \code{DefineAcousticLayer}, or \code{PreDefined} whihc requires \code{SumNASCData} as input.}

\item{LayerDefinitionMethod}{See \code{\link{DefineAcousticLayer}}}

\item{Resolution}{Numeric: A single numeric giving the thickness of the layers.}

\item{LayerTable}{A table of Layer name, MinLayerDepth in meters and MaxLayerDepth in meters, defining the Layers.}

\item{AcousticLayer}{The \code{\link{AcousticLayer}} process data.}

\item{SurveyDefinition}{The method to use for defining the Survey, one of \code{FunctionParameter} to define the Survey on the fly in this function, or \code{FunctionInput} to import Survey process data from a previously run process by the input \code{Survey}.}

\item{SurveyDefinitionMethod}{See \code{\link{DefineSurvey}}}

\item{SurveyTable}{A table of the two columns Stratum and Survey.}

\item{Survey}{The \code{\link{Survey}} process data.}

\item{PSUDefinition}{The method to use for defining the PSUs, one of \code{FunctionParameter} to define the PSUs on the fly in this function, or \code{FunctionInput} to import PSU process data from a previously run process by \code{AcousticPSU}.}

\item{PSUDefinitionMethod}{See \code{\link{DefineAcousticPSU}}}

\item{StratumPolygon}{The \code{\link{StratumPolygon}} process data.}

\item{AcousticPSU}{The \code{\link{AcousticPSU}} process data.}
}
\description{
This function averages \code{\link{SumNASCData}} horizontally to the acoustic PSU resolution, weighted by the log distance. Optionally, \code{\link{NASCData}} can be used as input, in which case \code{\link{SumNASC}} is run first.
}
\seealso{
\code{\link{NASC}} and \code{\link{MeanNASC}}.
}
