% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data.R
\name{ModelData}
\alias{ModelData}
\title{Model data used by RstoxBase}
\arguments{
\item{BioticData}{The \code{\link[RstoxData]{BioticData}} data (defined in RstoxData).}

\item{AcousticData}{The \code{\link[RstoxData]{AcousticData}} data (defined in RstoxData).}

\item{LandingData}{The \code{\link[RstoxData]{LandingData}} data (defined in RstoxData).}

\item{StoxBioticData}{The \code{\link[RstoxData]{StoxBioticData}} data (defined in RstoxData).}

\item{StoxAcousticData}{The \code{\link[RstoxData]{StoxAcousticData}} data (defined in RstoxData).}

\item{StoxLandingData}{The \code{\link[RstoxData]{StoxLandingData}} data (defined in RstoxData).}

\item{StratumAreaData}{The \code{\link{StratumAreaData}} data.}

\item{NASCData}{The \code{\link{NASCData}} data.}

\item{SumNASCData}{The \code{\link{SumNASCData}} data.}

\item{MeanNASCData}{The \code{\link{MeanNASCData}} data.}

\item{LengthDistributionData}{The \code{\link{LengthDistributionData}} data.}

\item{SumLengthDistributionData}{The \code{\link{SumLengthDistributionData}} data.}

\item{MeanLengthDistributionData}{The \code{\link{MeanLengthDistributionData}} data.}

\item{AssignmentLengthDistributionData}{The \code{\link{AssignmentLengthDistributionData}} data.}

\item{DensityData}{The \code{\link{DensityData}} data.}

\item{MeanDensityData}{The \code{\link{MeanDensityData}} data.}

\item{SpeciesCategoryCatchData}{The \code{\link{SpeciesCategoryCatchData}} data.}

\item{AbundanceData}{The \code{\link{AbundanceData}} data.}

\item{IndividualsData}{The \code{\link{IndividualsData}} data.}

\item{SuperIndividualsData}{The \code{\link{SuperIndividualsData}} data.}

\item{ReportSuperIndividualsData}{The \code{\link{ReportSuperIndividualsData}} data.}
}
\description{
The model data of a StoX model are the data generated during the model run based on input data and user settings and resources given in the project description (project.json file). Model data are transient and only exists from a process has been run until the project is closed.
}
\seealso{
\code{\link{ProcessData}} for process data types and \code{\link{DataTypes}} for all data types produced by \code{\link{RstoxBase}}.
}
