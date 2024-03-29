% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Density.R
\name{SweptAreaDensity}
\alias{SweptAreaDensity}
\title{Swept-area density}
\usage{
SweptAreaDensity(
  SweptAreaDensityMethod = c("LengthDistributed", "TotalCatch"),
  MeanLengthDistributionData,
  MeanSpeciesCategoryCatchData,
  SweepWidthMethod = c("Constant", "PreDefined"),
  SweepWidth = double(),
  DensityType = character()
)
}
\arguments{
\item{SweptAreaDensityMethod}{The method to use for the swept-area calculation, one of \"LengthDistributed\" for calculating density from the length distribution (\code{\link{MeanLengthDistributionData}}), and \"TotalCatch\" for calculating density from the total catch (\code{\link{MeanSpeciesCategoryCatchData}}).}

\item{MeanLengthDistributionData}{The \code{\link{MeanLengthDistributionData}} data.}

\item{MeanSpeciesCategoryCatchData}{The \code{\link{MeanSpeciesCategoryCatchData}} data.}

\item{SweepWidthMethod}{The method for calculating the sweep width. Possible options are (1) "Constant", which requires \code{SweepWidth} to be set as the constant sweep width, and (2) "PreDefined", impying that the sweep width is already incorporated in the \code{WeightedNumber} in the \code{MeanLengthDistributionData} using \code{link{GearDependentLengthDistributionCompensation}} or \code{link{LengthDependentLengthDistributionCompensation}}, or in the \code{MeanSpeciesCategoryCatchData} using \code{link{GearDependentSpeciesCategoryCatchCompensation}}.}

\item{SweepWidth}{The constant sweep width in meters.}

\item{DensityType}{The requested density type, currently only "AreaNumberDensity" is supported for SweptAreaDensityMethod = "LengthDistributed", and one of "AreaNumberDensity" and "AreaWeightDensity" (kg) for SweptAreaDensityMethod = "TotalCatch". All area densities are given per square nautical mile.}
}
\value{
An object of StoX data type \code{\link{DensityData}}. Note that the Density column of the Data table has unit kg nmi^-2 if SweptAreaDensityMethod is "TotalCatch" and DensityType is "AreaWeightensity". See also \code{\link{QuantityData}}
}
\description{
This function calculates the area density of fish as number of individuals or weight (kg) per square nautical mile, as determined by the \code{DensityType}.
}
\seealso{
See \code{\link{AcousticDensity}} for acoustic density. The DensityData leads to the \code{\link{QuantityData}} in a typical survey estimation model.
}
