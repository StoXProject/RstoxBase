% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Abundance.R
\name{Quantity}
\alias{Quantity}
\title{Calculate abundance in the strata}
\usage{
Quantity(MeanDensityData, StratumAreaData)
}
\arguments{
\item{MeanDensityData}{The \code{\link{MeanDensityData}} data.}

\item{StratumAreaData}{The \code{\link{StratumAreaData}} data.}
}
\value{
An object of StoX data type \code{\link{QuantityData}}. The \code{\link{QuantityData}} contains both an Abundance and a Biomass column, but only one of these can be populated.
}
\description{
This function calculates abundance as the product of density and stratum area
}
\details{
The function merge the \code{\link{MeanDensityData}} with the \code{\link{StratumAreaData}} by Stratum and calculates the abundance  as the product of density (number by square nautical miles) and area (square nautical miles). 
For acoustic-trawl estimates the abundance is calculated by Stratum, Layer, Beam, SpeciesCategory and IndividualTotalLength.
In swept-area estimates the abundance is calculated by Stratum, Layer, SpeciesCategory and IndividualTotalLength 

For swept-area estimates the density calculated by \code{\link{SweptAreaDensity}} can be given as "AreaWeightDensity" (as indicated by the column \code{DensityType}). In this case the \code{Biomass} column and not the \code{Abundance} column is populated in the output \code{\link{QuantityData}} from this function. The biomass is given in kilogram in this case, as Density is in kilogram per square nautical mile if \code{DensityType} = "AreaWeightDensity". This is different from the \code{Biomass} column of \code{\link{SuperIndividualsData}}, which is in gram, as it is generated from IndividualRoundWeight which is in gram.
}
\seealso{
\code{\link{SuperIndividuals}} for distributing Abundance to individuals.
}
