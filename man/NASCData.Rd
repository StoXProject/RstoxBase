% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data.R
\name{NASCData}
\alias{NASCData}
\title{Nautical area scattering coefficient (NASC) data}
\description{
The NASCData model data gives the NASC for each acoustic channel or Layer, and ESDU, PSU or Stratum.
}
\details{
\tabular{lllll}{
\bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
EDSU \tab Unique elementary distance sampling unit (EDSU/Log) identifier \tab None \tab Character \tab "2020821/2020-04-24T03:51:26.000Z" \cr   
Channel \tab Unique NASC depth channel identifier \tab None \tab Character \tab "2" \cr
Beam \tab Unique Beam identifier \tab None \tab Character \tab "38000/2" \cr
AcousticCategory \tab The acoustic category \tab None \tab Character \tab "HER" \cr
Frequency \tab The acoustic frequency of the Beam \tab hertz \tab Numeric \tab 38000 \cr
NASC \tab The nautical area scattering coefficient. \tab m^2/nmi^2 \tab Numeric \tab 59.24813 \cr
MinChannelDepth \tab The minimum depth of the depth Channel \tab m \tab Numeric \tab 0 \cr
MaxChannelDepth \tab The maximum depth of the depth Channel \tab m \tab Numeric \tab 10 \cr
NASCWeight \tab The LogDistance of the EDSU \tab None \tab None \tab 0.1 \cr
ChannelReferenceType \tab Unique ChannelReference identifier \tab None \tab Character \tab "P" \cr
ChannelReferenceDepth \tab The depth of the ChannelReference origin. 0 for pelagic channels. Not yet given for bottom channels, as BottomDepth is not yet defined for NMDEchosounder data \tab m \tab Numeric \tab 0 \cr
ChannelReferenceTilt \tab The tilt angle of the beam, where 180 is vertically downwards and 0 is vertically upwards \tab degree \tab Numeric \tab 180 \cr 
Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \cr
EffectiveLogDistance \tab The effective length of the EDSU \tab nmi \tab Numeric \tab 0.09 \cr
DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \cr
Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \cr
Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \cr
}
}
\seealso{
This data type is produced by \code{\link{NASC}}. NASCData is used in \code{\link{SumNASC}} with output \code{\link{SumNASCData}} and \code{\link{MeanNASC}} with output \code{\link{MeanNASCData}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
}
