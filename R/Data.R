##################################################
##################################################
#' StoX data types of the RstoxBase package
#' 
#' StoX data types are the data types used to transfer data and information between processes in a StoX estimation model.
#' 
#' @details
#' This RstoxBase package produces the folliwing StoX data types:
#' \itemize{
#' \item{\code{\link{StratumPolygon}}}
#' }
#' 
#' @seealso \code{\link[RstoxData]{RstoxData}} and \code{\link[RstoxFDA]{RstoxFDA}} for a list of all StoX data types produced by the other official StoX function packages.
#' 
#' @name DataTypes
#' 
NULL


##################################################
##################################################
#' StoX data type StratumPolygon
#' 
#' The StratumPolygon data type contains the polygons defining the strata of a survey, stored as an object of type \code{\link[sp]{SpatialPolygons}}. 
#' 
#' @details
#' The polygons are stored 
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name StratumPolygon
#' 
NULL



##################################################
##################################################
#' Acoustic Layer
#' 
#' The Acoustic Layer process data is a table of the three columns Layer, MinLayerDepth and MaxLayerDepth specifying the depth ranges for the acoustic layers on which the acoustic density should be calculated, where the Layer column gives the names of the layers.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AcousticLayer
#' 
NULL


##################################################
##################################################
#' Swept-area Layer
#' 
#' The Swept-area Layer process data is a table of the three columns Layer, MinLayerDepth and MaxLayerDepth specifying the depth ranges for the swept-area layers on which the swept-area density should be calculated, where the Layer column gives the names of the layers.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SweptAreaLayer
#' 
NULL



##################################################
##################################################
#' Acoustic Layer
#' 
#' The AcousticLayer process data is a table of the three columns Layer, MinLayerDepth and MaxLayerDepth specifying the depth ranges for the acoustic layers on which the acoustic density should be calculated, where the Layer column gives the names of the layers.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AcousticLayer
#' 
NULL


##################################################
##################################################
#' Swept-area Layer
#' 
#' The SweptAreaLayer process data is a table of the three columns Layer, MinLayerDepth and MaxLayerDepth specifying the depth ranges for the swept-area layers on which the swept-area density should be calculated, where the Layer column gives the names of the layers.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SweptAreaLayer
#' 
NULL

##################################################
##################################################
#' Acoustic PSU
#' 
#' The AcousticPSU process data contains two tables Stratum_PSU and EDSU_PSU, where Stratum_PSU has the two column Stratum and PSU giving the acoustic PSUs defined in each stratum (possibly none), and EDSU_PSU has the two column EDSU listing every EDSU in the data and PSU which tags each EDSU to a PSU. EDSUs can be un-tagged, which is indicated with PSU = "".
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AcousticPSU
#' 
NULL


##################################################
##################################################
#' Swept-area PSU
#' 
#' The SweptAreaPSU process data contains two tables Stratum_PSU and Station_PSU, where Stratum_PSU has the two column Stratum and PSU giving the swept-area PSUs defined in each stratum (possibly none), and Station_PSU has the two column Station listing every Station in the data and PSU which tags each Station to a PSU. Stations can be un-tagged, which is indicated with PSU = "".
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SweptAreaPSU
#' 
NULL



##################################################
##################################################
#' Length distribution data
#' 
#' The LengthDistributionData model data gives the frequency per Haul, SpeciesCategory and length group defined by the two variables IndividualTotalLengthCentimeter and LengthResolutionCentimeter. 
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name LengthDistributionData
#' 
NULL



##################################################
##################################################
#' Model data used in estimation models in StoX
#' 
#' The LengthDistributionData model data contains the columns Stratum, PSU, Station, Layer, Haul, SpeciesCategory, IndividualTotalLengthCentimeter, LengthResolutionCentimeter, WeightedCount, MinHaulDepth, MaxHaulDepth, MinLayerDepth, MaxLayerDepth, LengthDistributionWeight, Cruise, EffectiveTowedDistance, VerticalNetOpening, HorizontalNetOpening, TrawlDoorSpread and LengthDistributionType. [List all types here in a nice table]
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name StoxModelData
#' 
NULL










