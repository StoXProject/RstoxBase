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
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{StratumPolygon}}}
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
#' Model data used in estimation models in StoX
#' 
#' The model data of a StoX model is the data generated during the model run based on input data and user settings and resources given in the project description (project.json file). Model data are transient and only exists from a process has been run until the project is closed.
#' 
#' @details
#' This \code{\link{RstoxBase}} package produces the folliwing model data:
#' \itemize{
#' \item{\code{\link{LengthDistributionData}}}
#' \item{\code{\link{NASCData}}}
#' \item{\code{\link{DensityData}}}
#' \item{\code{\link{StratumAreaData}}}
#' \item{\code{\link{AbundanceData}}}
#' }
#' 
#' @name ModelData
#' 
NULL

##################################################
##################################################
#' Process data used in estimation models in StoX
#' 
#' The process data of a StoX model are data that are saved to the project description (project.json file), typically manual settings (tagging of \code{\link{EDSU}}s to \code{\link{PSU}}s) or data read from resource files other than acoustic, biotic and landing input data files stored in the input folder. 
#' 
#' @details
#' This \code{\link{RstoxBase}} package produces the folliwing process data:
#' \itemize{
#' \item{\code{\link{StratumPolygon}}}
#' \item{\code{\link{AcousticLayer}}}
#' \item{\code{\link{SweptAreaLayer}}}
#' \item{\code{\link{AcousticLayer}}}
#' \item{\code{\link{SweptAreaLayer}}}
#' \item{\code{\link{AcousticPSU}}}
#' \item{\code{\link{SweptAreaPSU}}}
#' \item{\code{\link{BioticAssignment}}}
#' \item{\code{\link{AcousticTargetStrength}}}
#' }
#' 
#' @name ProcessData
#' 
NULL


##################################################
##################################################
#' Resolution
#' 
#' Horizontal and vertical resolution in StoX
#' 
#' @details
#' Stox defines specific horizontal and vertical resolutions which are applied at different stages of a StoX model. The resolutions are grouped into the input resolution, the processing resolution and the report resolution.
#' 
#' The input resolution is \code{\link{Station}}/\code{\link{Haul}} (horizontal/vertical) for swept-area models and \code{\link{EDSU}}/\code{\link{Channel}} for acoustic-trawl models, and is the resolution of the input data.  \cr \cr
#' 
#' The process resolution is \code{\link{PSU}}/\code{\link{Layer}} (horizontal/vertical) for both swept-area and acoustic-trawl models, and is the resolution at which density is calculated. All data used as input when calculating density must be in this resolution.  \cr \cr
#' 
#' The report resolution is \code{\link{Stratum}}/\code{\link{Layer}} (horizontal/vertical) for both swept-area and acoustic-trawl models,, and is the resolution of the abundance estimates. 
#' 
#' @name Resolution
#' 
NULL

##################################################
##################################################
#' Stratum horizontal resolution
#' 
#' The horizontal resolution \code{Stratum} is the lowest horizontal resolution in StoX. 
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Stratum
#' 
NULL

##################################################
##################################################
#' Acoustic and swept-area primary sampling units
#' 
#' The horizontal resolution \code{PSU} is common for acoustic-trawl and swept-area estimation models, and defines the primary sampling units, typically transects for acoustic-trawl models and single trawl stations for swept-area models. 
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name PSU
#' 
NULL

##################################################
##################################################
#' Horizontal input resolution for acoustic-trawl models
#' 
#' The horizontal resolution \code{EDSU} is the input resolution for acoustic data (convensionally named Log). 
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name EDSU
#' 
NULL

##################################################
##################################################
#' Horizontal input resolution for swept-area models
#' 
#' The horizontal resolution \code{Station} is the input resolution for biotic data. One \code{Station} can contain several \code{\link{Haul}}, which represents the vertical resolution of a \code{Station}.
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Station
#' 
NULL


##################################################
##################################################
#' Layer vertical resolution
#' 
#' The vertical resolution \code{Layer} is the report resolution of a StoX project along with the horizontal report resolution \code{\link{Stratum}}.
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Layer
#' 
NULL


##################################################
##################################################
#' Input vertical resolution for swept-area models
#' 
#' The vertical resolution \code{Haul} is the input resolution for swept-area models. Each \code{\link{Station}} can contain several hauls.
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Haul
#' 
NULL


##################################################
##################################################
#' Input vertical resolution for acoustic-trawl models
#' 
#' The vertical resolution \code{Channel} is the input resolution for acoustic-trawl models. Each \code{\link{EDSU}} can contain several acoustic channels.
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Channel
#' 
NULL


##################################################
##################################################
#' StoX data type StratumPolygon
#' 
#' The StratumPolygon data type contains the polygons defining the strata of a survey, stored as an object of type \code{\link[sp]{SpatialPolygons}}. 
#' 
#' @details
#' The polygons are stored as gsojson in the project.json file and in output text files.
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
#' The LengthDistributionData model data contains the columns Stratum, PSU, Station, Layer, Haul, SpeciesCategory, IndividualTotalLengthCentimeter, LengthResolutionCentimeter, WeightedCount, MinHaulDepth, MaxHaulDepth, MinLayerDepth, MaxLayerDepth, LengthDistributionWeight, Cruise, EffectiveTowedDistance, VerticalNetOpening, HorizontalNetOpening, TrawlDoorSpread and LengthDistributionType. [List all types here in a nice table]
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name LengthDistributionData
#' 
NULL


##################################################
##################################################
#' Nautical area scattering coefficient (NASC) data
#' 
#' The NASCData model data gives the includes NASC for each acoustic channel or Layer, and ESDU, PSU or Stratum. 
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name NASCData
#' 
NULL

 
##################################################
##################################################
#' Biotic assignment data
#' 
#' The BioticAssignment process data is a table containing the columns Stratum, PSU, Layer, Haul and WeightingFactor, where Haul is a list of all Hauls assigned to each acoustic PSU of each acoustic layer. 
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name BioticAssignment
#' 
NULL


##################################################
##################################################
#' Density data
#' 
#' The DensityData model data gives the density of individuals as number per square nautical mile, by PSU or Stratum, and by Layer. 
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name DensityData
#' 
NULL


##################################################
##################################################
#' Acoustic target strength
#' 
#' The AcousticTargetStrength process data is a list of two tables, the TargetStrengthMethod, holding only the string naming the method to use, and the TargetStrengthTable, holding the parameters/values of the method. See \code{\link{DefineAcousticTargetStrength}} for details on the different methods.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AcousticTargetStrength
#' 
NULL


##################################################
##################################################
#' Stratum area
#' 
#' The StratumAreaData model data is a table of the area in square nautical miles of each stratum.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name StratumAreaData
#' 
NULL



##################################################
##################################################
#' Abundance
#' 
#' The AbundanceData model data is a table holding the abundance for each \code{\link{Stratum}}/\code{\link{Layer}}.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AbundanceData
#' 
NULL

