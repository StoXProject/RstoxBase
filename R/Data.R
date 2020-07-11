##################################################
##################################################
#' General parameters of RstoxFramework.
#' 
#' All functions referring to a project, a model, a process or an output table use the same parameters, listed here.
#' 
#' @param processData The current data produced by a previous instance of the function.
#' @param UseProcessData Logical: If TRUE use the existing function output in the process. 
#' @param modelType Character: A string naming the type of model, either "Acoustic" or "SweptArea".
#' 
#' @name general_arguments
#' 
NULL

##################################################
##################################################
#' StoX data types of the RstoxBase package
#' 
#' StoX data types are the data types used to transfer data and information between processes in a StoX estimation model. The data types are divided into two types, the \code{\link{ModelData}} and \code{\link{ProcessData}}.
#' 
#' @name DataTypes
#' 
NULL

##################################################
##################################################
#' Model data used by RstoxBase
#' 
#' The model data of a StoX model are the data generated during the model run based on input data and user settings and resources given in the project description (project.json file). Model data are transient and only exists from a process has been run until the project is closed.
#' 
#' @param BioticData The \code{\link[RstoxData]{BioticData}} data (defined in RstoxData).
#' @param AcousticData The \code{\link[RstoxData]{AcousticData}} data (defined in RstoxData).
#' @param LandingData The \code{\link[RstoxData]{LandingData}} data (defined in RstoxData).
#' @param StoxBioticData The \code{\link[RstoxData]{StoxBioticData}} data (defined in RstoxData).
#' @param StoxAcousticData The \code{\link[RstoxData]{StoxAcousticData}} data (defined in RstoxData).
#' @param StoxLandingData The \code{\link[RstoxData]{StoxLandingData}} data (defined in RstoxData).
#' @param StratumAreaData The \code{\link{StratumAreaData}} data.
#' @param NASCData The \code{\link{NASCData}} data.
#' @param SumNASCData The \code{\link{SumNASCData}} data.
#' @param MeanNASCData The \code{\link{MeanNASCData}} data.
#' @param LengthDistributionData The \code{\link{LengthDistributionData}} data.
#' @param SumLengthDistributionData The \code{\link{SumLengthDistributionData}} data.
#' @param MeanLengthDistributionData The \code{\link{MeanLengthDistributionData}} data.
#' @param AssignmentLengthDistribution The \code{\link{AssignmentLengthDistribution}} data.
#' @param DensityData The \code{\link{DensityData}} data.
#' @param SpeciesCategoryCatchData The \code{\link{SpeciesCategoryCatchData}} data.
#' @param AbundanceData The \code{\link{AbundanceData}} data.
#' @param IndividualsData The \code{\link{IndividualsData}} data.
#' @param SuperIndividualsData The \code{\link{SuperIndividualsData}} data.
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
#' @param StratumPolygon The \code{\link{StratumPolygon}} process data.
#' @param AcousticLayer The \code{\link{AcousticLayer}} process data.
#' @param AcousticPSU The \code{\link{AcousticPSU}} process data.
#' @param SweptAreaLayer The \code{\link{SweptAreaLayer}} process data.
#' @param SweptAreaPSU The \code{\link{SweptAreaPSU}} process data.
#' @param BioticAssignment The \code{\link{BioticAssignment}} process data.
#' @param AcousticTargetStrength The \code{\link{AcousticTargetStrength}} process data.
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
#' Length distribution data
#' 
#' The LengthDistributionData model data contains the columns Station, Haul, SpeciesCategory, IndividualTotalLengthCentimeter, LengthResolutionCentimeter, WeightedCount, MinHaulDepth, MaxHaulDepth, MinLayerDepth, MaxLayerDepth, LengthDistributionWeight, Cruise, EffectiveTowedDistance, VerticalNetOpening, HorizontalNetOpening, TrawlDoorSpread and LengthDistributionType. [List all types here in a nice table]
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name LengthDistributionData
#' 
NULL


##################################################
##################################################
#' Summed length distribution data
#' 
#' The SumLengthDistributionData model data is a list of two elements; the Data, which are similar to \code{\link{LengthDistributionData}} but added the Layer resolution column and summed vertically; and the Resolution, which is a table of the three columns Station, Layer and Haul containing the link between the vertical resolution variables before summing.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SumLengthDistributionData
#' 
NULL


##################################################
##################################################
#' Mean length distribution data
#' 
#' The MeanLengthDistributionData model data is a list of two elements; the Data, which are similar to \code{\link{LengthDistributionData}} but added the Layer vertical resolution column and the Stratum and PSU horizontal resolution columns, and summed vertically and averaged horizontally; and the Resolution, which is a table of the five columns Stratum, PSU, Station, Layer and Haul containing the link between the horizontal and vertical resolution variables before summing and averaging.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name MeanLengthDistributionData
#' 
NULL


##################################################
##################################################
#' Assignment length distribution data
#' 
#' The AssignmentLengthDistribution model data is a table averaged length distribution for each combination of Stratum (acoustic)PSU and (acoustic)Layer.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AssignmentLengthDistribution
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
#' Summed NASC data
#' 
#' The SumNASCData model data is a list of two elements; the Data, which are similar to \code{\link{NASCData}} but added the Layer resolution column and summed vertically; and the Resolution, which is a table of the three columns EDSU, Layer and Channel containing the link between the vertical resolution variables before summing.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SumNASCData
#' 
NULL


##################################################
##################################################
#' Mean NASC data
#' 
#' The MeanNASCData model data is a list of two elements; the Data, which are similar to \code{\link{NASCData}} but added the Layer vertical resolution column and the Stratum and PSU horizontal resolution columns, and summed vertically and averaged horizontally; and the Resolution, which is a table of the five columns Stratum, PSU, EDSU, Layer and Channel containing the link between the horizontal and vertical resolution variables before summing and averaging.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name MeanNASCData
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
#' Total catch of each SpeciesCategory
#' 
#' The SpeciesCategoryDensityData model data is a list of two elements; the HaulInfo, containing information about each Haul; and the SpeciesCategoryCatch, holding the total catch of each SpeciesCategory of each Haul, where the Hauls are organized as rows and SpeciesCategories as columns.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SpeciesCategoryCatchData
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
#' Super-individuals
#' 
#' The SuperIndividualsData model data is the \code{\link{IndivdualsData}} added Abundance.
#' 
#' @seealso \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SuperIndividualsData
#' 
NULL



