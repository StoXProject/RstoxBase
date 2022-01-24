##################################################
##################################################
#' StoX multipolygon WKT
#'
#'This data format is used in \eqn{\le} \href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13250}{StoX 2.7} to define strata polygons. 
#' 
#' @details 
#' A data frame used in early versions of \href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13250}{StoX} to define strata polygons, which combine one variable with strata names and one variable consisting of \href{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}{multipolygon.wkt} strings. Well-known text (WKT) is a text markup language for representing vector geometry objects.
#' 
#' @references 
#' Additonal information on WKT multipolygon specification and StoX can be found here:
#' 
#' \href{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}
#' 
#' Johnsen, E., Totland, A., Skålevik, Å., Holmin, A. J., Dingsør, G. E., Fuglebakk, E., & Handegard, N. O. (2019). StoX: An open source software for marine survey analyses. Methods in Ecology and Evolution, 10(9), 1523-1528. \doi{10.1111/2041-210X.13250}
#' 
#' @name StoX_multipolygon_WKT
#'
NULL

##################################################
##################################################
#' General parameters of RstoxBase
#' 
#' All functions referring to a project, a model, a process or an output table use the same parameters, listed here.
#' 
#' @param processData The current data produced by a previous instance of the function.
#' @param UseProcessData Logical: If TRUE use the existing function output in the process. 
#' @param LayerType Character: A string naming the type of model, either "Acoustic" or "Biotic".
#' @param PSUType Character: A string naming the type of model, either "Acoustic" or "Biotic" 
#' 
#' @name general_arguments
#' 
NULL

##################################################
##################################################
#' General report parameters of RstoxBase
#' 
#' All functions referring to a project, a model, a process or an output table use the same parameters, listed here.
#' 
#' @param TargetVariable The variable to report.
#' @param GroupingVariables The variables to report by. For most applications \code{GroupingVariables} should include "Survey" or "SpeciesCategory", unless the user needs to sum over all Survey and SpeciesCategory.
#' @param InformationVariables Variables to include as columns to the end of the report table. These cannot have more unique combinations than the \code{GroupingVariables}.
#' @param RemoveMissingValues Logical: If TRUE, remove missing values (NAs) from the \code{TargetVariable}. The default (FALSE) implies to report NA if at least one of the values used in the \code{ReportFunction} is NA. Use \code{RemoveMissingValues} = TRUE with extreme caution, as it may lead to under-estimation. E.g., if \code{RemoveMissingValues} = TRUE and a super-individual lacks \code{IndividualRoundWeight}, \code{Biomass} will be NA, and the portion of \code{Abundance} distributed to that super-individual will be excluded when summing \code{Biomass} (but included when summing \code{Abundance}). It is advised to always run with \code{RemoveMissingValues} = FALSE first, and make a thorough investigation to identify the source of any missing values. The function \code{link{ImputeSuperIndividuals}} can be used to impute the missing information from other super-individuals.
#' 
#' @name general_report_arguments
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
#' @param AssignmentLengthDistributionData The \code{\link{AssignmentLengthDistributionData}} data.
#' @param DensityData The \code{\link{DensityData}} data.
#' @param MeanDensityData The \code{\link{MeanDensityData}} data.
#' @param SpeciesCategoryCatchData The \code{\link{SpeciesCategoryCatchData}} data.
#' @param SumSpeciesCategoryCatchData The \code{\link{SumSpeciesCategoryCatchData}} data.
#' @param MeanSpeciesCategoryCatchData The \code{\link{MeanSpeciesCategoryCatchData}} data.
#' @param QuantityData The \code{\link{QuantityData}} data.
#' @param IndividualsData The \code{\link{IndividualsData}} data.
#' @param SuperIndividualsData The \code{\link{SuperIndividualsData}} data.
#' @param ReportSuperIndividualsData The \code{\link{ReportSuperIndividualsData}} data.
#' 
#' @name ModelData
#' 
#' @seealso \code{\link{ProcessData}} for process data types and \code{\link{DataTypes}} for all data types produced by \code{\link{RstoxBase}}.
#' 
NULL

##################################################
##################################################
#' Process data used in estimation models in StoX
#' 
#' The process data of a StoX model are data that are saved to the project description (project.json file), typically manual settings (tagging of \code{\link{EDSU}}s to \code{\link{PSU}}s) or data read from resource files other than acoustic, biotic and landing input data files stored in the input folder. 
#' 
#' @param StratumPolygon The \code{\link{StratumPolygon}} process data.
#' @param Survey The \code{\link{Survey}} process data.
#' @param AcousticLayer The \code{\link{AcousticLayer}} process data.
#' @param AcousticPSU The \code{\link{AcousticPSU}} process data.
#' @param BioticLayer The \code{\link{BioticLayer}} process data.
#' @param BioticPSU The \code{\link{BioticPSU}} process data.
#' @param BioticAssignment The \code{\link{BioticAssignment}} process data.
#' @param AcousticTargetStrength The \code{\link{AcousticTargetStrength}} process data.
#' @param Regression The \code{\link{Regression}} process data.
#' 
#' @name ProcessData
#' 
#' @seealso \code{\link{ModelData}} for model data types and \code{\link{DataTypes}} for all data types produced by \code{\link{RstoxBase}}.
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
#' Acoustic and biotic primary sampling units
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
#' @seealso This data type is produced by \code{\link{DefineStratumPolygon}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name StratumPolygon
#' 
NULL


##################################################
##################################################
#' StoX data type Survey
#' 
#' The Survey data is a table of the columns Stratum and Survey, defining which strata comprise the different surveys. 
#' 
#' @details
#' This data type can be useful if there are particular strata that are e.g. experimental, and others that are to be included in the official estimate, and replaces the "include in total" concept of StoX 2.7.
#' 
#' @seealso This data type is produced by \code{\link{DefineSurvey}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name Survey
#' 
NULL



##################################################
##################################################
#' Acoustic Layer
#' 
#' The Acoustic Layer process data is a table of the three columns Layer, MinLayerDepth and MaxLayerDepth specifying the depth ranges for the acoustic layers on which the acoustic density should be calculated, where the Layer column gives the names of the layers.
#' 
#' @seealso This data type is produced by \code{\link{DefineAcousticLayer}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AcousticLayer
#' 
NULL


##################################################
##################################################
#' Biotic Layer
#' 
#' The biotic Layer process data is a table of the three columns Layer, MinLayerDepth and MaxLayerDepth specifying the depth ranges for the biotic layers on which the biotic density should be calculated, where the Layer column gives the names of the layers.
#' 
#' @seealso This data type is produced by \code{\link{DefineBioticLayer}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name BioticLayer
#' 
NULL


##################################################
##################################################
#' Acoustic PSU
#' 
#' The AcousticPSU process data contains two tables Stratum_PSU and EDSU_PSU, where Stratum_PSU has the two column Stratum and PSU giving the acoustic PSUs defined in each stratum (possibly none), and EDSU_PSU has the two column EDSU listing every EDSU in the data and PSU which tags each EDSU to a PSU. EDSUs can be un-tagged, which is indicated with PSU = "".
#' 
#' @seealso This data type is produced by \code{\link{DefineAcousticPSU}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AcousticPSU
#' 
NULL


##################################################
##################################################
#' Biotic PSU
#' 
#' The BioticPSU process data contains two tables Stratum_PSU and Station_PSU, where Stratum_PSU has the two column Stratum and PSU giving the biotic PSUs defined in each stratum (possibly none), and Station_PSU has the two column Station listing every Station in the data and PSU which tags each Station to a PSU. Stations can be un-tagged, which is indicated with PSU = "".
#' 
#' @seealso This data type is produced by \code{\link{DefineBioticPSU}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name BioticPSU
#' 
NULL


##################################################
##################################################
#' Stratum area data
#' 
#' The StratumAreaData model data is a table of the area in square nautical miles of each stratum.
#' 
#' @seealso This data type is produced by \code{\link{StratumArea}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name StratumAreaData
#' 
NULL


##################################################
##################################################
#' Total catch per SpeciesCategory and Haul
#' 
#' The SpeciesCategoryCatchData contains the columns Station, Haul, SpeciesCategory, TotalCatchWeigth, TotalCatchNumber, MinHaulDepth, MaxHaulDepth, MinLayerDepth, MaxLayerDepth, LengthDistributionWeight, Cruise, EffectiveTowDistance, VerticalNetOpening, HorizontalNetOpening, TrawlDoorSpread and SpeciesCategoryCatchType.
#' 
#' @seealso This data type is produced by \code{\link{SpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Total catch per SpeciesCategory and Station
#' 
#' The SumSpeciesCategoryCatchData model data is a list of two elements; the Data, which are similar to \code{\link{SpeciesCategoryCatch}} but added the Layer resolution column and summed vertically; and the Resolution, which is a table of the three columns Station, Layer and Haul containing the link between the vertical resolution variables before summing.
#' 
#' @seealso This data type is produced by \code{\link{SumSpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SumSpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Total catch per SpeciesCategory and biotic PSU
#' 
#' The MeanSpeciesCategoryCatchData model data is a list of two elements; the Data, which are similar to \code{\link{SpeciesCategoryCatch}} but added the Layer vertical resolution column and the Stratum and PSU horizontal resolution columns, and summed vertically and averaged horizontally; and the Resolution, which is a table of the five columns Stratum, PSU, Station, Layer and Haul containing the link between the horizontal and vertical resolution variables before summing and averaging.
#' 
#' @seealso This data type is produced by \code{\link{MeanSpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name MeanSpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Length distribution data
#' 
#' The LengthDistributionData model data contains the columns Station, Haul, SpeciesCategory, IndividualTotalLength, LengthResolution, WeightedNumber, MinHaulDepth, MaxHaulDepth, MinLayerDepth, MaxLayerDepth, LengthDistributionWeight, Cruise, EffectiveTowDistance, VerticalNetOpening, HorizontalNetOpening, TrawlDoorSpread and LengthDistributionType.
#' 
#' @seealso This data type is produced by \code{\link{LengthDistribution}}, \code{\link{RegroupLengthDistribution}}, \code{\link{LengthDependentLengthDistributionCompensation}} and \code{\link{RelativeLengthDistribution}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
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
#' @seealso This data type is produced by \code{\link{SumLengthDistribution}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
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
#' @seealso This data type is produced by \code{\link{MeanLengthDistribution}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name MeanLengthDistributionData
#' 
NULL


##################################################
##################################################
#' Assignment length distribution data
#' 
#' The AssignmentLengthDistributionData model data is a table averaged length distribution for each combination of Stratum (acoustic)PSU and (acoustic)Layer.
#' 
#' @seealso This data type is produced by \code{\link{AssignmentLengthDistribution}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AssignmentLengthDistributionData
#' 
NULL


##################################################
##################################################
#' Nautical area scattering coefficient (NASC) data
#' 
#' The NASCData model data gives the includes NASC for each acoustic channel or Layer, and ESDU, PSU or Stratum. 
#' 
#' @seealso This data type is produced by \code{\link{NASC}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
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
#' @seealso This data type is produced by \code{\link{SumNASC}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
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
#' @seealso This data type is produced by \code{\link{MeanNASC}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name MeanNASCData
#' 
NULL


##################################################
##################################################
#' Biotic assignment
#' 
#' The BioticAssignment process data is a table containing the columns Stratum, PSU, Layer, Haul and WeightingFactor, where Haul is a list of all Hauls assigned to each acoustic PSU of each acoustic layer. 
#' 
#' @seealso This data type is produced by \code{\link{DefineBioticAssignment}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name BioticAssignment
#' 
NULL


##################################################
##################################################
#' Density data
#' 
#' The DensityData model data holds the density of individuals as number per square nautical mile, by PSU and Layer. The data type contains the tables Data and Resolution.
#' 
#' @seealso This data type is produced by \code{\link{AcousticDensity}} and \code{\link{SweptAreaDensity}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name DensityData
#' 
NULL


##################################################
##################################################
#' Mean density data
#' 
#' The MeanDensityData model data holds the density of individuals as number per square nautical mile, by Stratum and Layer. The data type contains the tables Data and Resolution.
#' 
#' @seealso This data type is produced by \code{\link{MeanDensity}} based on \code{\link{DensityData}} See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name MeanDensityData
#' 
NULL


##################################################
##################################################
#' Acoustic target strength
#' 
#' The AcousticTargetStrength process data is a list of two tables, the AcousticTargetStrengthModel, holding only the string naming the model to use, and the AcousticTargetStrengthTable, holding the table of the parameters/values of the model. See \code{\link{DefineAcousticTargetStrength}} for details of the different models.
#' 
#' @seealso This data type is produced by \code{\link{DefineAcousticTargetStrength}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name AcousticTargetStrength
#' 
NULL


##################################################
##################################################
#' Regression
#' 
#' The Regression process data is a list of two tables, the RegressionModel, holding only the string naming the model to use, the RegressionTable, holding the table of the parameters/values of the model. See \code{\link{DefineRegression}} for details of the different models. The function \code{\link{EstimateBioticRegression}} also outputs this datatype.
#' 
#' @seealso This data type is produced by \code{\link{DefineRegression}} and \code{\link{EstimateBioticRegression}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name Regression
#' 
NULL



##################################################
##################################################
#' Quantity data
#' 
#' The Quantity model data holds the abundance and biomass of individuals as number and weight (g), respectively, per Stratum and Layer. The data type contains the tables Data and Resolution.

#' 
#' @seealso This data type is produced by \code{\link{Quantity}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name QuantityData
#' 
NULL


##################################################
##################################################
#' Individuals data
#' 
#' The IndividualsData model data are the individuals from \code{\link{StoxBioticData}} used in the estimate. In Individuals() the \code{\link{StoxBioticData}} is merged with \code{\link{BioticAssignment}} in the case of acoustic-trawl models and with \code{\link{MeanLengthDistributionData}} in the case of swept-area models, by the Haul identifier stored in the \code{\link{StoxBioticData}}, the \code{\link{BioticAssignment}}, and in the Resolution table of the \code{\link{MeanLengthDistributionData}}. As the hauls may be linked to a different stratum than the one containing the haul, the Stratum column of the \code{\link{IndividualsData}} may not correspond to the actual stratum of the haul.
#' 
#' @seealso This data type is produced by \code{\link{Individuals}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name IndividualsData
#' 
NULL


##################################################
##################################################
#' Super-individuals data
#' 
#' The SuperIndividualsData model data is the \code{\link{IndividualsData}} added Abundance and Biomass.
#' 
#' @seealso This data type is produced by \code{\link{SuperIndividuals}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name SuperIndividualsData
#' 
NULL


##################################################
##################################################
#' Reported super-individuals data
#' 
#' The ReportSuperIndividualsData model data is a report of the \code{\link{SuperIndividualsData}}.
#' 
#' @seealso This data type is produced by \code{\link{ReportSuperIndividuals}}. See \code{\link{DataTypes}} for a list of all data types produced by \code{\link{RstoxBase}}
#' 
#' @name ReportSuperIndividualsData
#' 
NULL


##################################################
##################################################
#' Reported SpeciesCategoryCatch data
#' 
#' The ReportSpeciesCategoryCatchData model data is a report of the \code{\link{SpeciesCategoryCatchData}} which combines Haul info and a table of the catch of each SpeciesCategory.
#' 
#' @seealso This data type is produced by \code{\link{ReportSpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}
#' 
#' @name ReportSpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Reported Density data
#' 
#' The ReportDensityData model data is a report of the \code{\link{DensityData}}.
#' 
#' @seealso This data type is produced by \code{\link{ReportDensity}}. See \code{\link{DataTypes}} for a list of all data types produced by \code{\link{RstoxBase}}
#' 
#' @name ReportDensityData
#' 
NULL


##################################################
##################################################
#' Reported Quantity data
#' 
#' The ReportQuantityData model data is a report of the \code{\link{QuantityData}}.
#' 
#' @seealso This data type is produced by \code{\link{ReportQuantity}}. See \code{\link{DataTypes}} for a list of all data types produced by \code{\link{RstoxBase}}
#' 
#' @name ReportQuantityData
#' 
NULL


# ##################################################
# ##################################################
# #' Write StratumPolygon data
# #' 
# #' The WriteStratumPolygonData model data is used to write \code{\link{StratumPolygon}}.
# #' 
# #' @seealso This data type is produced by \code{\link{WriteStratumPolygon}}. See \code{\link{DataTypes}} for a list of all data types produced by \code{\link{RstoxBase}}
# #' 
# #' @name WriteStratumPolygonData
# #' 
# NULL

