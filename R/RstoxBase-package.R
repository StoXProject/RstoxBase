#' Base StoX Functions
#'
#' Base StoX functions used for survey estimation.
#'
#' The StoX functions defined in RstoxBase are those for defining resolution (e.g., PSUs and Layers), assignment, NASC data and StationLengthDistribution data, density, abundance and superindividual abundance
#' @docType package
#' @name RstoxBase
#'
"_PACKAGE"

# Global variables
utils::globalVariables(c(
	 ".", "..Cruise", "..DateTime", "..Hauls", "..LengthDistributionType", "..LengthInterval",
	 "..LengthIntervalWidths", "..VerticalResolutionMax", "..VerticalResolutionMin",
	 "..WeightingFactors", "..acceptedColumns", "..atMissingLengthGroup", "..by", "..cols",
	 "..columnsToKeep", "..extract", "..extractFromDataCopy", "..haulGrouping", "..intervalVector",
	 "..keys", "..keysSansSample", "..lengthVar", "..locatedStratum", "..meanBy",
	 "..paddingVariables", "..presentResolutionVariables", "..refvar", "..resolutionVar", "..sumBy",
	 "..variablesToGetFromAbundanceData", "..vars", "AcousticCategory", "AcousticCategoryKey",
	 "Area", "Beam", "BeamKey", "Biomass", "CatchFractionCount", "CatchFractionWeight", "Channel",
	 "ChannelReferenceDepth", "ChannelReferenceKey", "ChannelReferenceTilt", "ChannelReferenceType",
	 "Cruise", "CruiseKey", "DateTime", "Density", "DensityMethod", "DensityWeight", "Depth",
	 "DepthExponent", "EDSU", "EffectiveLogDistance", "EffectiveTowDistance", "Haul", "Individual",
	 "IndividualIndex", "IndividualKey", "IndividualRoundWeight", "IndividualTotalLength",
	 "IndividualTotalLengthMiddle", "Layer", "LengthDistributionType", "LengthExponent",
	 "LengthGroup", "LengthResolution", "LogDuration", "LogKey", "LogOrigin", "MaxChannelDepth",
	 "MaxChannelRange", "MeanLengthDistributionWeight", "MeanNASCWeight", "MergeStoxBiotic",
	 "MiddleDateTime", "MinChannelDepth", "MinChannelRange", "NASCKey", "NASCWeight", "NewValue",
	 "PSU", "ReplaceIndividual", "ReplaceIndividualIndex", "ReplaceLevel", "SSU", "SSUIndex",
	 "SplitAcousticCategory", "StartDateTime", "StationLevel", "StopDateTime", "Stratum",
	 "SummedWeights", "Survey", "SweepWidth", "TargetStrength", "TargetStrength0",
	 "TargetStrengthFunction", "TotalLength", "V1", "Value", "WeightedCount", "WeightingFactor",
	 "aggregationVariables", "assignmentID", "assignmentPasted", "backscatteringCrossSection",
	 "crossSection", "distance", "functionName", "haulWeightFactor", "imputeSeed", "includeintotal",
	 "individualCount", "individualWeightFactor", "inside", "intervalIndex", "midDepth",
	 "midIndividualTotalLength", "minDistance", "multiple", "packageName", "projectXMLFilePath",
	 "raisingFactor", "representativeBackscatteringCrossSection",
	 "representativeBackscatteringCrossSectionNormalized", "sd", "stratumPolygonFilePath",
	 "sumIndividualWeightFactor", "sumWeightedCount", "weighted", "weightingParameter", "x", "y"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxBase environment:
	initiateRstoxBase()
} 

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

