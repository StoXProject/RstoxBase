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
	 ".", "..Hauls", "..LengthDistributionType", "..LengthInterval", "..LengthIntervalWidths",
	 "..RowIndex", "..Stratum", "..VerticalResolutionMax", "..VerticalResolutionMin",
	 "..WeightingFactors", "..atMissingLengthGroup", "..by", "..extract", "..extractFromDataCopy",
	 "..haulGrouping", "..intervalVector", "..keys", "..keysSansSample", "..lengthVar",
	 "..presentResolutionVariables", "..refvar", "..resolutionVar", "..sumWeigthsBy",
	 "..variablesToGetFromAbundanceData", "..vars", "AllStrata", "Area", "Biomass",
	 "CatchFractionCount", "CatchFractionWeight", "ChannelReferenceDepth", "ChannelReferenceTilt",
	 "Cruise", "DateTime", "Density", "DensityWeight", "Depth", "DepthExponent",
	 "EffectiveLogDistance", "EffectiveTowedDistance", "Haul", "Individual", "IndividualIndex",
	 "IndividualKey", "IndividualRoundWeight", "IndividualTotalLength",
	 "IndividualTotalLengthMiddle", "Layer", "LengthDistributionType", "LengthExponent",
	 "LengthGroup", "LengthResolution", "LogDuration", "LogOrigin", "MaxChannelDepth",
	 "MaxChannelRange", "MeanLengthDistributionWeight", "MeanNASCWeight", "MergeStoxBiotic",
	 "MiddleDateTime", "MinChannelDepth", "MinChannelRange", "NASCDistributed", "NASCWeight",
	 "NewValue", "PSU", "ReplaceLevel", "ReplaceRowIndex", "RowIndex", "SSU", "SSUIndex",
	 "StartDateTime", "StationLevel", "StopDateTime", "Stratum", "SummedWeights", "Survey",
	 "SweepWidthNauticalMile", "TargetStrength", "TargetStrength0", "TargetStrengthFunction",
	 "TotalLength", "V1", "Value", "WeightedCount", "WeightingFactor", "assignmentID",
	 "assignmentPasted", "backscatteringCrossSection", "crossSection", "distance", "functionName",
	 "getRstoxFrameworkDefinitions", "haulWeightFactor", "imputeSeed", "individualCount", "inside",
	 "intervalIndex", "midDepth", "midIndividualTotalLength", "multiple", "packageName",
	 "raisingFactor", "representativeBackscatteringCrossSection",
	 "representativeBackscatteringCrossSectionNormalized", "sd", "sumWeightedCount", "weighted",
	 "weightingParameter", "x", "y"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxBase environment:
	initiateRstoxBase()
} 

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

