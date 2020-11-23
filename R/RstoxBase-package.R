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
	 ".", "..abundanceGrouping", "..atMissingLengthGroup", "..by", "..columnOrder", "..extract",
	 "..extractFromDataCopy", "..haulGrouping", "..Hauls", "..intervalVector", "..keys",
	 "..keysSansSample", "..LengthDistributionType", "..LengthInterval", "..LengthIntervalWidths",
	 "..lengthVar", "..presentResolutionVariables", "..refvar", "..relevantVariables",
	 "..resolutionVar", "..RowIndex", "..Stratum", "..sumWeigthsBy", "..toAdd", "..validVariables",
	 "..variablesToGetFromAbundanceData", "..vars", "..VerticalResolutionMax",
	 "..VerticalResolutionMin", "..WeightingFactors", ".N", ":=", "abundanceWeightFactor",
	 "AllStrata", "approx", "Area", "assignmentID", "assignmentPasted",
	 "backscatteringCrossSection", "Biomass", "CatchFractionCount", "CatchFractionWeight",
	 "ChannelReferenceDepth", "ChannelReferenceOrientation", "crossSection", "Cruise", "Density",
	 "DensityWeight", "Depth", "DepthExponent", "distance", "EDSU", "EDSUIIndex",
	 "EffectiveLogDistance", "EffectiveTowedDistance", "functionName",
	 "getRstoxFrameworkDefinitions", "Haul", "haulWeightFactor", "imputeSeed", "Individual",
	 "individualCount", "IndividualIndex", "IndividualKey", "IndividualRoundWeight",
	 "IndividualTotalLength", "IndividualTotalLengthMiddle", "inside", "intervalIndex", "Layer",
	 "LengthDistributionType", "LengthDistributionWeight", "LengthExponent", "LengthGroup",
	 "LengthResolution", "MaxChannelDepth", "MaxChannelRange", "MeanLengthDistributionWeight",
	 "MeanNASCWeight", "mergeDataTables", "MergeStoxBiotic", "midDepth", "midIndividualTotalLength",
	 "MinChannelDepth", "MinChannelRange", "multiple", "na.omit", "NASCDistributed", "NASCWeight",
	 "NewValue", "packageName", "PSU", "quantile", "raisingFactor", "ReplaceLevel",
	 "ReplaceRowIndex", "representativeBackscatteringCrossSection",
	 "representativeBackscatteringCrossSectionNormalized", "RowIndex", "RstoxBaseEnv", "sd",
	 "StartDateTime", "StopDateTime", "Stratum", "SummedWeights", "sumWeightedCount", "Survey",
	 "SweepWidthNauticalMile", "TargetStrength", "TargetStrength0", "TargetStrengthFunction",
	 "TotalLength", "V1", "Value", "weighted", "WeightedCount", "WeightingFactor",
	 "weightingParameter", "x", "y"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxBase environment:
	initiateRstoxBase()
} 

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

