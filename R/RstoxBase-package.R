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
	 ".", "..Cruise", "..DateTime", "..DensityType", "..Hauls", "..LengthDistributionType",
	 "..SpeciesCategoryCatchType", "..VerticalResolutionMax", "..VerticalResolutionMin",
	 "..WeightingFactors", "..acceptedColumns", "..atMissingLengthGroup", "..by", "..cols",
	 "..columnsToKeep", "..extract", "..extractFromDataCopy", "..haulGrouping", "..intervalVector",
	 "..keys", "..keysSansSample", "..lengthInterval", "..lengthIntervalWidths", "..lengthVar",
	 "..locatedStratum", "..meanBy", "..paddingVariables", "..presentResolutionVariables",
	 "..refvar", "..resolutionVar", "..sumBy", "..tomerge", "..variablesToGetFromQuantityData",
	 "..vars", "Abundance", "AcousticCategory", "AcousticCategoryKey", "AcousticTargetStrength",
	 "Area", "Beam", "BeamKey", "Biomass", "CatchFractionNumber", "CatchFractionWeight", "Channel",
	 "ChannelReferenceDepth", "ChannelReferenceKey", "ChannelReferenceTilt", "ChannelReferenceType",
	 "Cruise", "CruiseKey", "CruiseKey1", "DateTime", "Density", "DensityType", "DensityWeight",
	 "Depth", "DepthExponent", "EDSU", "EffectiveLogDistance", "EffectiveTowDistance",
	 "EstimationMethod", "HasAnyPositiveWeightedNumber", "Haul", "HaulKey", "Individual",
	 "IndividualIndex", "IndividualKey", "IndividualRoundWeight", "IndividualTotalLength",
	 "IndividualTotalLengthMiddle", "Layer", "LengthDistributionType", "LengthExponent",
	 "LengthGroup", "LengthResolution", "LogDuration", "LogKey", "LogOrigin", "MaxChannelDepth",
	 "MaxChannelRange", "MeanNASCWeight", "MiddleDateTime", "MinChannelDepth", "MinChannelRange",
	 "N", "NASCKey", "NASCWeight", "NumberOfAssignedHauls", "NumberOfAssignedHaulsWithCatch", "PSU",
	 "ReplaceIndividual", "ReplaceIndividualIndex", "ReplaceLevel", "SSU", "SSUIndex",
	 "SpeciesCategory", "SpeciesCategoryCatchWeight", "SplitAcousticCategory", "StartDateTime",
	 "Station", "StationLevel", "StopDateTime", "Stratum", "SummedWeights", "Survey", "SweepWidth",
	 "TableName", "TargetStrength", "TargetStrength0", "TargetStrengthFunction", "TotalLength",
	 "V1", "ValidHaul", "WeightedNumber", "WeightingFactor", "assignmentID", "assignmentPasted",
	 "backscatteringCrossSection", "crossSection", "distance", "functionName", "haulWeightFactor",
	 "imputeSeed", "includeintotal", "individualNumber", "individualWeightFactor", "inside",
	 "intervalIndex", "midIndividualTotalLength", "minDistance", "missingAssignment", "multiple",
	 "numberOfIndividuals", "numberOfSubSamples", "packageName", "raisingFactor",
	 "representativeBackscatteringCrossSection",
	 "representativeBackscatteringCrossSectionNormalized", "sumIndividualWeightFactor",
	 "sumWeightedNumber", "weighted", "weightingParameter", "x", "y"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxBase environment:
	initiateRstoxBase()
} 

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

