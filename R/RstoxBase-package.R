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
	 "..columnsToKeep", "..extract", "..extractFromDataCopy", "..haulGrouping",
	 "..horizontalResolution", "..keys", "..keysSansSample", "..lengthInterval",
	 "..lengthIntervalWidths", "..lengthVar", "..locatedStratum", "..meanBy", "..paddingVariables",
	 "..presentResolutionVariables", "..refvar", "..resolutionVar", "..sumBy", "..tomerge",
	 "..variablesToGetFromQuantityData", "..vars", "Abundance", "AcousticCategory",
	 "AcousticCategoryKey", "AcousticTargetStrength", "AllHaulsHaveAllSpeciesCategory",
	 "AllHaulsHaveAnySpeciesCategory", "Area", "Beam", "BeamKey", "Biomass", "CatchFractionNumber",
	 "CatchFractionWeight", "Channel", "ChannelReferenceDepth", "ChannelReferenceKey",
	 "ChannelReferenceTilt", "ChannelReferenceType", "ContainsAllSpeciesCategory",
	 "ContainsAnySpeciesCategory", "Cruise", "CruiseKey", "CruiseKey1", "DateTime", "Density",
	 "DensityType", "DensityWeight", "Depth", "DepthExponent", "EDSU", "EffectiveLogDistance",
	 "EffectiveTowDistance", "EstimationMethod", "Haul", "HaulKey", "Individual", "IndividualIndex",
	 "IndividualKey", "IndividualRoundWeight", "IndividualTotalLength",
	 "IndividualTotalLengthMiddle", "L1", "Latitude", "Layer", "LengthDistributionType",
	 "LengthExponent", "LengthResolution", "LogDuration", "LogKey", "LogOrigin", "Longitude",
	 "MaxChannelDepth", "MaxChannelRange", "MeanNASCWeight", "MiddleDateTime", "MinChannelDepth",
	 "MinChannelRange", "N", "NASCKey", "NASCWeight", "NumberOfAssignedHauls", "PSU",
	 "ReplaceIndividual", "ReplaceIndividualIndex", "ReplaceLevel", "SSU", "SSUIndex", "SSULabel",
	 "Sample", "SampleNumber", "SampleWeight", "SpeciesCategory", "SpeciesCategoryCatchWeight",
	 "SplitAcousticCategory", "StartDateTime", "Station", "StationLevel", "StopDateTime", "Stratum",
	 "StratumPolygon", "SummedWeights", "Survey", "SweepWidth", "TableName", "TargetStrength",
	 "TargetStrength0", "TargetStrengthFunction", "TempLengthGroupUsedInSuperIndividuals",
	 "TotalLength", "V1", "WeightedNumber", "WeightingFactor", "area", "area_hole", "assignmentID",
	 "assignmentPasted", "backscatteringCrossSection", "crossSection", "distance", "ggtitle",
	 "haulWeightFactor", "imputeSeed", "includeintotal", "individualNumber",
	 "individualWeightFactor", "inside", "insideRadius", "intervalIndex", "median",
	 "midIndividualTotalLength", "minDistance", "missingAssignment", "missingSpecies",
	 "numberOfIndividuals", "numberOfSubSamples", "polygonAreaSP_simple", "raisingFactor",
	 "representativeBackscatteringCrossSection",
	 "representativeBackscatteringCrossSectionNormalized", "setUnit", "sumArea",
	 "sumIndividualWeightFactor", "sumWeightedNumber", "x", "y"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxBase environment:
	initiateRstoxBase()
} 

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

