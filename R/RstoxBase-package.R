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
	 ".", "..Cruise", "..DateTime", "..DensityType", "..FileGroupingVariables",
	 "..TrackGroupingVariables", "..Hauls", "..LengthDistributionType",
	 "..SpeciesCategoryCatchType", "..VerticalResolutionMax", "..VerticalResolutionMin",
	 "..WeightingFactors", "..acceptedColumns", "..atMissingLengthGroup", "..by", "..cols",
	 "..columnNames", "..columnsToKeep", "..endNames", "..extract", "..extractFromDataCopy",
	 "..haulGrouping", "..keys", "..keysSansSample", "..lengthInterval", "..lengthIntervalWidths",
	 "..lengthVar", "..locatedStratum", "..meanBy", "..paddingVariables",
	 "..presentResolutionVariables", "..refvar", "..resolutionVar", "..startNames", "..sumBy",
	 "..tomerge", "..variablesToGetFromQuantityData", "..vars", "Abundance", "AcousticCategory",
	 "AcousticCategoryKey", "Area", "Beam", "BeamKey", "Biomass", "CatchFractionNumber",
	 "CatchFractionWeight", "Channel", "ChannelReferenceDepth", "ChannelReferenceKey",
	 "ChannelReferenceTilt", "ChannelReferenceType", "Coverage", "Cruise", "CruiseKey",
	 "CruiseKey1", "DateTime", "Density", "DensityType", "DensityWeight", "Depth", "DepthExponent",
	 "Direction", "Distance", "DistanceTransect", "Duration", "EDSU", "EffectiveLogDistance",
	 "EffectiveTowDistance", "EstimationMethod", "Haul", "HaulKey", "ImputationMethod",
	 "Individual", "IndividualIndex", "IndividualKey", "IndividualRoundWeight",
	 "IndividualTotalLength", "IndividualTotalLengthMiddle", "L1", "Latitude", "LatitudeEnd",
	 "LatitudeStart", "Layer", "LengthDistributionType", "LengthExponent", "LengthResolution",
	 "LogDuration", "LogKey", "LogOrigin", "Longitude", "LongitudeEnd", "LongitudeStart",
	 "MaxChannelDepth", "MaxChannelRange", "MeanNASCWeight", "MiddleDateTime", "MinChannelDepth",
	 "MinChannelRange", "NASCKey", "NASCWeight", "PSU", "PreySpeciesCategoryCatchWeightingFactor",
	 "ReplaceIndividual", "ReplaceIndividualIndex", "ReplaceLevel", "ReplaceStratumLayerIndividual",
	 "ReplaceStratumLayerIndividualIndex", "RuterNavn", "SSU", "SSUIndex", "Sample", "SampleNumber",
	 "SampleWeight", "SegmentType", "SpeciesCategory", "SpeciesCategoryCatchWeight", "Speed",
	 "SplitAcousticCategory", "StartDateTime", "Station", "StationLevel", "StopDateTime", "Stratum",
	 "StratumLayerIndividual", "StratumLayerIndividualIndex", "StratumName", "StratumPolygon",
	 "SummedWeights", "Survey", "SweepWidth", "TargetStrength", "TargetStrength0",
	 "TargetStrengthFunction", "TempLengthGroupUsedInSuperIndividuals", "TotalLength", "V1",
	 "WeightedNumber", "WeightingFactor", "X", "Y", "approx", "area", "area_hole", "assignmentID",
	 "assignmentPasted", "backscatteringCrossSection", "cov", "crossSection", "distance",
	 "haulWeightFactor", "head", "imputeSeed", "includeintotal", "individualNumber",
	 "individualWeightFactor", "inside", "insideRadius", "intervalIndex",
	 "midIndividualTotalLength", "minDistance", "missingAssignment", "missingSpecies",
	 "numberOfBeams", "numberOfIndividuals", "numberOfSubSamples", "polygonAreaSP_simple",
	 "raisingFactor", "representativeBackscatteringCrossSection",
	 "representativeBackscatteringCrossSectionNormalized", "runif", "setUnit", "sumArea",
	 "sumIndividualWeightFactor", "sumWeightedNumber", "temporary_denominator_column_name",
	 "temporary_numerator_column_name", "x", "y"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxBase environment:
	initiateRstoxBase()
} 

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

