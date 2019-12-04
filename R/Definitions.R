
# Define the variables of the main data types used in estimation models:
ModelVariables <- list(
  NASCData = list(
    horizontalResolution = c("Stratum", "PSU", "EDSU"), 
    verticalResolution = c("Layer", "Channel"), 
    groupingVariables = c("AcousticCategory", "ChannelReference", "Frequency"), 
    data = "NASC", 
    verticalDimension = c("MinRange", "MaxRange"), 
    weight = "NASCWeight", 
    other = "EffectiveLogDistance"
  ), 
  LengthDistribution = list(
    horizontalResolution = c("Stratum", "PSU", "Station"), 
    verticalResolution = c("Layer"), 
    groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
    data = "WeightedCount",
    verticalDimension = c("MinRange", "MaxRange"), 
    weight = "LengthDistributionWeight", 
    other = c("TowedDistance", "VerticalTrawlOpening", "TrawlDoorSpread", "LengthDistributionType")
  ), 
  Density = list(
    horizontalResolution = c("Stratum", "PSU"), 
    verticalResolution = c("Layer"), 
    groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
    data = "Density",
    verticalDimension = c("MinRange", "MaxRange"), 
    weight = "DensityWeight"
  ), 
  Abundance = list(
    horizontalResolution = c("Stratum"), 
    verticalResolution = c("Layer"), 
    groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
    data = "Abundance", 
    verticalDimension = c("MinRange", "MaxRange"), 
    weight = "AbundanceWeight"
  )
)



getColumnOrder <- function(dataType = c("NASCData", "LengthDistribution", "Density", "Abundance")) {
  dataType <- match.arg(dataType)
  unlist(resolution <- ModelVariables[[dataType]])
}




determineAggregationVariables <- function(
    data, 
    TargetResolution, 
    dataType = c("NASCData", "LengthDistribution", "Density", "Abundance"), 
    dimension = c("vertical", "horizontal")
    ) {
  
  # Get the requested type:
  dataType <- match.arg(dataType)
  dimension <- match.arg(dimension)
  
  # Get the relevant resolution variables:
  resolutionName <- paste0(dimension, "Resolution")
  resolution <- ModelVariables[[dataType]][[resolutionName]]
  # Get the present resolution variables:
  hasNAs <- data[, lapply(.SD, function(x) any(is.na(x))), .SDcols = resolution]
  presentResolution <- resolution[!hasNAs]
  
  # Get the finest resolution variable:
  finestResolution <- utils::tail(presentResolution,1)
  
  # And the resolution variables to aggregate by:
  aggregationResolution <- presentResolution[seq_len(min(which(presentResolution == TargetResolution)))]
  
  # Get the variables NOT to aggregate by, and diff these from all possigle grouping variables:
  aggregateBy <- setdiff(
    c(
      ModelVariables[[dataType]]$horizontalResolution, 
      ModelVariables[[dataType]]$verticalResolution, 
      ModelVariables[[dataType]]$groupingVariables
    ), 
    setdiff(resolution, aggregationResolution)
  )
  
  list(
    by = aggregateBy, 
    TargetResolution = TargetResolution, 
    presentResolution = presentResolution, 
    finestResolution = finestResolution, 
    dataVariable = ModelVariables[[dataType]]$data, 
    verticalDimension = ModelVariables[[dataType]]$verticalDimension
  )
}
