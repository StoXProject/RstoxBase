
# Define the variables of the main data types used in estimation models:
ModelVariables <- list(
<<<<<<< HEAD
    NASCData = list(
        horizontalResolution = c("Stratum", "PSU", "EDSU"), 
        vertictalResolution = c("Layer", "Channel"), 
        groupingVariables = c("AcousticCategory", "ChannelReference", "Frequency"), 
        data = "NASC", 
        verticalDimension = c("MinRange", "MaxRange"), 
        weight = "NASCWeight", 
        other = "LogDistance"
    ), 
    LengthDistribution = list(
        horizontalResolution = c("Stratum", "PSU", "Station"), 
        vertictalResolution = c("Layer"), 
        groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
        data = "WeightedCount",
        verticalDimension = c("MinRange", "MaxRange"), 
        weight = "LengthDistributionWeight", 
        other = c("TowedDistance", "VerticalTrawlOpening", "TrawlDoorSpread", "LengthDistributionType")
    ), 
    Density = list(
        horizontalResolution = c("Stratum", "PSU"), 
        vertictalResolution = c("Layer"), 
        groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
        data = "Density", 
        verticalDimension = c("MinRange", "MaxRange"), 
        weight = "DensityWeight"
    ), 
    Abundance = list(
        horizontalResolution = c("Stratum"), 
        vertictalResolution = c("Layer"), 
        groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
        data = "Abundance", 
        verticalDimension = c("MinRange", "MaxRange"), 
        weight = "AbundanceWeight"
    )
)


getColumnOrder <- function(dataType = c("NASCData", "LengthDistribution", "Density", "Abundance")) {
    dataType <- match.arg(dataType)
    unlist(resolution <- ModelVariables[[dataType]])
=======
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
>>>>>>> Acoustic
}



<<<<<<< HEAD
determineAggregationVariables <- function(data, TargetResolution, dataType = c("NASCData", "LengthDistribution", "Density", "Abundance"), dimension = c("vertictal", "horizontal")) {
    
    # Get the requested type:
    dataType <- match.arg(dataType)
    dimension <- match.arg(dimension)
    
    # Get the relevant resolution variables:
    resolutionName <- paste0(dimension, "Resolution")
    resolution <- ModelVariables[[dataType]][[resolutionName]]
    
    # Get the present resolution variables:
    hasNAs <- names(data)[data[, lapply(.SD, function(x) any(is.na(x))), .SDcols = resolution]]
    presentResolution <- resolution[!hasNAs]
    # Get the finest resolution variable:
    finestResolution <- presentResolution[1]
    
    # And the resolution variables to aggregate by:
    aggregationResolution <- presentResolution[seq_len(min(which(presentResolution == TargetResolution)))]
    
    # Get the variables NOT to aggregate by, and diff these from all possigle grouping variables:
    aggregateBy <- setdiff(
        c(
            ModelVariables[[dataType]]$horizontalResolution, 
            ModelVariables[[dataType]]$vertictalResolution, 
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
		

SumData <- function(data, TargetResolution, dataType = c("NASCData", "LengthDistribution", "Density", "Abundance")) {
    
    aggregationVariables <- determineAggregationVariables(data, TargetResolution = TargetResolution, dataType = dataType, dimension = "vertical")
        
    # Return immediately if TargetResolution equals inputResolution:
    if(TargetResolution == aggregationVariables$finestResolution) {
        return(data)
    }
    # Also return immediately if TargetResolution is not in presentResolutions:
    if(!TargetResolution %in% aggregationVariables$presentResolutions) {
        return(data)
    }
    
    # Sum the data, and update the range:
    data[, .(sum(x)), by = aggregationVariables$by, with = FALSE]
    
}


=======
determineAggregationVariables <- function(data, 
                                          TargetResolution, 
                                          dataType = c("NASCData", "LengthDistribution", "Density", "Abundance"), 
                                          dimension = c("vertical", "horizontal")) {
  
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

# 
# SumData <- function(data, TargetResolution, dataType = c("NASCData", "LengthDistribution", "Density", "Abundance")) {
#   
#   #get the grouping variables
#   aggregationVariables <- determineAggregationVariables(data, TargetResolution = TargetResolution, dataType = dataType, dimension = "vertical")
#   
#   
#   # Return immediately if TargetResolution equals inputResolution:
#   if(TargetResolution == aggregationVariables$finestResolution) {
#     print('Did not aggregate')
#     return(data)
#   }
#   # Also return immediately if TargetResolution is not in presentResolutions:
#   if(!TargetResolution %in% aggregationVariables$presentResolution) {
#     print('Did not aggregate2')
#     return(data)
#   }
#   
#   
#   # Sum the data, and update the range:
#   if(dataType =='NASCData')  data[, .(sum(NASC)), by = c(aggregationVariables$by)]  
#   
#   
#   #Add range i SumNasc
#   
#   #Add rest of the nasc info
#   
# }

MeanData <- function() {

}
>>>>>>> Acoustic

