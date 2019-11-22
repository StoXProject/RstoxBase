
# Define the variables of the main data types used in estimation models:
ModelVariables <- list(
    NASCData = list(
        horizontalResolution = c("Survey", "Stratum", "PSU", "EDSU"), 
        vertictalResolution = c("AllLayers", "Layer", "Channel"), 
        groupingVariables = c("AcousticCategory", "ChannelReference", "Frequency"), 
        verticalDimension = c("MinRange", "MaxRange"), 
        weight = "NASCWeight", 
        data = "NASC", 
        other = "LogDistance"
    ), 
    LengthDistribution = list(
        horizontalResolution = c("Survey", "Stratum", "PSU", "Station"), 
        vertictalResolution = c("AllLayers", "Layer"), 
        groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
        verticalDimension = c("MinRange", "MaxRange"), 
        weight = "LengthDistributionWeight", 
        data = "WeightedCount",
        other = c("TowedDistance", "VerticalTrawlOpening", "TrawlDoorSpread", "LengthDistributionType")
    ), 
    Density = list(
        horizontalResolution = c("Survey", "Stratum", "PSU"), 
        vertictalResolution = c("AllLayers", "Layer"), 
        groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
        verticalDimension = c("MinRange", "MaxRange"), 
        weight = "DensityWeight", 
        data = "Density"
    ), 
    Abundance = list(
        horizontalResolution = c("Survey", "Stratum"), 
        vertictalResolution = c("AllLayers", "Layer"), 
        groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
        verticalDimension = c("MinRange", "MaxRange"), 
        weight = "AbundanceWeight", 
        data = "Abundance"
    )
)



determineAggregationVariables <- function(data, TargetResolution, dataType = c("NASCData", "LengthDistribution", "Density", "Abundance"), dimension = c("vertictal", "horizontal")) {
    
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
    
    aggregationVariables <- determineAggregationVariables(data, TargetResolution = TargetResolution, dataType = dataType, dimension = dimension)
        
    # Return immediately if TargetResolution equals inputResolution:
    if(TargetResolution == aggregationVariables$finestResolution) {
        return(data)
    }
    # Also return immediately if TargetResolution is not in presentResolutions:
    if(!TargetResolution %in% aggregationVariables$presentResolutions) {
        return(data)
    }   
    
    # Sum the data, and update the range:
    #data[, .(), by = aggregationVariables$by, with = FALSE]
    
    
    
    
    
}
