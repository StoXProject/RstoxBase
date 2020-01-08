##################################################
##################################################
#' Definitions stored in the RstoxBase environment
#' 
#' This function declares the RstoxBase environment and writes vital definitions to it.
#' 
#' @return
#' A list of definitions.
#' 
#' @noRd
#' @seealso Use \code{\link{getRstoxBaseDefinitions}} to get the definitions.
#' 
initiateRstoxBase <- function(){
    
    # Define the variables of the main data types used in estimation models:
    modelVariables <- list(
        NASCData = list(
            horizontalResolution = c("Stratum", "PSU", "EDSU"), 
            verticalResolution = c("Layer", "Channel"), 
            groupingVariables = c("AcousticCategory", "ChannelReference", "Frequency"), 
            data = "NASC", 
            verticalDimension = c("MinChannelRange", "MaxChannelRange", "MinLayerRange", "MaxLayerRange"), 
            weight = "NASCWeight", 
            other = "EffectiveLogDistance"
        ), 
        LengthDistribution = list(
            horizontalResolution = c("Stratum", "PSU", "Station"), 
            verticalResolution = c("Layer", "Haul"), 
            groupingVariables = c("SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"), 
            data = "WeightedCount",
            verticalDimension = c("MinHaulDepth", "MaxHaulDepth", "MinLayerRange", "MaxLayerRange"), 
            weight = "LengthDistributionWeight", 
            other = c("TowedDistance", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread", "LengthDistributionType")
        ), 
        AssignmentLengthDistribution = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            groupingVariables = c("SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"), 
            data = "WeightedCount",
            verticalDimension = c("MinLayerRange", "MaxLayerRange"), 
            weight = "AssignmentLengthDistributionWeight", 
            other = NULL
        ), 
        Density = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
            data = "Density",
            verticalDimension = c("MinLayerRange", "MaxLayerRange"), 
            weight = "DensityWeight", 
            other = NULL
        ), 
        Abundance = list(
            horizontalResolution = c("Stratum"), 
            verticalResolution = c("Layer"), 
            groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
            data = "Abundance", 
            verticalDimension = c("MinLayerRange", "MaxLayerRange"), 
            weight = NULL, 
            other = NULL
        )
    )
    
    #### Assign to RstoxEnv and return the definitions: ####
    definitionsNames <- ls()
    definitions <- lapply(definitionsNames, get, pos = environment())
    names(definitions) <- definitionsNames
    
    #### Create the RstoxBaseEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxBase:::RstoxBaseEnv: ####
    utils::globalVariables("RstoxBaseEnv")
    assign("RstoxBaseEnv", new.env(), parent.env(environment()))
    assign("definitions", definitions, envir=get("RstoxBaseEnv"))
    
    #### Return the definitions: ####
    definitions
}


##################################################
##################################################
#' Get RstoxBase definitions
#' 
#' This function gets vital definitions from the RstoxBase environment.
#' 
#' @param name  An optional string vector denoting which definitions to extract.
#' @param ...   values overriding the values of definitions.
#' 
#' @return
#' A list of definitions.
#' 
#' @examples
#' getRstoxBaseDefinitions()
#' 
#' @export
#' 
getRstoxBaseDefinitions <- function(name = NULL, ...) {
    
    # Save the optional inputs for overriding the output:
    l <- list(...)
    
    # Get all or a subset of the definitions:
    definitions <- get("RstoxBaseEnv")$definitions
    if(length(name)){
        definitions <- definitions[[name]]
    }
    
    l <- l[names(l) %in% names(definitions)]
    if(length(l)){
        definitions <- utils::modifyList(definitions, l)
    }
    
    definitions
}






getColumnOrder <- function(dataType = c("NASCData", "LengthDistribution", "Density", "Abundance")) {
    dataType <- match.arg(dataType)
    modelVariables <- getRstoxBaseDefinitions("modelVariables")
    unlist(resolution <- modelVariables[[dataType]])
}



detectDataType <- function(data) {
    modelVariables <- getRstoxBaseDefinitions("modelVariables")
    allPresent <- sapply(modelVariables, function(var) all(unlist(var) %in% names(data)))
    if(!any(allPresent)) {
        warning("The input data does not contain all the extected variables.")
    }
    names(modelVariables)[allPresent]
}


determineAggregationVariables <- function(
        data, 
        TargetResolution, 
        dimension = c("vertical", "horizontal")
        ) {
    
    # Get the requested type:
    dimension <- match.arg(dimension)
    dataType <- detectDataType(data)
    modelVariables <- getRstoxBaseDefinitions("modelVariables")
    
    # Get the relevant resolution variables:
    resolutionName <- paste0(dimension, "Resolution")
    resolution <- modelVariables[[dataType]][[resolutionName]]
    # Get the present resolution variables:
    hasAnyNonNA <- unlist(data[, lapply(.SD, function(x) any(!is.na(x))), .SDcols = resolution])
    presentResolution <- resolution[hasAnyNonNA]
    
    # Get the finest resolution variable:
    finestResolution <- utils::tail(presentResolution,1)
    
    # And the resolution variables to aggregate by:
    aggregationResolution <- presentResolution[seq_len(min(which(presentResolution == TargetResolution)))]
    
    # Get the variables NOT to aggregate by:
    setToNA <- setdiff(resolution, aggregationResolution)
    
    # ... and diff these from all possigle grouping variables:
    aggregateBy <- setdiff(
        c(
            modelVariables[[dataType]]$horizontalResolution, 
            modelVariables[[dataType]]$verticalResolution, 
            modelVariables[[dataType]]$groupingVariables
        ), 
        setToNA
    )
    
    out <- list(
        by = aggregateBy, 
        setToNA = setToNA, 
        TargetResolution = TargetResolution, 
        presentResolution = presentResolution, 
        finestResolution = finestResolution, 
        dataVariable = modelVariables[[dataType]]$data, 
        verticalDimension = modelVariables[[dataType]]$verticalDimension, 
        modelVariables = modelVariables
    )
    return(out)
}
