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
    dataTypeDefinition <- list(
        NASCData = list(
            horizontalResolution = c("Stratum", "PSU", "EDSU"), 
            verticalResolution = c("Layer", "Channel"), 
            groupingVariables = c("AcousticCategory", "ChannelReference", "Frequency"), 
            data = "NASC", 
            #verticalDimension = c("MinChannelRange", "MaxChannelRange", "MinLayerRange", "MaxLayerRange"), 
            verticalRawDimension = c("MinChannelRange", "MaxChannelRange"), 
            verticalLayerDimension = c("MinLayerRange", "MaxLayerRange"), 
            weighting = "NASCWeight", 
            other = "EffectiveLogDistance"
        ), 
        LengthDistribution = list(
            horizontalResolution = c("Stratum", "PSU", "Station"), 
            verticalResolution = c("Layer", "Haul"), 
            groupingVariables = c("SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"), 
            data = "WeightedCount",
            #verticalDimension = c("MinHaulDepth", "MaxHaulDepth", "MinLayerRange", "MaxLayerRange"), 
            verticalRawDimension = c("MinHaulDepth", "MaxHaulDepth"), 
            verticalLayerDimension = c("MinLayerRange", "MaxLayerRange"), 
            weighting = "LengthDistributionWeight", 
            other = c("TowedDistance", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread", "LengthDistributionType")
        ), 
        AssignmentLengthDistribution = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            groupingVariables = c("SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"), 
            data = "WeightedCount",
            verticalLayerDimension = c("MinLayerRange", "MaxLayerRange"), 
            weighting = "AssignmentLengthDistributionWeight", 
            other = NULL
        ), 
        Density = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
            data = "Density",
            verticalLayerDimension = c("MinLayerRange", "MaxLayerRange"), 
            weighting = "DensityWeight", 
            other = NULL
        ), 
        Abundance = list(
            horizontalResolution = c("Stratum"), 
            verticalResolution = c("Layer"), 
            groupingVariables = c("SpeciesCategory", "LengthResolutionCentimeter", "LengthGroup"), 
            data = "Abundance", 
            verticalLayerDimension = c("MinLayerRange", "MaxLayerRange"), 
            weighting = NULL, 
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






getColumnOrder <- function(dataType) {
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    unlist(resolution <- dataTypeDefinition[[dataType]])
}

setColumnOrder <- function(data, dataType, allow.partial = TRUE, keep.all = TRUE) {
    # Get the column order:
    columnOrder <- getColumnOrder(dataType)
    
    # Select only the column names present in the data:
    if(allow.partial) {
        columnOrder <- intersect(columnOrder, names(data))
    }
    
    # Order the columns:
    data.table::setcolorder(data, columnOrder)
    
    if(!keep.all) {
        data <- data[, ..columnOrder]
    }
    
    return(data)
}



detectDataType <- function(data, only.data = FALSE) {
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    if(only.data) {
        present <- sapply(dataTypeDefinition, function(var) all(var$data %in% names(data)))
    }
    else {
        present <- sapply(dataTypeDefinition, function(var) all(unlist(var) %in% names(data)))
    }
    
    if(!any(present)) {
        warning("The input data does not contain all the expected variables.")
    }
    else if(sum(present) > 1) {
        warning("More than one element of the input list contains the expected variables (", paste(names(dataTypeDefinition)[present], collapse = ","), "). The first selected:")
    }
    
    output <- utils::head(names(dataTypeDefinition)[present], 1)
}


getAllAggregationVariables <- function(data, dataType = NULL) {
    # Get the requested type:
    if(length(dataType) == 0) {
        dataType <- detectDataType(data)
    }
    
    # Get the definitions:
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    
    # Return the full vector of aggregation variables:
    aggregateBy <- c(
        dataTypeDefinition[[dataType]]$horizontalResolution, 
        dataTypeDefinition[[dataType]]$verticalResolution, 
        dataTypeDefinition[[dataType]]$groupingVariables
    )
    
    return(aggregateBy)
}

determineAggregationVariables <- function(
        data, 
        TargetResolution, 
        dimension = c("vertical", "horizontal")
        ) {
    
    # Get the requested type:
    dimension <- match.arg(dimension)
    dataType <- detectDataType(data)
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    
    # Get the relevant resolution variables:
    resolutionName <- paste0(dimension, "Resolution")
    resolution <- dataTypeDefinition[[dataType]][[resolutionName]]
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
            dataTypeDefinition[[dataType]]$horizontalResolution, 
            dataTypeDefinition[[dataType]]$verticalResolution, 
            dataTypeDefinition[[dataType]]$groupingVariables
        ), 
        setToNA
    )
    
    out <- list(
        by = aggregateBy, 
        setToNA = setToNA, 
        TargetResolution = TargetResolution, 
        presentResolution = presentResolution, 
        finestResolution = finestResolution, 
        dataVariable = dataTypeDefinition[[dataType]]$data, 
        weightingVariable = dataTypeDefinition[[dataType]]$weighting, 
        verticalRawDimension = dataTypeDefinition[[dataType]]$verticalRawDimension, 
        verticalLayerDimension = dataTypeDefinition[[dataType]]$verticalLayerDimension, 
        dataTypeDefinition = dataTypeDefinition
    )
    return(out)
}
