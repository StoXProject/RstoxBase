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
            categoryVariable = "AcousticCategory", 
            groupingVariables = c("Frequency"), 
            coordinateSystemOrigin = "ChannelReferenceDepth", 
            coordinateSystemOrientation = "ChannelReferenceOrientation", 
            data = "NASC", 
            #verticalDimension = c("MinChannelRange", "MaxChannelRange", "MinLayerDepth", "MaxLayerDepth"), 
            verticalRawDimension = c("MinChannelRange", "MaxChannelRange"), 
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "NASCWeight", 
            type = "ChannelReferenceType", 
            other = "EffectiveLogDistance"
        ), 
        LengthDistributionData = list(
            horizontalResolution = c("Stratum", "PSU", "Station"), 
            verticalResolution = c("Layer", "Haul"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"), 
            data = "WeightedCount",
            #verticalDimension = c("MinHaulDepth", "MaxHaulDepth", "MinLayerDepth", "MaxLayerDepth"), 
            verticalRawDimension = c("MinHaulDepth", "MaxHaulDepth"), 
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "LengthDistributionWeight", 
            type = "LengthDistributionType", 
            other = c("EffectiveTowedDistance", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
        ), 
        AssignmentLengthDistributionData = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"), 
            data = "WeightedCount",
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "AssignmentLengthDistributionWeight", 
            other = NULL
        ), 
        DensityData = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"), 
            data = "Density",
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "DensityWeight", 
            other = NULL
        ), 
        AbundanceData = list(
            horizontalResolution = c("Stratum"), 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"), 
            data = "Abundance", 
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = NULL, 
            other = NULL
        )
    )
    
    # Define the variables of the main data types used in estimation models:
    getRequiredVariables <- function(x) {
        c(
            utils::tail(x$horizontalResolution, 1), 
            utils::tail(x$verticalResolution, 1), 
            x$categoryVariable, 
            x$groupingVariables, 
            x$weighting
            #utils::head(x$groupingVariables, 1), 
            #x$data, 
            #x$weighting
        )
    }
    dataTypeRequiredVariables <- lapply(dataTypeDefinition, getRequiredVariables)
    
    proj4string <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    targetStrengthParameters <- list(
        Standard = c("m", "a"), 
        DepthDependent = c("m", "a", "d")
    )
    
    AcousticPSUPrefix <- "PSU"
    SweptAreaPSUPrefix <- "PSU"
    
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
    columns <- unlist(dataTypeDefinition[[dataType]])
    return(columns)
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



detectDataType <- function(data) {
    
    #dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    #if(only.data) {
    #    present <- sapply(dataTypeDefinition, function(var) all(var$data %in% names(data)))
    #}
    #else {
    #    present <- sapply(dataTypeDefinition, function(var) all(unlist(var) %in% names(data)))
    #}
    
    dataTypeRequiredVariables <- getRstoxBaseDefinitions("dataTypeRequiredVariables")
    present <- sapply(dataTypeRequiredVariables, function(var) all(var %in% names(data)))
    
    if(!any(present)) {
        missing <- lapply(dataTypeRequiredVariables, function(var) setdiff(var %in% names(data)))
        
        missing <- lapply(missing, paste, collapse = ", ")
        missing <- paste(names(missing), missing, sep = ": ", collapse = ". ")
        
        
        warning("The input data does not contain all the expected variables. The following are needed: ", missing)
    }
    else if(sum(present) > 1) {
        message("More than one element of the input list contains the expected variables (", paste(names(dataTypeRequiredVariables)[present], collapse = ", "), "). The first selected:")
    }
    
    output <- utils::head(names(dataTypeRequiredVariables)[present], 1)
    return(output)
}


getAllDataTypeVariables <- function(dataType, unlist = TRUE) {
    aggregateBy <- getDataTypeDefinition(
        dataType = dataType, 
        unlist = unlist
    )
}




getAllAggregationVariables <- function(dataType, exclude.groupingVariables = FALSE) {
    
    # Define the elements to return:
    aggregationElements <- c("horizontalResolution", "verticalResolution", "categoryVariable", if(!exclude.groupingVariables) "groupingVariables")
    
    # Get the definitions:
    aggregateBy <- getDataTypeDefinition(
        dataType = dataType, 
        elements = aggregationElements, 
        unlist = TRUE
    )
    
    return(aggregateBy)
}

getAllResolutionVariables <- function(dataType) {
    
    # Define the elements to return:
    resolutionElements <- c("horizontalResolution", "verticalResolution")
    
    # Get the definitions:
    resolution <- getDataTypeDefinition(
        dataType = dataType, 
        elements = resolutionElements, 
        unlist = TRUE
    )
    
    return(resolution)
}

getDataTypeDefinition <- function(dataType, elements = NULL, unlist = FALSE) {
    
    # Get the requested type:
    if(length(dataType) == 0) {
        dataType <- detectDataType(data)
    }
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    thisDataTypeDefinition <- dataTypeDefinition[[dataType]]
    
    # Extract the elements to return:
    if(length(elements)) {
        thisDataTypeDefinition <- thisDataTypeDefinition[elements]
    }
    # Unlist if specified:
    if(unlist) {
        thisDataTypeDefinition <- unlist(thisDataTypeDefinition)
    }
    
    return(thisDataTypeDefinition)
}

determineAggregationVariables <- function(
        data, 
        dataType, 
        targetResolution, 
        dimension = c("vertical", "horizontal")
        ) {
    
    # Get the requested type:
    dimension <- match.arg(dimension)
    #dataType <- detectDataType(data)
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    thisDataTypeDefinition <- dataTypeDefinition[[dataType]]
    
    # Get the relevant resolution variables:
    resolutionName <- paste0(dimension, "Resolution")
    resolution <- thisDataTypeDefinition[[resolutionName]]
    # Get the present resolution variables:
    hasAnyNonNA <- unlist(data[, lapply(.SD, function(x) any(!is.na(x))), .SDcols = resolution])
    presentResolution <- resolution[hasAnyNonNA]
    
    # Get the finest resolution variable:
    finestResolution <- utils::tail(presentResolution,1)
    
    # And the resolution variables to aggregate by:
    # If the target resolution is not in the presen resolution, abort:
    presentIsTarget <- presentResolution == targetResolution
    if(!any(presentIsTarget)) {
        stop("The target resolution is not one of the columns of the present resolution")
    }
    aggregationResolution <- presentResolution[seq_len(min(which(presentIsTarget)))]
    
    # Get the variables NOT to aggregate by:
    setToNA <- setdiff(resolution, aggregationResolution)
    
    # ... and diff these from all possigle grouping variables:
    aggregateBy <- setdiff(
        c(
            thisDataTypeDefinition$horizontalResolution, 
            thisDataTypeDefinition$verticalResolution, 
            thisDataTypeDefinition$categoryVariable, 
            thisDataTypeDefinition$groupingVariables
        ), 
        setToNA
    )
    
    out <- list(
        by = aggregateBy, 
        setToNA = setToNA, 
        targetResolution = targetResolution, 
        presentResolution = presentResolution, 
        finestResolution = finestResolution, 
        dataVariable = thisDataTypeDefinition$data, 
        weightingVariable = thisDataTypeDefinition$weighting, 
        otherVariables = thisDataTypeDefinition$other, 
        verticalRawDimension = thisDataTypeDefinition$verticalRawDimension, 
        verticalLayerDimension = thisDataTypeDefinition$verticalLayerDimension, 
        dataTypeDefinition = dataTypeDefinition
    )
    return(out)
}




