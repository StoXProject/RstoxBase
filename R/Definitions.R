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
        # NASC: 
        NASCData = list(
            horizontalResolution = "EDSU", 
            verticalResolution = "Channel", 
            categoryVariable = "AcousticCategory", 
            groupingVariables = "Frequency", 
            data = "NASC", 
            verticalRawDimension = c("MinChannelDepth", "MaxChannelDepth"), 
            verticalLayerDimension = NULL, 
            weighting = "NASCWeight", 
            type = "ChannelReferenceType", 
            other = c("Cruise", "EffectiveLogDistance", "DateTime", "Longitude", "Latitude")
        ), 
        SumNASCData = list(
            horizontalResolution = "EDSU", 
            verticalResolution = "Layer", 
            categoryVariable = "AcousticCategory", 
            groupingVariables = "Frequency", 
            data = "NASC", 
            verticalRawDimension = NULL, 
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "SumNASCWeight", 
            type = "ChannelReferenceType", 
            other = c("EffectiveLogDistance", "DateTime", "Longitude", "Latitude")
        ), 
        MeanNASCData = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = "Layer", 
            categoryVariable = "AcousticCategory", 
            groupingVariables = "Frequency", 
            data = "NASC", 
            verticalRawDimension = NULL, 
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "MeanNASCWeight", 
            type = "ChannelReferenceType", 
            other = NULL
        ), 
        # LengthDistribution:
        LengthDistributionData = list(
            horizontalResolution = "Station", 
            verticalResolution = "Haul", 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = "WeightedCount",
            verticalRawDimension = c("MinHaulDepth", "MaxHaulDepth"), 
            verticalLayerDimension = NULL, 
            weighting = "LengthDistributionWeight", 
            type = "LengthDistributionType", 
            other = c("Cruise", "EffectiveTowedDistance", "DateTime", "Longitude", "Latitude", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
        ), 
        SumLengthDistributionData = list(
            horizontalResolution = "Station", 
            verticalResolution = "Layer", 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = "WeightedCount",
            verticalRawDimension = NULL, 
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "SumLengthDistributionWeight", 
            type = "LengthDistributionType", 
            other = c("EffectiveTowedDistance", "DateTime", "Longitude", "Latitude", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
        ), 
        MeanLengthDistributionData = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = "Layer", 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = "WeightedCount",
            verticalRawDimension = NULL, 
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "MeanLengthDistributionWeight", 
            type = "LengthDistributionType", 
            other = NULL
        ), 
        AssignmentLengthDistributionData = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = "WeightedCount",
            verticalLayerDimension = NULL, # Not needed, as this datatype is only used in AcousticDensity.
            weighting = NULL, 
            type = "LengthDistributionType", 
            other = NULL
        ), 
        # Density:
        DensityData = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = "Density",
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "DensityWeight", 
            other = NULL
        ), 
        MeanDensityData = list(
            horizontalResolution = c("Stratum"), 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = "Density",
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "MeanDensityWeight", 
            other = NULL
        ), 
        # Abundance:
        AbundanceData = list(
            horizontalResolution = c("Stratum"), 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = "Abundance", 
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = NULL, 
            other = NULL
        ), 
        IndividualsData = list(
            horizontalResolution = "Stratum", 
            verticalResolution = c("Layer", "Haul"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = NULL, 
            verticalLayerDimension = NULL, # Not relevant
            weighting = NULL, 
            other = NULL
        ), 
        SuperIndividualsData = list(
            horizontalResolution = "Stratum", 
            verticalResolution = c("Layer", "Haul"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = c("Abundance",  "Biomass"), 
            verticalLayerDimension = NULL, # Not relevant
            weighting = NULL, 
            other = NULL
        ), 
        ImputeSuperIndividualsData = list(
            horizontalResolution = "Stratum", 
            verticalResolution = c("Layer", "Haul"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = c("Abundance",  "Biomass"), 
            verticalLayerDimension = NULL, # Not relevant
            weighting = NULL, 
            other = NULL
        ), 
        BioticAssignment = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = "Layer", 
            data = "Haul", 
            weighting = "WeightingFactor", 
            other = NULL
        )
    )
    
    resolutionClasses <- list(
        NASC = list(
            vertical= c("Layer", "Channel"), 
            horizontal = c("Stratum", "PSU", "EDSU")
        ), 
        LengthDistribution = list(
            vertical= c("Layer", "Haul"), 
            horizontal = c("Stratum", "PSU", "Station")
        ), 
        Density = list(
            vertical= c("Layer"), 
            horizontal = c("Stratum", "PSU")
        ), 
        Abundance = list(
            vertical= c("Layer"), 
            horizontal = c("Stratum")
        )
    )
    
    allResolution = list(
        NASCData = "NASC", 
        SumNASCData = "NASC", 
        MeanNASCData = "NASC", 
        LengthDistributionData = "LengthDistribution", 
        SumLengthDistributionData = "LengthDistribution", 
        MeanLengthDistributionData = "LengthDistribution", 
        DensityData = "Density",
        MeanDensityData = "Density",
        AbundanceData = "Abundance"
    )
    
    emptyStratumPolygon <- sp::SpatialPolygonsDataFrame(
        sp::SpatialPolygons(list()), 
        data = data.frame()
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
        LengthDependent = c("TargetStrength0", "LengthExponent"), 
        LengthAndDepthDependent = c("TargetStrength0", "LengthExponent", "DepthExponent"), 
        LengthExponent = "LengthExponent", 
        TargetStrengthByLength = c("TargetStrength", "TotalLength")
    )
    
    targetStrengthMethodTypes  <- list(
        LengthDependent = "Function", 
        LengthAndDepthDependent = "Function", 
        LengthExponent = "Function", 
        TargetStrengthByLength = "Table"
    )
    
    # Define the various acoustic target strength functions:
    # 1. TS = TS0 + M log10(Lcm): 
    TargetStrengthFunction_LengthDependent <- function(midIndividualTotalLength, TargetStrength0, LengthExponent, Depth) {
        TargetStrength0 + 
            LengthExponent * log10(midIndividualTotalLength)
    }
    # 2. TS = TS0 + M log10(Lcm) + D log10(1 + D/10) # Ona 2003:
    TargetStrengthFunction_LengthAndDepthDependent <- function(midIndividualTotalLength, TargetStrength0, LengthExponent, Depth) {
        TargetStrength0 + 
            LengthExponent * log10(midIndividualTotalLength) + 
            DepthExponent * log10(1 + Depth/10)
    }
    # 3. TS = M log10(Lcm): 
    TargetStrengthFunction_LengthExponent <- function(midIndividualTotalLength, LengthExponent) {
        LengthExponent * log10(midIndividualTotalLength)
    }
    
    AcousticPSUPrefix <- "PSU"
    BioticPSUPrefix <- "PSU"
    
    nauticalMileInMeters <- 1852
    
    # List of functions avilable for report functions:
    reportFunctions <- list(
        summaryStox = "RstoxBase", 
        sum = "base", 
        mean = "base", 
        weighted.mean = "stats", 
        median = "stats", 
        min = "base", 
        max = "base", 
        sd = "stats", 
        var = "stats", 
        cv = "RstoxBase", 
        summary = "base", 
        quantile = "stats", 
        percentile_5_95 = "RstoxBase"
    )
    
    #### Assign to RstoxBaseEnv and return the definitions: ####
    definitionsNames <- ls()
    definitions <- lapply(definitionsNames, get, pos = environment())
    names(definitions) <- definitionsNames
    
    #### Create the RstoxBaseEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxBase:::RstoxBaseEnv: ####
    #utils::globalVariables("RstoxBaseEnv")
    utils::globalVariables(c(
        "RstoxBaseEnv", 
        ":=", ".", ".N", 
        "..abundanceGrouping", 
        "..atMissingLengthGroup", 
        "..by", 
        "..columnOrder",
        "..haulGrouping", 
        "..Hauls", 
        "..intervalVector", 
        "..keys", 
        "..keysSansSample", 
        "..LengthInterval", 
        "..LengthIntervalWidths", 
        "..lengthVar", 
        "..refvar", 
        "..relevantVariables",
        "..resolutionVar", 
        "..toAdd", 
        "..validVariables", 
        "..vars", 
        "..VerticalResolutionMax", 
        "..VerticalResolutionMin", 
        "..WeightingFactors", 
        "abundanceWeightFactor", 
        "Area", 
        "assignmentID", 
        "Density", 
        "DensityWeight", 
        "EffectiveLogDistance", 
        "Haul", 
        "individualCount", 
        "IndividualTotalLength", 
        "IndividualTotalLengthMiddle", 
        "intervalIndex", 
        "LengthDistributionType", 
        "LengthDistributionWeight", 
        "LengthGroup", 
        "LengthResolution", 
        "NASCWeight", 
        "PSU", 
        "raisingFactor", 
        "WeightedCount", 
        "WeightingFactor"
    ))
    assign("RstoxBaseEnv", new.env(), parent.env(environment()))
    assign("definitions", definitions, envir = get("RstoxBaseEnv"))
    
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






#getColumnOrder <- function(dataType) {
#    #dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
#    #columns <- unlist(dataTypeDefinition[[dataType]])
#    #return(columns)
#    getDataTypeDefinition(dataType, unlist = TRUE)
#}

#setColumnOrder <- function(data, dataType, allow.partial = TRUE, keep.all = TRUE) {
formatOutput <- function(data, dataType, keep.all = TRUE, allow.missing = FALSE) {
    
    # Remove any duplicated columns:
    if(any(duplicated(names(data)))) {
        data[, which(duplicated(names(data))) := NULL]
    }
    
    # Get the column order:
    #columnOrder <- getColumnOrder(dataType)
    columnOrder <- getDataTypeDefinition(dataType, unlist = TRUE)
    if(allow.missing) {
        columnOrder <- intersect(columnOrder, names(data))
    }
    
    # Order the columns:
    data.table::setcolorder(data, columnOrder)
    
    if(!keep.all) {
        #toRemove <- setdiff(names(data), columnOrder)
        #if(length(toRemove)) {
        #    data[, eval(toRemove) := NULL]
        #}
        
        removeColumnsByReference(
            data = data, 
            toRemove =  setdiff(names(data), columnOrder)
        )
        
        #data <- data[, ..columnOrder]
    }
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
        
        
        warning("StoX: The input data does not contain all the expected variables. The following are needed: ", missing)
    }
    else if(sum(present) > 1) {
        message("More than one element of the input list contains the expected variables (", paste(names(dataTypeRequiredVariables)[present], collapse = ", "), "). The first selected:")
    }
    
    output <- utils::head(names(dataTypeRequiredVariables)[present], 1)
    return(output)
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

getResolutionVariables <- function(dataType = NULL, dimension = c("horizontal", "vertical")) {
    
    # Define the elements to return:
    resolutionElements <- paste0(dimension, "Resolution")
    
    # Get the definitions:
    resolution <- unique(
        getDataTypeDefinition(
            dataType = dataType, 
            elements = resolutionElements, 
            unlist = TRUE
        )
    )
    
    return(resolution)
}

getAllResolutionVariables <- function(dataType, dimension = NULL, other = FALSE) {
    # Define the valid dimensions, and select the other if requested:
    validDimensions <- c("horizontal", "vertical")
    
    if(!length(dimension)) {
        dimension <- validDimensions
    }
    else {
        dimension <- match.arg(dimension, validDimensions)
        if(other) {
            dimension <- setdiff(validDimensions, dimension)
        }
    }
    
    allResolution <- getRstoxBaseDefinitions("allResolution")[[dataType]]
    resolutionClasses <- getRstoxBaseDefinitions("resolutionClasses")
    if(length(allResolution)) {
        unlist(resolutionClasses[[allResolution]][dimension])
    }
    else {
        NULL
    }
}

# dataType can be NULL, implying all data types, a vector of data type names or a logical function of data type names, such as endswith(x, "NASCData"):
getDataTypeDefinition <- function(dataType = NULL, elements = NULL, unlist = FALSE) {
    
    # Get the requested type:
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    # If given as a function of data type names (names of 'dataTypeDefinition'), apply the function to subset the data types:
    if(is.function(dataType)) {
        dataTypeDefinitionNames <- names(dataTypeDefinition)
        dataType <- subset(dataTypeDefinitionNames, dataType(dataTypeDefinitionNames))
    }
    # If NULL use all data types:
    if(length(dataType) == 0) {
        dataType <- names(dataTypeDefinition)
    }
    thisDataTypeDefinition <- dataTypeDefinition[dataType]
    
    # Extract the elements to return:
    if(length(elements)) {
        thisDataTypeDefinition <- lapply(thisDataTypeDefinition, "[", elements)
    }
    if(length(dataType) == 1) {
        thisDataTypeDefinition <- thisDataTypeDefinition[[dataType]]
    }
        
    # Unlist if specified:
    if(unlist) {
        thisDataTypeDefinition <- unlist(thisDataTypeDefinition)
    }
    
    return(thisDataTypeDefinition)
}


#getAllDataTypeDefinitions <- function(elements = NULL, unlist = FALSE) {
#    
#    # Get the requested type:
#    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
#    
#    # Extract the elements to return:
#    if(length(elements)) {
#        dataTypeDefinition <- lapply(dataTypeDefinition, "[", elements)
#    }
#    # Unlist if specified:
#    if(unlist) {
#        dataTypeDefinition <- unlist(dataTypeDefinition)
#    }
#    
#    return(dataTypeDefinition)
#}

determineAggregationVariables <- function(
        data, 
        dataType, 
        targetResolution, 
        dimension = c("vertical", "horizontal")
        ) {
    
    # Get the requested type:
    dimension <- match.arg(dimension)
    #dataType <- detectDataType(data)
    #dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    #thisDataTypeDefinition <- dataTypeDefinition[[dataType]]
    dataTypeDefinition <- getDataTypeDefinition(dataType)
    
    # Get the relevant resolution variables:
    thisResolution <- getAllResolutionVariables(
        dataType = dataType, 
        dimension = dimension
    )
    thisResolution <- intersect(thisResolution, names(data))
    otherResolution <- getAllResolutionVariables(
        dataType = dataType, 
        dimension = dimension, 
        other = TRUE
    )
    otherResolution <- intersect(otherResolution, names(data))
    
    # Get the present resolution variables:
    hasAnyNonNA <- unlist(data[, lapply(.SD, function(x) any(!is.na(x))), .SDcols = thisResolution])
    presentResolution <- thisResolution[hasAnyNonNA]
    
    ## Get the finest resolution variable:
    #finestResolution <- utils::tail(presentResolution, 1)
    #
    ## Get the higher resolution than the finest:
    #allButFinestResolution <- setdiff(presentResolution, finestResolution)
    
    # Get the next resolution, that is the resolution one level higher than the present resolution:
    nextResolution <- if(length(presentResolution) == 1) NA else presentResolution[length(presentResolution) - 1]
    
    # And the resolution variables to aggregate by:
    # If the target resolution is not in the presen resolution, abort:
    presentIsTarget <- presentResolution == targetResolution
    if(!any(presentIsTarget)) {
        stop("TargetResolution (", targetResolution, ") is not one of the columns of the present resolution (", paste(presentResolution, collapse = ", "), "). Possibly, the specified TargetResolution has not been added to the data. In that case, specify the function inputs *PSU or *Layer, where * can be Acoustic or Biotic")
    }
    aggregationResolution <- presentResolution[seq_len(min(which(presentIsTarget)))]
    
    # Get the variables NOT to aggregate by:
    setToNA <- setdiff(thisResolution, aggregationResolution)
    
    # ... and diff these from all possigle grouping variables:
    aggregateBy <- setdiff(
        c(
            aggregationResolution, 
            otherResolution, 
            dataTypeDefinition$categoryVariable, 
            dataTypeDefinition$groupingVariables
        ), 
        setToNA
    )
    
    out <- list(
        #dataTypeDefinition = dataTypeDefinition, 
        by = aggregateBy, 
        setToNA = setToNA, 
        targetResolution = targetResolution, 
        presentResolution = presentResolution, 
        #finestResolution = finestResolution, 
        #allButFinestResolution = allButFinestResolution, 
        nextResolution = nextResolution, 
        dataVariable = dataTypeDefinition$data, 
        weightingVariable = dataTypeDefinition$weighting, 
        otherVariables = dataTypeDefinition$other, 
        verticalRawDimension = dataTypeDefinition$verticalRawDimension, 
        verticalLayerDimension = dataTypeDefinition$verticalLayerDimension
    )
    return(out)
}



orderDataByReference <- function(data, dataType) {
    orderColumns <- getAllAggregationVariables(dataType)
    setorderv(data, cols = orderColumns)
}

