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
            groupingVariables = c("Beam", "Frequency"), 
            data = "NASC", 
            verticalRawDimension = c("MinChannelDepth", "MaxChannelDepth"), 
            verticalLayerDimension = NULL, 
            weighting = "NASCWeight", 
            type = c("ChannelReferenceType"), 
            other = c("ChannelReferenceDepth", "ChannelReferenceOrientation", "Cruise", "EffectiveLogDistance", "DateTime", "Longitude", "Latitude")
        ), 
        SumNASCData = list(
            Data = list(
                horizontalResolution = "EDSU", 
                verticalResolution = "Layer", 
                categoryVariable = "AcousticCategory", 
                groupingVariables = c("Beam", "Frequency"), 
                data = "NASC", 
                verticalRawDimension = NULL, 
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = "SumNASCWeight", 
                type = c("ChannelReferenceType"), 
                other = c("ChannelReferenceDepth", "ChannelReferenceOrientation", "EffectiveLogDistance", "DateTime", "Longitude", "Latitude")
            ), 
            Resolution = list(
                horizontalResolution = "EDSU", 
                verticalResolution = c("Layer", "Channel")
            )
        ), 
        MeanNASCData = list(
            Data = list(
                surveyDefinition = "Survey", 
                horizontalResolution = c("Stratum", "PSU"), 
                verticalResolution = "Layer", 
                categoryVariable = "AcousticCategory", 
                groupingVariables = c("Beam", "Frequency"), 
                data = "NASC", 
                verticalRawDimension = NULL, 
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = "MeanNASCWeight", 
                type = c("ChannelReferenceType"), 
                other = c("ChannelReferenceDepth", "ChannelReferenceOrientation")
            ), 
            Resolution = list(
                horizontalResolution = c("Stratum", "PSU", "EDSU"), 
                verticalResolution = c("Layer", "Channel")
            )
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
            Data = list(
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
            Resolution = list(
                horizontalResolution = "Station", 
                verticalResolution = c("Layer", "Haul")
            )
        ), 
        MeanLengthDistributionData = list(
            Data = list(
                surveyDefinition = "Survey", 
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
            Resolution = list(
                horizontalResolution = c("Stratum", "PSU", "Station"), 
                verticalResolution = c("Layer", "Haul")
            )
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
            surveyDefinition = "Survey", 
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c(
                "IndividualTotalLength", "LengthResolution", 
                "Beam", "Frequency" # The relevant acoustic variables
            ), 
            data = "Density",
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "DensityWeight", 
            other = NULL
        ), 
        MeanDensityData = list(
            surveyDefinition = "Survey", 
            horizontalResolution = "Stratum", 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c(
                "IndividualTotalLength", "LengthResolution", 
                "Beam", "Frequency" # The relevant acoustic variables
            ), 
            data = "Density",
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = "MeanDensityWeight", 
            other = NULL
        ), 
        # Abundance:
        AbundanceData = list(
            surveyDefinition = "Survey", 
            horizontalResolution = "Stratum", 
            verticalResolution = c("Layer"), 
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c(
                "IndividualTotalLength", "LengthResolution", 
                "Beam", "Frequency" # The relevant acoustic variables
            ), 
            data = "Abundance", 
            verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
            weighting = NULL, 
            other = NULL
        ), 
        IndividualsData = list(
            Data = list(
                horizontalResolution = c("Stratum"), 
                #verticalResolution = c("Layer", "Haul"), 
                verticalResolution = c("Layer"), 
                #categoryVariable = "SpeciesCategory", 
                #groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
                #groupingVariables = c("Haul", "Individual"), 
                data = NULL, 
                verticalLayerDimension = NULL, # Not relevant
                weighting = NULL, 
                other = NULL
            ), 
            VariableNames = list()
        ), 
        # Prioritise the aggregation variables (horizontalResolution, verticalResolution, categoryVariable and groupingVariables) of the AbundanceData, followed by the IndividualRoundWeight, which is used to calculate Biomass (dividing Abundance by it); and finally the aggregation variables of the IndividualsData, as the purpose of the SuperIndividualsData is to distribute Abundance and Biomass onto the individuals:
        SuperIndividualsData = list(
            Data = list(
                surveyDefinition = "Survey", 
                horizontalResolution = "Stratum", 
                #verticalResolution = c("Layer", "Haul"), 
                verticalResolution = c("Layer"), 
                categoryVariable = "SpeciesCategory", 
                #groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
                #groupingVariables = c("IndividualTotalLength", "LengthResolution", "IndividualRoundWeight", "Haul", "Individual"), 
                groupingVariables = c(
                    "IndividualTotalLength", "LengthResolution", "IndividualRoundWeight", 
                    "Beam", "Frequency" # The relevant acoustic variables
                ), 
                data = c("Abundance",  "Biomass"), 
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = NULL, 
                other = NULL
            ), 
            VariableNames = list()
        ), 
        BioticAssignment = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = "Layer", 
            categoryVariable = "Haul", 
            weighting = "WeightingFactor", 
            other = NULL
        ), 
        AcousticPSU = list(
            Stratum_PSU = list(
                horizontalResolution = c("Stratum", "PSU")
            ), 
            EDSU_PSU = list(
                horizontalResolution = c("EDSU", "PSU")
            )
        ), 
        AcousticPSUByTime = list(
            horizontalResolution = c("Stratum", "PSU"), 
            categoryVariable = "Cruise", 
            groupingVariables = c("StartDateTime", "StopDateTime")
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
    
    # This was in use for the discareded detectDataType():
    ## Define the variables of the main data types used in estimation models:
    #getRequiredVariables <- function(x) {
    #    c(
    #        utils::tail(x$horizontalResolution, 1), 
    #        utils::tail(x$verticalResolution, 1), 
    #        x$categoryVariable, 
    #        x$groupingVariables, 
    #        x$weighting
    #        #utils::head(x$groupingVariables, 1), 
    #        #x$data, 
    #        #x$weighting
    #    )
    #}
    #dataTypeRequiredVariables <- lapply(dataTypeDefinition, getRequiredVariables)
    
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
    reportFunctions <- data.table::data.table(
        functionName = c(
            "summaryStox", 
            "sum", 
            "mean", 
            "weighted.mean", 
            "median", 
            "min", 
            "max", 
            "sd", 
            "var", 
            "cv", 
            "summary", 
            "quantile", 
            "percentile_5_95"
        ), 
        packageName = c(
            "RstoxBase", 
            "base", 
            "base", 
            "stats", 
            "stats", 
            "base", 
            "base", 
            "stats", 
            "stats", 
            "RstoxBase", 
            "base", 
            "stats", 
            "RstoxBase"
        ), 
        weighted = c(
            FALSE, 
            FALSE, 
            FALSE, 
            TRUE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE
        ), 
        weightingParameter = c(
            "", 
            "", 
            "", 
            "w", 
            "", 
            "", 
            "", 
            "", 
            "", 
            "", 
            "", 
            "", 
            ""
        ), 
        multiple = c(
            TRUE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            TRUE, 
            TRUE, 
            TRUE
        )
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
# secondaryColumnOrder can be used to tidy a datatype beyond the variables specified by getDataTypeDefinition().
formatOutputOld <- function(data, dataType, keep.all = TRUE, allow.missing = FALSE, secondaryColumnOrder = NULL) {
    
    # Remove any duplicated columns:
    if(any(duplicated(names(data)))) {
        data[, which(duplicated(names(data))) := NULL]
    }
    
    # Get the column order:
    #columnOrder <- getColumnOrder(dataType)
    columnOrder <- c(
        getDataTypeDefinition(dataType, unlist = TRUE), 
        secondaryColumnOrder
    )
    if(allow.missing) {
        columnOrder <- intersect(columnOrder, names(data))
    }
    
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
    
    # Order the columns:
    data.table::setcolorder(data, columnOrder)
    
    # Order the rows:
    data.table::setorder(data, na.last = TRUE)
    
    # Delete any keys, as we use the argument 'by' for all merging and aggregation:
    data.table::setkey(data, NULL)
}




#' 
#' @export
#' 
formatOutput <- function(data, dataType, keep.all = TRUE, allow.missing = FALSE, secondaryColumnOrder = NULL) {
    
    # If data is only one table:
    if(data.table::is.data.table(data)) {
        dataTypeDefinition <- getDataTypeDefinition(dataType, unlist = TRUE)
        formatOutputOneTable(
            table = data, 
            tableDefinition = dataTypeDefinition, 
            keep.all = keep.all, 
            allow.missing = allow.missing, 
            secondaryColumnOrder = secondaryColumnOrder
        ) 
    }
    # ... or a list of tables:
    else if(is.list(data) && data.table::is.data.table(data[[1]])) {
        dataTypeDefinition <- lapply(names(data), function(tableName) getDataTypeDefinition(dataType, subTable = tableName, unlist = TRUE))
        mapply(
            formatOutputOneTable, 
            table = data, 
            tableDefinition = dataTypeDefinition, 
            MoreArgs = list(
                keep.all = keep.all, 
                allow.missing = allow.missing, 
                secondaryColumnOrder = secondaryColumnOrder
            )
        )
    }
}


formatOutputOneTable <- function(table, tableDefinition, keep.all = TRUE, allow.missing = FALSE, secondaryColumnOrder = NULL) {
    
    # Return immediately if empty table:
    if(!nrow(table)) {
        return(table)
    }
    
    # Remove any duplicated columns:
    if(any(duplicated(names(table)))) {
        table[, which(duplicated(names(table))) := NULL]
    }
    
    # Get the column order:
    columnOrder <- c(
        tableDefinition, 
        secondaryColumnOrder
    )
    if(allow.missing) {
        columnOrder <- intersect(columnOrder, names(table))
    }
    
    if(!keep.all) {
        removeColumnsByReference(
            data = table, 
            toRemove =  setdiff(names(table), columnOrder)
        )
    }
    
    # Order the columns:
    data.table::setcolorder(table, columnOrder)
    
    # Order the rows:
    data.table::setorder(table, na.last = TRUE)
    
    # Delete any keys, as we use the argument 'by' for all merging and aggregation:
    data.table::setkey(table, NULL)
}




# We moved away from using this feature:
#detectDataType <- function(data) {
#    
#    #dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
#    #if(only.data) {
#    #    present <- sapply(dataTypeDefinition, function(var) all(var$data %in% names(data)))
#    #}
#    #else {
#    #    present <- sapply(dataTypeDefinition, function(var) all(unlist(var) %in% names(data)))
#    #}
#    
#    dataTypeRequiredVariables <- getRstoxBaseDefinitions("dataTypeRequiredVariables")
#    present <- sapply(dataTypeRequiredVariables, function(var) all(var %in% names(data)))
#    
#    if(!any(present)) {
#        missing <- lapply(dataTypeRequiredVariables, function(var) setdiff(var %in% names(data)))
#        
#        missing <- lapply(missing, paste, collapse = ", ")
#        missing <- paste(names(missing), missing, sep = ": ", collapse = ". ")
#        
#        
#        warning("StoX: The input data does not contain all the expected variables. The following are needed: ", missing)
#    }
#    else if(sum(present) > 1) {
#        message("More than one element of the input list contains the expected variables (", paste(names(dataTypeRequiredVariables)[present#], collapse = ", "), "). The first selected:")
#    }
#    
#    output <- utils::head(names(dataTypeRequiredVariables)[present], 1)
#    return(output)
#}




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
getDataTypeDefinition <- function(dataType, subTable = "Data", elements = NULL, unlist = FALSE) {
    
    # Get the requested type:
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    
    # If given as a function of data type names (names of 'dataTypeDefinition'), apply the function to subset the data types:
    if(is.function(dataType)) {
        dataTypeDefinitionNames <- names(dataTypeDefinition)
        dataType <- subset(dataTypeDefinitionNames, dataType(dataTypeDefinitionNames))
    }
    # Does not make perfect sense, since we are always, or should always be, interested in exactly one dataType:
    ### # If NULL use all data types:
    ### if(length(dataType) == 0) {
    ###     dataType <- names(dataTypeDefinition)
    ### }
    thisDataTypeDefinition <- dataTypeDefinition[[dataType]]
    
    # Extract the elements to return:
    #if(length(elements)) {
    #    thisDataTypeDefinition <- lapply(thisDataTypeDefinition, "[", elements)
    #}
    #if(length(dataType) == 1) {
    #    thisDataTypeDefinition <- thisDataTypeDefinition[[dataType]]
    #}
    
    if(is.list(thisDataTypeDefinition) && is.list(thisDataTypeDefinition[[1]])) {
        if(subTable %in% names(thisDataTypeDefinition)) {
            thisDataTypeDefinition <- thisDataTypeDefinition[[subTable]]
        }
        else {
            stop("The dataType may be non-existing, or the subTable may not be present for the given dataType")
        }
    }
    
    # Select the elements to return:
    if(length(elements)) {
        thisDataTypeDefinition <- thisDataTypeDefinition[elements]
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
        dimension = c("vertical", "horizontal"),
        subTable = "Data"
        ) {
    
    # Get the requested type:
    dimension <- match.arg(dimension)
    #dataType <- detectDataType(data)
    #dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    #thisDataTypeDefinition <- dataTypeDefinition[[dataType]]
    dataTypeDefinition <- getDataTypeDefinition(dataType, subTable = subTable)
    
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



#orderDataByReference <- function(data, dataType) {
#    orderColumns <- getAllAggregationVariables(dataType)
#    setorderv(data, cols = orderColumns, na.last = TRUE)
#}
#
#


#' 
#' @export
#' 
getReportFunctions <- function(getMultiple = NULL) {
    reportFunctions <- getRstoxBaseDefinitions("reportFunctions")
    if(length(getMultiple)) {
        reportFunctions[multiple == getMultiple, functionName]
    }
    else {
        reportFunctions$functionName
    }
    
}
