##################################################
##################################################
#' Get the common intervals of possibly overlapping intervals:
#' 
#' @param data A data.table/data.frame holding the columns representing lower and upper interval.
#' @param varMin The name of the lower interval column. By default interpreted from the first column of \code{data}.
#' @param varMax The name of the upper interval column. By default interpreted from the second column of \code{data}.
#' @param lowerName The name of the lower interval column in the output table. By default set to \code{varMin}.
#' @param upperName The name of the upper interval column in the output table. By default set to \code{varMax}.
#' 
getCommonIntervals <- function(data, varMin = NULL, varMax = NULL, lowerName = NULL, upperName = NULL) {
    
    # Function to get the first common interval of a table:
    getOneCommonInterval <- function(data, varMin, varMax) {
        # Start out with the first lower value:
        lower <- data[[varMin]][1]
        # Set an initial value of the first upper value:
        currentUpper <- data[[varMax]][data[[varMin]] == lower]
        upper <- max(currentUpper)
        
        # Find any intervals that partly overlap with the first common interval:
        overlapping <- TRUE
        while(overlapping) {
            inside <- lower < data[[varMin]] & data[[varMin]] < upper
            if(any(inside)) {
                # Check whether the maximum of the upper values of the intervals that are inside the common interval exceeds the upper value:
                maxOfInside <- max(data[[varMax]][inside])
                if(maxOfInside > upper) {
                    upper <- maxOfInside
                }
                else {
                    overlapping <- FALSE
                }
            }
            else {
                overlapping <- FALSE
            }
        }
        
        c(
            lower = lower, 
            upper = upper
        )
    }
    
    # Interpret the varMin and varMax:
    if(length(varMin) == 0) {
        varMin <- names(data)[1]
    }
    if(length(varMax) == 0) {
        varMax <- names(data)[2]
    }
    
    # Set the names of the output "lower" and "upper" columns:
    if(length(lowerName) == 0) {
        lowerName <- varMin
    }
    if(length(upperName) == 0) {
        upperName <- varMax
    }
    
    # Remove rows with NAs:
    hasNA <- rowSums(is.na(data)) > 0
    if(any(hasNA)) {
        data <- data[!hasNA, ]
    }
    
    # Order the input:
    data <- data[order(data[[varMin]]), ]
    
    # Declare a list to save the common intervals to:
    intervals <- list()
    
    # Get all common intervals:
    while(nrow(data)) {
        # Add the current interval:
        thisInterval <- getOneCommonInterval(data, varMin, varMax)
        intervals <- c(intervals, list(thisInterval))
        # Shave off the data:
        used <- data[[varMax]] <= thisInterval["upper"]
        data <- subset(data, !used)
    }
    intervals <- data.table::rbindlist(lapply(intervals, as.list))
    
    # Rename the "lower" and "upper" column:
    intervals[, .(lower = lowerName)]
    intervals[, .(upper = upperName)]
    
    intervals
}

# Function to generate StoxBiotic keys:
#getStoxBioticKeys <- function(levels = NULL) {
#    if(length(levels) == 0) {
#        levels <- RstoxData::getStoxBioticLevels()
#    }
#    paste0(levels, "Key")
#}
getStoxBioticKeys <- function(levels = NULL) {
    if(length(levels) == 0) {
        levels <- c(
            "Cruise", 
            "Station", 
            "Haul", 
            "SpeciesCategory", 
            "Sample", 
            "Individual"
        )
    }
    paste0(levels, "Key")
}


# Function to get Layer indices:
findLayer <- function(minDepth, maxDepth, layerProcessData, acceptNA = TRUE) {
    
    # Special case if layerProcessData$Layer == "WaterColumn", in which case NA is interpreted as Inf:
    if(length(layerProcessData$Layer) == 1 && layerProcessData$Layer == "WaterColumn") {
        layerProcessData$MaxLayerDepth <- Inf
    }
    
    # This needs to be verified!!!!!!!!!!!!!
    layerRangeVector <- c(layerProcessData$MinLayerDepth, utils::tail(layerProcessData$MaxLayerDepth, 1))
    indMin <- findInterval(minDepth, layerRangeVector)
    indMax <- findInterval(maxDepth, layerRangeVector, rightmost.closed = TRUE)
    LayerInd <- pmin(indMin, indMax, na.rm = acceptNA)
    
    if(any(LayerInd %in% 0)) {
        stop("There are max depth values that are smaller than the min depth values.")
    }
    
    if(acceptNA && nrow(layerProcessData) == 1 && layerProcessData$MinLayerDepth == 0 && layerProcessData$MaxLayerDepth == Inf) {
        LayerInd <- replace(LayerInd, is.na(LayerInd), 1)
    }
    
    LayerData <- data.table::data.table(
        Layer = layerProcessData$Layer[LayerInd], 
        MinLayerDepth = layerProcessData$MinLayerDepth[LayerInd], 
        MaxLayerDepth = layerProcessData$MaxLayerDepth[LayerInd]
    )
    
    LayerData
}

# Function to add Stratum and PSU:
addPSUProcessData <- function(data, PSUProcessData = NULL, ...) {
    
    # If present, add the PSUProcessData to the start of the data
    if(length(PSUProcessData) && length(PSUProcessData$Stratum_PSU)) {
        # Merge first the PSUProcessData (except the PSUStartEndDateTime, which is used for matching new data into the same PSU definitions):
        notPSUByTime <- names(PSUProcessData) != "PSUByTime"
        PSUProcessData <- RstoxData::mergeDataTables(PSUProcessData[notPSUByTime], output.only.last = TRUE, ...)
        # Then merge the result with the data:
        by <- intersect(names(PSUProcessData), names(data))
        # Remove columns with only NAs in the data in 'by':
        onlyNAs <- unlist(data[, lapply(.SD, function(x) all(is.na(x))), .SDcols = by])
        if(any(onlyNAs)) {
            #toRemove <- by[onlyNAs]
            #toRemove <- intersect(toRemove, names(data))
            #data[, (toRemove):=NULL]
            removeColumnsByReference(
                data = data, 
                toRemove =  by[onlyNAs]
            )
            
            by <- by[!onlyNAs]
        }
        data <- merge(PSUProcessData, data, by = by, ...)
    }
    else if(! "PSU" %in% names(data)) {
        #warning("StoX: PSUs not defined, possibly due to no data inside the any strata")
        data.table::setDT(data)
        toAdd <- c("Stratum", "PSU")
        data.table::set(data, j = toAdd, value = NA_character_)
    }
    
    
    ## Set the order of the columns:
    ##dataType <- detectDataType(data)
    #data <- setColumnOrder(data, dataType = dataType, keep.all = TRUE)
    
    return(data)
}

# Function to add Stratum and PSU:
addSurveyProcessData <- function(data, SurveyProcessData = NULL, ...) {
    
    # If present, add the PSUProcessData to the start of the data
    if(length(SurveyProcessData)) {
        # Merge the SurveyProcessData into the data:
        data <- RstoxData::mergeByIntersect(SurveyProcessData, data, ...)
    }
    else if(! "Stratum" %in% names(data)) {
        # Add NA columns for Survey and Stratum if "Stratum" is not a column:
        data.table::setDT(data)
        toAdd <- c("Survey", "Stratum")
        data.table::set(data, j = toAdd, value = NA_character_)
    }
    
    return(data)
}

# Function to add Layer:
addLayerProcessData <- function(data, dataType, layerProcessData = NULL, acceptNA = TRUE) {
    
    # Insert the Layer column from the layerProcessData input, and otherwise by NAs:
    if(length(layerProcessData)) {
        
        # Get the variables to aggregate by etc.:
        dataTypeDefinition <- getDataTypeDefinition(dataType = dataType)
        varMin <- dataTypeDefinition$verticalRawDimension[1]
        varMax <- dataTypeDefinition$verticalRawDimension[2]
        
        # Get min and max depth of the raw vertical distribution, either as a copy of the depths, or a calculation of the depths given the depth and orientation of the coordinate system:
        # ... NOTE (2021-04-29) The element coordinateSystemOrigin is not defined, and only the else is active here:
        #if(length(dataTypeDefinition$coordinateSystemOrigin)) {
        #    minDepth <- data[[dataTypeDefinition$coordinateSystemOrigin]] - 
        #        data[[varMin]] * cos(data[[dataTypeDefinition$coordinateSystemOrientation]] #* pi/ 180)
        #    maxDepth <- data[[dataTypeDefinition$coordinateSystemOrigin]] - 
        #        data[[varMax]] * cos(data[[dataTypeDefinition$coordinateSystemOrientation]] #* pi/ 180)
        #}
        #else {
            minDepth <- data[[varMin]]
            maxDepth <- data[[varMax]]
        #}
        
        layerData <- findLayer(
            minDepth = minDepth, 
            maxDepth = maxDepth, 
            layerProcessData = layerProcessData, 
            acceptNA = acceptNA
        )
        
        data <- data.table::data.table(layerData, data)
    }
    else if(! "Layer" %in% names(data)) {
        data.table::setDT(data)
        toAdd <- c("Layer", "MinLayerDepth", "MaxLayerDepth")
        data.table::set(data, j = toAdd, value = NA_character_)
    }
    
    return(data)
}




# Stolen from https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector:
#' Test whether all values are equal.
#' 
#' @param x An R object coercable to numeric.
#' @param tol The tolerance of the equality
#' @param ... Arguments passed on to range and mean, particularly na.rm.
#' 
#' @export
#' 
allEqual <- function(x, tol = .Machine$double.eps ^ 0.5, ...) {
    if (length(x) == 1) {
        return(TRUE)
    }
    if(is.numeric(x)) {
        x <- range(x, ...) / mean(x, ...)
        isTRUE(all.equal(x[1], x[2], tolerance = tol))
    }
    else {
        if(isTRUE(list(...)$na.rm)) {
            x <- x[!is.na(x)]
        }
        length(table(x, useNA = "ifany")) == 1
    }
}




sumRawResolutionData <- function(
    data, dataType, 
    LayerDefinition = c("FunctionParameter", "FunctionInput"), 
    LayerProcessData = NULL, 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    LayerType = c("Acoustic", "Biotic")
) {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    dataCopy = data.table::copy(data)
    
    # Add the Layers and PSUs either from function inputs or by automatic methods using function parameters:
    LayerDefinition <- RstoxData::match_arg_informative(LayerDefinition)
    
    # Get the Layers:
    if(identical(LayerDefinition, "FunctionParameter")) {
        LayerProcessData <- DefineLayer(
            StoxData = dataCopy, 
            DefinitionMethod = LayerDefinitionMethod, 
            Resolution = Resolution, 
            LayerTable = LayerTable, 
            LayerType = LayerType
        )
    }
    # Add the Layers:
    if(length(LayerProcessData)) {
        dataCopy <- addLayerProcessData(
            dataCopy, 
            dataType = dataType, 
            layerProcessData = LayerProcessData
        )
    }
    else {
        stop("LayerProcessData must be given if LayerDefinition = \"FunctionInput\"")
    }
    
    # Get the resolution table, holding the Station/EDSU and all vertical resolution variables:
    ### resolutionVariables <- getAllResolutionVariables(
    ###     dataType = dataType#, 
    ###     #dimension = "vertical"
    ### )
    
    sumDataType <- paste0("Sum", dataType)
    resolutionVariables <- unlist(getDataTypeDefinition(sumDataType, subTable = "Resolution"))
    
    presentResolutionVariables <- intersect(resolutionVariables, names(dataCopy))
    Resolution <- unique(dataCopy[, ..presentResolutionVariables])
    
    # Sum the data vertically:
    aggregatedData <- applySumToData(data = dataCopy, dataType = dataType)
    
    # Get the resolution as the resolution columns defined for NASCData (identical to those of L)
    return(
        list(
            Data = aggregatedData, 
            Resolution = Resolution
        )
    )
}




meanRawResolutionData <- function(
    data, dataType, 
    # PSU: 
    PSUDefinition = c("FunctionParameter", "FunctionInput"), 
    PSUProcessData = NULL, 
    PSUDefinitionMethod = c("Identity", "None"), 
    # Survey:
    SurveyDefinition = c("FunctionParameter", "FunctionInput"), 
    SurveyProcessData = NULL, 
    SurveyDefinitionMethod = c("AllStrata", "Table"), 
    SurveyTable = data.table::data.table(), 
    # General:
    StratumPolygon = NULL, 
    PSUType = c("Acoustic", "Biotic")
) {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    dataCopy = data.table::copy(data$Data)
    
    # Add the PSUs either from function input or by automatic method using function parameter:
    # Define the PSUs:
    PSUDefinition <- RstoxData::match_arg_informative(PSUDefinition)
    if(identical(PSUDefinition, "FunctionParameter")) {
        PSUProcessData <- DefinePSU(
            StratumPolygon = StratumPolygon, 
            MergedStoxDataStationLevel = dataCopy, 
            DefinitionMethod = PSUDefinitionMethod, 
            PSUType = PSUType
        )
    }
    # Add the PSUs:
    if(length(PSUProcessData)) {
        dataCopy <- addPSUProcessData(dataCopy, PSUProcessData = PSUProcessData, all = TRUE)
    }
    else {
        stop("PSUProcessData must be given if PSUDefinition = \"FunctionInput\"")
    }
    
    # Get the Surveys:
    SurveyDefinition <- RstoxData::match_arg_informative(SurveyDefinition)
    if(identical(SurveyDefinition, "FunctionParameter")) {
        # Get the stratum names and the SurveyTable:
        stratumNames <- unique(dataCopy$Stratum)
        # Remove missing Stratum names:
        stratumNames <- stratumNames[!is.na(stratumNames)]
        # Get the SurveyTable
        SurveyProcessData <- getSurveyTable(
            DefinitionMethod = SurveyDefinitionMethod, 
            stratumNames = stratumNames, 
            SurveyTable = SurveyTable
        )
    }
    # Add the Survey:
    if(length(SurveyProcessData)) {
        dataCopy <- addSurveyProcessData(dataCopy, SurveyProcessData = SurveyProcessData, all = TRUE)
    }
    else {
        stop("SurveyProcessData must be given if SurveyDefinition = \"FunctionInput\"")
    }
    
    ### # Get the resolution table, holding the Station/EDSU and all vertical resolution variables:
    ### resolutionVariables <- getAllResolutionVariables(
    ###     dataType = dataType#, 
    ###     #dimension = "horizontal"
    ### )
    
    meanDataType <- sub("Sum", "Mean", dataType)
    resolutionVariables <- unlist(getDataTypeDefinition(meanDataType, subTable = "Resolution"))
     
    extractFromDataCopy <- intersect(resolutionVariables, names(dataCopy))
    mergeBy <- intersect(names(data$Resolution), extractFromDataCopy)
    Resolution <- unique(
        merge(
            data$Resolution, 
            dataCopy[, ..extractFromDataCopy], 
            allow.cartesian = TRUE, 
            by = mergeBy
        )
    )
    
    # Average the data horizontally:
    aggregatedData <- applyMeanToData(data = dataCopy, dataType = dataType, targetResolution = "PSU")
    
    # Get the resolution as the resolution columns defined for NASCData (identical to those of L)
    return(
        list(
            Data = aggregatedData, 
            Resolution = Resolution
        )
    )
}








applySumToData <- function(data, dataType) {
    
    targetDataType <- paste0("Sum", dataType)
    
    # Get the variables to aggregate by etc.:
    #aggregationVariables <- determineAggregationVariables(
    #    data = data, 
    #    dataType = targetDataType, 
    #    targetResolution = "Layer", 
    #    dimension = "vertical"
    #)
    #print(aggregationVariables)
    ## Extract the 'by' element:
    #by <- aggregationVariables$by
    
    # Sum of the data variable for each horizontalResolution, categoryVariable and groupingVariables:
    dataVariable <- getDataTypeDefinition(dataType, elements = "data", unlist = TRUE)
    sumBy <- getDataTypeDefinition(targetDataType, elements = c("horizontalResolution", "verticalResolution", "categoryVariable", "groupingVariables"), unlist = TRUE)
    for(var in dataVariable) {
        data[, c(var) := sum(x = get(var)), by = sumBy]
    }
    
    
    # Add the weighting variable:
    weightingVariable <- getDataTypeDefinition(dataType, elements = "weighting", unlist = TRUE)
    targetWeightingVariable <- getDataTypeDefinition(targetDataType, elements = "weighting", unlist = TRUE)
    data[, c(targetWeightingVariable) := get(weightingVariable)]
    
    # Remove duplicated rows:
    data <- subset(data, !duplicated(data[, ..sumBy]))
    
    return(data)
}

applyMeanToData <- function(data, dataType, targetResolution = "PSU") {
    
    ##### IMPORTANT NOTE!: #####
    # This function needs to be coded with care. The input data contains rows only for the data measured for each category and grouping variable. E.g., there could be cod of length 50 cm in some but not all hauls. When averaging we need to get the sum of the weights for each combination of the resolution variables (Step 1). Then these sums are merged into the data (Step 2). And finally the data variable is summed and divided by the summed weights (Step 3):
    ##########
    # Store the original data type defnition, particularly for summing the weights:
    #originalDataTypeDefinition <- getDataTypeDefinition(dataType = dataType)
    
    # Get the variables to aggregate by etc.:
    targetDataType <- paste0("Mean", sub("Sum", "", dataType))
    #aggregationVariables <- determineAggregationVariables(
    #    data = data, 
    #    dataType = targetDataType, 
    #    targetResolution = targetResolution, 
    #    dimension = "horizontal"
    #)
    
    # Extract the 'by' element:
    
    # Weighted average of the data variable over the grouping variables, weighted by the weighting variable:
    dataVariable <- getDataTypeDefinition(dataType, elements = "data", unlist = TRUE)
    weightingVariable <- getDataTypeDefinition(dataType, elements = "weighting", unlist = TRUE)
    targetWeightingVariable <- getDataTypeDefinition(targetDataType, elements = "weighting", unlist = TRUE)
    
    
    #targetWeightingVariable <- aggregationVariables$weightingVariable
    #weightingVariable <- originalDataTypeDefinition$weighting
    
    
    #### Step 1: Sum the weights: ####
    # Extract the horizontal resolution of the input and output data type, and weighting variable:
    horizontalResolution <- getDataTypeDefinition(dataType, elements = c("horizontalResolution"), unlist = TRUE)
    targetHorizontalResolution <- c(
        getDataTypeDefinition(targetDataType, elements = c("horizontalResolution"), unlist = TRUE), 
        # Add the Beam for NASCData and DensiyData. This is not exactly a horizontal resolution, so the name is somewhat confusing:
        getDataTypeDefinition(targetDataType, elements = "obserationVariable", unlist = TRUE)
    )
    # Omit in case the obserationVariable is NULL or not given:
    targetHorizontalResolution <- intersect(targetHorizontalResolution, names(data))
    extract <- c(
        horizontalResolution, 
        targetHorizontalResolution, 
        weightingVariable, 
        dataVariable
    )
    extract <- unique(extract)
    
    summedWeighting <- data[, ..extract]
    # Uniquify so that we get only one value per Station/EDSU. This ignores the categoryVariable and groupingVariables, and assumes that the weightingVariable is constant across these variables (length group for swept area data and beam for acoustic data):
    summedWeighting <- unique(summedWeighting, by = c(horizontalResolution, targetHorizontalResolution))
    
    # Then sum the weights by the next resolution, PSU for mean of stations/EDSUs and Stratum for mean of PSUs:
    # Use utils::tail(horizontalResolution, 1) here to get the Station/Haul/PSU:
    # 2022-08-08: Added !is.na(get(dataVariable)) here to avoid this warning for rows with e.g. missing NACS:
    weightingVariableNA <- summedWeighting[, is.na(get(weightingVariable))]
    firstHorizontalResolutionNotNA <- summedWeighting[, !is.na(get(utils::tail(horizontalResolution, 1)))]
    anyDataVariablesNotNA <- summedWeighting[, lapply(dataVariable, function(x) !is.na(get(x)))]
    anyDataVariablesNotNA <- rowSums(anyDataVariablesNotNA) > 0
    naWeights <- weightingVariableNA & firstHorizontalResolutionNotNA & anyDataVariablesNotNA
    
    if(any(naWeights)) {
        warning("StoX: There are missing values for ", weightingVariable, ". This can result in missing values in ", targetDataType, ". The following ", horizontalResolution, " have missing ", weightingVariable, ":\n\t", paste(summedWeighting[[utils::tail(horizontalResolution, 1)]][naWeights], collapse = "\n\t"))
    }
    #summedWeighting[, SummedWeights := sum(get(weightingVariable), na.rm = TRUE), by = targetHorizontalResolution]
    summedWeighting[, SummedWeights := sum(get(weightingVariable), na.rm = FALSE), by = targetHorizontalResolution]
    
    # Extract the next resolution and the summed weights and uniquify:
    extract <- c(targetHorizontalResolution, "SummedWeights")
    summedWeighting <- summedWeighting[, ..extract]
    summedWeighting <- unique(summedWeighting)
    ########
    
    
    #### Step 2: ####
    # Merge the resulting summed weights with the data, by the next resolution:
    data <- merge(data, summedWeighting, by = targetHorizontalResolution, all = TRUE)
    ########
    
    
    
    #sumBy <- getDataTypeDefinition(targetDataType, elements = c("horizontalResolution", "categoryVariable", "groupingVariables"), unlist = TRUE)
    #data[, SummedWeights := sum(get(weightingVariable), na.rm = TRUE), by = sumBy]
    
    #### Step 2: Sum the data and divide by the summed weights: ####
    # Finally weighted sum the data, and divide by the summed weights (the last step is the crusial part):
    meanBy <- getDataTypeDefinition(targetDataType, elements = c("horizontalResolution", "verticalResolution", "categoryVariable", "groupingVariables"), unlist = TRUE)
    # Beam is not present for LengthDistributionData:
    meanBy <- intersect(meanBy, names(data))
    
    for(var in dataVariable) {
        data[, c(var) := sum(get(var) * get(weightingVariable), na.rm = FALSE) / SummedWeights, by = meanBy]
    }
    
    # Store the new weights by the summed original weights:
    data[, c(targetWeightingVariable) := SummedWeights]
    ########
    
    # Remove duplicated rows:
    data <- subset(data, !duplicated(data[, ..meanBy]))
    
    return(data)
}



# https://stackoverflow.com/questions/24833247/how-can-one-work-fully-generically-in-data-table-in-r-with-column-names-in-varia:
quote.convert <- function(x) {
    eval(
        parse(
            text = paste0('quote(', x ,')')
        )
    )
}


#keepOnlyRelevantColumns <- function(data, dataType) {
#    allDataTypeVariables <- getAllDataTypeVariables(dataType, unlist = TRUE)
#    data[, ..allDataTypeVariables]
#}


#keepOnlyRelevantColumns <- function(data, dataType) {
#    allDataTypeVariables <- getAllDataTypeVariables(dataType, unlist = TRUE)
#    toRemove <- setdiff(names(data), allDataTypeVariables)
#    if(length(toRemove)) {
#        data[, (toRemove) := NULL]
#    }
#    
#    setcolorder(data, allDataTypeVariables)
#}



JavaJEXL2R <- function(x, eval=TRUE) {
    
    if(length(x) == 0 || nchar(x) == 0) {
        return("")
    }
    
    # Define transformation from Java JEXL to R syntax:
    pattern_replacement <- rbind(
        c(" not", " !"), 
        c(" and ", " && "), 
        c(" or ", " || "), 
        c(" eq ", " == "), 
        c(" ne ", " != "), 
        c(" lt ", " < "), 
        c(" le ", " <= "), 
        c(" gt ", " > "), 
        c(" ge ", " >= "), 
        c("true", "TRUE"),  
        c("false", "FALSE"),  
        c("null", "NULL")
    )
    
    # Replace Jexl with R:
    for(i in seq_len(nrow(pattern_replacement))) {
        x <- gsub(pattern_replacement[i,1], pattern_replacement[i,2], x)
    }
    
    # Return the evaluated exprexsion:
    if(eval) {
        x <- eval(parse(text = x))
    }
    
    return(x)
}

# Check the types of the SpeciesLink:
checkTypes <- function(table) {
    
    if(length(table) == 0) {
        return(FALSE)
    }
    
    # Get the name of the table and the function name:
    parameterTableName <- deparse(substitute(table))
    functionName <- sub("()", "", deparse(sys.call(-1)[1]), fixed = TRUE)
    if(length(functionName) == 0) {
        stop("This function is only meant for use inside a StoX function.")
    }
    
    # Get the format of the parameter table:
    format <- stoxFunctionAttributes[[functionName]]$functionParameterFormat[[parameterTableName]]
    # Get the parameter table info holding the types:
    #parameterTableInfo <- processPropertyFormats[[format]]$info
    
    if(!all(names(table) %in% processPropertyFormats[[format]]$columnNames)) {
        missing <- setdiff(processPropertyFormats[[format]]$columnNames, names(table))
        stop("All columns must be present in the ", parameterTableName, " (", missing, " missing)")
    }
    
    # Get a table of the column name and type of the input table:
    types <- sapply(table, function(x) class(x)[1])
    # numeric and double are identical in R, so we convert numeric to double, which is what is given by the functionParameterFormat:
    types <- replace(types, types == "numeric", "double")
    # Build a table similar to the parameterTableInfo:
    types <- list(
        columnNames = names(table), 
        variableTypes = types
    )
    
    # Order both and check for identity:
    types <- orderListBy(types, "columnNames")
    processPropertyFormats[[format]] <- orderListBy(processPropertyFormats[[format]], "columnNames")
    
    commonElements <- intersect(names(processPropertyFormats[[format]]), names(types))
    if(!identical(processPropertyFormats[[format]][commonElements], types[commonElements])) {
        # Print error message for those different:
        differs <- mapply(compareTypes, processPropertyFormats[[format]]$variableTypes, types$variableTypes)
        if(any(differs)) {
            stop("The input ", parameterTableName, " contains columns of the wrong type (", paste0(processPropertyFormats[[format]]$columnNames[differs], ": ", processPropertyFormats[[format]]$variableTypes[differs], " (was ", types$variableTypes[differs], ")", collapse = ", "), ")")
        }
    }
    else {
        return(TRUE)
    }
}


orderListBy <- function(x, by) {
    o <- order(x[[by]])
    lapply(x, function(thisx) if(!is.function(thisx)) thisx[o] else x)
}


compareTypes <- function(x, y, allow.numeric = TRUE) {
    out <- identical(x, t)
    if(allow.numeric) {
        out <- out | all(is.numeric(x), is.numeric(y))
    }
    return(out)
}




removeColumnsByReference <- function(data, toRemove) {
    toRemove <- intersect(toRemove, names(data))
    if(length(toRemove)) {
        data[, c(toRemove) := NULL]
    }
}



#### Tools to perform resampling: ####

#' Function to sample after sorting:
#' 
#' @param x The vector to sample from.
#' @param size The length of the sampled vector.
#' @param seed The seed to use for the sampling.
#' @param replace Logical: If TRUE sample with replacement.
#' @param sorted Sort the vector before sampling.
#' @param index.out Return indices at which to sample rather than the actual samples.
#' 
#' @export
#' 
sampleSorted <- function(x, size, seed, replace = TRUE, sorted = TRUE, index.out = FALSE){
    # If not given, get the size of the sample as the length of the vector:
    lx <- length(x)
    if(missing(size)){
        size <- lx
    }
    if(sorted){
        # Changed to platform independent sorting in StoX 3.2.0. This should not have any effect on the applications of sampleSorted(), which are in imputation of replace individual indices, and sampling of PSU and Haul in bootstrapping:
        #x <- sort(x)
        #print((
        #    sort(x), 
        #    stringi::stri_sort(x, locale = "en_US_POSIX"))
        #)
        if(is.character(x)) {
            x <- stringi::stri_sort(x, locale = "en_US_POSIX")
        }
        else {
            x <- sort(x)
        }
        
    }
    # Sample:
    set.seed(seed)
    ### # To increase uniqueness in sampling, resample the seed from the length of the vector to sample:
    ### if(redraw.seed) {
    ###     # Add a fixed number to the length in order to aviod reusing 
    ###     seed <- sample.int(lx, 1)
    ###     set.seed(seed)
    ### }
    sampled <- x[sample.int(lx, size = size, replace = replace)]
    
    if(index.out) {
        sampled <- match(sampled, x)
    }
    return(sampled)
}

#' Generate a seed vector consistently
#' 
#' This function should be used for all random sampling in all Rstox packages.
#' 
#' @param seed A single integer setting the seed of the seed vector generation.
#' @param size A single integer giving the length of the seed vector.
#' 
#' @export
#' 
getSeedVector <- function(seed, size = 1) {
    set.seed(seed)
    sample(getSequenceToSampleFrom(), size, replace = FALSE)
}

getSequenceToSampleFrom <- function(){
    seedSequenceLength <- getRstoxBaseDefinitions("seedSequenceLength")
    seq_len(seedSequenceLength)
}




# Define report functions:

#' The summary function introduced in StoX <= 2.7.
#' 
#' @inheritParams stats::quantile
#' @param percentages The percentages to get percentiles for, equivalent to 100 * probs in \code{\link[stats]{quantile}}.
#' 
#' @export
#' 
summaryStox <- function(x, percentages = c(5, 50, 95), na.rm = FALSE) {
    probs <- percentages / 100
    if(any(probs < 0 | probs > 1 | is.na(probs))) {
        stop("StoX: The Percentages must be between 0 and 100.")
    }
    if(any(is.na(x)) && isFALSE(na.rm)) {
        # Get quantiles of 0 just to get the names:
        percentilesVector <- stats::quantile(0, probs = probs, na.rm = FALSE)
        percentilesVector[seq_along(percentilesVector)] <- NA_real_
    }
    else {
        percentilesVector <- stats::quantile(x, probs = probs, na.rm = na.rm)
    }
    
    out <- c(
        # Accept na.rm = FALSE in quantile, as  in median():
        percentilesVector,
        mean = mean(x, na.rm = na.rm),
        sd = stats::sd(x, na.rm = na.rm),
        cv = cv(x, na.rm = na.rm)
    )
    # Set 0 sd to NA, with a warning assuming that this is a result of only one PSU selected in the stratum:
    if(!is.na(out["sd"]) && out["sd"] == 0) {
        if(out["mean"] != 0) {
            warning("StoX: Standard deviation 0 was set to NA. StoX does not accept standard deviation 0, as it is either a sign of insufficient number of bootstraps; only one value to sample from in the bootstrapping (e.g. only one station in a stratum); or 0 in all bootstrap runs, which occurs e.g. if a length group is present in the survey but not in the stratum in question, causing 0 for all bootstrap runs in that stratum.")
        }
        out["sd"] <- NA
        out["cv"] <- NA
    }
    
    return(out)
}


#' The coefficient of variation, i.e., the standard deviation divided by the mean.
#' 
#' @param x A numeric object
#' @param na.rm Logical: If TRUE remove the missing values prior to calculation.
#' 
#' @export
#' 
cv <- function(x, na.rm = FALSE) {
    stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}



isEmptyString <- function(x) {
    is.character(x) && (
        !length(x) || 
            (length(x) == 1 && nchar(x) == 0)
        )
}

#' Generate Start, Middle and Stop DateTime variables
#'
#' @param StoxDataStationLevel Either \code{\link{StoxAcousticData}} or \code{\link{StoxBioticData}}, depending on \code{type}.
#' @param type A string naming the type of StoX data, one of "Acoustic" and "Biotic".
#' 
#' @return An object of StoX data type \code{\link{MergeStoxAcousticData}}.
#'
StoxDataStartMiddleStopDateTime <- function(
    StoxDataStationLevel, 
    type = c("Acoustic", "Biotic")
) {
    type <- RstoxData::match_arg_informative(type)
    
    if(type == "Acoustic") {
        
        # Fill the start, middle and end DateTime with the DateTime directly, given the LogOrigin:
        # Fill the StartDateTime with the DateTime directly, given the LogOrigin:
        StoxDataStationLevel[LogOrigin == "start", StartDateTime := DateTime]
        # Interpret StartDateTime from LogDuration and LogOrigin:
        StoxDataStationLevel[LogOrigin == "middle", StartDateTime := DateTime - fifelse(is.na(LogDuration), 0, LogDuration) / 2]
        StoxDataStationLevel[LogOrigin == "end", StartDateTime := DateTime - fifelse(is.na(LogDuration), 0, LogDuration)]
        
        # Extrapolate to the middle and end times:
        StoxDataStationLevel[, MiddleDateTime := StartDateTime + fifelse(is.na(LogDuration), 0, LogDuration) / 2]
        StoxDataStationLevel[, StopDateTime := StartDateTime + fifelse(is.na(LogDuration), 0, LogDuration)]
    }
    else if(type == "Biotic") {
        # Biotic stations are defined with a single time point. Duration in given for each Haul, whereas StoxAcoustic has duration on the Log:
        StoxDataStationLevel[, StartDateTime := DateTime]
        StoxDataStationLevel[, MiddleDateTime := DateTime]
        StoxDataStationLevel[, StopDateTime := DateTime]
    }
    else {
        stop("Invalid type. Must be either Acoustic or Biotic.")
    }
    
    
    return(StoxDataStationLevel)
}



#' Replace all NAs in a data.table by reference
#' 
#' @param DT A data.table.
#' @param cols A vector of column names in which to replace the NAs. 
#' @param replacement the object to replace by.
#' 
#' @export
#' 
#replaceNAByReference <- function(DT, cols = NULL, replacement = 0) {
#    if(!length(cols)) {
#        cols <- names(DT)
#    }
#    for (j in cols) {
#        data.table::set(DT, which(is.na(DT[[j]]) & is.numeric(DT[[j]])), j, replacement)
#    }
#}
replaceNAByReference <- function(DT, cols = NULL, replacement = 0) {
    if(!length(cols)) {
        cols <- names(DT)
    }
    if(length(replacement)) {
        if(!is.list(replacement)) {
            replacement <- structure(list(replacement), names = RstoxData::firstClass(replacement))
        }
        for (j in cols) {
            data.table::set(DT, which(is.na(DT[[j]]) & class(DT[[j]]) %in% names(replacement)), j, replacement[[RstoxData::firstClass(DT[[j]])]])
        }
    }
}




detectInvalidUTF8 <- function(x) {
    areNA <- is.na(x)
    Encoding(x) <- "UTF-8"
    x <- iconv(x, "UTF-8", "UTF-8")
    newNA <- areNA != is.na(x)
    return(newNA)
}


firstNonNA <- function(x) {
    if(!is.na(x[1])) {
        return(x[1])
    }
    else {
        areNA <- is.na(x)
        if(all(areNA)) {
            warning("All values are NA.")
            return(x[1])
        }
        else {
            return(x[min(which(!areNA))])
        }
    }
}














##################################################
##################################################
#' General parameters of treating project.xml file from StoX <= 2.7
#' 
#' @param projectPath (Optional) The path to the StoX 2.7 project.
#' @param projectXMLFileName (Optional) The name of the StoX 2.7 project.xml file, defaulted to "project.xml".
#' @param projectXMLFilePath The path to the project.xml file.
#' @param projectXMLList A list as read by \code{\link{readProjectXMLToList}}.
#' @param modelName The name of the model (possible values are "baseline", "baseline-report", "r" and "r-report").
#' @param processName he name of the process.
#' @param parameterName The name of the parameter.
#' @param parameterValue The value of the parameter.
#' @param newParameterValue The new value of the parameter.
#' @param functionName The name of the function of the process to add.
#' @param modify A named list specifying the modification of a process. Possible elements are remove, add and modify.
#' 
#' @name general_arguments2.7
#' 
NULL





#' Read a project.xml file from StoX <= 2.7
#' 
#' @inheritParams general_arguments2.7
#' 
#' @export
#' 
readProjectXMLToList <- function(projectXMLFilePath = NULL, projectPath = NULL, projectXMLFileName = "project.xml") {
    # Get the path to the project.xml file if only the projectPath is given:
    if(length(projectPath))  {
        projectXMLFilePath <- getProjectXMLFilePath(projectPath = projectPath, projectXMLFileName = projectXMLFileName, projectXMLFilePath = projectXMLFilePath)
    }
    
    # Read the project.xml file into a list:
    if(!length(projectXMLFilePath)) {
        stop("File path to the project.xml file must be given.")
    }
    #doc = XML::xmlParse(projectXMLFile)
    doc = xml2::read_xml(projectXMLFilePath)
    #projectList <- XML::xmlToList(doc)
    projectList <- xml2::as_list(doc)$project
    
    return(projectList)
}


#' Write a project.xml file from StoX <= 2.7 from a list
#' 
#' @inheritParams general_arguments2.7
#' 
#' @export
#' 
writeProjectXMLToList <- function(projectXMLList, projectXMLFilePath, projectPath = NULL, projectXMLFileName = "project.xml") {
    
    ns <- attr(projectXMLList, "xmlns")
    xml <- xml2::as_xml_document(list(project = projectXMLList), ns = ns)
    
    # Get the path to the project.xml file if only the projectPath is given:
    if(length(projectPath))  {
        projectXMLFilePath <- getProjectXMLFilePath(projectPath = projectPath, projectXMLFileName = projectXMLFileName, projectXMLFilePath = projectXMLFilePath)
    }
    
    xml2::write_xml(xml, file = projectXMLFilePath, encoding = "UTF-8")
    
    return(projectXMLFilePath)
}



#' Get the path to the project.xml file from StoX <= 2.7.
#' 
#' @inheritParams general_arguments2.7
#' 
#' @export
#' 
getProjectXMLFilePath <- function(projectPath, projectXMLFileName = "project.xml", projectXMLFilePath = NULL) {
    if(!length(projectXMLFilePath)) {
        projectXMLFilePath <- file.path(projectPath, "process", projectXMLFileName)
    }
    
    return(projectXMLFilePath)    
}



#' Modify a parameter of a process of a list representation of a project.xml file from StoX <= 2.7.
#' 
#' @inheritParams general_arguments2.7
#' 
modifyParameter_ProjectXMLList <- function(projectXMLList, modelName, processName, parameterName, newParameterValue, functionName = NULL) {
    # Find the parameter of the process of the model:
    at <- findModelProcessParameter_ProjectXMLList(
        projectXMLList = projectXMLList, 
        modelName = modelName, 
        processName = processName, 
        parameterName = parameterName, 
        functionName = functionName
    )
    
    # Replace:
    projectXMLList[[at$atModel]][[at$atProcess]][[at$atParameter]] <- newParameterValue
    
    return(projectXMLList)
}


findModelProcessParameter_ProjectXMLList <- function(projectXMLList, modelName, processName, parameterName, functionName = NULL) {
    # Get the requested model:
    modelNames <- sapply(projectXMLList, attr, "name")
    atModel <- which(modelNames == modelName)
    #model <- projectXMLList[[atModel]]
    
    # Get the requested process:
    if(length(functionName)) {
        functionNames <- unlist(lapply(projectXMLList[[atModel]], "[[", "function"))
        atProcess <- which(functionNames == functionName)
    }
    else {
        processNames <- sapply(projectXMLList[[atModel]], attr, "name")
        atProcess <- which(processNames == processName)
    }
    
    # Get the requested parameter:
    parameterNames <- sapply(projectXMLList[[atModel]][[atProcess]], attr, "name")
    atParameter <- which(parameterNames == parameterName)
    
    at <- list(
        atModel = atModel, 
        atProcess = atProcess, 
        atParameter = atParameter
    )
    
    return(at)
}










#namesList2AttributedList <- function(x) {
#    if(is.list(x))  {
#        x <- lapply(x, namesList2AttributedList)
#        namesx <- names(x)
#        x <- lapply(namesx, function(name) {
#            attr(x[[name]], name) <- name;
#            return(x[[name]])
#            })
#        x <-  unname(x)
#        
#        return(x)
#    }
#    else{
#        return(x)
#    }
#}

















readStox2.7ProcessDataTable <- function(projectXMLFilePath, processDataName, oldName = NULL, newName = NULL, drop = FALSE) {
    # Read the project.xml file to a list:
    projectList <- readProjectXMLToList(projectXMLFilePath)
    
    # Discard any name that are not present in the process data:
    invalid <- setdiff(processDataName, names(projectList$processdata))
    if(length(invalid)) {
        warning("StoX: The following process data names are not present in the project.xml file ", projectXMLFilePath, " and were ignored.")
        processDataName <- intersect(processDataName, names(projectList$processdata))
    }
    
    processDataTables <- lapply(processDataName, readStox2.7ProcessDataTableOne, projectList = projectList)
    names(processDataTables) <- processDataName
    
    # Rename columns:
    if(length(oldName) && length(newName)) {
        processDataTables <- lapply(processDataTables, data.table::setnames, old = oldName, new = newName, skip_absent = TRUE)
    }
    
    if(drop && length(processDataTables) == 1) {
        processDataTables <- processDataTables[[1]]
    }
    
    return(processDataTables)
}


readStox2.7ProcessDataTableOne <- function(processDataName, projectList) {
    # Get the names of the data:
    dataNames <- names(projectList$processdata[[processDataName]][1])
    attNames <- names(attributes(projectList$processdata[[processDataName]][[1]]))
    columnNames <- c(
        dataNames, 
        attNames
    )
    
    # Get the data:
    data <- matrix(unlist(projectList$processdata[[processDataName]]), ncol = length(dataNames), byrow = TRUE)
    # Get the attributes:
    att <- lapply(projectList$processdata[[processDataName]], attributes)
    att <- matrix(unlist(att), ncol = length(attNames), byrow = TRUE)
    
    # Add the attributes to the data:
    processDataTable <- data.table::as.data.table(
        cbind(
            data, 
            att
        )
    )
    names(processDataTable) <- columnNames
    
    return(processDataTable)
}


#' Read a the StratumPolygon from a project.xml file from StoX <= 2.7
#' 
#' @param projectXMLFilePath The path to the project.xml file.
#' @param remove_includeintotal Logical: If TRUE, remove the column includeintotal column.
#' @param StratumNameLabel The label of the column giving the stratum names.
#' @param stratumPolygonFilePath The path to the file to write the StratumPolygon to (geojson file).
#' 
#' @export
#' 
readStratumPolygonFrom2.7 <- function(projectXMLFilePath, remove_includeintotal = TRUE, StratumNameLabel = "StratumName") {
    
    # Cannot use this as the stratum polygon is stored differently from a table in the project.xml, with two lines per polygon:
    ### # Read the data from the StoX 2.7 project.xml file.
    ### StratumPolygon <- readStox2.7ProcessDataTable(
    ###     projectXMLFilePath, 
    ###     processDataName = "stratumpolygon",
    ###     oldName = c("polygonkey", "polygon"), 
    ###     newName = c("Stratum", "Polygon"), 
    ###     drop = TRUE
    ### )
    
    # Read the project.xml file into a list:
    projectList <- readProjectXMLToList(projectXMLFilePath)
    # Convert the stratumpolygon to a table:
    StratumPolygon <- stratumpolygon2.7ToTable(projectList$processdata$stratumpolygon)
    # Rename the columns:
    data.table::setnames(StratumPolygon, old = c("polygonkey", "polygon"), new = c(StratumNameLabel, "geometry"), skip_absent = TRUE)
    
    # Remove the unused includeintotal column:
    if(remove_includeintotal) {
        StratumPolygon[, includeintotal := NULL]
    }
    else {
        StratumPolygon[, includeintotal := as.logical(includeintotal)]
    }
    
    return(StratumPolygon)
}
#'
#' @export
#' @rdname readStratumPolygonFrom2.7
#' 
writeStratumPolygonFrom2.7 <- function(projectXMLFilePath, stratumPolygonFilePath, remove_includeintotal = TRUE) {
    
    # Read the StratumPolygon from the StoX 2.7 project.xml file.
    StratumPolygon <- readStratumPolygonFrom2.7(projectXMLFilePath, remove_includeintotal = remove_includeintotal)
    
    # Write the StratumPolygon
    data.table::fwrite(
        StratumPolygon, 
        stratumPolygonFilePath, 
        col.names = FALSE, 
        sep = "\t"
    )
    
    return(stratumPolygonFilePath)
}



stratumpolygon2.7ToTable <- function(stratumpolygon) {
    # Get polygon keys:
    #polygonkey <- sapply(stratumpolygon, function(x) x$.attrs["polygonkey"])
    polygonkey <- sapply(stratumpolygon, attr, "polygonkey")
    
    # Convert to a list with one list per polygon:
    stratumpolygonList <- split(stratumpolygon, polygonkey)
    # ... and extract the includeintotal and polygon:
    stratumpolygonList <- lapply(stratumpolygonList, unlist)
    
    # Rbind to a data.table and add names:
    stratumpolygonTable <- do.call(rbind, stratumpolygonList)
    stratumpolygonTable <- data.table::data.table(names(stratumpolygonList), stratumpolygonTable)
    names(stratumpolygonTable) <- c("polygonkey", "includeintotal", "geometry")
    
    return(stratumpolygonTable)
}


#' Read a the AcousticPSU from a project.xml file from StoX <= 2.7
#' 
#' @param projectXMLFilePath The path to the project.xml file.
#' 
#' @export
#' 
readAcousticPSUFrom2.7 <- function(projectXMLFilePath) {
    
    # Read the old project.xml file:
    #projectList <- readProjectXMLToList(projectXMLPath2.7)
    
    ## Create the Stratum_PSU table:
    #Stratum_PSU <- data.table::as.data.table(
    #    cbind(
    #        Stratum = unlist(projectList$processdata$psustratum), 
    #        PSU = sapply(projectList$processdata$psustratum, attr, "psu")
    #    )
    #)
    ## And the EDSU_PSU table:
    #EDSU_PSU <- data.table::as.data.table(
    #    cbind(
    #        PSU = unlist(projectList$processdata$edsupsu), 
    #        EDSU = sapply(projectList$processdata$edsupsu, attr, "edsu")
    #    )
    #)
    #names(EDSU_PSU) <- c("PSU", "EDSU")
    
    processDataNames2.7 = c("edsupsu", "psustratum")
    processDataNames3.0 = c("EDSU_PSU", "Stratum_PSU")
    AcousticPSU <- readStox2.7ProcessDataTable(
        projectXMLFilePath, 
        processDataName = processDataNames2.7, 
        oldName = c("psu", "edsu", "stratum"), 
        newName = c("PSU", "EDSU", "Stratum")
    )
    names(AcousticPSU) <- processDataNames3.0
    
    
    ### data.table::setcolorder(EDSU_PSU, c("EDSU", "PSU"))
    
    # Parse out the Cruise and DateTime:
    #splitted <- AcousticPSU$EDSU_PSU[, strsplit(EDSU, "/")][[1]]
    splitted <- strsplit(AcousticPSU$EDSU_PSU$EDSU, "/")
    
    Cruise <- sapply(splitted, "[[", 1)
    Date <- sapply(splitted, "[[", 3)
    Time <- sapply(splitted, "[[", 4)
    DateTime <- paste0(Date, "T", Time, ".000Z")
    
    AcousticPSU$EDSU_PSU[, EDSU := paste(..Cruise, ..DateTime, sep = "/")]
    
    # Add empty PSUByTime. This will be added in DefineAcousticPSU:
    AcousticPSU$PSUByTime = data.table::data.table()

    
    return(AcousticPSU)
}


#' Read a the AcousticPSU from a project.xml file from StoX <= 2.7
#' 
#' @param projectXMLFilePath The path to the project.xml file.
#' @param MergedStoxDataHaulLevel Merged StoxBioticData down to Haul.
#' 
#' @export
#' 
readBioticPSUFrom2.7 <- function(projectXMLFilePath, MergedStoxDataHaulLevel) {
    
    # Read the BioticPSU from the StoX 2.7 process data:
    processDataNames2.7 = c("edsupsu", "psustratum")
    processDataNames3.0 = c("Station_PSU", "Stratum_PSU")
    BioticPSU <- readStox2.7ProcessDataTable(
        projectXMLFilePath, 
        processDataName = processDataNames2.7, 
        oldName = c("psu", "edsu", "stratum"), 
        newName = c("PSU", "Station", "Stratum")
    )
    names(BioticPSU) <- processDataNames3.0
    
    #### Convert the Station column to match the definition used in StoX >= 3, which is CruiseKey-StationKey: ####
    # Create a table from the MergedStoxDataHaulLevel containing the the following:
    # CruiseKey, which will be used to form the Station as defined in StoX >= 3
    # CruiseKey1, which is the first element of the CruiseKey, and corresponds to the first element of the EDSU in the StoX 2.7 edsupsu process data
    # HaulKey, which corresponds to the second element of the edsu in the StoX 2.7 edsupsu process data
    # Station, which we are replacing edsu in the edsu in the StoX 2.7 edsupsu process data by 
    toMergeFromStoxData <- MergedStoxDataHaulLevel[, c("CruiseKey", "Station", "HaulKey")]
    toMergeFromStoxData[, CruiseKey1 := sapply(strsplit(CruiseKey, "/"), "[[", 1)]
    if("CruiseKey" %in% names(BioticPSU$Station_PSU)) {
        BioticPSU$Station_PSU[, CruiseKey := NULL]
    }
    
    # Get the CruiseKey1 and the HaulKey to merge by:
    BioticPSU$Station_PSU[, CruiseKey1 := sapply(strsplit(Station, "/"), "[[", 1)]
    BioticPSU$Station_PSU[, HaulKey := sapply(strsplit(Station, "/"), "[[", 2)]
    
    # Issue a warning if there are cruises that are present in the process data read from the project.xml file but not in the data:
    notPresent <- ! BioticPSU$Station_PSU$CruiseKey1 %in% toMergeFromStoxData$CruiseKey1
    if(any(notPresent)) {
        warning("StoX: The following biotic PSUs were not recognized in the StoxBioticData. This can either be due to different data used in the original (StoX <= 2.7) and new (StoX >= 3.0.0) project, or that the table edsupsu of the old project.xml file contains errors in the edsu column. In the latter case, it may be that Methtod = \"Station\" and not \"UseProcessData\" in DefineSweptAreaPSU() so that the edsupsu process data is not relevant. In that case please rerun the old StoX project, save so that the edsupsu table is updatted, and try again:\n", paste("\t", BioticPSU$Station_PSU$Station[notPresent], collapse = "\n"))
    }
    # .. and delete Station, as it will be replaced by the Station from toMergeFromStoxData:
    BioticPSU$Station_PSU[, Station := NULL]
    
    # Insert Station by merging:
    BioticPSU$Station_PSU <- merge(BioticPSU$Station_PSU, toMergeFromStoxData, by = c("CruiseKey1", "HaulKey"), all.x = TRUE)
    
    return(BioticPSU)
}




#' Read a the BioticAssignment from a project.xml file from StoX <= 2.7
#' 
#' @param projectXMLFilePath The path to the project.xml file.
#' 
#' @export
#' 
readBioticAssignmentFrom2.7 <- function(projectXMLFilePath) {
    
    # Read the data from the StoX 2.7 project.xml file.
    processDataNames2.7 = c("bioticassignment", "suassignment", "psustratum")
    BioticAssignment2.7 <- readStox2.7ProcessDataTable(
        projectXMLFilePath, 
        processDataName = processDataNames2.7, 
        oldName = c("stationweight",   "assignmentid", "station", "sampleunit", "estlayer", "stratum", "psu"), 
        newName = c("WeightingFactor", "AssignmentID", "Haul",    "PSU",        "Layer",    "Stratum", "PSU")
    )
    
    if(!BioticAssignment2.7$suassignment[, allEqual(Layer)]) {
        stop("Currently, only StoX 2.7 projects with layer type \"WaterColumn\" can be automatically converted to StoX 3.0 and higher.")
    }
    BioticAssignment2.7$suassignment[, Layer := "WaterColumn"]
    
    # Add stratum:
    BioticAssignment <- merge(BioticAssignment2.7$bioticassignment, BioticAssignment2.7$suassignment, by = "AssignmentID", allow.cartesian = TRUE)
    BioticAssignment <- merge(BioticAssignment, BioticAssignment2.7$psustratum, by = "PSU", allow.cartesian = TRUE)
    
    # Make sure WeightingFactor is numeric:
    BioticAssignment[, WeightingFactor := as.numeric(WeightingFactor)]
    
    
    return(BioticAssignment)
}



#expandStoxData <- function(stoxData, dimensionVariables, targetVariables, informationVariables = NULL) {
#    
#    # Get the grid of the dimensionVariables:
#    grid <- do.call(data.table::CJ, lapply(stoxData[, ..dimensionVariables], unique))
#    # Add the informationVariables:
#    if(length(informationVariables)) {
#        dimensionAndInformationVariables <- c(dimensionVariables, informationVariables)
#        grid <- merge(grid, stoxData[, ..dimensionAndInformationVariables], all.x = TRUE, by = dimensionVariables)
#    }
#    grid$index_ <- seq.int(nrow(grid))
#    
#    # Save indices in the grid at which data are present:
#    arePresent <- grid[stoxData[, ..dimensionVariables], on = dimensionVariables]$index_
#    # Save indices in the grid at which there are NAs in the data:
#    #areNA <- arePresent[is.na(stoxData[[targetVariable]])]
#    #areNA <- intersect(which(rowSums(stoxData[, lapply(.SD, is.na), .SDcols = targetVariables]) == 0), arePresent)
#    areNA <- lapply(targetVariables, function(x) stoxData[, which(is.na(get(x)))])
#    names(areNA) <- targetVariables
#    
#    # Insert the data onto the grid:
#    stoxData <- stoxData[grid[, ..dimensionVariables], on = dimensionVariables]
#    
#    # Convert the NAs to 0 for the targetVariables:
#    # First, set all NA to 0, both those from the original stoxData and those introduced by the grid:
#    replaceNAByReference(stoxData, cols = targetVariables, replacement = 0)
#    # Then restore the NAs from the original stoxData!!:
#    #stoxData[areNA, eval(targetVariables) := NA]
#    lapply(targetVariables, function(x) stoxData[areNA[[x]], eval(x) := NA])
#    
#    return(stoxData)
#}



#' Get all arguments from inside a function
#' 
#' @param orig_values Logical: If TRUE use the original values (as defined in the function).
#' @export
#' 
allargs <- function(orig_values = FALSE) {
    
    # Borrowed from https://stackoverflow.com/questions/11885207/get-all-parameters-as-list
    
    # get formals for parent function
    parent_formals <- formals(sys.function(sys.parent(n = 1)))
    
    # Get names of implied arguments
    fnames <- names(parent_formals)
    
    # Get currently set values for named variables in the parent frame
    args <- evalq(as.list(environment()), envir = parent.frame())
    
    # Get the list of variables defined in '...'
    args <- args[fnames]
    
    
    if(orig_values) {
        # get default values
        defargs <- as.list(parent_formals)
        defargs <- defargs[unlist(lapply(defargs, FUN = function(x) class(x) != "name"))]
        args[names(defargs)] <- defargs
        setargs <- evalq(as.list(match.call())[-1], envir = parent.frame())
        args[names(setargs)] <- setargs
    }
    return(args)
}

# Set default general options:
setDefaults <- function(x, defaults) {
    presentNames <- intersect(names(x), names(defaults))
    for(name in presentNames) {
        if(!length(x[[name]])) {
            x[[name]] <- defaults[[name]]
        }
    }
    
    return(x)
}





#' Set default general options
#' 
#' @param x A list of arguments to the StoxFunction.
#' @param StoxFunctionName The name of the StoxFunction.
#' @param stoxFunctionAttributes The stoxFunctionAttributes to use.
#' 
#' @export
#' 
setDefaultsInStoxFunction <- function(x, StoxFunctionName, stoxFunctionAttributes) {
    # The following line failed if the StoX function was run inside e.g. an mapply():
    #StoxFunctionName <- tail(as.character(as.list(sys.call(-1))[[1]]), 1)
    defaults <- stoxFunctionAttributes[[StoxFunctionName]]$functionParameterDefaults
    #if(length(condition) && is.character(condition) && nchar(condition)) {
    #    
    #}
    
    presentNames <- intersect(names(x), names(defaults))
    for(name in presentNames) {
        if(!length(x[[name]])) {
            if(is.function(defaults[[name]])) {
                x[[name]] <- defaults[[name]](x)
            }
            else {
                x[[name]] <- defaults[[name]]
            }
        }
    }
    
    return(x)
}






renameListByNames <- function(list, old, new) {
    if(length(old) != length(new)) {
        stop("'old' and 'new' must have the same length.")
    }
    if(any(duplicated(old))) {
        stop("'old' cannot contain duplicates.")
    }
    valid <- intersect(old, names(list))
    invalid <- setdiff(old, names(list))
    if(length(invalid)) {
        warning("The following names are specified as 'old' but are not found in the list. These are ignored.")
        new <- new[old %in% names(list)]
    }
    atRename <- match(valid, names(list))
    names(list)[atRename] <- new
    
    return(list)
}


    


#' Get base unit of a variable of a StoX dataType
#' 
#' @param dataType The name of a StoX \code{\link{DataTypes}}.
#' @param variableName The variable name to get base units from.
#' @param element The element to get, one of "unit" and "quantity".
#' @param list.out Logical: If TRUE return both elements in a list.
#' @export
#' 
getBaseUnit <- function(dataType, variableName, element = c("unit", "quantity"), list.out = FALSE) {
    
    
    emptyOutput <- output <- list(
        unit = NA, 
        quantity = NA
    )
    
    if(!length(dataType)) {
        return(emptyOutput)
    }
    
    element <- match.arg(element)
    dataTypeUnits <- getRstoxBaseDefinitions("dataTypeUnits")
    
    #output <- dataTypeUnits[[dataType]][[variableName]]
    # The dataTypeUnits were changed to a data.table on 2023-03-08:
    this_dataType <- dataType
    this_variableName <- variableName
    output <- subset(dataTypeUnits, dataType == this_dataType & variableName == this_variableName)
    if(!length(output)) {
        return(emptyOutput)
    }
    
    if(!list.out) {
        return(output[[element]])
    }
    else {
        return(output)
    }
}

#' Does the variable of the StoX dataType have a base unit?
#' 
#' @inheritParams getBaseUnit
#' 
hasBaseUnit <- function(dataType, variableName) {
    baseUnit <- getBaseUnit(dataType = dataType, variableName = variableName, list.out = TRUE)
    
    if(NROW(baseUnit)) {
        !is.na(baseUnit$unit) && !is.na(baseUnit$quantity)
    }
    else {
        FALSE
    }
}

#' Set base unit to a variable of the StoX dataType
#' 
#' @param x The object to set base units to.
#' @inheritParams getBaseUnit
#' 
setBaseUnit <- function(x, dataType, variableName) {
    if(!RstoxData::hasUnit(x, property = "shortname")) {
        baseUnit <- getBaseUnit(dataType = dataType, variableName = variableName, list.out = TRUE)
        id <- RstoxData::findUnit(quantity = baseUnit$quantity, shortname = baseUnit$unit)
        x <- setUnit(x, id)
    }
    return(x)
}

#' Set units to a variable of the StoX dataType
#' 
#' @param x A StoX data
#' @inheritParams getBaseUnit
#' @param unit The unit to set to the variable of the StoX Data
#' @export
#' 
setUnitRstoxBase <- function(x, dataType, variableName, unit = NULL) {
    this_hasBaseUnit <- hasBaseUnit(dataType = dataType, variableName = variableName)
    
    if(length(unit) && this_hasBaseUnit) {
        # Set the base unit if the objectt does not have a unit:
        x <- setBaseUnit(x, dataType, variableName)
        
        # Get the quantity:
        quantity <- getBaseUnit(dataType = dataType, variableName = variableName, element = "quantity")
        id <- RstoxData::findUnit(quantity = quantity, shortname = unit)
        x <- setUnit(x, id)
    }
    else if(length(unit) && !this_hasBaseUnit) {
        warning("StoX: Units not defined for variable ", variableName, " of dataType ", dataType, ". The unit (", unit, ") was ignored.")
    }
    
    return(x)
}



#' Convert to factor and put NAs first
#' 
#' @param x A vector.
#' @export
#' 
factorNAfirst <- function(x){
    if(is.numeric(x)){
        # Create an ordered vector spanning the range of x:
        r <- range(x, na.rm = TRUE)
        if(diff(r) == 0){
            levels <- r[1]
        }
        else{
            levels <- seq(r[1], r[2], by = median(diff(sort(unique(x))), na.rm = TRUE))
        }
        
        # Add NAs first:
        if(any(is.na(x))){
            levels <- c(NA, levels)
        }
    }
    else{
        levels <- unique(x)
    }
    # Convert a single NA to "NA":
    if(length(levels) == 1 && is.na(levels)){
        levels <- "NA"
        x <- rep("NA", length(x))
    }
    
    levels <- sort(levels, na.last = FALSE)
    
    factor(x, levels = levels, exclude = NULL)
}


