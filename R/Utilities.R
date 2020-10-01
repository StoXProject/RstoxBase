
## Function to get the least common multiple of a vector of integers:
#getLeastCommonMultiple <- function(x, max = NULL, N = 100) {
#    multiples <- lapply(x, function(y) y * seq_len(N))
#    commonMultiples <- multiples[[1]]
#    for(i in seq_len(length(l) - 1)) {
#        commonMultiples <- intersect(commonMultiples, multiples[[i + 1]])
#    }
#    
#    if(length(max)) {
#        subset(commonMultiples, commonMultiples <= max)
#    }
#    else {
#        min(commonMultiples)
#    }
#    
#}

#' 
#' @export
#' 
# Function to expand a data table so that the cells that are vectors are transposed and the rest repeated to fill the gaps:
expandDT <- function(DT, toExpand = NULL) {
    # Set the columns to expand:
    if(length(toExpand) == 0) {
        lens <- lapply(DT, lengths)
        lensLargerThan1 <- sapply(lens, function(l) any(l > 1))
        toExpand <- names(DT)[lensLargerThan1]
    }
    
    if(length(toExpand)) {
        expanded <- lapply(toExpand, function(x) DT[, unlist(get(x))])
        names(expanded) <- toExpand
        DT <- do.call(
            cbind, 
            c(
                list(
                    DT[rep(1:.N, lengths(get(toExpand[1]))), !toExpand, with = FALSE]
                ), 
                #lapply(toExpand, function(x) DT[, unlist(get(x))])
                expanded
            )
        )
    }
    
    DT
}


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
    
    # This needs to be verified!!!!!!!!!!!!!
    layerRangeVector <- c(layerProcessData$MinLayerDepth, utils::tail(layerProcessData$MaxLayerDepth, 1))
    indMin <- findInterval(minDepth, layerRangeVector)
    indMax <- findInterval(maxDepth, layerRangeVector, rightmost.closed = TRUE)
    LayerInd <- pmin(indMin, indMax, na.rm = acceptNA)
    
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
        notPSUStartEndDateTime <- names(PSUProcessData) != "PSUStartEndDateTime"
        PSUProcessData <- RstoxData::mergeDataTables(PSUProcessData[notPSUStartEndDateTime], output.only.last = TRUE, ...)
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

# Function to add Layer:
addLayerProcessData <- function(data, dataType, layerProcessData = NULL, acceptNA = TRUE) {
    
    # Insert the Layer column from the layerProcessData input, and otherwise by NAs:
    if(length(layerProcessData)) {
        # Get the variables to aggregate by etc.:
        dataTypeDefinition <- getDataTypeDefinition(dataType = dataType)
        varMin <- dataTypeDefinition$verticalRawDimension[1]
        varMax <- dataTypeDefinition$verticalRawDimension[2]
        
        # Get min and max depth of the raw vertical distribution, either as a copy of the depths, or a calculation of the depths given the depth and orientation of the coordinate system:
        if(length(dataTypeDefinition$coordinateSystemOrigin)) {
            minDepth <- data[[dataTypeDefinition$coordinateSystemOrigin]] - 
                data[[varMin]] * cos(data[[dataTypeDefinition$coordinateSystemOrientation]] * pi/ 180)
            maxDepth <- data[[dataTypeDefinition$coordinateSystemOrigin]] - 
                data[[varMax]] * cos(data[[dataTypeDefinition$coordinateSystemOrientation]] * pi/ 180)
        }
        else {
            minDepth <- data[[varMin]]
            maxDepth <- data[[varMax]]
        }
        
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


meanData <- function(data, dataType, PSUDefinition = c("PreDefined", "FunctionInput"), PSUProcessData = NULL, targetResolution = "PSU") {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    dataCopy = data.table::copy(data)
    
    # Add the PSUs if PSUDefinition is "FunctionInput" and PSUProcessData is given:
    PSUDefinition <- match.arg(PSUDefinition)
    if(identical(PSUDefinition, "FunctionInput")) {
        dataCopy <- addPSUProcessData(dataCopy, PSUProcessData = PSUProcessData, all = TRUE)
    }
    else if(identical(PSUDefinition, "PreDefined")) {
        if(all(is.na(data$PSU))) {
            stop("PSUs must be present in the data if PSUDefinition is \"PreDefined\"")
        }
    }
    else {
        stop("Invalid PSUDefinition, must be one of \"PreDefined\" and \"FunctionInput\"")
    }
    
    # Get the variables to aggregate by etc.:
    aggregationVariables <- determineAggregationVariables(
        data = dataCopy, 
        dataType = dataType, 
        targetResolution = targetResolution, 
        dimension = "horizontal"
    )
    # Extract the 'by' element:
    by <- aggregationVariables$by
    
    # Check that the average can be made, that is that the vertical resolution is identical throughout each unit in the targetResolution:
    if(utils::tail(aggregationVariables$presentResolution, 1) == aggregationVariables$finestResolution) {
        valid <- data[, lapply(aggregationVariables$verticalRawDimension, allEqual), by = by]
    }
    
    # Weighted average of the data variable over the grouping variables, weighted by the weighting variable:
    dataVariable <- aggregationVariables$dataVariable
    weightingVariable <- aggregationVariables$weightingVariable
    
    # Extract the resolution and weighting variables, and uniquify:
    extract <- c(aggregationVariables$presentResolution, weightingVariable)
    summedWeighting <- dataCopy[, ..extract]
    summedWeighting <- unique(summedWeighting)
    # Then sum the weights by the next resolution, PSU for mean of stations/EDSUs and Stratum for mean of PSUs:
    summedWeighting[, SummedWeights := sum(get(weightingVariable), na.rm = TRUE), by = eval(aggregationVariables$nextResolution)]
    
    # Extract the next resolution and the summed weights and uniquify:
    extract <- c(aggregationVariables$nextResolution, "SummedWeights")
    summedWeighting <- summedWeighting[, ..extract]
    summedWeighting <- unique(summedWeighting)
    
    # Merge the resulting summed weights with the data, by the next resolution:
    summedWeightingBy <- aggregationVariables$nextResolution
    dataCopy <- merge(dataCopy, summedWeighting, by = summedWeightingBy, all = TRUE)
    
    # Finally weighted sum the data, and divide by the summed weights (the last step is the crusial part):
    dataCopy[, c(dataVariable) := sum(get(dataVariable) * get(weightingVariable), na.rm = TRUE) / SummedWeights, by = by]
    # Replace the weights by the summed weights:
    dataCopy[, c(weightingVariable) := SummedWeights]
    dataCopy[, SummedWeights := NULL]
    
    # Set the resolution variables which were summed over to NA:
    set(
        dataCopy, 
        j = c(
            aggregationVariables$setToNA, 
            aggregationVariables$otherVariables
        ), 
        value = NA
    )
    
    # Remove duplicated rows:
    dataCopy <- subset(dataCopy, !duplicated(dataCopy[, ..by]))
    
    # Set the order of the columns:
    formatOutput(dataCopy, dataType = dataType, keep.all = FALSE)
    ## Order by 'by':
    #setorderv(dataCopy, cols = by, order = 1L, na.last = TRUE)
    
    # Keep only the releavnt columns:
    #keepOnlyRelevantColumns(dataCopy, dataType)
    
    return(dataCopy)
}

# No longer used:
sumData <- function(data, dataType, LayerDefinition = c("PreDefined", "FunctionInput"), layerProcessData = NULL, targetResolution = "Layer") {
    
    # Make a copy of the input, since we are summing and setting values by reference:
    dataCopy = data.table::copy(data)
    
    # Add the PSUs if PSUDefinition is "FunctionInput" and PSUProcessData is given:
    LayerDefinition <- match.arg(LayerDefinition)
    if(identical(LayerDefinition, "FunctionInput")) {
        dataCopy <- addLayerProcessData(dataCopy, dataType = dataType, layerProcessData = layerProcessData)
    }
    else if(identical(LayerDefinition, "PreDefined")) {
        if(all(is.na(data$Layer))) {
            stop("Layers must be present in the data if LayerDefinition is \"PreDefined\"")
        }
    }
    else {
        stop("Invalid LayerDefinition, must be one of \"PreDefined\" and \"FunctionInput\"")
    }
    # Get the variables to aggregate by etc.:
    aggregationVariables <- determineAggregationVariables(
        data = dataCopy, 
        dataType = dataType, 
        targetResolution = targetResolution, 
        dimension = "vertical"
    )
    # Extract the 'by' element:
    by <- aggregationVariables$by
    
    # Weighted average of the data variable over the grouping variables, weighted by the weighting variable:
    dataVariable <- aggregationVariables$dataVariable
    
    #LengthDistributionData[, WeightedCount := sum(WeightedCount), by = by]
    dataCopy[, c(dataVariable) := sum(x = get(dataVariable)), by = by]
    
    # Set the resolution variables which were summed over to NA:
    set(
        dataCopy, 
        j = c(
            aggregationVariables$setToNA, 
            aggregationVariables$verticalRawDimension
        ), 
        value = NA
    )
    
    # Remove duplicated rows:
    dataCopy <- subset(dataCopy, !duplicated(dataCopy[, ..by]))
    
    # Keep only the releavnt columns:
    #keepOnlyRelevantColumns(dataCopy, dataType)
    formatOutput(dataCopy, dataType = dataType, keep.all = FALSE)
    
    return(dataCopy)
}
















sumRawResolutionData <- function(
    data, dataType, 
    LayerDefinition = c("FunctionParameter", "FunctionInput"), 
    LayerProcessData = NULL, 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    LayerType = c("Acoustic", "Biotic")
) {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    dataCopy = data.table::copy(data)
    
    # Add the Layers and PSUs either from function inputs or by automatic methods using function parameters:
    LayerDefinition <- match.arg(LayerDefinition)
    
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
    resolutionVariables <- getAllResolutionVariables(
        dataType = dataType#, 
        #dimension = "vertical"
    )
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
    PSUDefinition = c("FunctionParameter", "FunctionInput"), 
    PSUProcessData = NULL, 
    PSUDefinitionMethod = c("Identity", "None"), 
    StratumPolygon = NULL, 
    PSUType = c("Acoustic", "Biotic")
) {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    dataCopy = data.table::copy(data$Data)
    resolutionCopy = data.table::copy(data$Resolution)
    
    # Add the PSUs either from function input or by automatic method using function parameter:
    PSUDefinition <- match.arg(PSUDefinition)
    
    # Get the PSUs:
    if(identical(PSUDefinition, "FunctionParameter")) {
        PSUProcessData <- DefinePSU(
            StratumPolygon = StratumPolygon, 
            StoxData = dataCopy, 
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
    
    # Get the resolution table, holding the Station/EDSU and all vertical resolution variables:
    resolutionVariables <- getAllResolutionVariables(
        dataType = dataType#, 
        #dimension = "horizontal"
    )
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
    
    # Average the data horizonally:
    aggregatedData <- applyMeanToData(data = dataCopy, dataType = dataType, targetResolution = "PSU")
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(aggregatedData)
    
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
    aggregationVariables <- determineAggregationVariables(
        data = data, 
        dataType = targetDataType, 
        targetResolution = "Layer", 
        dimension = "vertical"
    )
    # Extract the 'by' element:
    by <- aggregationVariables$by
    
    # Sum of the data variable over the grouping variables:
    dataVariable <- aggregationVariables$dataVariable
    data[, c(dataVariable) := sum(x = get(dataVariable)), by = by]
    
    # Add the weighting variable:
    oldWeightingVariable <- getDataTypeDefinition(dataType, elements = "weighting", unlist = TRUE)
    weightingVariable <- aggregationVariables$weightingVariable
    data[, c(weightingVariable) := get(oldWeightingVariable)]
    
    # Remove the resolution variables which were summed over, and the verticalRawDimension:
    #set(
    #    data, 
    #    j = c(
    #        aggregationVariables$setToNA, 
    #        aggregationVariables$verticalRawDimension
    #    ), 
    #    value = NA
    #)
    data[, (c(aggregationVariables$setToNA, aggregationVariables$verticalRawDimension)) := NULL] 
    
    # Remove duplicated rows:
    data <- subset(data, !duplicated(data[, ..by]))
    
    # Keep only the releavnt columns:
    formatOutput(data, dataType = targetDataType, keep.all = FALSE)
    # Order the rows:
    orderDataByReference(data, targetDataType)
    
    return(data)
}

applyMeanToData <- function(data, dataType, targetResolution = "PSU") {
    
    # Store the original data type defnition, particularly for summing the weights:
    originalDataTypeDefinition <- getDataTypeDefinition(dataType)
    
    # Get the variables to aggregate by etc.:
    targetDataType <- paste0("Mean", sub("Sum", "", dataType))
    aggregationVariables <- determineAggregationVariables(
        data = data, 
        dataType = targetDataType, 
        targetResolution = targetResolution, 
        dimension = "horizontal"
    )
    # Extract the 'by' element:
    by <- aggregationVariables$by
    
    #### Check that the average can be made, that is that the vertical resolution is identical throughout each unit in the targetResolution:
    ###if(utils::tail(aggregationVariables$presentResolution, 1) == aggregationVariables$finestResolution) {
    ###    valid <- data[, lapply(aggregationVariables$verticalRawDimension, allEqual), by = by]
    ###}
    # Weighted average of the data variable over the grouping variables, weighted by the weighting variable:
    dataVariable <- aggregationVariables$dataVariable
    targetWeightingVariable <- aggregationVariables$weightingVariable
    weightingVariable <- originalDataTypeDefinition$weighting

    # Extract the resolution and weighting variables:
    extract <- c(aggregationVariables$presentResolution, weightingVariable)
    summedWeighting <- data[, ..extract]
    # Finally uniquify to the Stratum/Layer:
    summedWeighting <- unique(summedWeighting)
    # Then sum the weights by the next resolution, PSU for mean of stations/EDSUs and Stratum for mean of PSUs:
    summedWeighting[, SummedWeights := sum(get(weightingVariable), na.rm = TRUE), by = eval(aggregationVariables$nextResolution)]
    
    # Extract the next resolution and the summed weights and uniquify:
    extract <- c(aggregationVariables$nextResolution, "SummedWeights")
    summedWeighting <- summedWeighting[, ..extract]
    summedWeighting <- unique(summedWeighting)
    
    # Merge the resulting summed weights with the data, by the next resolution:
    summedWeightingBy <- aggregationVariables$nextResolution
    data <- merge(data, summedWeighting, by = summedWeightingBy, all = TRUE)
    
    # Finally weighted sum the data, and divide by the summed weights (the last step is the crusial part):
    data[, c(dataVariable) := sum(get(dataVariable) * get(weightingVariable), na.rm = TRUE) / SummedWeights, by = by]
    # Store the new weights by the summed original weights:
    data[, c(targetWeightingVariable) := SummedWeights]
    ###data[, SummedWeights := NULL]
    
    # Set the resolution variables which were summed over to NA:
    #set(
    #    data, 
    #    j = c(
    #        aggregationVariables$setToNA, 
    #        aggregationVariables$otherVariables
    #    ), 
    #    value = NA
    #)
    ###data[, (c(aggregationVariables$setToNA, aggregationVariables$otherVariables)) := NULL] 
    
    
    # Set the order of the columns:
    formatOutput(data, dataType = targetDataType, keep.all = FALSE)
    # Order the rows:
    orderDataByReference(data, targetDataType)
    
    
    # Remove duplicated rows:
    data <- subset(data, !duplicated(data[, ..by]))
    
    
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
    lapply(x, "[", o)
}


compareTypes <- function(x, y, allow.numeric = TRUE) {
    out <- identical(x, t)
    if(allow.numeric) {
        out <- out | all(is.numeric(x), is.numeric(y))
    }
    return(out)
}



checkResolutionPSU_Layer <- function(data, dataType) {
    
    # Set the target resolution:
    horizontalResolution <- "PSU"
    verticalResolution <- "Layer"
    # Get the column expected to be NA:
    expectNAHorizontal <- determineAggregationVariables(data, dataType, horizontalResolution, dimension = "horizontal")$setToNA
    expectNAVertical <- determineAggregationVariables(data, dataType, verticalResolution, dimension = "vertical")$setToNA
    wrongHorizontalResolution <- data[, !all(is.na(get(expectNAHorizontal)))] || data[, all(is.na(get(horizontalResolution)))]
    wrongVerticalResolution <- data[, !all(is.na(get(expectNAVertical)))] || data[, all(is.na(get(verticalResolution)))]
    
    if(wrongHorizontalResolution && wrongVerticalResolution) {
        stop("The data does not have the correct horizontal (", horizontalResolution, ") and vertical (", verticalResolution, ") resolution")
    }
    else if(wrongHorizontalResolution) {
        stop("The data does not have the correct horizontal resolution (", horizontalResolution, ")")
    }
    else if(wrongVerticalResolution) {
        stop("The data does not have the correct vertical resolution (", verticalResolution, ")")
    }
    
    return(TRUE)
}


removeColumnsByReference <- function(data, toRemove) {
    toRemove <- intersect(toRemove, names(data))
    if(length(toRemove)) {
        data[, c(toRemove) := NULL]
    }
}



#### Tools to perform resampling: ####

# Function to sample after sorting:
#' 
#' @export
#' 
sampleSorted <- function(x, size, seed, replace = TRUE, sorted = TRUE, index.out = FALSE, redraw.seed = FALSE){
    # If not given, get the size of the sample as the length of the vector:
    lx <- length(x)
    if(missing(size)){
        size <- lx
    }
    if(sorted){
        x <- sort(x)
    }
    # Sample:
    set.seed(seed)
    # To increase uniqueness in sampling, resample the seed from the length of the vector to sample:
    if(redraw.seed) {
        seed <- sample.int(lx, 1)
        set.seed(seed)
    }
    sampled <- x[sample.int(lx, size = size, replace = replace)]
    
    if(index.out) {
        sampled <- match(sampled, x)
    }
    return(sampled)
}

#' 
#' @export
#' 
getSeedVector <- function(seed, size = 1) {
    set.seed(seed)
    sample(getSequenceToSampleFrom(), size, replace = FALSE)
}

getSequenceToSampleFrom <- function(){
    seedSequenceLength <- getRstoxFrameworkDefinitions("seedSequenceLength")
    seq_len(seedSequenceLength)
}


# Define report functions:
summaryStox <- function(x, na.rm = FALSE) {
    Percentile5 <- stats::quantile(x, 0.05, na.rm = na.rm)
    Median <- stats::median(x, na.rm = na.rm)
    Percentile95 <- stats::quantile(x, 0.95, na.rm = na.rm)
    Mean <- base::mean(x, na.rm = na.rm)
    SD <- stats::sd(x, na.rm = na.rm)
    CV <- SD / Mean
    summaryStox <- c(
        Percentile5 = Percentile5, 
        Median = Median, 
        Percentile95 = Percentile95, 
        Mean = Mean, 
        SD = SD, 
        CV = CV
    )
    return(summaryStox)
}
CV = function(x, na.rm = FALSE) {
    sd(x) / mean(x, na.rm = na.rm)
}
percentile_5_95 = function(x) {
    quantiile(x, c(5, 95) / 100)
}


# Define report functions:
#' 
#' @export
#' 
summaryStox <- function(x, na.rm = FALSE) {
    c(
        quantile(x, c(0.05, 0.5, 0.95), na.rm = na.rm),
        mean = mean(x, na.rm = na.rm),
        sd = sd(x, na.rm = na.rm),
        cv = cv(x, na.rm = na.rm)
    )
}
#' 
#' @export
#' 
cv <- function(x, na.rm = FALSE) {
    sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
#' 
#' @export
#' 
percentile_5_95 <- function(x, na.rm = FALSE) {
    quantile(x, c(0.05, 0.95), na.rm = na.rm)
}


isEmptyString <- function(x) {
    is.character(x) && (
        !length(x) || 
            (length(x) == 1 && nchar(x) == 0)
        )
}

