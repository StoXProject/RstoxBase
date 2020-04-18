
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
findLayer <- function(minDepth, maxDepth, layerDefinition, acceptNA = TRUE) {
    
    # This needs to be verified!!!!!!!!!!!!!
    layerRangeVector <- c(layerDefinition$MinLayerDepth, utils::tail(layerDefinition$MaxLayerDepth, 1))
    indMin <- findInterval(minDepth, layerRangeVector)
    indMax <- findInterval(maxDepth, layerRangeVector, rightmost.closed = TRUE)
    LayerInd <- pmin(indMin, indMax, na.rm = acceptNA)
    
    if(acceptNA && nrow(layerDefinition) == 1 && layerDefinition$MinLayerDepth == 0 && layerDefinition$MaxLayerDepth == Inf) {
        LayerInd <- replace(LayerInd, is.na(LayerInd), 1)
    }
    
    LayerData <- data.table::data.table(
        Layer = layerDefinition$Layer[LayerInd], 
        MinLayerDepth = layerDefinition$MinLayerDepth[LayerInd], 
        MaxLayerDepth = layerDefinition$MaxLayerDepth[LayerInd]
    )
    
    LayerData
}

# Function to add Stratum and PSU:
addPSUDefinition <- function(data, dataType, PSUDefinition = NULL, ...) {
    
    # If present, add the PSUDefinition to the start of the data
    if(length(PSUDefinition) == 0 || length(PSUDefinition$Stratum_PSU) == 0) {
        warning("PSUs not defined, possibly due to no data inside the any strata")
        data.table::setDT(data)
        toAdd <- c("Stratum", "PSU")
        data.table::set(data, j = toAdd, value = NA)
    }
    else {
        # Merge first the PSUDefinition:
        PSUDefinition <- RstoxData::mergeDataTables(PSUDefinition, output.only.last = TRUE, ...)
        # Then merge the result with the data:
        by <- intersect(names(PSUDefinition), names(data))
        data <- merge(PSUDefinition, data, by = by, ...)
    }
    
    
    # Set the order of the columns:
    #dataType <- detectDataType(data)
    data <- setColumnOrder(data, dataType = dataType, keep.all = TRUE)
    
    return(data)
}

# Function to add Layer:
addLayerDefinition <- function(data, dataType, layerDefinition = NULL, acceptNA = TRUE) {
    
    # Insert the Layer column from the layerDefinition input, and otherwise by NAs:
    if(length(layerDefinition)) {
        # Get the variables to aggregate by etc.:
        dataTypeDefinition <- getDataTypeDefinition(dataType = dataType)
        varMin <- dataTypeDefinition$verticalRawDimension[1]
        varMax <- dataTypeDefinition$verticalRawDimension[2]
        
        #layerData <- findLayer(data = data, layerDefinition = layerDefinition, varMin = varMin, varMax = varMax)
        
        
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
            layerDefinition = layerDefinition, 
            acceptNA = acceptNA
        )
        
        data <- data.table::data.table(layerData, data)
    }
    else {
        data.table::setDT(data)
        toAdd <- c("Layer", "MinLayerDepth", "MaxLayerDepth")
        data.table::set(data, j = toAdd, value=NA)
    }
    
    # Set the order of the columns:
    #dataType <- detectDataType(data)
    data <- setColumnOrder(data, dataType = dataType, keep.all = TRUE)
    
    return(data)
}




# Stolen from https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector:
allEqual <- function(x, tol = .Machine$double.eps ^ 0.5) {
    if (length(x) == 1) return(TRUE)
    x <- range(x) / mean(x)
    isTRUE(all.equal(x[1], x[2], tolerance = tol))
}


meanData <- function(data, dataType, targetResolution = "PSU") {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    dataCopy = data.table::copy(data)
    
    # Get the variables to aggregate by etc.:
    aggregationVariables <- determineAggregationVariables(
        data = dataCopy, 
        dataType = dataType, 
        targetResolution = targetResolution, 
        dimension = "horizontal"
    )
    # Extract the 'by' element:
    by <- aggregationVariables$by
    print(by)
    
    # Check that the average can be made, that is that the vertical resolution is identical throughout each unit in the targetResolution:
    if(utils::tail(aggregationVariables$presentResolution, 1) == aggregationVariables$finestResolution) {
        valid <- data[, lapply(aggregationVariables$verticalRawDimension, allEqual), by = by]
    }
    
    # Weighted average of the data variable over the grouping variables, weighted by the weighting variable:
    dataVariable <- aggregationVariables$dataVariable
    weightingVariable <- aggregationVariables$weightingVariable
    summedWeightingVariable <- aggregationVariables$summedWeightingVariable
    
    #LengthDistributionData[, WeightedCount := sum(WeightedCount), by = by]
    #dataCopy[, c(dataVariable, weightingVariable) := list(
    #    stats::weighted.mean(x = get(dataVariable), w = get(weightingVariable)), 
    #    sum(get(weightingVariable))
    #    ), by = by]
    ### dataCopy[, c(dataVariable, weightingVariable) := list(
    ###     sum(get(dataVariable) * get(weightingVariable)) / get(summedWeightingVariable), 
    ###     sum(get(weightingVariable))
    ### ), by = by]
    
    # Sum the weights:
    extract <- c(aggregationVariables$presentResolution, weightingVariable)
    summedWeighting <- dataCopy[, ..extract]
    summedWeighting <- unique(summedWeighting)
    summedWeighting[, SummedWeights := sum(get(weightingVariable), na.rm = TRUE), by = eval(aggregationVariables$nextResolution)]
    extract <- c(aggregationVariables$nextResolution, "SummedWeights")
    summedWeighting <- summedWeighting[, ..extract]
    summedWeighting <- unique(summedWeighting)
    
    summedWeightingBy <- aggregationVariables$nextResolution
    dataCopy <- merge(dataCopy, summedWeighting, by = summedWeightingBy, all = TRUE)
    
    dataCopy[, c(dataVariable) := sum(get(dataVariable) * get(weightingVariable), na.rm = TRUE) / SummedWeights, by = by]
    
    dataCopy[, c(weightingVariable) := SummedWeights]
    dataCopy[, SummedWeights := NULL]
    
    
    
    
    ### # Replace the SummedWeights:
    ### dataCopy[, c(summedWeightingVariable) := NULL]
    ### by <- aggregationVariables$nextResolution
    ### dataCopy <- merge(dataCopy, summedWeighting, by = by)
    
    #dataCopy[, c(summedWeightingVariable) := sum(get(summedWeightingVariable)), by = eval(aggregationVariables$nextResolution)]
    
    ## Calculate the sum divided by the numner of rows:
    #dataCopy[, c(dataVariable, weightingVariable) := list(
    #    sum(get(dataVariable) * get(weightingVariable)) / length(get(dataVariable)), 
    #    sum(get(weightingVariable))
    #), 
    #by = by]
    #
    #
    #dataCopy[, c(dataVariable, weightingVariable) := list(
    #    stats::weighted.mean(x = get(dataVariable), w = get(weightingVariable)), 
    #    sum(get(weightingVariable))
    #), by = by]
    
    # Set the resolution variables which were summed over to NA:
    set(
        dataCopy, 
        j = c(
            aggregationVariables$setToNA, 
            aggregationVariables$otherVariables
        ), 
        value=NA
    )
    
    
    # Remove duplicated rows:
    dataCopy <- subset(dataCopy, !duplicated(dataCopy[, ..by]))
    
    # Order by 'by':
    setorderv(dataCopy, cols = by, order = 1L, na.last = TRUE)
    
    dataCopy
}

sumData <- function(data, dataType, targetResolution = "Layer") {
    
    # Make a copy of the input, since we are summing and setting values by reference:
    dataCopy = data.table::copy(data)
    
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
        value=NA
    )
    
    # Remove duplicated rows:
    dataCopy <- subset(dataCopy, !duplicated(dataCopy[, ..by]))
    
    dataCopy
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


keepOnlyRelevantColumns <- function(data, dataType) {
    allDataTypeVariables <- getAllDataTypeVariables(dataType, unlist = TRUE)
    toRemove <- setdiff(names(data), allDataTypeVariables)
    data[, (toRemove) := NULL]
    setcolorder(data, allDataTypeVariables)
}



JavaJEXL2R <- function(x, eval=TRUE){
    
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
    for(i in seq_len(nrow(pattern_replacement))){
        x <- gsub(pattern_replacement[i,1], pattern_replacement[i,2], x)
    }
    
    # Return the evaluated exprexsion:
    if(eval){
        x <- eval(parse(text = x))
    }
    
    return(x)
}
