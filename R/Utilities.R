
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
getStoxBioticKeys <- function(levels = NULL) {
    if(length(levels) == 0) {
        levels <- RstoxData::getStoxBioticLevels()
    }
    paste0(levels, "Key")
}

# Function to get Layer indices:
findLayer <- function(data, layerDefinition, varMin, varMax, acceptNA = TRUE) {
    
    layerRangeVector <- c(layerDefinition$MinLayerRange, tail(layerDefinition$MaxLayerRange, 1))
    indMin <- findInterval(data[[varMin]], layerRangeVector)
    indMax <- findInterval(data[[varMax]], layerRangeVector, rightmost.closed = TRUE)
    LayerInd <- pmax(indMin, indMax, na.rm = acceptNA)
    
    if(acceptNA && nrow(layerDefinition) == 1 && layerDefinition$MinLayerRange == 0 && layerDefinition$MaxLayerRange == Inf) {
        LayerInd <- replace(LayerInd, is.na(LayerInd), 1)
    }
    
    LayerData <- data.table::data.table(
        Layer = layerDefinition$Layer[LayerInd], 
        MinLayerRange = layerDefinition$MinLayerRange[LayerInd], 
        MaxLayerRange = layerDefinition$MaxLayerRange[LayerInd]
    )
    
    LayerData
}

# Function to add Stratum and PSU:
addPSUDefinition <- function(data, PSUDefinition = NULL, ...) {
    
    # If present, add the PSUDefinition to the start of the data
    if(length(PSUDefinition)) {
        # Merge first the PSUDefinition:
        PSUDefinition <- RstoxData::mergeDataTables(PSUDefinition, output.only.last = TRUE, ...)
        # Then merge the result with the data:
        by <- intersect(names(PSUDefinition), names(data))
        data <- merge(PSUDefinition, data, by = by, ...)
    }
    # Otherwise add columns of NAs (by reference, thus applying the setDT() first):
    else {
        data.table::setDT(data)
        toAdd <- c("Stratum", "PSU")
        data.table::set(data, j = toAdd, value=NA)
    }
    
    # Set the order of the columns:
    dataType <- detectDataType(data)
    data <- setColumnOrder(data, dataType = dataType, keep.all = TRUE)
    
    return(data)
}

# Function to add Layer:
addLayerDefinition <- function(data, layerDefinition = NULL, ...) {
    
    # Insert the Layer column from the layerDefinition input, and otherwise by NAs:
    if(length(layerDefinition)) {
        # Get the variables to aggregate by etc.:
        dataTypeDefinition <- getDataTypeDefinition(data = data)
        varMin <- dataTypeDefinition$verticalRawDimension[1]
        varMax <- dataTypeDefinition$verticalRawDimension[2]
        
        layerData <- findLayer(data = data, layerDefinition = layerDefinition, varMin = varMin, varMax = varMax)
        
        data <- data.table::data.table(layerData, data)
    }
    else {
        data.table::setDT(data)
        toAdd <- c("Layer", "MinLayerRange", "MaxLayerRange")
        data.table::set(data, j = toAdd, value=NA)
    }
    
    # Set the order of the columns:
    dataType <- detectDataType(data)
    data <- setColumnOrder(data, dataType = dataType, keep.all = TRUE)
    
    return(data)
}




# Stolen from https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector:
allEqual <- function(x, tol = .Machine$double.eps ^ 0.5) {
    if (length(x) == 1) return(TRUE)
    x <- range(x) / mean(x)
    isTRUE(all.equal(x[1], x[2], tolerance = tol))
}


meanData <- function(data, targetResolution = "PSU") {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    dataCopy = data.table::copy(data)
    
    # Get the variables to aggregate by etc.:
    aggregationVariables <- determineAggregationVariables(
        data = dataCopy, 
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
    
    #LengthDistributionData[, WeightedCount := sum(WeightedCount), by = by]
    dataCopy[, c(dataVariable) := weighted.mean(x = get(dataVariable), w = get(weightingVariable)), by = by]
    
    # Set the resolution variables which were summed over to NA:
    set(dataCopy, j = aggregationVariables$setToNA, value=NA)
    
    # Remove duplicated rows:
    dataCopy <- subset(dataCopy, !duplicated(dataCopy[, ..by]))
    
    dataCopy
}

sumData <- function(data, targetResolution = "Layer") {
    
    # Make a copy of the input, since we are summing and setting values by reference:
    dataCopy = data.table::copy(data)
    
    # Get the variables to aggregate by etc.:
    aggregationVariables <- determineAggregationVariables(
        data = dataCopy, 
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
    set(dataCopy, j = aggregationVariables$setToNA, value=NA)
    
    # Remove duplicated rows:
    dataCopy <- subset(dataCopy, !duplicated(dataCopy[, ..by]))
    
    dataCopy
}










buildExpression <- function(expression) {
    output <- paste(expression$columnName, expression$operator, if(is.character(expression$value)) deparse(paste(expression$value)) else paste(expression$value))
    if(expression$negate) {
        output <- paste("!(", output, ")")
    }
    
    output
}



recurseToDataTable <- function (L, fun) {
    if(inherits(L, "data.table")) {
        fun(L)
    }
    else if(is.list(L)){
        lapply(L, recurseToDataTable, fun)
    }
    else{
        L
    }
}

recurseToGroup <- function (L) {
    # If there are still groups, continue:
    if(is.list(L) && "group" %in% names(L)) {
        if(any(unlist(lapply(L[names(L) == "group"], names)) == "group")) {
            paste(
                "( ", 
                paste(
                    unlist( lapply(L[names(L) == "group"], recurseToGroup) ), 
                    collapse = paste0(
                        ") ", 
                        L$linkOperator, 
                        " ("
                    )
                ), 
                " )"
            )
        }
        # If there are no groups, execute:
        else if(all(unlist(lapply(L[names(L) == "group"], names) == "expression"))) {
            paste(
                "( ", 
                paste(unlist(L[names(L) == "group"]), collapse = paste0(") ", L$linkOperator, " (")), 
                " )"
            )
        }
    }
    else {
        L
    }
}



# Filter list structure:
l <- list(
    group = list(
        linkOperator = "|", 
        negate = FALSE,
        group = list(
            expression = data.table::data.table(
                negate = FALSE,
                columnName = "IndividualTotalLengthCentimeter", 
                operator = "%in%", 
                value = list(c(12,13,14))
            )
        ), 
        group = list(
            linkOperator = "|", 
            negate = FALSE,
            group = list(
                expression = data.table::data.table(
                    negate = FALSE,
                    columnName = "SpeciesCategoryKey", 
                    operator = "==", 
                    value = "sild'G03/161722.G03/126417/NA"
                )
            ), 
            group = list(
                linkOperator = "&", 
                negate = FALSE,
                group = list(
                    expression = data.table::data.table(
                        negate = FALSE,
                        columnName = "SpeciesCategoryKey", 
                        operator = "==", 
                        value = "torsk/164712/126436/NA"
                    )
                ), 
                group = list(
                    expression = data.table::data.table(
                        negate = TRUE,
                        columnName = "IndividualRoundWeightGram", 
                        operator = ">=", 
                        value = 200
                    )
                )
            ), 
            group = list(
                expression = data.table::data.table(
                    negate = FALSE,
                    columnName = "SpeciesCategoryKey", 
                    operator = "==", 
                    value = "lodde/162035/126735/NA"
                )
            )
        )
    )
)

r <- recurseToDataTable(l, buildExpression)
rr <- recurseToGroup(r)

