
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



# Function to get the common intervals of possibly overlapping intervals:
getCommonIntervals <- function(data, varMin = NULL, varMax = NULL, lowerName = NULL, upperName = NULL) {
    
    # Function to get the first common interval of a table:
    getOneCommonInterval <- function(data) {
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
        thisInterval <- getOneCommonInterval(data)
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
    dataType <- detectDataType(data, only.data = TRUE)
    data <- setColumnOrder(data, dataType = dataType, keep.all = TRUE)
    
    return(data)
}

# Function to add Layer:
addLayerDefinition <- function(data, layerDefinition = NULL, ...) {
    
    # Insert the Layer column from the layerDefinition input, and otherwise by NAs:
    if(length(layerDefinition)) {
        layerData <- findLayer(data = data, layerDefinition = layerDefinition, varMin = "MinHaulDepth", varMax = "MaxHaulDepth")
        
        data <- data.table::data.table(layerData, data)
    }
    else {
        data.table::setDT(data)
        toAdd <- c("Layer", "MinLayerRange", "MaxLayerRange")
        data.table::set(data, j = toAdd, value=NA)
    }
    
    # Set the order of the columns:
    dataType <- detectDataType(data, only.data = TRUE)
    data <- setColumnOrder(data, dataType = dataType, keep.all = TRUE)
    
    return(data)
}


