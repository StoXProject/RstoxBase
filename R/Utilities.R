
# Function to get the least common multiple of a vector of integers:
getLeastCommonMultiple <- function(x, max = NULL, N = 100) {
    multiples <- lapply(x, function(y) y * seq_len(N))
    commonMultiples <- multiples[[1]]
    for(i in seq_len(length(l) - 1)) {
        commonMultiples <- intersect(commonMultiples, multiples[[i + 1]])
    }
    
    if(length(max)) {
        subset(commonMultiples, commonMultiples <= max)
    }
    else {
        min(commonMultiples)
    }
    
}

# Function to get possible breaks of 
getPossibleBeaksOfRange <- function(StoxAcousticData) {
    numberOfEDSUs <- nrow(StoxAcousticData$Log)
    # Accept only the ranges occurring 'numberOfEDSUs' times:
    tableOfMinRange <- table(StoxAcousticData$NASC$MinRange)
    tableOfMaxRange <- table(StoxAcousticData$NASC$MaxRange)
    possibleMinRanges <- as.numeric(names(tableOfMinRange))[tableOfMinRange == numberOfEDSUs]
    possibleMaxRanges <- as.numeric(names(tableOfMaxRange))[tableOfMaxRange == numberOfEDSUs]
    # Include also the minimum and maximum range:
    minMinRange <- min(StoxAcousticData$NASC$MinRange)
    maxMaxRange <- max(StoxAcousticData$NASC$MaxRange)
    possibleBreaks <- sort(
        unique(
            minMinRange, 
            possibleMinRanges, 
            possibleMaxRanges, 
            maxMaxRange
        )
    )
    
    possibleBreaks
}