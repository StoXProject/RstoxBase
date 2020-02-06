##################################################
##################################################
#' Length distribution
#' 
#' This function calculates length frequency distribution per Stratum, swept-area PSU, swept-area layer, SpeciesCategory and length group defined by the combination of IndividualTotalLengthCentimeter and LengthResolutionCentimeter.
#' 
#' @inheritParams DefineSweptAreaPSU
#' @param SweptAreaPSU		A list of \code{\link{SweptAreaPSU}} data.
#' @param SweptAreaLayer	A list of \code{\link{SweptAreaLayer}} data.
#' @param LengthDistType	The type of length distribution to use, one of "LengthDist", "NormLengthDist" and "PercentLengthDist" (see 'Details').
#' @param RaisingFactorPriority A character string naming the variable to prioritise when generating raising factors for summing length distributions from different (sub)samples of one SpeciesCategory of a Haul, one of "Weight" and "Count".
#'
#' @details *********
#' 
#' @return
#' A \code{\link{LengthDistributionData}} object.
#' 
#' @examples
#' 
#' @seealso 
#' 
#' @export
#' @import data.table
#' 
LengthDistribution <- function(
    StoxBioticData, 
    SweptAreaPSU = NULL, 
    SweptAreaLayer = NULL, 
    LengthDistributionType = c("NormalizedLengthDistribution", "LengthDistribution", "PercentLengthDistribution"), 
    RaisingFactorPriority = c("Weight", "Count"), 
    acceptNA = TRUE
    # allowMissingWeight = TRUE
) {
    
    ####################################
    ##### 0. Initial preparations: #####
    ####################################
    # Get the DefinitionMethod:
    LengthDistributionType <- match.arg(LengthDistributionType)
    # Get the DefinitionMethod:
    RaisingFactorPriority <- match.arg(RaisingFactorPriority)
    
    # Get specification of the data type:
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")$LengthDistributionData
    ####################################
    
    
    ############################
    ##### 1. Merge levels: #####
    ############################
    StoxBioticDataMerged <- RstoxData::mergeDataTables(StoxBioticData, output.only.last = TRUE)
    ############################
    
    
    ####################################################
    ##### 2. Get the count in each length group: #######
    ####################################################
    keys <- c(
        getStoxBioticKeys(setdiff(names(StoxBioticData), "Individual")), 
        # The length group is defined as the combination of IndividualTotalLengthCentimeter and LengthResolutionCentimeter. See 'dataTypeDefinition' in initiateRstoxBase(): 
        dataTypeDefinition$categoryVariable, 
        dataTypeDefinition$groupingVariables
    )
    # Declare the variables used below:
    .N <- NULL
    LengthDistributionData <- StoxBioticDataMerged[, WeightedCount := as.double(.N), by = keys]
    LengthDistributionData <- subset(LengthDistributionData, !duplicated(LengthDistributionData[, ..keys]))
    ####################################################
    
    
    ###################################################################
    ##### 3. Add the weights depending on LengthDistributionType: #####
    ###################################################################
    # LengthDistributionType "NormalizedLengthDistribution" implies to normalize by the EffectiveTowedDistance, rendering the effective EffectiveTowedDistance as 1:
    if(LengthDistributionType == "NormalizedLengthDistribution") {
        LengthDistributionData$LengthDistributionWeight <- LengthDistributionData$EffectiveTowedDistance
    }
    else if(LengthDistributionType == "LengthDistribution") {
        LengthDistributionData$LengthDistributionWeight <- 1
    }
    # For LengthDistributionType "PercentLengthDistribution" weights are not relevant, since length distributions are simply averaged, and are set to 1. This type is used in acoustic-trawl models, where the biotic station weighting is applied when averaging the length distributions within each biotic station assignment:
    else if(LengthDistributionType == "PercentLengthDistribution") {
        LengthDistributionData$LengthDistributionWeight <- 1
    }
    else {
        stop("Invalid LengthDistributionType. See ?RstoxBase::LengthDistributionType")
    }
    ###################################################################
    
    
    ######################################################
    ##### 4. Add horizontal and vertical resolution: #####
    ######################################################
    # Order the length distribution data:
    #data.table::setorder(LengthDistributionData)
    
    # Insert the Stratum and PSU column by the SweptAreaPSU input, and otherwise by NAs:
    LengthDistributionData <- addPSUDefinition(LengthDistributionData, dataType = "LengthDistributionData", PSUDefinition = SweptAreaPSU)
    
    # Insert the Layer column by the SweptAreaLayer input, and otherwise by NAs:
    LengthDistributionData <- addLayerDefinition(LengthDistributionData, dataType = "LengthDistributionData", layerDefinition = SweptAreaLayer)
    ######################################################
    
    
    #######################################################
    ##### 5. Aggregate multiple samples to each haul: #####
    #######################################################
    # Create a data table of different raising factors in the columns:
    raisingFactorTable <- data.frame(
        Weight = LengthDistributionData$CatchFractionWeightKilogram / LengthDistributionData$SampleWeightKilogram, 
        Count = LengthDistributionData$CatchFractionCount / LengthDistributionData$SampleCount, 
        Percent = 1
    )
    
    # Apply the parameter RaisingFactorPriority:
    colorder <- names(raisingFactorTable)
    colorder <- c(RaisingFactorPriority, setdiff(colorder, RaisingFactorPriority))
    raisingFactorTable <- raisingFactorTable[, colorder]
    
    # Get the raisingFactor:
    getIndexOfFirstNonNA <- function(x, LengthDistributionType) {
        n <- if(LengthDistributionType == "PercentLengthDistribution") 3 else 2
        min(n, which(!is.na(x)))
    }
    raisingFactorIndex <- apply(raisingFactorTable, 1, getIndexOfFirstNonNA, LengthDistributionType = LengthDistributionType)
    LengthDistributionData$raisingFactor <- raisingFactorTable[cbind(seq_along(raisingFactorIndex), raisingFactorIndex)]
    
    # Apply the raising factor and sum over samples:
    keysSansSample <- setdiff(keys, getStoxBioticKeys("Sample"))
    LengthDistributionData <- LengthDistributionData[, WeightedCount := sum(WeightedCount * raisingFactor), by = keysSansSample]
    LengthDistributionData <- subset(LengthDistributionData, !duplicated(LengthDistributionData[, ..keysSansSample]))
    #######################################################
    
    
    #####################################
    ##### 6. Divide by the weights: #####
    #####################################
    LengthDistributionData[, WeightedCount := WeightedCount / LengthDistributionWeight]
    # Add the LengthDistributionType to the LengthDistributionData:
    LengthDistributionData$LengthDistributionType <- LengthDistributionType
    #####################################
    
    
    ########################
    ##### 7. Clean up: #####
    ########################
    # Convert the PercentLengthDistribution to percent:
    if(LengthDistributionType == "PercentLengthDistribution") {
        LengthDistributionData <- RelativeLengthDistribution(LengthDistributionData)
    }
    
    # Extract only the relevant columns:
    LengthDistributionData <- setColumnOrder(LengthDistributionData, dataType = "LengthDistributionData", keep.all = FALSE)
    
    # Order the rows 
    orderBy <- unlist(dataTypeDefinition[c("horizontalResolution", "verticalResolution", "categoryVariable", "groupingVariables")])
    setorderv(LengthDistributionData, cols = orderBy)
    ########################
    
    
    return(LengthDistributionData)
}


##################################################
##################################################
#' Regroup length distribution to common intervals
#' 
#' This function aggregates the \code{WeightedCount} of the LengthDistributionData
#' 
#' @param LengthDistributionData The length distribution data.
#' @param LengthInterval Specifies the new length intervals, either given as a single numeric value representing the constant length interval widths, (starting from 0), or a vector of the interval breaks.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{LengthDistributionData}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
RegroupLengthDistribution <- function(
    LengthDistributionData, 
    LengthInterval
) {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    LengthDistributionDataCopy = data.table::copy(LengthDistributionData)
    
    # Get the minimum and maximum lower and upper length interval breaks:
    minLength <- min(LengthDistributionDataCopy$IndividualTotalLengthCentimeter, na.rm = TRUE)
    maxLength <- max(LengthDistributionDataCopy$IndividualTotalLengthCentimeter + LengthDistributionDataCopy$LengthResolutionCentimeter, na.rm = TRUE)
    
    # Create a vector of breaks, if not given in the input 'LengthInterval':
    if(length(LengthInterval) == 1) {
        # Convert to indices:
        minLengthIntervalIndexFrom0 <- floor(minLength / LengthInterval)
        # Add one intervavl if the ceiling and floor is equal, since rightmost.closed = FALSE in findInterval():
        maxLengthIntervalIndexFrom0 <- ceiling(maxLength / LengthInterval) + as.numeric(ceiling(maxLength / LengthInterval) == floor(maxLength / LengthInterval))
        # Create a vector of evenly spaced breaks:
        LengthInterval <- seq(minLengthIntervalIndexFrom0, maxLengthIntervalIndexFrom0) * LengthInterval
    }
    
    # Check that there are no existing length intervals that are inside one of the new intervals:
    # Get the possible intervals:
    lengthGroupMinMax <- unique(LengthDistributionDataCopy[, .(lengthIntervalMin = IndividualTotalLengthCentimeter, lengthIntervalMax = IndividualTotalLengthCentimeter + LengthResolutionCentimeter)])
    #possibleIntervals <- getCommonIntervals(data = lengthGroupMinMax)
    
    strictlyInside <- function(x, table, margin = 1e-6) {
        any(x - margin > table[, 1] & x + margin < table[, 2], na.rm=TRUE)
    }
    
    invalidIntervalBreaks <- sapply(LengthInterval, strictlyInside, lengthGroupMinMax)
    
    # Check whether any of the new interval limits are inside the possible intervals:
    if(any(invalidIntervalBreaks)) {
        at <- which(invalidIntervalBreaks)
        stop("The following intervals intersect partially with the possible intervals: ", paste(paste(LengthInterval[at], LengthInterval[at + 1], sep = " - "), collapse = ", "))
    }
    
    # Get the inteval widths, and replace LengthResolutionCentimeter with the appropriate widths:
    LengthIntervalWidths <- diff(LengthInterval)
    numIntervals <- length(LengthIntervalWidths)
    # Temporary add the index of the length intervals:
    LengthDistributionDataCopy[, intervalIndex := findInterval(IndividualTotalLengthCentimeter, ..LengthInterval)]
    
    # Issue a warning if the intervalIndex is NA (values outside of the LengthInterval):
    anyBelow <- any(LengthDistributionDataCopy$intervalIndex < 1, na.rm = TRUE)
    anyAbove <- any(LengthDistributionDataCopy$intervalIndex > numIntervals, na.rm = TRUE)
    if(any(anyBelow, anyAbove)) {
        warning("Not all individuals are inside the length intervals defined by the input LengthInterval of RegroupLengthDistribution(). The range of the intervals must be <= ", minLength, " and > ", maxLength, " (all intervals, including the last interval are defined as open).")
    }
    
    # Replace with the new LengthResolutionCentimeter:
    LengthDistributionDataCopy[, LengthResolutionCentimeter := ..LengthIntervalWidths[intervalIndex]]
    # Replace IndividualTotalLengthCentimeter with the new lower interval breaks:
    LengthDistributionDataCopy[, IndividualTotalLengthCentimeter := ..LengthInterval[intervalIndex]]
    
    # Finally, aggregate the WeightedCount in the new length groups:
    # Extract the 'by' element:
    by <- getAllAggregationVariables(dataType="LengthDistributionData")
    LengthDistributionDataCopy[, WeightedCount := sum(WeightedCount), by = by]
    # Delete duplicated rows:
    LengthDistributionDataCopy <- unique(LengthDistributionDataCopy)
    
    # Remove the temporary intervalIndex:
    LengthDistributionDataCopy[, intervalIndex := NULL]
    
    return(LengthDistributionDataCopy)
}


##################################################
##################################################
#' Catchability of trawls by fish length
#' 
#' This function compensates for length dependent herding by the trawl doors into the net, or length dependent selectivity by the mesh size.
#' 
#' @inheritParams RegroupLengthDistribution
#' @param CompensationMethod Parameter descrption.
#' @param LengthDependentSweepWidthParameters A data.frame or data.table of parameters of the LengthDependentSweepWidth method, containing the columns SpeciesCategory, LMin, LMax, Alpha and Beta (see details).
#' @param LengthDependentSelectivityParameters A data.frame or data.table of parameters of the LengthDependentSelectivity method, containing the columns SpeciesCategory, LMax, Alpha and Beta (see details).
#' 
#' @return
#' A \code{\link{LengthDistributionData}} object.
#' 
#' @export
#' @import data.table
#' 
LengthDependentCatchCompensation <- function(
    LengthDistributionData, 
    CompensationMethod = c("LengthDependentSweepWidth", "LengthDependentSelectivity"), 
    LengthDependentSweepWidthParameters = NULL, 
    LengthDependentSelectivityParameters = NULL
) {
    # Get the catchability method:
    CompensationMethod <- match.arg(CompensationMethod)
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    LengthDistributionDataCopy = data.table::copy(LengthDistributionData)
    
    # Function to apply the length dependent sweep width function.
    #   w = w * 1852 / (Alpha * L^Beta), 
    # where 
    #   L = LMin if L < LMin 
    # and 
    #   L = LMax if L > LMax:
    applyLengthDependentSweepWidth <- function(WeightedCount, IndividualTotalLengthCentimeterMiddle, LMin, LMax, Alpha, Beta) {
        # Condition to ensure that the function is applied only on the appropriate rows, to avid coding error:
        if(any(is.na(LMin))) {
            stop("The function applyLengthDependentSweepWidth() cannot be applied on rows with missing LMin. Subset the rows before applying the function.")
        }
        
        # Set the lengths lower than LMin to LMin: 
        IndividualTotalLengthCentimeterMiddle <- pmax(IndividualTotalLengthCentimeterMiddle, LMin)
        
        # And the lengths larger than LMax to LMax: 
        IndividualTotalLengthCentimeterMiddle <- pmin(IndividualTotalLengthCentimeterMiddle, LMax)
        
        # Calculate the factor to multiply the WeightedCount:
        sweepWidth <- Alpha * IndividualTotalLengthCentimeterMiddle^Beta
        WeightedCount <- WeightedCount / sweepWidth
        
        return(WeightedCount)
    }
    
    # Function to apply the length dependent selectivity function.
    #   w = w * fact, 
    # where 
    #   fact = Alpha * exp(L * Beta)
    # and 
    #   fact = 1 if L > LMax:
    applyLengthDependentSelectivity <- function(WeightedCount, IndividualTotalLengthCentimeterMiddle, LMax, Alpha, Beta) {
        # Condition to ensure that the function is applied only on the appropriate rows, to avid coding error:
        if(any(is.na(LMax))) {
            stop("The function applyLengthDependentSweepWidth() cannot be applied on rows with missing LMax. Subset the rows before applying the function.")
        }
        
        # Calculate the factor to multiply the WeightedCount:
        fact <- Alpha * exp(IndividualTotalLengthCentimeterMiddle * Beta)
        # Set the factor to 1 outside of the range LMin to LMax. This is  questionable, and we do not turn on this functionality before this method is approved:
        stop("CatchabilityMethod = \"LengthDependentSelectivity\" is not yet supported.")
        fact[IndividualTotalLengthCentimeterMiddle > LMax] <- 1
        WeightedCount <- WeightedCount * fact
        
        return(WeightedCount)
    }
    
    # Run the appropriate method:
    if(CompensationMethod == "LengthDependentSweepWidth") {
        LengthDistributionDataCopy <- runLengthDependentCompensationFunction(
            data = LengthDistributionDataCopy, 
            compensationMethod = CompensationMethod, 
            compensationFunction = applyLengthDependentSweepWidth, 
            parametertable = LengthDependentSweepWidthParameters, 
            requiredParameters = c("LMin", "LMax", "Alpha", "Beta"), 
            groupingVariable = "SpeciesCategory"
        )
        
        # Finally, set the LengthDistributionType:
        LengthDistributionDataCopy[, LengthDistributionType := "SweepWidthCompensatedLengthDistribution"]
    }
    else if(CompensationMethod == "LengthDependentSelectivity") {
        LengthDistributionDataCopy <- runLengthDependentCompensationFunction(
            data = LengthDistributionDataCopy, 
            compensationMethod = CompensationMethod, 
            compensationFunction = applyLengthDependentSelectivity, 
            parametertable = LengthDependentSelectivityParameters, 
            requiredParameters = c("LMax", "Alpha", "Beta"), 
            groupingVariable = "SpeciesCategory"
        )
        
        # Finally, set the LengthDistributionType:
        LengthDistributionDataCopy[, LengthDistributionType := "SelectivityCompensatedLengthDistribution"]
    }
    
    return(LengthDistributionDataCopy)
}

# Function to run a length dependent compensation function, given its method name, parameter table, vector of required parameters and the specific grouping variable, which in all current cases is "SpeciesCategory":
# It is possible to simplify this function to only take the method as input, requiring that the function is named apply<methodname>, the parameter table is named <methodname>Parameters, and the function has the parameters WeightedCount and IndividualTotalLengthCentimeterMiddle followed by the required parameters (then R would determine the required parameters from the formals of the function). We should discuss whether to proceed with this strategy:
runLengthDependentCompensationFunction <- function(data, compensationMethod, compensationFunction, parametertable, requiredParameters, groupingVariable = "SpeciesCategory") {
    
    # Check that the parametertable is given:
    if(length(parametertable) == 0) {
        stop("The parameter table for ", compensationMethod, " must be given")
    }
    else if(!data.table::is.data.table(parametertable)) {
        parametertable <- data.table::as.data.table(parametertable)
    }
    
    # Check for presence of all required columns:
    allRequiredParameters <- c(groupingVariable, requiredParameters)
    if(!all(names(parametertable) %in% allRequiredParameters)) {
        stop("The parameter table for ", compensationMethod, " must contain the following columns: ", paste(allRequiredParameters, collapse = ", "))
    }
    
    # Check that none of the elements of the parameter table are NA:
    if(any(is.na(parametertable))) {
        stop("None og the elements of the parametertable can be missing")
    }
    
    
    # First add the columns LMin, LMax, Alpha, Beta and IndividualTotalLengthCentimeterMiddle:
    data <- data.table::data.table(
        data, 
        extractColumnsBy(
            values = data[[groupingVariable]], 
            table = parametertable, 
            refvar = groupingVariable, 
            vars = requiredParameters
        )
    )
    
    # Add also the mid point of each length interval:
    data[, IndividualTotalLengthCentimeterMiddle := IndividualTotalLengthCentimeter + LengthResolutionCentimeter / 2]
    
    # Apply the compensationFunction:
    valid <- !is.na(data[[requiredParameters[1]]])
    if(!all(valid)) {
        warning("Length dependent compensation was not applied to all species categories in the length distribution data")
    }
    functionInputColumns <- c("WeightedCount", "IndividualTotalLengthCentimeterMiddle", requiredParameters)
    data[valid, WeightedCount := do.call(compensationFunction, .SD), .SDcols = functionInputColumns]
    
    # Remove the temporary columns:
    data[, (requiredParameters) := vector("list", length(requiredParameters))]
    
    return(data)
}

# Function to extract the variables 'vars' from a data.table at the rows matching the input vector 'select' to the column 'refvar' of the table:
extractColumnsBy <- function(values, table, refvar, vars) {
    matchIndices <- match(values, table[, ..refvar])
    table[matchIndices, ..vars]
}


##################################################
##################################################
#' Relative length distribution
#' 
#' This function converts a length distribution to a relative length distribution as percent within each SpeciesCategory for the present horizontal and verticacl resolution.
#' 
#' @inheritParams RegroupLengthDistribution
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
RelativeLengthDistribution <- function(LengthDistributionData) {
    # Make a copy of the input, since we are averaging and setting values by reference:
    LengthDistributionDataCopy = data.table::copy(LengthDistributionData)
    
    # Get the columns to aggregate over, excluding the length groups (summing over these):
    by <- getAllAggregationVariables(dataType="LengthDistributionData", exclude.groupingVariables = TRUE)
    
    # Apply the division by the sum:
    LengthDistributionDataCopy[, WeightedCount := WeightedCount / sum(WeightedCount) * 100, by = by]
    
    return(LengthDistributionDataCopy)
}


##################################################
##################################################
#' (Weighted) Average length distribution horizontally
#' 
#' This function calculates average length distribution, weighted by the EffectiveTowedDistance for the case that LengthDistributionType = "NormalizedLengthDistribution".
#' 
#' @inheritParams RegroupLengthDistribution
#' @param TargetResolution The horizontal resolution of the output.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{LengthDistributionData}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @export
#' @import data.table
#' 
MeanLengthDistribution <- function(LengthDistributionData, TargetResolution = "PSU") {
    meanData(LengthDistributionData, dataType = "LengthDistributionData", targetResolution = TargetResolution)
}


##################################################
##################################################
#' Sum length distribution vertically
#' 
#' This function sums length distribution to swept area layers.
#' 
#' @inheritParams RegroupLengthDistribution
#' @param TargetResolution The vertical resolution of the output.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{LengthDistributionData}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @export
#' @import data.table
#' 
SumLengthDistribution <- function(LengthDistributionData, TargetResolution = "Layer") {
    sumData(LengthDistributionData, dataType = "LengthDistributionData", targetResolution = TargetResolution)
}


##################################################
##################################################
#' Length distribution assigned to each cell of acoustic PSU and Layer
#' 
#' This funciton calculates weighted average of the length distribution of hauls assigned to each acoustic PSU and Layer. The weights are set by \code{\link{BioticAssignmentWeighting}}.
#' 
#' @param LengthDistributionData    A list of \code{\link{LengthDistributionData}} data.
#' @param AcousticPSU           A list of \code{\link{AcousticPSU}} data.
#' @param BioticAssignment      A list of \code{\link{BioticAssignment}} data.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
AssignmentLengthDistribution <- function(LengthDistributionData, NASCData, BioticAssignment) {
    
    # Function to add assignment IDs:
    addAssignmentID <- function(BioticAssignment) {
        assignmentID <- as.numeric(factor(sapply(Assignment$Haul, paste, collapse = "_")))
        data.table::data.table(
            BioticAssignment, 
            assignmentID = assignmentID
        )
    }
    
    # Determine assignment IDs:
    BioticAssignment <- addAssignmentID(BioticAssignment)
    
    # Calculate weighted average length distribution for each assignment ID:
    
    
    
    
}


