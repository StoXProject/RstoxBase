##################################################
##################################################
#' Catchability of trawls by fish length
#' 
#' This function compensates for length dependent herding by the trawl doors into the net, or length dependent selectivity by the mesh size.
#' 
#' @inheritParams RegroupLengthDistribution
#' @param CatchabilityMethod Parameter descrption.
#' @param LengthDependentSweepWidthParameters A data.frame or data.table of parameters of the LengthDependentSweepWidth method, containing the columns SpeciesCategory, LMin, LMax, Alpha and Beta (see details).
#' @param LengthDependentSelectivityParameters A data.frame or data.table of parameters of the LengthDependentSelectivity method, containing the columns SpeciesCategory, LMax, Alpha and Beta (see details).
#' 
#' @return
#' A \code{\link{LengthDistribution}} object.
#' 
#' @export
#' @import data.table
#' 
LengthDependentCatchCompensation <- function(LengthDistribution, CompensationMethod = c("LengthDependentSweepWidth", "LengthDependentSelectivity"), LengthDependentSweepWidthParameters = NULL, LengthDependentSelectivityParameters = NULL) {
    
    # Get the catchability method:
    CompensationMethod <- match.arg(CompensationMethod)
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    LengthDistributionCopy = data.table::copy(LengthDistribution)
    
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
        fact <- 1852 / (Alpha * IndividualTotalLengthCentimeterMiddle^Beta)
        WeightedCount <- WeightedCount * fact
        
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
        runLengthDependentCompensationFunction(
            data = LengthDistributionCopy, 
            compensationMethod = CompensationMethod, 
            compensationFunction = applyLengthDependentSweepWidth, 
            parametertable = LengthDependentSweepWidthParameters, 
            requiredParameters = c("LMin", "LMax", "Alpha", "Beta"), 
            groupingVariable = "SpeciesCategory"
        )
    }
    else if(CompensationMethod == "LengthDependentSelectivity") {
        runLengthDependentCompensationFunction(
            data = LengthDistributionCopy, 
            compensationMethod = CompensationMethod, 
            compensationFunction = applyLengthDependentSelectivity, 
            parametertable = LengthDependentSelectivityParameters, 
            requiredParameters = c("LMax", "Alpha", "Beta"), 
            groupingVariable = "SpeciesCategory"
        )
    }
    
    
    return(LengthDistributionCopy)
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
    data[, (allRequiredParameters) := vector("list", length(allRequiredParameters))]
    
    return(data)
}

# Function to extract the variables 'vars' from a data.table at the rows matching the input vector 'select' to the column 'refvar' of the table:
extractColumnsBy <- function(values, table, refvar, vars) {
    matchIndices <- match(values, table[, ..refvar])
    table[matchIndices, ..vars]
}


##################################################
##################################################
#' Regroup length distribution to common intervals
#' 
#' This function aggregates the \code{WeightedCount} of the LengthDistribution
#' 
#' @param LengthDistribution The length distribution data.
#' @param LengthInterval The new length intervals, either given as a vector of interval breaks, or a single numeric value, in which case a vector of intervavl breaks is created covering the range of the original length interval breaks.
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
RegroupLengthDistribution <- function(LengthDistribution, LengthInterval) {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    LengthDistributionCopy = data.table::copy(LengthDistribution)
    
    # Create a vector of breaks, if not given in the input 'LengthInterval':
    if(length(LengthInterval) == 1) {
        # Get the minimum and maximum lower and upper length interval breaks:
        minLength <- min(LengthDistribution$IndividualTotalLengthCentimeter, na.rm = TRUE)
        maxLength <- max(LengthDistribution$IndividualTotalLengthCentimeter + LengthDistribution$LengthResolutionCentimeter, na.rm = TRUE)
        # Convert to indices:
        minLengthIntervalIndexFrom0 <- floor(minLength / LengthInterval)
        maxLengthIntervalIndexFrom0 <- ceiling(maxLength / LengthInterval)
        # Create a vector of evenly spaced breaks:
        LengthInterval <- seq(minLengthIntervalIndexFrom0, maxLengthIntervalIndexFrom0) * LengthInterval
    }
    
    # Check that there are no existing length intervals that are inside one of the new intervals:
    # Get the possible intervals:
    lengthGroupMinMax <- unique(LengthDistributionCopy[, .(lengthIntervalMin = IndividualTotalLengthCentimeter, lengthIntervalMax = IndividualTotalLengthCentimeter + LengthResolutionCentimeter)])
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
    # Temporary add the index of the length intervals:
    LengthDistributionCopy[, intervalIndex := findInterval(IndividualTotalLengthCentimeter, ..LengthInterval)]
    
    # Replace with the new LengthResolutionCentimeter:
    LengthDistributionCopy[, LengthResolutionCentimeter := ..LengthIntervalWidths[intervalIndex]]
    # Replace IndividualTotalLengthCentimeter with the new lower interval breaks:
    LengthDistributionCopy[, IndividualTotalLengthCentimeter := ..LengthInterval[intervalIndex]]
    
    # Finally, aggregate the WeightedCount in the new length groups:
    # Extract the 'by' element:
    by <- getAllAggregationVariables(dataType="LengthDistribution")
    LengthDistributionCopy[, WeightedCount := sum(WeightedCount), by = by]
    # Delete duplicated rows:
    LengthDistributionCopy <- unique(LengthDistributionCopy)
    
    # Remove the temporary intervalIndex:
    LengthDistributionCopy[, intervalIndex := NULL]
    
    return(LengthDistributionCopy)
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
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
RelLengthDist <- function() {
    # Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


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
#' A data.table is returned with awesome stuff.
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
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")$LengthDistribution
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
        dataTypeDefinition$groupingVariables
    )
    # Declare the variables used below:
    .N <- NULL
    LengthDistribution <- StoxBioticDataMerged[, WeightedCount := as.double(.N), by = keys]
    LengthDistribution <- subset(LengthDistribution, !duplicated(LengthDistribution[, ..keys]))
    ####################################################
    
    ######################################################
    ##### 3. Add horizontal and vertical resolution: #####
    ######################################################
    # Order the length distribution data:
    #data.table::setorder(LengthDistribution)
    
    # Insert the Stratum and PSU column by the SweptAreaPSU input, and otherwise by NAs:
    LengthDistribution <- addPSUDefinition(LengthDistribution, PSUDefinition = SweptAreaPSU)
    
    # Insert the Layer column by the SweptAreaLayer input, and otherwise by NAs:
    LengthDistribution <- addLayerDefinition(LengthDistribution, layerDefinition = SweptAreaLayer)
    ######################################################
    
    
    #######################################################
    ##### 4. Aggregate multiple samples to each haul: #####
    #######################################################
    # Create a data table of different raising factors in the columns:
    raisingFactorTable <- data.frame(
        Weight = LengthDistribution$CatchFractionWeightKilogram / LengthDistribution$SampleWeightKilogram, 
        Count = LengthDistribution$CatchFractionCount / LengthDistribution$SampleCount, 
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
    LengthDistribution$raisingFactor <- raisingFactorTable[cbind(seq_along(raisingFactorIndex), raisingFactorIndex)]
    
    # Apply the raising factor and sum over samples:
    keysSansSample <- setdiff(keys, getStoxBioticKeys("Sample"))
    LengthDistribution <- LengthDistribution[, WeightedCount := sum(WeightedCount * raisingFactor), by = keysSansSample]
    LengthDistribution <- subset(LengthDistribution, !duplicated(LengthDistribution[, ..keysSansSample]))
    #######################################################
    
    
    ###################################################################
    ##### 5. Add the weights depending on LengthDistributionType: #####
    ###################################################################
    # LengthDistributionType "NormalizedLengthDistribution" implies to normalize by the TowedDistance, rendering the effective TowedDistance as 1:
    if(LengthDistributionType == "NormalizedLengthDistribution") {
        LengthDistribution$LengthDistributionWeight <- LengthDistribution$TowedDistance
    }
    else if(LengthDistributionType == "LengthDistribution") {
        LengthDistribution$LengthDistributionWeight <- 1
    }
    # For LengthDistributionType "PercentLengthDistribution" weights are not relevant, since length distributions are simply averaged, and are set to 1:
    else if(LengthDistributionType == "PercentLengthDistribution") {
        LengthDistribution$LengthDistributionWeight <- 1
    }
    else {
        stop("Invalid LengthDistributionType. See ?RstoxBase::LengthDistributionType")
    }
    
    # Divide by the weights:
    LengthDistribution[, WeightedCount := WeightedCount / LengthDistributionWeight]
    
    # Add the LengthDistributionType to the LengthDistribution:
    LengthDistribution$LengthDistributionType <- LengthDistributionType
    ###################################################################
    
    
    ########################
    ##### 6. Clean up: #####
    ########################
    # Extract only the relevant columns:
    LengthDistribution <- setColumnOrder(LengthDistribution, dataType = "LengthDistribution", keep.all = FALSE)
    
    # Order the rows 
    orderBy <- unlist(dataTypeDefinition[c("horizontalResolution", "verticalResolution", "groupingVariables")])
    setorderv(LengthDistribution, cols = orderBy)
    ########################
    
    
    return(LengthDistribution)
}

##################################################
##################################################
#' Length distribution assigned to each cell of acoustic PSU and Layer
#' 
#' This funciton calculates weighted average of the length distribution of hauls assigned to each acoustic PSU and Layer. The weights are set by \code{\link{BioticAssignmentWeighting}}.
#' 
#' @param LengthDistribution    A list of \code{\link{LengthDistribution}} data.
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
AssignmentLengthDistribution <- function(LengthDistribution, NASCData, BioticAssignment) {
    
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



meanData <- function(data, targetResolution = "PSU") {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    dataCopy = data.table::copy(data)
    
    # Check that the average can be made, that is that the vertical resolution is identical throughout each unit in the targetResolution:
    checkVerticalResolution <- function(data, targetResolution = "PSU") {
        
        # Get the variables to aggregate by etc.:
        aggregationVariables <- determineAggregationVariables(
            data = dataCopy, 
            targetResolution = targetResolution, 
            dimension = "horizontal"
        )
        
        
        aggregationVariables$verticalRawDimension
        
        
    }
    
    # Get the variables to aggregate by etc.:
    aggregationVariables <- determineAggregationVariables(
        data = dataCopy, 
        targetResolution = targetResolution, 
        dimension = "horizontal"
    )
    
    # Weighted average of the data variable over the grouping variables, weighted by the weighting variable:
    dataVariable <- aggregationVariables$dataVariable
    weightingVariable <- aggregationVariables$weightingVariable
    # Extract the 'by' element:
    by <- aggregationVariables$by
    #LengthDistribution[, WeightedCount := sum(WeightedCount), by = by]
    dataCopy[, c(dataVariable) := weighted.mean(x = get(dataVariable), w = get(weightingVariable)), by = by]
    
    # Set the resolution variables which were summed over to NA:
    set(dataCopy, j = aggregationVariables$setToNA, value=NA)
    
    # Remove duplicated rows:
    dataCopy <- subset(dataCopy, !duplicated(dataCopy[, ..by]))
    
    dataCopy
}

