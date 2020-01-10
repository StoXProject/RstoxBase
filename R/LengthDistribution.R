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
Catchability <- function() {
    # Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
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
RegroupLengthDistribution <- function(LengthDistributionData) {
    
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
    
    # Get the DefinitionMethod:
    LengthDistributionType <- match.arg(LengthDistributionType)
    # Get the DefinitionMethod:
    RaisingFactorPriority <- match.arg(RaisingFactorPriority)
    
    # Get specification of the data type:
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")$LengthDistribution
    
    # 1. Merge the Haul, SpeciesCategory, Sample and Individual level:
    StoxBioticDataMerged <- RstoxData::mergeDataTables(StoxBioticData, output.only.last = TRUE)
    
    # Get the count in each length group, defined as the combination of IndividualTotalLengthCentimeter and LengthResolutionCentimeter:
    keys <- c(
        #getStoxBioticKeys(c("Cruise", "Station", "Haul", "SpeciesCategory", "Sample")), 
        getStoxBioticKeys(setdiff(names(StoxBioticData), "Individual")), 
        #"IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"
        dataTypeDefinition$groupingVariables
    )
    
    # Declare the variables used below:
    .N <- NULL
    LengthDistributionData <- StoxBioticDataMerged[, WeightedCount := as.double(.N), by = keys]
    LengthDistributionData <- subset(LengthDistributionData, !duplicated(LengthDistributionData[, ..keys]))
    
    # Order the length distribution data:
    data.table::setorder(LengthDistributionData)
    
    # Insert the Stratum and PSU column by the SweptAreaPSU input, and otherwise by NAs:
    LengthDistributionData <- addPSUDefinition(LengthDistributionData, PSUDefinition = SweptAreaPSU)
    
    # Insert the Layer column by the SweptAreaLayer input, and otherwise by NAs:
    LengthDistributionData <- addLayerDefinition(LengthDistributionData, layerDefinition = SweptAreaLayer)
    
    
    # # Insert the Layer column by the SweptAreaLayer input, and otherwise by NAs:
    # if(length(SweptAreaLayer)) {
    #     LayerData <- findLayer(Data = SweptAreaLayer, Layer = SweptAreaLayer, varMin = "MinHaulDepth", varMax = "Max# HaulDepth")
    #     
    #     LengthDistributionData <- data.table::data.table(LayerData, LengthDistributionData)
    # }
    # else {
    #     LengthDistributionData <- data.table::data.table(
    #         Layer = NA, 
    #         MinLayerRange = NA, 
    #         MaxLayerRange = NA, 
    #         LengthDistributionData
    #     )
    # }
    
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
    
    
    # Add the weights depending on LengthDistributionType:
    # LengthDistributionType "NormalizedLengthDistribution" implies to normalize by the TowedDistance, rendering the effective TowedDistance as 1:
    if(LengthDistributionType == "NormalizedLengthDistribution") {
        LengthDistributionData$LengthDistributionWeight <- LengthDistributionData$TowedDistance
    }
    else if(LengthDistributionType == "LengthDistribution") {
        LengthDistributionData$LengthDistributionWeight <- 1
    }
    # For LengthDistributionType "PercentLengthDistribution" weights are not relevant, since length distributions are simply averaged, and are set to 1:
    else if(LengthDistributionType == "PercentLengthDistribution") {
        LengthDistributionData$LengthDistributionWeight <- 1
    }
    else {
        stop("Invalid LengthDistributionType. See ?RstoxBase::LengthDistributionType")
    }
    
    # Divide by the weights:
    LengthDistributionData[, WeightedCount := WeightedCount / LengthDistributionWeight]
    
    # Add the LengthDistributionType to the LengthDistributionData:
    LengthDistributionData$LengthDistributionType <- LengthDistributionType
    
    # Extract only the relevant columns:
    #relevantColums <- c(
    #    "Stratum", 
    #    "PSU", 
    #    "Station", 
    #    "Layer", 
    #    "Haul", 
    #    "SpeciesCategory", 
    #    "IndividualTotalLengthCentimeter", 
    #    "LengthResolutionCentimeter", 
    #    "WeightedCount", 
    #    "MinHaulDepth", 
    #    "MaxHaulDepth", 
    #    "MinLayerRange", 
    #    "MaxLayerRange", 
    #    "LengthDistributionWeight", 
    #    "TowedDistance", 
    #    "VerticalNetOpening", 
    #    "TrawlDoorSpread", 
    #    "LengthDistributionType"
    #)
    ###relevantColums <- unlist(dataTypeDefinition)
    ###LengthDistributionData <- LengthDistributionData[, ..relevantColums]
    ###data.table::setcolorder(LengthDistributionData, relevantColums)
    LengthDistributionData <- setColumnOrder(LengthDistributionData, dataType = "LengthDistribution", keep.all = FALSE)
    
    # Order the rows 
    #orderBy <- c("Stratum", "PSU", "Station", "Layer", "Haul", "SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter")
    orderBy <- unlist(dataTypeDefinition[c("horizontalResolution", "verticalResolution", "groupingVariables")])
    setorderv(LengthDistributionData, cols = orderBy)
    
    return(LengthDistributionData)
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



meanData <- function(data, TargetResolution = "PSU") {
    
    browser()
    aggregationVariables <- determineAggregationVariables(
        data = data, 
        TargetResolution = TargetResolution, 
        dimension = "horizontal"
    )
    # Extract the 'by' element:
    by <- aggregationVariables$by
    
    # Sum over the grouping variables:
    dataVariable <- aggregationVariables$dataVariable
    #LengthDistributionData[, WeightedCount := sum(WeightedCount), by = by]
    data[, c(dataVariable) := sum(get(dataVariable)), by = by]
    
    # Set the resolution variables which were summed over to NA:
    set(data, j = aggregationVariables$setToNA, value=NA)
    
    # Remove duplicated rows:
    data <- subset(data, !duplicated(data[, ..by]))
    
    data
    
}

