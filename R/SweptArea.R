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
LengthWeightRelationship <- function() {
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
RegroupLengthDist <- function() {
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
#' @param SweptAreaPSU		A list of \code{\link[roxygen2]{SweptAreaPSU}} data.
#' @param SweptAreaLayer		A list of \code{\link[roxygen2]{SweptAreaLayer}} data.
#' @param LengthDistType		The type of length distribution to use, one of "LengthDist", "NormLengthDist" and "PercentLengthDist" (see 'Details').
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
    RaisingFactorPriority = c("Weight", "Count")
    # allowMissingWeight = TRUE
    ) {
    
    # Get the DefinitionMethod:
    LengthDistributionType <- match.arg(LengthDistributionType)
    # Get the DefinitionMethod:
    RaisingFactorPriority <- match.arg(RaisingFactorPriority)
    
    # Temporary hack to assure unique keys:
    StoxBioticData <- lapply(StoxBioticData, unique)
    
    # 1. Merge the Haul, SpeciesCategory, Sample and Individual level:
    StoxBioticDataMerged <- RstoxData::mergeDataTables(StoxBioticData)$Individual
    
    # Get the count in each length group, defined as the combination of IndividualTotalLengthCentimeter and LengthResolutionCentimeter:
    keys <- c(
        getStoxBioticKeys(c("Cruise", "Station", "Haul", "SpeciesCategory", "Sample")), 
        "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter"
    )
    
    # Declare the variables used below:
    .N <- NULL
    LengthDistributionData <- StoxBioticDataMerged[, WeightedCount := as.double(.N), by = keys]
    LengthDistributionData <- subset(LengthDistributionData, !duplicated(LengthDistributionData[, ..keys]))
    #Station <- NULL
    #Haul <- NULL
    #SpeciesCategory <- NULL
    #TowedDistance <- NULL
    #MinHaulDepth <- NULL
    #MaxHaulDepth <- NULL
    #VerticalNetOpening <- NULL
    #HorizontalNetOpening <- NULL
    #TrawlDoorSpread <- NULL
    #CatchFractionWeightKilogram <- NULL
    #CatchFractionCount  <- NULL
    #SampleWeightKilogram <- NULL
    #SampleCount <- NULL
    
    
    # Count occurrences:
    #LengthDistributionData <- StoxBioticDataMerged[, .(
    #    WeightedCount = as.double(.N), 
    #    Station = head(Station, 1), 
    #    Haul = head(Haul, 1), 
    #    SpeciesCategory = head(SpeciesCategory, 1), 
    #    TowedDistance = head(TowedDistance, 1), 
    #    MinHaulDepth = head(MinHaulDepth, 1), 
    #    MaxHaulDepth = head(MaxHaulDepth, 1), 
    #    VerticalNetOpening = head(VerticalNetOpening, 1), 
    #    HorizontalNetOpening = head(HorizontalNetOpening, 1), 
    #    TrawlDoorSpread = head(TrawlDoorSpread, 1), 
    #    CatchFractionWeightKilogram = head(CatchFractionWeightKilogram, 1),
    #    CatchFractionCount = head(CatchFractionCount, 1),
    #    SampleWeightKilogram = head(SampleWeightKilogram, 1),
    #    SampleCount = head(SampleCount, 1)
    #), by = keys]
    
    # Order the length distribution data:
    data.table::setorder(LengthDistributionData)
    
    # Insert the Stratum and PSU column by the SweptAreaPSU input, and otherwise by NAs:
    if(length(SweptAreaPSU)) {
        LengthDistributionData <- merge(SweptAreaPSU, LengthDistributionData)
    }
    else {
        LengthDistributionData <- data.table::data.table(Stratum = NA, PSU = NA, LengthDistributionData)
    }
    
    # Insert the Layer column by the SweptAreaLayer input, and otherwise by NAs:
    if(length(SweptAreaLayer)) {
        LayerInd <- rowSums(
            outer(LengthDistributionData$MinHaulDepth, SweptAreaLayer$MinDepth, ">="), 
            outer(LengthDistributionData$MaxHaulDepth, SweptAreaLayer$MaxDepth, "<")
        )
        LengthDistributionData <- data.table::data.table(
            Layer = SweptAreaLayer$Layer[LayerInd], 
            MinLayerRange = SweptAreaLayer$MinDepth[LayerInd], 
            MaxLayerRange = SweptAreaLayer$MaxDepth[LayerInd], 
            LengthDistributionData
        )
    }
    else {
        LengthDistributionData <- data.table::data.table(
            Layer = NA, 
            MinLayerRange = NA, 
            MaxLayerRange = NA, 
            LengthDistributionData
        )
    }
    
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
        LengthDistributionData$LengthDistributionWeight <- 1
    }
    else if(LengthDistributionType == "LengthDistribution") {
        LengthDistributionData$LengthDistributionWeight <- LengthDistributionData$TowedDistance
    }
    # For LengthDistributionType "PercentLengthDistribution" weights are not relevant, since length distributions are simply averaged, and are set to 1:
    else if(LengthDistributionType == "PercentLengthDistribution") {
        LengthDistributionData$LengthDistributionWeight <- 1
    }
    else {
        stop("Invalid LengthDistributionType. See ?RstoxBase::LengthDistributionType")
    }
    
    # Extract only the relevant columns:
    relevantColums <- c(
        
    )
    LengthDistributionData <- LengthDistributionData[]
    
    LengthDistributionData
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
StationSpecCatDensity <- function() {
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
SweptAreaDensity <- function() {
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
TotalLengthDist <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


