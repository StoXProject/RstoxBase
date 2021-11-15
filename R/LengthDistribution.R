##################################################
##################################################
#' Length distribution
#' 
#' This function calculates length frequency distributions for each biotic Station and Haul by SpeciesCategory.
#' 
#' @inheritParams ModelData
#' @param LengthDistributionType The type of length distribution to use, one of "Standard", "Normalized" and "Percent" (see 'Details').
#' @param RaisingFactorPriority A character string naming the variable to prioritise when generating raising factors for summing length distributions from different (sub)samples of one SpeciesCategory of a Haul, one of "Weight" and "Count".
#'
#' @details 
#' The \emph{LengthDistribution} function produces length frequency distributions for each biotic Station and Haul by SpeciesCategory. A SpeciesCategory is usually a taxonomic species, but the categorization may follow other criteria.The catch of one SpeciesCategory is often split into one or more CatchFractions. If the catch of a species consists of several distinct size ranges, it is common to perform such splitting. For each CatchFraction, a CatchFractionWeight has been calculated and raised to Haul level. The sum of all CatchFractionWeights for a SpeciesCategory is therefore equal to the total catch weight of the trawl Haul. A CatchFractionCount is calculated in the same manner.
#' 
#' From each CatchFraction, a Sample is usually taken. Various types of individual characteristics or population parameters are measures. The most common parameters are individual length followed by weight. Other parameters are age, sex, maturity etc. The Sample weight and number is recorded. From the CatchFractionWeight and the SampleWeight, a raising factor (\emph{r}) is calculated as:
#' 
#' \deqn{r = \frac{CatchFractionWeight}{SampleWeight}}
#' 
#' alternatively, the raising factor r can be calculated as:
#' 
#' \deqn{r = \frac{CatchFractionCount}{SampleCount}}
#' 
#' A \emph{RaisingFactorPriority} parameter determines whether to make the first attempt on calculating the raising factor by weight or count variables. If the initial attempt fails due to lack of data, a new attempt is done using the alternative variables.
#' 
#' To produce a length frequency distribution for the Haul by SpeciesCategory, each Sample length distribution is first multiplied with the raising factor of the Sample. A total length distribution for the entire catch, is produced by adding the adjusted length distributions from all the Samples into one common length distribution for the SpeciesCategory in a Haul.
#' 
#' The Samples may have different length group intervals. If this is the case the intervals may overlap between Samples.The combination of length frequencies from all Samples of a SpeciesCategory can be expressed as:
#' 
#'   
#' \deqn{d_s = \displaystyle\sum_{i=1}^{n} r_{s,i} \times d_{s,i}}
#' 
#' where  
#' 
#'\eqn{d_s}    is the resultant length distribution for the station or Haul \eqn{s},
#' 
#'\eqn{d_{s,i}}  is the length distribution of Sample no \eqn{i} at Haul \eqn{s}, 
#' 
#'\eqn{r_{s,i}}   is a raising factor for Sample no \eqn{i} at Haul \eqn{s},
#' 
#'\eqn{n}     is the number of Samples
#' 
#' The LengthDistribution function can generate three different distributions types:
#' 
#'1) \strong{Standard}
#' 
#' A calculated length distribution as if every individual of the SpeciesCategory in the Haul had been length measured. This is mainly done as described above. A raising factor for each Sample will be attempted calculated using either CatchFractionWeight and SampleWeight or CatchFractionCount and SampleCount. If both attempts to calculate a raising factor for one or more Samples fail, no LengthDistribution can be created for the SpeciesCategory in this Haul.
#' 
#' 2) \strong{Normalized}
#' 
#' Normalized length distribution to one nautical mile towing distance. This distribution shows the length distribution as if the towing distance had been one nautical mile long and the entire catch had been length measured. The length distributions Standard is used together with the towing distance of the trawl station, to calculate this distribution. The number of fish from Standard in each length group is divided by the towing distance. It is worth noting that length distributions of type Normalied from several stations may be compared since they are independent of effort (towing distance). Each of the length distributions reflects the CPUE of the trawl hauls. They are in other words implicitly weighted by CPUE. If towing distance is lacking for a station, length distributions of type Normalized cannot be made.
#' 
#' 3) \strong{Percent}
#' 
#' Length distribution in percent. Length distributions of this type reflects the shape of the length distribution and contains therefore no implicit weighting.The calculation of percent length distributions is done as follows:
#'
#' A) If a SpeciesCategory at a station only have one Sample, the percent distribution is generated directly from the Sample length distribution. There is no need for knowing the raising factor.
#'
#' B) If a SpeciesCategory in a Hauls have more than one Sample, the percent distributions are generated by converting the Standard into percent distribution. This implies that distributions with more than one Sample and with missing raising factors will not be generated as no Standard distribution exist for these.
#'   
#'\strong{General comments on the function}
#'   
#' Note that some StoX models require one specific LengthDistributionType as output from the process and as input to other processes in the model.
#' 
#' 
#' @return
#' An object of StoX data type \code{\link{LengthDistributionData}}.
#' 
#' @export
#' 
LengthDistribution <- function(
    StoxBioticData, 
    LengthDistributionType = c("Normalized", "Standard", "Percent"), 
    RaisingFactorPriority = c("Weight", "Count")
) {
    
    ####################################
    ##### 0. Initial preparations: #####
    ####################################
    # Get the DefinitionMethod:
    LengthDistributionType <- match.arg(LengthDistributionType)
    # Get the DefinitionMethod:
    RaisingFactorPriority <- match.arg(RaisingFactorPriority)
    
    # Get specification of the data type:
    #dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")$LengthDistributionData
    dataTypeDefinition <- getDataTypeDefinition(dataType = "LengthDistributionData")
    ####################################
    
    
    ############################
    ##### 1. Merge levels: #####
    ############################
    StoxBioticDataMerged <- RstoxData::MergeStoxBiotic(StoxBioticData)
    
    # Delete empty sub samples e.g. if all individuals are filtered out. In this case a row will exist in StoxBioticDataMerged for this sample, thus appearing as an individual although it is not:
    # First count the individuals in sub samples:
    StoxBioticDataMerged[ , numberOfSubSamples := length(unique(SampleKey)), by = "Haul"]
    StoxBioticDataMerged[ , numberOfIndividuals := sum(!is.na(IndividualTotalLength)), by = "Sample"]
    # Then subset to only sub samples with individuals, that is delete rows from samples with sub samples, and where there are no non-missing lengths:
    StoxBioticDataMerged <- subset(StoxBioticDataMerged, !(numberOfSubSamples > 1 & numberOfIndividuals == 0))
    ############################
    
    ####################################################
    ##### 2. Get the count in each length group: #######
    ####################################################
    keys <- c(
        # Get all keys except the individual key (since we are supposed to count individuals). This includes unique hauls (CruiseKey, StationKey, HaulKey, SpeciesCategoryKey, SampleKey):
        #getStoxBioticKeys(setdiff(names(StoxBioticData), "Individual")), 
        RstoxData::getStoxKeys("StoxBiotic", level = "Individual", keys.out = "all.but.present"), 
        # Use SpeciesCategory as key (this is obsolete, but clarifies that length distributions are per species):
        dataTypeDefinition$categoryVariable, 
        # The length group is defined as the combination of IndividualTotalLength and LengthResolution. See 'dataTypeDefinition' in initiateRstoxBase(): 
        dataTypeDefinition$groupingVariables
    )
    # Declare the variables used below:
    LengthDistributionData <- StoxBioticDataMerged[, WeightedCount := as.double(.N), by = keys]
    if(!nrow(LengthDistributionData)) {
        warning("StoX: Empty Individual table.")
        return(LengthDistributionData)
    }
    
    # 2021-05-27: There does not seem to be any good reason for the following subset of NA keys. In fact it remoevs stations with zero fish, which biases the data!!!!!!!!!!!!!!:
    ## Remove rows with NA in 'keys' and subsequently remove duplicates:
    #LengthDistributionData <- subset(LengthDistributionData, rowSums(is.na(LengthDistributionData[, ..keys])) == 0)
    
    LengthDistributionData <- subset(LengthDistributionData, !duplicated(LengthDistributionData[, ..keys]))
    ####################################################
    
    ######################################################
    ##### 4. Add horizontal and vertical resolution: #####
    ######################################################
    # Order the length distribution data:
    #data.table::setorder(LengthDistributionData)
    
    # Add the weights, which are 1 for all types of length distributions (other more advanced weights may come later). Add this here, before adding the PSU definition, since this will lead to NA for strata with no PSUs:
    LengthDistributionData$LengthDistributionWeight <- 1
    
    # Insert the Stratum and PSU column by the BioticPSU input, and otherwise by NAs:
    #LengthDistributionData <- addPSUProcessData(LengthDistributionData, PSUProcessData = if(IncludePSU) BioticPSU, all = TRUE)
    
    # Insert the Layer column by the BioticLayer input, and otherwise by NAs:
    #LengthDistributionData <- addLayerProcessData(LengthDistributionData, dataType = "LengthDistributionData", layerProcessData = if(IncludeLayer) BioticLayer)
    ######################################################
    
    
    #######################################################
    ##### 5. Aggregate multiple samples to each haul: #####
    #######################################################
    # Create a data table of different raising factors in the columns:
    raisingFactorTable <- data.frame(
        Weight = LengthDistributionData$CatchFractionWeight / LengthDistributionData$SampleWeight, 
        Count = LengthDistributionData$CatchFractionCount / LengthDistributionData$SampleCount, 
        Percent = 1
    )
    
    # Apply the parameter RaisingFactorPriority:
    colorder <- names(raisingFactorTable)
    colorder <- c(RaisingFactorPriority, setdiff(colorder, RaisingFactorPriority))
    raisingFactorTable <- raisingFactorTable[, colorder]
    
    # Get the raisingFactor:
    getIndexOfFirstNonNA <- function(x, LengthDistributionType) {
        n <- if(LengthDistributionType == "Percent") 3 else 2
        min(n, which(!is.na(x)))
    }
    raisingFactorIndex <- apply(raisingFactorTable, 1, getIndexOfFirstNonNA, LengthDistributionType = LengthDistributionType)
    LengthDistributionData$raisingFactor <- raisingFactorTable[cbind(seq_along(raisingFactorIndex), raisingFactorIndex)]
    
    # Apply the raising factor and sum over samples:
    #keysSansSample <- setdiff(keys, getStoxBioticKeys("Sample"))
    SampleKey <- RstoxData::getStoxKeys("StoxBiotic", level = "Sample", keys.out = "only.present")
    keysSansSample <- setdiff(keys, SampleKey)
    #keysSansSample <- RstoxData::getStoxKeys("StoxBiotic", level = "Sample", keys.out = "all.but.present")
    
    LengthDistributionData <- LengthDistributionData[, WeightedCount := sum(WeightedCount * raisingFactor), by = keysSansSample]
    LengthDistributionData <- subset(LengthDistributionData, !duplicated(LengthDistributionData[, ..keysSansSample]))
    #######################################################
    
    
    #########################################################################################
    ##### 6. Divide by the effective towed distance for normalized length distribution: #####
    #########################################################################################
    if(LengthDistributionType == "Normalized") {
        atEffectiveTowDistance0 <- which(LengthDistributionData[, EffectiveTowDistance == 0])
        if(length(atEffectiveTowDistance0)) {
            warning("StoX: The following Hauls have EffectiveTowDistance = 0, which causes WeightedCount = Inf. This may result in loss of data at a later stage, e.g. in SplitNASC:\n", paste("\t", unique(LengthDistributionData$Haul[atEffectiveTowDistance0]), collapse = "\n"))
        }
        LengthDistributionData[, WeightedCount := WeightedCount / EffectiveTowDistance]
    }
    
    # Add the LengthDistributionType to the LengthDistributionData:
    LengthDistributionData[, LengthDistributionType := ..LengthDistributionType]
    #LengthDistributionData$LengthDistributionType <- LengthDistributionType
    #####################################
    
    
    ########################
    ##### 7. Clean up: #####
    ########################
    # Convert the PercentLengthDistribution to percent:
    if(LengthDistributionType == "Percent") {
        LengthDistributionData <- RelativeLengthDistribution(LengthDistributionData)
    }
    
    # Format the output:
    formatOutput(LengthDistributionData, dataType = "LengthDistributionData", keep.all = FALSE)
    
    ## Order the rows:
    #orderDataByReference(LengthDistributionData, "LengthDistributionData")
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(LengthDistributionData)
    ########################
    
    
    return(LengthDistributionData)
}


##################################################
##################################################
#' Regroup length distribution to common intervals
#' 
#' The RegroupLengthDistribution function is used to set a common length group resolution for one or all SpeciesCategories throughout the output length distribution dataset. The function aggregates the \code{WeightedCount} of the LengthDistributionData
#' 
#' @inheritParams ModelData
#' @param LengthInterval Specifies the new length intervals, either given as a single numeric value representing the constant length interval widths, (starting from 0), or a vector of the interval breaks.
#' 
#' @details
#' NOTE. Function parameter \emph{SpeciesCategory} is NOT YET IMPLEMENTED. Consequently, a common LengthInterval is applied to all SpeciesCategories in the input data.
#' 
#' The RegroupLengthDistribution function is used to set a common length group resolution for one or all SpeciesCategories throughout the length distributions in the output dataset and across all Hauls.A function parameter \emph{SpeciesCategory} is used to choose either All or one SpeciesCategory from the input LengthDistribution data set. A dropdown list of available SpeciesCategories in the input data is available for the selection. The function parameter \emph{LengthInterval} is used to set the desired output length interval in centimeters.  The new length interval can never have finer resolution than the coarsest resolution found in the data that are due to be regrouped. A least common multiple of the length group intervals in the input is calculated. This value is the highest resolution possible. If the user chooses a finer resolution than this for the parameter LengthInterval, a warning will be given, and the least common multiple will be applied for the output LengthDistribution dataset from the process.
#' 
#' @return
#' An object of StoX data type \code{\link{LengthDistributionData}}.
#' 
#' @export
#' 
RegroupLengthDistribution <- function(
    LengthDistributionData, 
    LengthInterval = numeric()
) {
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    LengthDistributionDataCopy = data.table::copy(LengthDistributionData)
    
    # Regroup the length intervals:
    LengthDistributionDataCopy <- RegroupLengthData(
        LengthDistributionDataCopy, 
        lengthInterval = LengthInterval
    )
    
    # Aggregate the WeightedCount in the new length groups:
    # Extract the 'by' element:
    by <- getAllAggregationVariables(dataType="LengthDistributionData")
    LengthDistributionDataCopy[, WeightedCount := sum(WeightedCount), by = by]
    # Delete duplicated rows:
    LengthDistributionDataCopy <- unique(LengthDistributionDataCopy)
    
    return(LengthDistributionDataCopy)
}

RegroupLengthData<- function(
    LengthData, 
    lengthInterval = numeric()
) {
    
    # Get the new length intervals:
    lengthInterval <- getLengthInterval(LengthData, lengthInterval = lengthInterval)
    
    # Temporary add the index of the length intervals:
    LengthData[, intervalIndex := findInterval(IndividualTotalLength, ..lengthInterval)]
    
    # Get the interval widths, and count the number of hits in each interval:
    lengthIntervalWidths <- diff(lengthInterval)
    
    # Replace with the new LengthResolution:
    LengthData[, LengthResolution := ..lengthIntervalWidths[intervalIndex]]
    # Replace IndividualTotalLength with the new lower interval breaks:
    LengthData[, IndividualTotalLength := ..lengthInterval[intervalIndex]]
    
    # Remove the temporary intervalIndex:
    LengthData[, intervalIndex := NULL]
    
    return(LengthData)
}


getLengthInterval <- function(LengthData, lengthInterval = numeric()) {
    
    # Get the minimum and maximum lower and upper length interval breaks:
    if(all(is.na(LengthData$IndividualTotalLength))) {
        stop("IndividualTotalLength is all NA in ", deparse(substitute(LengthDistributionData)))
    }
    minLength <- min(LengthData$IndividualTotalLength, na.rm = TRUE)
    maxLength <- max(LengthData$IndividualTotalLength + LengthData$LengthResolution, na.rm = TRUE)
    
    # Create a vector of breaks, if not given in the input 'lengthInterval':
    if(length(lengthInterval) == 1) {
        # Convert to indices:
        minLengthIntervalIndexFrom0 <- floor(minLength / lengthInterval)
        # Add one interval if the ceiling and floor is equal, since rightmost.closed = FALSE in findInterval():
        maxLengthIntervalIndexFrom0 <- ceiling(maxLength / lengthInterval) + as.numeric(ceiling(maxLength / lengthInterval) == floor(maxLength / lengthInterval))
        
        lengthInterval <- seq(minLengthIntervalIndexFrom0, maxLengthIntervalIndexFrom0) * lengthInterval
    }
    else {
        stop("The function parameter lengthInterval must be set as a single numeric value")
    }
    
    # Check that there are no new intervals that are inside one of the existing length intervals:
    # Get the possible intervals:
    lengthGroupMinMax <- unique(LengthData[, .(lengthIntervalMin = IndividualTotalLength, lengthIntervalMax = IndividualTotalLength + LengthResolution)])
    #possibleIntervals <- getCommonIntervals(data = lengthGroupMinMax)
    
    invalidIntervalBreaks <- lapply(lengthInterval, strictlyInside, lengthGroupMinMax)
    atInvalidIntervalBreaks <- lengths(invalidIntervalBreaks) > 0
    
    # Check whether any of the new interval limits are inside the possible intervals:
    if(any(atInvalidIntervalBreaks)) {
        at <- which(atInvalidIntervalBreaks)
        stop("The following intervals intersect partially with the possible intervals. Change the regroup intervals or search for possible errors in the data: \n", 
             paste(
                 paste0(
                     "Regroup interval ", 
                     paste(
                         lengthInterval[at], 
                         lengthInterval[at + 1], 
                         sep = " - "
                     ), 
                     " intersecting partially with possible intervals ", 
                     sapply(invalidIntervalBreaks[at], function(this) paste(lengthGroupMinMax[this, do.call(paste, c(.SD, sep = "-"))], collapse = ", "))
                 ), 
                 collapse = "\n"
             ), 
             "."
        )
    }
    
    return(lengthInterval)
}


strictlyInside <- function(x, table, margin = 1e-6) {
    which(x - margin > table[, 1] & x + margin < table[, 2])
}


##################################################
##################################################
#' Apply the sweep of different gear (and cruise)
#' 
#' This function multiplies the WeightedCount of a LengthDistributionData by the sweep width given by \code{CompensationTable}. The result is a sweep width compensated length distribution (LengthDistributionType starting with "SweepWidthCompensated").
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param CompensationMethod The method to use for the length dependent catch compensation, i.e. specifying which columns to provide the sweep width for.
#' @param CompensationTable A table of the sweep width per combination of the variables specified in \code{CompensationMethod}. Note that all combinations present in the data must be given in the table, as the output should be sweep width compensated for all rows with non-missing WeightedCount.
#' 
#' @return
#' A \code{\link{LengthDistributionData}} object.
#' 
#' @export
#' 
GearDependentCatchCompensation <- function(
    LengthDistributionData, 
    CompensationMethod = c("Gear", "Cruise", "GearAndCruise"), 
    CompensationTable = data.table::data.table()
) {
    
    # Sweep width compensation cannot be performed on olready sweep width compensated LengthDistributionData:
    if(startsWith(firstNonNA(LengthDistributionData$LengthDistributionType), "SweepWidthCompensated")) {
        stop("The LengthDistributionData are already sweep width compensated (LengthDistributionType starting with \"SweepWidthCompensated\")")
    }
    
    # Get the catchability method:
    CompensationMethod <- match.arg(CompensationMethod)
    # Split by "And":
    CompensationMethod <- strsplit(CompensationMethod, "And")[[1]]
    
    # Accept only the columns "SweepWidth" and those given by CompensationMethod:
    acceptedColumns <- c("SweepWidth", CompensationMethod)
    if(!all(acceptedColumns %in% names(CompensationTable))) {
        stop("The CompensationTable must contain the columns ", paste(acceptedColumns, collapse = ", "))
    }
    CompensationTable <- CompensationTable[, ..acceptedColumns]
    
    # Check that all combinations in the LengthDistributionData of the variablas specified by CompensationMethod are present in CompensationTable:
    #uniqueCombinationsInLengthDistributionData <- unique(LengthDistributionData[, ..CompensationMethod])
    #uniqueCombinationsInCompensationTable <- unique(CompensationTable[, ..CompensationMethod])
    #if(!all(uniqueCombinationsInLengthDistributionData %in% uniqueCombinationsInCompensationTable)) {
    #    stop("All combinations of the variables ", paste(CompensationMethod, collapse = ", "), " that are present in the LengthDistributionData must be present also in the CompensationTable.")
    #}
    checkAllCombinations(
        LengthDistributionData = LengthDistributionData, 
        table = CompensationTable, 
        variables = CompensationMethod
    )
    
    # Merge the CompensationTable into the LengthDistributionData:
    LengthDistributionDataCopy <- data.table::copy(LengthDistributionData)
    # Use all.x = TRUE as we should keep all rows of the LengthDistributionData, but not necessaroily all rows of the CompensationTable:
    LengthDistributionDataCopy <- merge(LengthDistributionDataCopy, CompensationTable, by = CompensationMethod, all.x = TRUE)
    
    # Multiply by the sweep width in nautical miles, as normalizaion in the direcion of the vessel involves dividing by disance in nautical miles:
    LengthDistributionDataCopy[, WeightedCount := WeightedCount / (SweepWidth / getRstoxBaseDefinitions("nauticalMileInMeters"))]
    
    # Set the LengthDistributionType
    LengthDistributionDataCopy[, LengthDistributionType := paste0("SweepWidthCompensated", LengthDistributionType)]
    
    # Format the output:
    formatOutput(LengthDistributionDataCopy, dataType = "LengthDistributionData", keep.all = FALSE)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(LengthDistributionDataCopy)
    
    return(LengthDistributionDataCopy)
}


checkAllCombinations <- function(LengthDistributionData, table, variables) {
    # Ignore NAs:
    containNA <- rowSums(LengthDistributionData[, lapply(.SD, is.na), .SDcols =  variables])
    validRows <- containNA == 0
    # Check that all combinations in the LengthDistributionData of the variablas specified by variables are present in CompensationTable:
    uniqueCombinationsInLengthDistributionData <- unique(subset(LengthDistributionData, validRows)[, do.call(paste, .SD),.SDcols =  variables])
    uniqueCombinationsInTable <- unique(table[, do.call( paste, .SD),.SDcols =  variables])
    
    if(!all(uniqueCombinationsInLengthDistributionData %in% uniqueCombinationsInTable)) {
        stop("All combinations of the variables ", paste(variables, collapse = ", "), " that are present in the LengthDistributionData must be present also in the ", deparse(substitute(table)))
    }
}

##################################################
##################################################
#' Catchability of trawls by fish length
#' 
#' This function compensates for length dependent herding by the trawl doors into the net, or length dependent selectivity by the mesh size.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param CompensationMethod The method to use for the length dependent catch compensation, one of "LengthDependentSweepWidth" for adjusting the sweep width according to the fish length dependent herding effect quantified through the function alpha * length ^ beta; and "LengthDependentSelectivity" for compensating for mash size selectivity through the net using the function Alpha * e ^ (length * Beta).
#' @param LengthDependentSweepWidthParameters A table of parameters of the LengthDependentSweepWidth method, containing the columns SpeciesCategory, LMin, LMax, Alpha and Beta (see details).
#' @param LengthDependentSelectivityParameters A table of parameters of the LengthDependentSelectivity method, containing the columns SpeciesCategory, LMax, Alpha and Beta (see details).
#' 
#' @return
#' A \code{\link{LengthDistributionData}} object.
#' 
#' @export
#' 
LengthDependentCatchCompensation <- function(
    LengthDistributionData, 
    CompensationMethod = c("LengthDependentSweepWidth", "LengthDependentSelectivity"), 
    LengthDependentSweepWidthParameters = data.table::data.table(), 
    LengthDependentSelectivityParameters = data.table::data.table()
) {
    # Get the catchability method:
    CompensationMethod <- match.arg(CompensationMethod)
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    LengthDistributionDataCopy = data.table::copy(LengthDistributionData)
    
    # Run the appropriate method:
    if(CompensationMethod == "LengthDependentSweepWidth") {
        
        # Check that all combinations in the LengthDistributionData of the variablas specified by CompensationMethod are present in LengthDependentSweepWidthParameters:
        checkAllCombinations(
            LengthDistributionData = LengthDistributionData, 
            table = LengthDependentSweepWidthParameters, 
            variables = "SpeciesCategory"
        )
        
        # Sweep width compensation cannot be performed on olready sweep width compensated LengthDistributionData:
        if(startsWith(firstNonNA(LengthDistributionData$LengthDistributionType), "SweepWidthCompensated")) {
            stop("The LengthDistributionData are already sweep width compensated (LengthDistributionType starting with \"SweepWidthCompensated\")")
        }
        
        LengthDistributionDataCopy <- runLengthDependentCompensationFunction(
            data = LengthDistributionDataCopy, 
            compensationMethod = CompensationMethod, 
            compensationFunction = applyLengthDependentSweepWidth, 
            parametertable = LengthDependentSweepWidthParameters, 
            requiredParameters = c("LMin", "LMax", "Alpha", "Beta"), 
            groupingVariable = "SpeciesCategory"
        )
        
        # Finally, set the LengthDistributionType:
        LengthDistributionDataCopy[, LengthDistributionType := paste0("SweepWidthCompensated", LengthDistributionType)]
    }
    else if(CompensationMethod == "LengthDependentSelectivity") {
        
        # Check that all combinations in the LengthDistributionData of the variablas specified by CompensationMethod are present in LengthDependentSweepWidthParameters:
        checkAllCombinations(
            LengthDistributionData = LengthDistributionData, 
            table = LengthDependentSelectivityParameters, 
            variables = "SpeciesCategory"
        )
        
        LengthDistributionDataCopy <- runLengthDependentCompensationFunction(
            data = LengthDistributionDataCopy, 
            compensationMethod = CompensationMethod, 
            compensationFunction = applyLengthDependentSelectivity, 
            parametertable = LengthDependentSelectivityParameters, 
            requiredParameters = c("LMax", "Alpha", "Beta"), 
            groupingVariable = "SpeciesCategory"
        )
        
        # Change added on 2021-04-09:
        # Do not add SelectivityCompensated to LengthDistributionType:
        # Finally, set the LengthDistributionType:
        #LengthDistributionDataCopy[, LengthDistributionType := paste0("SelectivityCompensated", LengthDistributionType)]
    }
    
    # Format the output:
    formatOutput(LengthDistributionDataCopy, dataType = "LengthDistributionData", keep.all = FALSE)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(LengthDistributionDataCopy)
    
    return(LengthDistributionDataCopy)
}

# Function to apply the length dependent sweep width function.
#   w = w / 1852 / (Alpha * L^Beta), 
# where 
#   L = LMin if L < LMin 
# and 
#   L = LMax if L > LMax:
applyLengthDependentSweepWidth <- function(WeightedCount, IndividualTotalLengthMiddle, LMin, LMax, Alpha, Beta) {
    # Condition to ensure that the function is applied only on the appropriate rows, to avid coding error:
    if(any(is.na(LMin))) {
        stop("The function applyLengthDependentSweepWidth() cannot be applied on rows with missing LMin. Subset the rows before applying the function.")
    }
    
    # Set the lengths lower than LMin to LMin: 
    IndividualTotalLengthMiddle <- pmax(IndividualTotalLengthMiddle, LMin)
    
    # And the lengths larger than LMax to LMax: 
    IndividualTotalLengthMiddle <- pmin(IndividualTotalLengthMiddle, LMax)
    
    # Calculate the factor to multiply the WeightedCount by:
    sweepWidth <- Alpha * IndividualTotalLengthMiddle^Beta
    sweepWidthInNauticalMiles <- sweepWidth / getRstoxBaseDefinitions("nauticalMileInMeters")
    
    WeightedCount <- WeightedCount / sweepWidthInNauticalMiles
    
    return(WeightedCount)
}

# Function to apply the length dependent selectivity function.
#   w = w * fact, 
# where 
#   fact = Alpha * exp(L * Beta)
# and 
#   fact = 1 if L > LMax:
applyLengthDependentSelectivity <- function(WeightedCount, IndividualTotalLengthMiddle, LMax, Alpha, Beta) {
    # Condition to ensure that the function is applied only on the appropriate rows, to avid coding error:
    if(any(is.na(LMax))) {
        stop("The function applyLengthDependentSelectivity() cannot be applied on rows with missing LMax. Subset the rows before applying the function.")
    }
    
    # Calculate the factor to multiply the WeightedCount:
    fact <- Alpha * exp(IndividualTotalLengthMiddle * Beta)
    # Set the factor to 1 outside of the range LMin to LMax. This is  questionable, and we do not turn on this functionality before this method is approved:
    stop("CatchabilityMethod = \"LengthDependentSelectivity\" is not yet supported.")
    fact[IndividualTotalLengthMiddle > LMax] <- 1
    WeightedCount <- WeightedCount * fact
    
    return(WeightedCount)
}


# Function to run a length dependent compensation function, given its method name, parameter table, vector of required parameters and the specific grouping variable, which in all current cases is "SpeciesCategory":
# It is possible to simplify this function to only take the method as input, requiring that the function is named apply<methodname>, the parameter table is named <methodname>Parameters, and the function has the parameters WeightedCount and IndividualTotalLengthMiddle followed by the required parameters (then R would determine the required parameters from the formals of the function). We should discuss whether to proceed with this strategy:
runLengthDependentCompensationFunction <- function(data, compensationMethod, compensationFunction, parametertable, requiredParameters, groupingVariable = "SpeciesCategory") {
    
    
    # Check that the parametertable is given:
    if(!length(parametertable)) {
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
    
    # Check that all groupingVariable present in the data are present also in the  parametertable:
    notPresent <- stats::na.omit(setdiff(data[[groupingVariable]], parametertable[[groupingVariable]]))
    if(length(notPresent)) {
        warning("StoX: The following ", groupingVariable, " are not present in the parametertable. Length dependent compensation was not applied to these ", groupingVariable, ".")
    }
    
    
    # First add the columns LMin, LMax, Alpha, Beta and IndividualTotalLengthMiddle:
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
    #data[, IndividualTotalLengthMiddle := IndividualTotalLength + LengthResolution / 2]
    data[, IndividualTotalLengthMiddle := getMidIndividualTotalLength(.SD)]
    
    # Apply the compensationFunction:
    valid <- !is.na(data[[requiredParameters[1]]])
    #if(!all(valid)) {
    #    warning("StoX: Length dependent compensation was not applied to all species categories in the length distribution data")
    #}
    functionInputColumns <- c("WeightedCount", "IndividualTotalLengthMiddle", requiredParameters)
    data[valid, WeightedCount := do.call(compensationFunction, .SD), .SDcols = functionInputColumns]
    
    # Remove the temporary columns:
    data[, (requiredParameters) := vector("list", length(requiredParameters))]
    
    return(data)
}

# Function to extract the variables 'vars' from a data.table at the rows matching the input vector 'select' to the column 'refvar' of the table:
extractColumnsBy <- function(values, table, refvar, vars) {
    matchIndices <- match(values, unlist(table[, ..refvar]))
    table[matchIndices, ..vars]
}


##################################################
##################################################
#' Relative length distribution
#' 
#' This function converts a length distribution to a relative length distribution as percent within each SpeciesCategory for the present horizontal and vertical resolution.
#' 
#' @inheritParams ModelData
#' 
#' @details
#' This \emph{RelativeLengthDistribution} function converts a length distribution to a relative length distribution as percent within each SpeciesCategory for the present horizontal and vertical resolution. Depending on the \emph{LengthDistributionType} of the input LengthDistributionData, there may be small differences in this functions output data even if they originate from the same biotic dataset (Biotic.xml file). See function \code{\link{LengthDistribution}} for a detailed explanation on how different LengthDistributionTypes are created, leading to differences in the output from the RelativeLengthDistribution function.
#' 
#' @return
#' An object of StoX data type \code{\link{LengthDistributionData}}.
#' 
#' @export
#' 

RelativeLengthDistribution <- function(LengthDistributionData) {
    # Make a copy of the input, since we are averaging and setting values by reference:
    LengthDistributionDataCopy = data.table::copy(LengthDistributionData)
    
    # Get the columns to aggregate over, excluding the length groups (summing over these):
    by <- getAllAggregationVariables(dataType="LengthDistributionData", exclude.groupingVariables = TRUE)
    
    # Apply the division by the sum:
    LengthDistributionDataCopy[, WeightedCount := WeightedCount / sum(WeightedCount) * 100, by = by]
    LengthDistributionDataCopy[, LengthDistributionType := "Percent"]
    
    return(LengthDistributionDataCopy)
}


##################################################
##################################################
#' Sum length distribution vertically
#' 
#' This function summes LengthDistributionData data vertically.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param LayerDefinition The method to use for defining the Layers, one of \code{FunctionParameter} to define the Layers on the fly in this function, or \code{FunctionInput} to import Layer process data from a previously run process by \code{BioticLayer}.
#' @param LayerDefinitionMethod See \code{\link{DefineBioticLayer}}
#' @inheritParams DefineBioticLayer
#' 
#' @return
#' An \code{\link{SumLengthDistributionData}} object.
#' 
#' @export
#' 
SumLengthDistribution <- function(
    LengthDistributionData, 
    LayerDefinition = c("FunctionParameter", "FunctionInput"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    BioticLayer = NULL
) {
    
    SumLengthDistributionData <- sumRawResolutionData(
        data = LengthDistributionData, dataType = "LengthDistributionData", 
        LayerDefinition = LayerDefinition, 
        LayerProcessData = BioticLayer, 
        LayerDefinitionMethod = LayerDefinitionMethod, 
        Resolution = Resolution, 
        LayerTable = LayerTable, 
        LayerType = "Biotic"
    )
    
    # Format the output:
    formatOutput(SumLengthDistributionData, dataType = "SumLengthDistributionData", keep.all = FALSE)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(SumLengthDistributionData)
    
    return(SumLengthDistributionData)
}



##################################################
##################################################
#' Mean length distribution
#' 
#' This function averages LengthDistributionData data horizontally, weighted by the effective towed distance.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param LayerDefinition The method to use for defining the Layers, one of \code{FunctionParameter} to define the Layers on the fly in this function, \code{FunctionInput} to import Layer process data from a previously run process by the input \code{BioticLayer}, or \code{PreDefined} whihc requires \code{SumLengthDistributionData} as input.
#' @param LayerDefinitionMethod See \code{\link{DefineBioticLayer}}
#' @inheritParams DefineBioticLayer
#' @param SurveyDefinition The method to use for defining the Survey, one of \code{FunctionParameter} to define the Survey on the fly in this function, or \code{FunctionInput} to import Survey process data from a previously run process by the input \code{Survey}.
#' @param SurveyDefinitionMethod See \code{\link{DefineSurvey}}
#' @inheritParams DefineSurvey
#' @param PSUDefinition The method to use for defining the PSUs, one of \code{FunctionParameter} to define the PSUs on the fly in this function, or \code{FunctionInput} to import PSU process data from a previously run process by \code{BioticPSU}.
#' @param PSUDefinitionMethod See \code{\link{DefineBioticPSU}}
#' @inheritParams DefineBioticPSU
#' 
#' @return
#' An \code{\link{MeanLengthDistributionData}} object.
#' 
#' @export
#' 
MeanLengthDistribution <- function(
    LengthDistributionData, 
    SumLengthDistributionData, 
    # Parameters of the sum part:
    # Layer: 
    LayerDefinition = c("FunctionParameter", "FunctionInput", "PreDefined"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    BioticLayer = NULL, 
    # Survey: 
    SurveyDefinition = c("FunctionParameter", "FunctionInput"), 
    SurveyDefinitionMethod = c("AllStrata", "SurveyTable"), 
    SurveyTable = data.table::data.table(), 
    Survey = NULL, 
    # Parameters of the mean part:
    # PSU: 
    PSUDefinition = c("FunctionParameter", "FunctionInput"), 
    PSUDefinitionMethod = c("StationToPSU", "None"), 
    StratumPolygon = NULL, 
    BioticPSU = NULL
) {
    
    
    # Skip the sum part if predefined:
    LayerDefinition <- match.arg(LayerDefinition)
    if(LayerDefinition != "PreDefined") {
        SumLengthDistributionData <- SumLengthDistribution(
            LengthDistributionData = LengthDistributionData, 
            LayerDefinition = LayerDefinition, 
            LayerDefinitionMethod = LayerDefinitionMethod, 
            Resolution = Resolution, 
            LayerTable = LayerTable, 
            BioticLayer = BioticLayer
        )
    }
    
    # Convert the PSUDefinitionMethod to "Identity" if "EDSUToPSU":
    PSUDefinitionMethod <- match.arg(PSUDefinitionMethod)
    if(grepl("StationToPSU", PSUDefinitionMethod, ignore.case = TRUE)) {
        PSUDefinitionMethod <- "Identity"
    }
    
    # Run the mean part:
    MeanLengthDistributionData <- meanRawResolutionData(
        data = SumLengthDistributionData, dataType = "SumLengthDistributionData", 
        # PSU:
        PSUDefinition = PSUDefinition, 
        PSUProcessData = BioticPSU, 
        PSUDefinitionMethod = PSUDefinitionMethod, 
        # Survey:
        SurveyDefinition = SurveyDefinition, 
        SurveyProcessData = Survey, 
        SurveyDefinitionMethod = SurveyDefinitionMethod, 
        SurveyTable = SurveyTable, 
        # General:
        StratumPolygon = StratumPolygon, 
        PSUType = "Biotic"
    )
    
    
    # Format the output:
    formatOutput(MeanLengthDistributionData, dataType = "MeanLengthDistributionData", keep.all = FALSE)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(MeanLengthDistributionData)
    
    return(MeanLengthDistributionData)
}


##################################################
##################################################
#' Length distribution assigned to each cell of acoustic PSU and Layer
#' 
#' This funciton calculates weighted average of the length distribution of hauls assigned to each acoustic PSU and Layer. The weights are set by \code{\link{BioticAssignmentWeighting}}.
#' 
#' @inheritParams ProcessData
#' @inheritParams ModelData
#' 
#' @details
#' The purpose of the \emph{AssignmentLengthDistribution} function is to produces one total length distribution for each combination of assigned biotic stations with corresponding weighting variables. 
#'
#'If the biotic station length distributions which shall make up the total length distribution is of type \emph{Standard} (length distribution as if the complete catch on deck was measured), this will give an \emph{implicit weighting by catch as well as towing distance}.
#'
#'If the biotic station length distributions which shall make up the total length distribution is of type \emph{Normalized} (length distribution as if the complete catch on deck was measured and as if the towing distance had been 1 nautical mile), this will give an \emph{implicit weighting by catch}.
#'
#'If the biotic station length distributions which shall make up the total length distribution is of type \emph{Percent} (sum of percentages for all length groups in the distribution is 100 and the shape of the distribution is the aim), this will give \emph{NO implicit weighting}.
#'
#'One total length distribution is calculated as follows:
#'
#'1)	For each biotic station a weighting factor for each station is calculated from the weight variables of the assigned stations:
#'
#'\deqn{W_s = \frac{w_s}{\sum_{y=1}^{n} w_y}}
#'
#'where
#'
#'\eqn{W_s} 		= weighting factor for station \eqn{s}
#'
#'\eqn{n}		    = number of trawl stations to be combined
#'
#'\eqn{w_s}  		= the value of the weight variable for station \eqn{s}
#'
#'\eqn{w_y} 		= the value of the weight variable for station \eqn{y}
#'
#'
#'2)	For each length distribution by biotic station (\eqn{d_s}), the number or percentage value in each length group is multiplied by \eqn{W_s}:
#'
#'\deqn{dw_s = W_s \times d_s}
#'
#'3)	The total length distribution \eqn{d_t} is finally calculated by adding the numbers in each length interval for all stations where \eqn{n} is the number of stations to be combined:
#'
#'\deqn{d_t = \displaystyle\sum_{y=1}^{n} dw_y}
#'
#'\eqn{d_t} is the total length distribution for one assignment.
#'
#' 
#' @return
#' An object of StoX datatype \code{\link{AssignmentLengthDistributionData}}.
#' 
#' @seealso \code{\link{LengthDistribution}} to produce the input LengthDistributionData, and \code{\link{DefineBioticAssignment}} to produce the input BioticAssignment.
#' 
#' @export
#' 
AssignmentLengthDistribution <- function(LengthDistributionData, BioticAssignment) {
    
    ### # Require LengthDistributionType "Percent":
    ### if(!isLengthDistributionType(LengthDistributionData, "Percent")) {
    ###     stop("LengthDistributionData used as input to AssignmentLengthDistribution() must be of LengthDistributionType \"Percent\"")
    ### }
    
    # Determine assignment IDs:
    BioticAssignmentCollapsed <- BioticAssignment[, .(assignmentPasted = paste0(paste(Haul, WeightingFactor, sep = ",", collapse = "\n"), "\n")), by = c("Stratum", "PSU", "Layer")]
    uniqueAssignmentPasted <- unique(BioticAssignmentCollapsed$assignmentPasted)
    BioticAssignmentCollapsed[, assignmentID := match(assignmentPasted, uniqueAssignmentPasted)]
    
    # Get the assignment length distribution of each unique assignment ID:
    uniqueAssignmentPastedDT <- data.table::data.table(
        assignmentID = seq_along(uniqueAssignmentPasted), 
        assignmentPasted = uniqueAssignmentPasted
    )
    uniqueAssignmentLengthDistributionData <- uniqueAssignmentPastedDT[, getAssignmentLengthDistributionDataOne(assignmentPasted, LengthDistributionData = LengthDistributionData, percent = TRUE), by = "assignmentID"]
    
    # Merge the mean length distribution of each assignment with the BioticAssignmentCollapsed excluded "assignmentPasted":
    BioticAssignmentCollapsed[, assignmentPasted := NULL]
    AssignmentLengthDistributionData <- merge(BioticAssignmentCollapsed, uniqueAssignmentLengthDistributionData, by = "assignmentID", allow.cartesian = TRUE)
    
    # Set the LengthDistributionType to "Percent":
    AssignmentLengthDistributionData[, LengthDistributionType := "Percent"]
    
    # Format the output:
    formatOutput(AssignmentLengthDistributionData, dataType = "AssignmentLengthDistributionData", keep.all = FALSE)
    
    return(AssignmentLengthDistributionData)
}


# Function to get the assignment length distribution of one assignmentID, represented by one line in BioticAssignmentUnique:
getAssignmentLengthDistributionDataOne <- function(assignmentPasted, LengthDistributionData, percent = TRUE) {
    
    # Define the data variable:
    dataVariable <- getDataTypeDefinition(dataType = "LengthDistributionData", elements = "data", unlist = TRUE)
    
    # Extract the subset of the data given by the hauls:
    BioticAssignment <- data.table::fread(text = assignmentPasted, col.names = c("Haul", "WeightingFactor"), colClasses = c("character", "double"))
    Hauls <- BioticAssignment$Haul
    WeightingFactors <- BioticAssignment$WeightingFactor
    thisLengthDistributionData <- subset(LengthDistributionData, Haul %in% Hauls)
    
    # Overwrite the weights by those defined in the BioticAssignment object:
    weightingVariable <- getDataTypeDefinition(dataType = "LengthDistributionData", elements = "weighting", unlist = TRUE)
    thisLengthDistributionData[, c(weightingVariable) := ..WeightingFactors[match(Haul, ..Hauls)]]
    
    # Get the category and grouping variables (SpeciesCategory, IndividualTotalLength, LengthResolution), and sum across hauls for each combination of these variables, weighted by the "WeightedCount":
    by <- getDataTypeDefinition(dataType = "LengthDistributionData", elements = c("categoryVariable", "groupingVariables"), unlist = TRUE)
    thisLengthDistributionData[, c(dataVariable) := sum(x = get(dataVariable) * get(weightingVariable)), by = by]
    
    # Extract only the relevant columns:
    ###formatOutput(thisLengthDistributionData, dataType = "AssignmentLengthDistributionData", keep.all = FALSE, allow.missing = TRUE)
    # Remove also the resolution variables, as the output from this function will be merged with BioticAssignmentData by AssignmentID (and not by these resolution avriables):
    removeColumnsByReference(
        data = thisLengthDistributionData, 
        toRemove = getResolutionVariables("AssignmentLengthDistributionData")
    )
    
    
    
    # Subset to the unique rows (since the sum was by reference):
    #thisLengthDistributionData <- unique(thisLengthDistributionData)
    thisLengthDistributionData <- unique(thisLengthDistributionData, by = by)
    
    # This was a misunderstanding: 
    ### # Normalize for each species category:
    ### bySpecies <- getDataTypeDefinition(dataType = "LengthDistributionData", elements = c("categoryVariable"), unlist = TRUE)
    ### scaling <- if(percent) 100 else 1
    ### thisLengthDistributionData[, c(dataVariable) := get(dataVariable) / sum(get(dataVariable), na.rm = TRUE) * scaling, by = bySpecies]
    
    # Order by the category and grouping variables:
    orderBy <- getDataTypeDefinition(dataType = "LengthDistributionData", elements = c("categoryVariable", "groupingVariables"), unlist = TRUE)
    setorderv(thisLengthDistributionData, cols = orderBy)
    
    return(thisLengthDistributionData)
}

