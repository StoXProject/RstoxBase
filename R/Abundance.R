##################################################
##################################################
#' Calculate abundance of a stratum
#' 
#' This function calcualtes abundance as the product of density and area of each stratum.
#' 
#' @inheritParams ModelData
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{AbundanceData}}. 
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{SuperIndividuals}} for distributing Abundance to individuals.
#' 
#' @export
#' 
Abundance <- function(
    MeanDensityData, 
    StratumAreaData
) {
	
    # Merge the stratum area with the DensityData to an AbundanceData (remove the area at the end of the function):
    AbundanceData <- merge(MeanDensityData, StratumAreaData, by ="Stratum")
    
    # Multiply the area and the density:
    AbundanceData[, Abundance := Area * Density]
    
    # Keep only the releavnt columns:
    #keepOnlyRelevantColumns(AbundanceData, "AbundanceData")
    formatOutput(AbundanceData, dataType = "AbundanceData", keep.all = FALSE)
    
    return(AbundanceData)
}


##################################################
##################################################
#' Indivduals to distribute abundance to to create super-individuals
#' 
#' This function defines and returns the individuals used in the estimation model to which to distribute the abundance to create super-individuals.
#' 
#' @inheritParams ProcessData
#' @inheritParams ModelData
#' @param AbundanceType The type of abundance, one of "Acoustic" and "SweptArea".
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
#' @seealso \code{\link{SuperIndividuals}} for distributing Abundance to the Individuals.
#' 
#' @export
#' 
Individuals <- function(
    StoxBioticData, 
    AbundanceType = c("Acoustic", "SweptArea"), 
    BioticAssignment, 
    MeanLengthDistributionData
) {
    
    # Get the DefinitionMethod:
    AbundanceType <- match.arg(AbundanceType)
    
    # Merge StoxBtiotic:
    MergedStoxBioticData <- RstoxData::MergeStoxBiotic(StoxBioticData)
    
    # Get the resolution variables of AbundanceData:
    abundanceResolutionVariables <- getResolutionVariables("AbundanceData")
    
    # Get all hauls of each Stratum and Layer:
    if(AbundanceType == "Acoustic") {
        # Get the hauls with positive WeightingFactor:
        usedHauls <- BioticAssignment[WeightingFactor > 0, .(Haul = unique(Haul)), by = abundanceResolutionVariables]
    }
    else if(AbundanceType == "SweptArea") {
        # Get the original resolution from the MeanLengthDistributionData:
        usedHauls <- MeanLengthDistributionData$Resolution
        
        # Get the unique rows, while extracting only the Haul and abundance resolution columns:
        usedHauls <- usedHauls[, .(Haul = unique(Haul)), by = abundanceResolutionVariables]
        
        # Remove rows with any NAs:
        usedHauls <- na.omit(usedHauls)
        ## Extract only the Haul column:
        #usedHauls <- usedHauls$Haul
        
        #usedHauls <- LengthDistributionData[, .(Haul = unique(Haul)), by = abundanceResolutionVariables]
    }
    else {
        stop("Invalid DensityType")
    }
    
    # Expand the vectors of hauls:
    #usedHauls <- expandDT(usedHauls)
    
    # Merge individuals into the Stratum, Layer, Haul table:
    IndividualsData <- merge(usedHauls, MergedStoxBioticData, by = "Haul", allow.cartesian = TRUE)
    
    ## Remove rows with SpeciesCategory NA (change added on 2020-04-20, since NAs in SpeciesCategory are vital for including all Stations, PSUs# and Strata when using meanData(), but should not present when considering individuals):
    #IndividualsData <- IndividualsData[!is.na(SpeciesCategory), ]
    
    # Remove rows with missing IndividualKey, indicating they are not individuals but merely rows for Hauls included in the PSU/Layer:
    IndividualsData <- IndividualsData[!is.na(IndividualKey), ]
    
    # Order the columns, but keep all columns:
    formatOutput(IndividualsData, dataType = "IndividualsData", keep.all = TRUE)
    
    
    return(IndividualsData)
}




##################################################
##################################################
#' Super-indivduals with abundance
#' 
#' This function distributes Abundance to the individuals defined by \code{\link{Individuals}}.
#' 
#' @inheritParams ModelData
#' @param DistributionMethod The method used for distributing the abundance, one of "Equal" for equal abundance to all individuals of each Stratum, Layer, SpeciesCategory and length group, and "HaulDensity" to weight by the haul density.
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
#' 
SuperIndividuals <- function(
    IndividualsData, 
    AbundanceData, 
    DistributionMethod = c("Equal", "HaulDensity"), 
    LengthDistributionData
) {
    # Get the DistributionMethod:
    DistributionMethod <- match.arg(DistributionMethod)
    
    # Make a copy of the IndividualsData:
    SuperIndividualsData <- data.table::copy(IndividualsData)
    # Make sure the AbundanceData is proper data.table:
    AbundanceData <- data.table::setDT(AbundanceData)
    
    
    # Add common length group IDs in SuperIndividualsData and AbundanceData, given the length groups in AbundanceData (by referencem, so the inputs are modified):
    #AbundanceData <- as.data.table(AbundanceData)
    #addLengthGroups2(
    #    AbundanceData
    #)
    
    #print(head(AbundanceData, 2))
    # Add length groups to SuperIndividualsData, based on the lengths and resolutions of the AbundanceData:
    addLengthGroupsByReference(data = SuperIndividualsData, master = AbundanceData)
    # Add length groups also to the AbundanceData:
    addLengthGroupsByReference(data = AbundanceData, master = AbundanceData)
    
    # Merge the abundance into the SuperIndividualsData, by the resolution and category variables of the AbundanceData and the LengthGroup introduced in addLengthGroups():
    abundanceGrouping <- c(
        getDataTypeDefinition(dataType = "AbundanceData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable"), unlist = TRUE), 
        "LengthGroup"
    )
    SuperIndividualsData <- merge(
        SuperIndividualsData, 
        AbundanceData[, c(..abundanceGrouping, "Abundance")], 
        by = abundanceGrouping, 
        allow.cartesian = TRUE
    )
    
    # Append an individualCount to the SuperIndividualsData, representing the number of individuals in each category given by 'by':
    
    # Distributing abundance equally between all individuals of each Stratum, Layer, SpeciesCategory and LengthGroup:
    if(DistributionMethod == "Equal") {
        SuperIndividualsData[, individualCount := as.double(.N), by = abundanceGrouping]
        SuperIndividualsData[, haulWeightFactor := 1]
    }
    else if(DistributionMethod == "HaulDensity") {
        
        # Give an error if the LengthDistributionType is "Percent" or "Standard":
        validLengthDistributionType <- c("Normalized", "SweepWidthCompensatedNormalized", "SelectivityCompensatedNormalized")
        if(! LengthDistributionData$LengthDistributionType[1] %in% validLengthDistributionType) {
            stop("The LengthDistributionType of the input LengthDistributionData must be one of ", paste(validLengthDistributionType, collapse = ", "))
        }
        
        # Add length group IDs also in in LengthDistributionData:
        # Make sure the AbundanceData is proper data.table:
        LengthDistributionData <- data.table::setDT(LengthDistributionData)
        addLengthGroupsByReference(data = LengthDistributionData, master = AbundanceData)
        # We need to unique since there may have been multiple lines in the same length group:
        LengthDistributionData <- unique(LengthDistributionData)
        
        
        #addLengthGroups(
        #    data = LengthDistributionData, 
        #    master = AbundanceData, 
        #    lengthVar = "IndividualTotalLengthCentimeter", 
        #    resolutionVar = "LengthResolutionCentimeter"
        #)
        # In case that the length resolution is higher in the LengthDistributionData than in the AbundanceData, uniquify the LengthDistributionData:
        
        
        
        
        # Sum in each length group:
        haulGrouping <- c(
            "Haul", 
            getDataTypeDefinition(dataType = "SuperIndividualsData", elements = "categoryVariable", unlist = TRUE), 
            "LengthGroup"
        )
        LengthDistributionData[, WeightedCount := sum(WeightedCount), by = haulGrouping]
        keep <- !duplicated(LengthDistributionData[, ..haulGrouping])
        LengthDistributionData <- subset(LengthDistributionData, keep)
        
        # Add the haul density as the WeightedCount to the SuperIndividualsData (requiring Normalized LengthDistributionType):
        SuperIndividualsData <- merge(
            SuperIndividualsData, 
            #LengthDistributionData[, c(..haulGrouping, "WeightedCount", "sumWeightedCount", "haulWeightFactor")], 
            #LengthDistributionData[, ..haulGrouping], 
            LengthDistributionData[, c(..haulGrouping, "WeightedCount")], 
            by = haulGrouping
        )
        
        # Sum the haul densities (stored as WeightedCount) over all hauls of each Stratum/Layer/SpeciesCategory/LengthGroup:
        SuperIndividualsData[, sumWeightedCount := sum(WeightedCount, na.rm = TRUE), by = abundanceGrouping]
        # Get the Haul weight factor as the WeightedCount divided by sumWeightedCount:
        SuperIndividualsData[, haulWeightFactor := WeightedCount / sumWeightedCount , by = haulGrouping]
        
        
        
        
        # Get the sum of the WeightedCount in each Stratum/SpeciesCategory/LengthGroup:
        #sumBy <- c(
        #    getDataTypeDefinition(dataType = "SuperIndividualsData", elements = c("horizontalResolution", "categoryVariable"), unlist = TRUE), 
        #    "LengthGroup"
        #)
        ###SuperIndividualsData[, sumWeightedCount := sum(WeightedCount, na.rm = TRUE), by = abundanceGrouping]
            
        # Divide the WeightedCount by the sum:
        #SuperIndividualsData[, abundanceWeightFactor := WeightedCount / sumWeightedCount , by = haulGrouping]
        
        # Get the number of individuals in each Haul:
        SuperIndividualsData[, individualCount := as.double(.N), by = haulGrouping]
        ###SuperIndividualsData[, individualCount := 1]
        
        # Remove WeightedCount and sumWeightedCount:
        SuperIndividualsData[, WeightedCount := NULL]
        SuperIndividualsData[, sumWeightedCount := NULL]
    }
    else{
        stop("Invalid DistributionMethod")
    }
    
    ## Order by the grouping variables:
    #SuperIndividualsData <- setorderv(SuperIndividualsData, abundanceGrouping)
    
    # Multiply by abundance weighting factors:
    #print(SuperIndividualsData[, "abundanceWeightFactor"], 20)
    #print(SuperIndividualsData[, "individualCount"], 20)
    SuperIndividualsData[, Abundance := Abundance * haulWeightFactor]
    
    # Divide by the number of individuals (regardless of DistributionMethod)
    SuperIndividualsData[, Abundance := Abundance / individualCount]
    
    # Order the columns, but keep all columns:
    formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE)
    
    # Remove the columns "individualCount" and "abundanceWeightFactor", manually since the data type SuperIndividualsData is not uniquely defined (contains all columns of StoxBiotic):
    SuperIndividualsData[, haulWeightFactor := NULL]
    
    SuperIndividualsData[, individualCount := NULL]
    #SuperIndividualsData[, abundanceWeightFactor := NULL]
    SuperIndividualsData[, LengthGroup := NULL]
    
    # Order the rows:
    orderDataByReference(SuperIndividualsData, "SuperIndividualsData")
    
    ### # Remove the temporary columns:
    ### toRemove <- c("LengthGroup", "abundanceWeightFactor", "individualCount")
    ### #SuperIndividualsData[, c(toRemove) := NULL]
    ### 
    ### # Order 
    ### toOrderFirst <- c(
    ###     getDataTypeDefinition(dataType = "AbundanceData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable"), unlist = TRUE), 
    ###     "Haul"
    ### )
    ### data.table::setcolorder(SuperIndividualsData, toOrderFirst)

    return(SuperIndividualsData)
}


# Function to add length group IDs in two tables given the length groups in the second:
addLengthGroupsByReferenceOneSpecies <- function(
    data, 
    master, 
    species, 
    lengthVar = "IndividualTotalLengthCentimeter", 
    resolutionVar = "LengthResolutionCentimeter"
) {
    
    # Get the indices at the given species in 'data' and 'master':
    speciesVar <- getDataTypeDefinition(dataType = "AbundanceData", elements = "categoryVariable", unlist = TRUE)
    atSpeciesInData <- which(data[[speciesVar]] %in% species)
    atSpeciesInMaster <- which(master[[speciesVar]] %in% species)
    if(length(atSpeciesInData) == 0 || length(atSpeciesInMaster) == 0) {
        stop("The species ", species, " is not present in both data and master.")
    }
    
    
    # Get the unique length intervals of the master:
    uniqueLengthGroups <- unique(master[atSpeciesInMaster, c(..lengthVar, ..resolutionVar)])
    
    if(all(is.na(uniqueLengthGroups))) {
        return(FALSE)
    }
    
    # Order the unique length intervals:
    uniqueLengthGroups <- uniqueLengthGroups[order(uniqueLengthGroups[[lengthVar]]), ]
    
    # (1) Match the length groups exactly:
    data[atSpeciesInData, LengthGroup := match(
        paste(..lengthVar, ..resolutionVar), 
        eval(
            paste(
                uniqueLengthGroups[[lengthVar]], 
                uniqueLengthGroups[[resolutionVar]]
            )
        )
    )]
    
    # (2) Match missing length:
    missingLength <- atSpeciesInData[is.na(data[[lengthVar]][atSpeciesInData])]
    if(length(missingLength)) {
        # The 'atMissingLengthGroup' is guaranteed to be of length 1:
        atMissingLengthGroup <- which(is.na(uniqueLengthGroups[[lengthVar]]))
        if(length(atMissingLengthGroup)) {
            data[missingLength, LengthGroup := ..atMissingLengthGroup]
        }
    }
    
    # (3) For the length intervals not exactly matched in the master, find the appropriate intervals. The intervalVector may contain intervals that are not present in the data (intervals between present intervals):
    #data[, is.na(LengthGroup)]
    intervalVector <- sort(
        unique(c(
            uniqueLengthGroups[[lengthVar]], 
            uniqueLengthGroups[[lengthVar]] + uniqueLengthGroups[[resolutionVar]]
        ))
    )
    indexVector <- NA
    present <- intervalVector %in% uniqueLengthGroups[[lengthVar]]
    indexVector[present] <- seq_len(sum(present))
    
    # Get the LengthGroup using findInterval (and the quote.convert() function):
    notExactlyMatched <- atSpeciesInData[is.na(data$LengthGroup[atSpeciesInData])]
    data[notExactlyMatched, LengthGroup := findInterval(
        eval(quote.convert(lengthVar)), 
        ..intervalVector
    )]
    # Convert to the indices in the uniqueLengthGroups:
    data[notExactlyMatched, LengthGroup := indexVector[LengthGroup]]
    
    # Also replace the lengthVar and resolutionVar by those in the master:
    data[atSpeciesInData, `:=`(c(lengthVar), uniqueLengthGroups[[lengthVar]][LengthGroup])]
    data[atSpeciesInData, `:=`(c(resolutionVar), uniqueLengthGroups[[resolutionVar]][LengthGroup])]
    
    return(TRUE)
}




# Function to add length group IDs in two tables given the length groups in the second:
addLengthGroupsByReference <- function(
    data, 
    master, 
    lengthVar = "IndividualTotalLengthCentimeter", 
    resolutionVar = "LengthResolutionCentimeter"
) {
    
    # Run a for loop through the common species:
    speciesVar <- getDataTypeDefinition(dataType = "AbundanceData", elements = "categoryVariable", unlist = TRUE)
    speciesInData <- unique(data[[speciesVar]])
    speciesInMaster <- unique(master[[speciesVar]])
    
    # If there are species in the master that are not in the data, report a warning:
    speciesOnlyInMaster <- na.omit(setdiff(speciesInMaster, speciesInData))
    if(length(speciesOnlyInMaster)) {
        warning("StoX: The species categories ", paste(speciesOnlyInMaster, collapse = ", "), " are present in the master but not in the data")
    }
    # If there are species in the data that are not in the master, report a warning:
    speciesOnlyInData <- na.omit(setdiff(speciesInMaster, speciesInData))
    if(length(speciesOnlyInData)) {
        warning("StoX: The species categories ", paste(speciesOnlyInData, collapse = ", "), " are present in the data but not in the master. These species categories will be removed from the output.")
    }

    # Keep only the common species:    
    species <- intersect(speciesInData, speciesInMaster)
    
    for(thisspecies in species) {
        addLengthGroupsByReferenceOneSpecies(
            data = data, 
            master = master, 
            species = thisspecies, 
            lengthVar = lengthVar, 
            resolutionVar = resolutionVar)
    }
    
    return(TRUE)
}




##################################################
##################################################
#' Indivduals to distribute abundance to to create super-individuals
#' 
#' This function defines and returns the individuals used in the estimation model to which to distribute the abundance to create super-individuals.
#' 
#' @inheritParams ProcessData
#' @inheritParams ModelData
#' @param AbundanceType The type of abundance, one of "Acoustic" and "SweptArea".
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
#' @seealso \code{\link{SuperIndividuals}} for distributing Abundance to the Individuals.
#' 
#' @export
#' 
ImputeSuperIndividuals <- function(
    SuperIndividualsData, 
    Seed = 1
) {
    
    # Check that the length resolution is constant: 
    if(!allEqual(SuperIndividualsData$LengthResolutionCentimeter)) {
        stop("All individuals must have identical LengthResolutionCentimeter in the current version.")
    }
    
    ImputeSuperIndividualsData <- ImputeData(
        data = SuperIndividualsData, 
        imputeAtMissing = "IndividualAge", 
        imputeByEqual = "IndividualTotalLengthCentimeter", 
        groupBy = "SpeciesCategory", 
        seed = Seed
    )
    
    # Order the columns, but keep all columns:
    formatOutput(ImputeSuperIndividualsData, dataType = "ImputeSuperIndividualsData", keep.all = TRUE)
    
    return(ImputeSuperIndividualsData)
}


ImputeData <- function(
    data, 
    imputeAtMissing = "IndividualAge", 
    imputeByEqual = "IndividualTotalLengthCentimeter", 
    groupBy = "SpeciesCategory", 
    seed = 1
) {
    
    dataCopy <- data.table::copy(data)
    RowIndex <- seq_len(nrow(dataCopy))
    dataCopy[, RowIndex := ..RowIndex]
    
    levels <- list(
        "Haul", 
        "Stratum", 
        "AllStrata"
    )
    # Add an AllStrata column to the data to facilitate the AllStrata level: 
    dataCopy[, AllStrata := "AllStrata"]
    setcolorder(dataCopy, neworder = "AllStrata")
    
    # Get a vector with the seed of each level:
    seedVector <- structure(as.list(getSeedVector(size = length(levels), seed = seed)), names = levels)
    
    dataCopy[, ReplaceRowIndex := NA_integer_]
    dataCopy[, ReplaceLevel := NA_character_]
    for(level in levels) {
        # Get the vector of columns to impute by:
        by <- c(groupBy, level, imputeByEqual)
        
        # Get the table of seeds for each unique combination of the columns defined by 'by':
        seedTable <- unique(dataCopy[, ..by])
        data.table::setorder(seedTable)
        seedTable <- data.table::data.table(
            seedTable, 
            imputeSeed = getSeedVector(size = nrow(seedTable), seed = seedVector[[level]])
        )
        
        # Add the seeds to the data (recycled):
        dataCopy <- merge(dataCopy, seedTable, by = by, all = TRUE, sort = FALSE)
        
        # Perform the imputation:
        getImputeRowIndicesOneLevel(
            dataCopy, 
            imputeAtMissing = imputeAtMissing, 
            level = level, 
            by = by
        )
        
        # Remove the imputeSeed column:
        dataCopy[, imputeSeed := NULL]
    }
    
    # Perform the imputation:
    dataCopy <- replaceMissingData(dataCopy)
    
    # Delete the RowIndex and ReplaceRowIndex:
    dataCopy$ReplaceIndividual <- dataCopy[(ReplaceRowIndex), "Individual"]
    dataCopy[, RowIndex := NULL]
    dataCopy[, ReplaceRowIndex := NULL]
    dataCopy[, AllStrata := NULL]
    
    return(dataCopy)
}

# Function to get the imputation row indices of one level ("Haul", "Stratum", NULL). This function is applied using for loop over the levels:
getImputeRowIndicesOneLevel <- function(
    dataCopy, 
    imputeAtMissing = "Age", 
    level = "Haul", 
    by
) {
    # Get the row indices to replace data from by applying the function getImputeRowIndicesOneGroup by the groupBy input, the level (one of Haul, Stratum, NULL) and the imputeByEqual input. 
    dataCopy[, 
         c("ReplaceRowIndex", "ReplaceLevel") := getImputeRowIndicesOneGroup(
             .SD, 
             imputeAtMissing = imputeAtMissing, 
             level = level
         ), 
         by = by]
}

# Function to get the imputation row indices of one table of one level ("Haul", "Stratum", NULL). This function is applied using data table with 'by':
getImputeRowIndicesOneGroup <- function(
    dataCopyOneGroup, 
    imputeAtMissing, 
    level
) {
    # Get the super individuals with missing data (and which have not been given ReplaceRowIndex):
    missingData <- dataCopyOneGroup[, is.na(get(imputeAtMissing)) & is.na(ReplaceRowIndex)]
    # Get the number of missing and present rows:
    NMissingRows <- sum(missingData)
    NPresentRows <- sum(!missingData)
    
    # We chose (as it may be cleaner) to create the output row indices as a vector of NAs, instead of using data.table:
    ReplaceRowIndex <- dataCopyOneGroup$ReplaceRowIndex
    ReplaceLevel <- dataCopyOneGroup$ReplaceLevel
    
    if(NMissingRows > 0 && NPresentRows > 0) {
        
        
        # Sample the rows with present data:
        #sampleIndexInPresent <- sample.int(NPresentRows, NMissingRows, replace = TRUE)
        sampleIndexInPresent <- sampleSorted(
            dataCopyOneGroup[!missingData, Individual], 
            size = NMissingRows, 
            seed = dataCopyOneGroup$imputeSeed[1], 
            replace = TRUE, 
            index.out = TRUE
        )
        ReplaceRowIndex[missingData] <- dataCopyOneGroup[!missingData, RowIndex][sampleIndexInPresent]
        
        # Add also the replace level:
        ReplaceLevel[missingData] <- level
    }
    
    return(
        list(
            ReplaceRowIndex = ReplaceRowIndex, 
            ReplaceLevel = ReplaceLevel
        )
    )
}



replaceMissingData <- function(x) {
    
    # Get the matrix indices of data to replace, which are those that are missing in the rows impute:
    rowsToImpute <- x[!is.na(ReplaceRowIndex), RowIndex]
    rowsToImputeFrom <- x[!is.na(ReplaceRowIndex), ReplaceRowIndex]
    
    x <- as.data.frame(x)
    atReplace <- is.na(x[rowsToImpute, ]) & !is.na(x[rowsToImputeFrom, ])
    x[rowsToImpute, ][atReplace] <- x[(rowsToImputeFrom), ][atReplace]
    
    x <- data.table::setDT(x)
    return(x)
}





