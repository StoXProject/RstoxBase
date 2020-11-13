##################################################
##################################################
#' Calculate abundance of a stratum
#' 
#' This function calculates abundance as the product of density and area of each stratum.
#' 
#' @inheritParams ModelData
#' 
#' @details
#' The function merge the \code{\link{MeanDensityData}} with the \code{\link{StratumAreaData}} by Stratum and calculates the abundance by Stratum and IndividualTotalLength as the product of density [number by square nautical miles] and area [square nautical miles].
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
    
    # Format the output:
    # Changed added on 2020-10-16, where the datatypes DensityData and AbundanceData are now considered non-rigid:
    #formatOutput(AbundanceData, dataType = "AbundanceData", keep.all = FALSE)
    formatOutput(AbundanceData, dataType = "AbundanceData", keep.all = FALSE, allow.missing = TRUE)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(AbundanceData)
    
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
    MergeStoxBioticData <- RstoxData::MergeStoxBiotic(StoxBioticData)
    
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
    
    # Merge individuals into the Stratum, Layer, Haul table, for the purpose of gettinig all individuals used in the model, possibly repeating individuals if the same hauls are used for several Stratum-Layer combinations (which can be the case e.g. for several layers):
    IndividualsData <- merge(usedHauls, MergeStoxBioticData, by = "Haul", allow.cartesian = TRUE)
    
    ## Remove rows with SpeciesCategory NA (change added on 2020-04-20, since NAs in SpeciesCategory are vital for including all Stations, PSUs# and Strata when using meanData(), but should not present when considering individuals):
    #IndividualsData <- IndividualsData[!is.na(SpeciesCategory), ]
    
    # Remove rows with missing IndividualKey, indicating they are not individuals but merely rows for Hauls included in the PSU/Layer:
    IndividualsData <- IndividualsData[!is.na(IndividualKey), ]
    
    # Order the columns, but keep all columns. Also add the names of the MergeStoxBioticData as secondaryColumnOrder to tidy up by moving the Haul column (used as by in the merging) back into its original position:
    formatOutput(IndividualsData, dataType = "IndividualsData", keep.all = TRUE, secondaryColumnOrder = names(MergeStoxBioticData))
    
    # Add the attribute 'variableNames':
    setattr(
        IndividualsData, 
        "stoxDataVariableNames",
        attr(MergeStoxBioticData, "stoxDataVariableNames")
    )
    
    attr(IndividualsData, "stoxDataVariableNames") <- attr(MergeStoxBioticData, "stoxDataVariableNames")
    
    # Not needed here, since we only copy data: 
    #Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(IndividualsData)
    
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
    
    # Add length groups to SuperIndividualsData, based on the lengths and resolutions of the AbundanceData:
    addLengthGroupsByReference(data = SuperIndividualsData, master = AbundanceData)
    # Add length groups also to the AbundanceData:
    addLengthGroupsByReference(data = AbundanceData, master = AbundanceData)
    
    # Merge the abundance into the SuperIndividualsData, by the resolution and category variables of the AbundanceData and the LengthGroup introduced in addLengthGroups(). This is akward coding, with two instances of getDataTypeDefinition(), but the problem is the LengthGroup, and that we do not need "the columns IndividualTotalLength" and "LengthResolution":
    
    abundanceGrouping <- c(
        getDataTypeDefinition(dataType = "AbundanceData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable"), unlist = TRUE), 
        "LengthGroup"
    )
    # Get the variables to add from the AbundanceData, which are "Abundance", the vertical dimension and grouping variables, but not those which may be present in the IndividualsData ("IndividualTotalLength", "LengthResolution"), and also the abundanceGrouping:
    variablesToGetFromAbundanceData <- c(
        "Abundance", 
        getDataTypeDefinition(dataType = "AbundanceData", elements = c("surveyDefinition", "verticalLayerDimension", "groupingVariables"), unlist = TRUE)
    )
    variablesToGetFromAbundanceData <- setdiff(variablesToGetFromAbundanceData, names(SuperIndividualsData))
    variablesToGetFromAbundanceData <- intersect(variablesToGetFromAbundanceData, names(AbundanceData))
    variablesToGetFromAbundanceData <- c(
        abundanceGrouping, 
        variablesToGetFromAbundanceData
    )
    
    # Merge AbundanceData into the IndividualsData
    SuperIndividualsData <- merge(
        SuperIndividualsData, 
        AbundanceData[, ..variablesToGetFromAbundanceData], 
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
    SuperIndividualsData[, Abundance := Abundance * haulWeightFactor]
    
    # Divide by the number of individuals (regardless of DistributionMethod)
    SuperIndividualsData[, Abundance := Abundance / individualCount]
    
    # Add Biomass:
    SuperIndividualsData[, Biomass := Abundance * IndividualRoundWeight]
    
    # Format the output but keep all columns:
    formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE)
    
    # Remove the columns "individualCount" and "abundanceWeightFactor", manually since the data type SuperIndividualsData is not uniquely defined (contains all columns of StoxBiotic):
    SuperIndividualsData[, haulWeightFactor := NULL]
    
    SuperIndividualsData[, individualCount := NULL]
    #SuperIndividualsData[, abundanceWeightFactor := NULL]
    SuperIndividualsData[, LengthGroup := NULL]
    
    ## Order the rows:
    #orderDataByReference(SuperIndividualsData, "SuperIndividualsData")
    
    # Add the attribute 'variableNames':
    setattr(
        SuperIndividualsData, 
        "stoxDataVariableNames",
        attr(IndividualsData, "stoxDataVariableNames")
    )
    
    
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

    # Not needed here, since we only copy data: 
    #Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(SuperIndividualsData)
    
    return(SuperIndividualsData)
}


# Function to add length group IDs in two tables given the length groups in the second:
addLengthGroupsByReferenceOneSpecies <- function(
    data, 
    master, 
    species, 
    lengthVar = "IndividualTotalLength", 
    resolutionVar = "LengthResolution"
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
    lengthVar = "IndividualTotalLength", 
    resolutionVar = "LengthResolution"
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
    
    if(!length(species)) {
        data[, LengthGroup := NA]
    }
    else {
        for(thisspecies in species) {
            addLengthGroupsByReferenceOneSpecies(
                data = data, 
                master = master, 
                species = thisspecies, 
                lengthVar = lengthVar, 
                resolutionVar = resolutionVar
            )
        }
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
    if(!allEqual(SuperIndividualsData$LengthResolution)) {
        stop("All individuals must have identical LengthResolution in the current version.")
    }
    
    # Get the columns to impute:
    individualNames <- attr(SuperIndividualsData, "stoxDataVariableNames")$Individual
    # Subtract the keys: 
    IndividualKeys <- setdiff(individualNames, RstoxData::getStoxKeys("StoxBiotic"))
    
    # Impute the SuperIndividualsData:
    SuperIndividualsData <- ImputeData(
        data = SuperIndividualsData, 
        imputeAtMissing = "IndividualAge", 
        imputeByEqual = "IndividualTotalLength", 
        groupBy = "SpeciesCategory", 
        seed = Seed, 
        columnNames = individualNames
    )
    
    # Re-calculate the Biomass:
    SuperIndividualsData[, Biomass := Abundance * IndividualRoundWeight]

    # Format the output but keep all columns:
    formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE)
    
    # Not needed here, since we only copy data: 
    #Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(SuperIndividualsData)
    
    return(SuperIndividualsData)
}


ImputeData <- function(
    data, 
    imputeAtMissing = "IndividualAge", 
    imputeByEqual = "IndividualTotalLength", 
    groupBy = "SpeciesCategory", 
    seed = 1, 
    columnNames = NULL, 
    levels = list(
        "Haul", 
        "Stratum", 
        "AllStrata"
    )
) {
    
    # Get the data and add the RowIndex foor use when identifying which rows to impute from:
    dataCopy <- data.table::copy(data)
    RowIndex <- seq_len(nrow(dataCopy))
    dataCopy[, RowIndex := ..RowIndex]
    
    # Introduce an Individual index for use in the sorted sampling:
    dataCopy[, IndividualIndex := as.numeric(as.factor(Individual))]
    
    # Add an AllStrata column to the data to facilitate the AllStrata level: 
    dataCopy[, AllStrata := "AllStrata"]
    #setcolorder(dataCopy, neworder = "AllStrata")
    
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
    dataCopy <- replaceMissingData(dataCopy, columnNames = columnNames)
    
    # Delete the RowIndex and ReplaceRowIndex:
    dataCopy$ReplaceIndividual <- dataCopy[(ReplaceRowIndex), "Individual"]
    dataCopy[, IndividualIndex := NULL]
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
    .SDcols <- c(imputeAtMissing, "ReplaceRowIndex", "ReplaceLevel", "IndividualIndex", "imputeSeed", "RowIndex")
    dataCopy[, 
         c("ReplaceRowIndex", "ReplaceLevel") := getImputeRowIndicesOneGroup(
             .SD, 
             imputeAtMissing = imputeAtMissing, 
             level = level
         ), 
         .SDcols = .SDcols, 
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
            dataCopyOneGroup[!missingData, IndividualIndex], 
            size = NMissingRows, 
            seed = dataCopyOneGroup$imputeSeed[1], 
            replace = TRUE, 
            index.out = TRUE, 
            redraw.seed = TRUE
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



replaceMissingData <- function(x, columnNames) {
    
    # Get the matrix indices of data to replace, which are those that are missing in the rows impute:
    rowsToImpute <- x[!is.na(ReplaceRowIndex), RowIndex]
    rowsToImputeFrom <- x[!is.na(ReplaceRowIndex), ReplaceRowIndex]
    
    # Loop through the columns and replace missing data:
    namesx <- names(x)
    if(!length(columnNames)) {
        columnNames <- namesx
    }
    
    for(columnName in columnNames) {
        if(columnName %in% namesx) {
            atReplacement <- x[rowsToImpute, is.na(get(columnName))] & x[rowsToImputeFrom, !is.na(get(columnName))]
            if(any(atReplacement)) {
                atMissing <- rowsToImpute[atReplacement]
                atPresent <- rowsToImputeFrom[atReplacement]
                replacement <- x[atPresent, get(columnName)]
                x[atMissing, eval(columnName) := replacement]
            }
        }
    }
    
    #
    #x <- as.data.frame(x)
    #atReplace <- is.na(x[rowsToImpute, ]) & !is.na(x[rowsToImputeFrom, ])
    #x[rowsToImpute, ][atReplace] <- x[(rowsToImputeFrom), ][atReplace]
    #
    #x <- data.table::setDT(x)
    return(x)
}





