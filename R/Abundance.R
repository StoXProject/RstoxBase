##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param DensityData The \code{\link{DensityData}} data.
#' @param StratumArea The \code{\link{StratumArea}} data.
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
Abundance <- function(DensityData, StratumArea) {
	
    # Merge the stratum area with the DensityData to an AbundanceData (remove the area at the end of the function):
    AbundanceData <- merge(DensityData, StratumArea, by ="Stratum")
    
    # Multiply the area and the density:
    AbundanceData[, Abundance := Area * Density]
    
    # Keep only the releavnt columns:
    keepOnlyRelevantColumns(AbundanceData, "AbundanceData")
    
    return(AbundanceData)
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @inheritParams DefineSweptAreaPSU
#' @inheritParams RegroupLengthDistribution
#' @param DensityType The type of density, one of "Acoustic" and "SweptArea".
#' @param BioticAssignment The \code{\link{BioticAssignment}} process data.
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
Individuals <- function(StoxBioticData, DensityType = c("Acoustic", "SweptArea"), BioticAssignment, LengthDistributionData) {
    
    # Get the DefinitionMethod:
    DensityType <- match.arg(DensityType)
    
    # Merge StoxBtiotic:
    MergedStoxBioticData <- RstoxData::MergeStoxBiotic(StoxBioticData)
    
    # Get the resolution variables of AbundanceData:
    abundanceResolutionVariables <- getAllResolutionVariables("AbundanceData")
    
    # Get all hauls of each Stratum and Layer:
    if(DensityType == "Acoustic") {
        usedHauls <- BioticAssignment[, .(Haul = unique(Haul)), by = abundanceResolutionVariables]
    }
    else if(DensityType == "SweptArea") {
        usedHauls <- LengthDistributionData[, .(Haul = unique(Haul)), by = abundanceResolutionVariables]
    }
    else {
        stop("Invalid DensityType")
    }
    
    # Expand the vectors of hauls:
    usedHauls <- expandDT(usedHauls)
    
    # Merge individuals into the Stratum, Layer, Haul table:
    IndividualsData <- merge(usedHauls, MergedStoxBioticData, by = "Haul", allow.cartesian = TRUE)
    
    # Remove rows with SpeciesCategory NA (change added on 2020-04-20, since NAs in SpeciesCategory are vital for including all Stations, PSUs and Strata when using meanData(), but should not present when considering individuals):
    IndividualsData <- IndividualsData[!is.na(SpeciesCategory), ]
    
    
    return(IndividualsData)
}




##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @inheritParams RegroupLengthDistribution
#' @param IndividualsData   The 
#' @param AbundanceData Parameter descrption.
#' @param AbundWeightMethod Parameter descrption.
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
SuperIndividuals <- function(IndividualsData, AbundanceData, AbundWeightMethod = c("Equal", "HaulDensity"), LengthDistributionData) {
    # Get the AbundWeightMethod:
    AbundWeightMethod <- match.arg(AbundWeightMethod)
    
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
    if(AbundWeightMethod == "Equal"){
        SuperIndividualsData[, individualCount := as.double(.N), by = abundanceGrouping]
        SuperIndividualsData[, abundanceWeightFactor := 1]
    }
    else if(AbundWeightMethod == "HaulDensity") {
        
        # Give an error if the LengthDistributionType is "Percent" or "Standard":
        validLengthDistributionType <- c("Normalized", "SweepWidthCompensatedNormalized", "SelectivityCompensatedNormalized")
        if(! LengthDistributionData$LengthDistributionType[1] %in% validLengthDistributionType) {
            stop("The LengthDistributionType of the input LengthDistributionData must be one of ", paste(validLengthDistributionType, collapse = ", "))
        }
        
        # Add length groups IDs also in in LengthDistributionData:
        # Make sure the AbundanceData is proper data.table:
        LengthDistributionData <- data.table::setDT(LengthDistributionData)
        addLengthGroupsByReference(data = LengthDistributionData, master = AbundanceData)
        
        
        #addLengthGroups(
        #    data = LengthDistributionData, 
        #    master = AbundanceData, 
        #    lengthVar = "IndividualTotalLengthCentimeter", 
        #    resolutionVar = "LengthResolutionCentimeter"
        #)
        # In case that the length resolution is higher in the LengthDistributionData than in the AbundanceData, uniquify the LengthDistributionData:
        haulGrouping <- c(
            "Haul", 
            getDataTypeDefinition(dataType = "AbundanceData", elements = "categoryVariable", unlist = TRUE), 
            "LengthGroup"
        )
        LengthDistributionData[, WeightedCount := sum(WeightedCount), by = haulGrouping]
        keep <- !duplicated(LengthDistributionData[, ..haulGrouping])
        LengthDistributionData <- subset(LengthDistributionData, keep)
        
        
        # Add the haul density as the WeightedCount to the SuperIndividualsData (requiring Normalized LengthDistributionType):
        SuperIndividualsData <- merge(
            SuperIndividualsData, 
            LengthDistributionData[, c(..haulGrouping, "WeightedCount")], 
            by = haulGrouping
        )
        
        # Multiply the equal abundanceWeightFactor by the haul density divided by its sum for each combination of Stratum, Layer, SpeciesCategory and LengthGroup:
        #sumBy <- c(
        #    getDataTypeDefinition(dataType = "AbundanceData", elements = "categoryVariable", unlist = TRUE), 
        #    "LengthGroup"
        #)
        SuperIndividualsData[, abundanceWeightFactor := WeightedCount / sum(WeightedCount) , by = haulGrouping]
        
        # Get the number of individuals in each Haul:
        SuperIndividualsData[, individualCount := as.double(.N), by = haulGrouping]
        
        # Remove "WeightedCount":
        SuperIndividualsData[, WeightedCount := NULL]
    }
    else{
        stop("Invalid AbundWeightMethod")
    }
    
    # Order by the grouping variables:
    SuperIndividualsData <- setorderv(SuperIndividualsData, abundanceGrouping)
    
    # Multiply by abundance weighting factors:
    #print(SuperIndividualsData[, "abundanceWeightFactor"], 20)
    #print(SuperIndividualsData[, "individualCount"], 20)
    SuperIndividualsData[, Abundance := Abundance * abundanceWeightFactor]
    
    # Divide by the number of individuals (regardless of AbundWeightMethod)
    SuperIndividualsData[, Abundance := Abundance / individualCount]
    
    # Remove the temporary columns:
    toRemove <- c("LengthGroup", "abundanceWeightFactor", "individualCount")
    #SuperIndividualsData[, c(toRemove) := NULL]
    
    # Order 
    toOrderFirst <- c(
        getDataTypeDefinition(dataType = "AbundanceData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable"), unlist = TRUE), 
        "Haul"
    )
    data.table::setcolorder(SuperIndividualsData, toOrderFirst)

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
        data[missingLength, LengthGroup := ..atMissingLengthGroup]
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
    speciesOnlyInMaster <- setdiff(speciesInMaster, speciesInData)
    if(length(speciesOnlyInMaster)) {
        warning("StoX: The species categories ", paste(speciesOnlyInMaster, collapse = ", "), " are present in the master but not in the data")
    }
    # If there are species in the data that are not in the master, report a warning:
    speciesOnlyInData <- setdiff(speciesInMaster, speciesInData)
    if(length(speciesOnlyInMaster)) {
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

