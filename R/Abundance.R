##################################################
##################################################
#' Calculate abundance in the strata
#' 
#' This function calculates abundance as the product of density and stratum area 
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' 
#' @details
#' The function merge the \code{\link{MeanDensityData}} with the \code{\link{StratumAreaData}} by Stratum and calculates the abundance  as the product of density (number by square nautical miles) and area (square nautical miles). 
#' For acoustic-trawl surveys the abundance is calculated by Stratum, Layer, Beam, SpeciesCategory and IndividualTotalLength.
#' In swept-area surveys the abundance is calculated by Stratum, Layer, SpeciesCategory and IndividualTotalLength 
#' 
#' @return
#' An object of StoX data type \code{\link{AbundanceData}}. 
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
    AbundanceData <- data.table::copy(MeanDensityData)
    AbundanceData$Data <- merge(AbundanceData$Data, StratumAreaData, by ="Stratum")
    
    # Multiply the area and the density:
    AbundanceData$Data[, Abundance := Area * Density]
    
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
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param AbundanceType The type of abundance, one of "Acoustic" and "SweptArea".
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
        # Get the PSUs that have positive WeightedCount (changed on 2021-03-09):
        dataVariable <- getDataTypeDefinition("MeanLengthDistributionData", subTable = "Data", elements = "data", unlist = TRUE)
        # Make sure to omit NAs, which indicate hauls which are not tagged to any PSU (e.g. outside of any stratum):
        #PSUs <- MeanLengthDistributionData$Data[get(dataVariable) > 0, unique(PSU)]
        PSUs <- MeanLengthDistributionData$Data[get(dataVariable) > 0, stats::na.omit(unique(PSU))]
        
        # Get the unique rows, while extracting only the Haul and abundance resolution columns:
        usedHauls <- MeanLengthDistributionData$Resolution[PSU %in% PSUs, .(Haul = unique(Haul)), by = abundanceResolutionVariables]
        
        # Remove rows with any NAs:
        usedHauls <- stats::na.omit(usedHauls)
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
    areKeys <- endsWith(names(MergeStoxBioticData), "Key")
    keys <- names(MergeStoxBioticData)[areKeys]
    #formatOutput(IndividualsData, dataType = "IndividualsData", keep.all = TRUE, secondaryColumnOrder = keys)
    formatOutput(IndividualsData, dataType = "IndividualsData", keep.all = TRUE, secondaryColumnOrder = names(MergeStoxBioticData), secondaryRowOrder = keys)
    
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
#' @inheritParams ProcessData
#' @param DistributionMethod The method used for distributing the abundance, one of "Equal" for equal abundance to all individuals of each Stratum, Layer, SpeciesCategory and length group, and "HaulDensity" to weight by the haul density. For \code{DistributionMethod} = "HaulDensity" the \code{LengthDistributionData} must be given. It is recommended to use the same \code{LengthDistributionData} that was used to produce the \code{\link{AbundanceData}} (via \code{link{DensityData}}).
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
    # Make sure the AbundanceData is proper data.table. Comment on 2021-03-18: Why is this needed????:
    AbundanceData$Data <- data.table::setDT(AbundanceData$Data)
    
    # Add length groups to SuperIndividualsData, based on the lengths and resolutions of the AbundanceData:
    addLengthGroup(data = SuperIndividualsData, master = AbundanceData$Data)
    # Add length groups also to the AbundanceData:
    addLengthGroup(data = AbundanceData$Data, master = AbundanceData$Data)
    
    # Merge the AbundanceData into the SuperIndividualsData, by the resolution and category variables of the AbundanceData and the LengthGroup introduced in addLengthGroups().
    # Stratum/Layer/SpeciesCategory/LengthGroup
    mergeBy <- c(
        getDataTypeDefinition(dataType = "AbundanceData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable"), unlist = TRUE), 
        "LengthGroup" # Here we use the temporary LengthGroup variable instead of the groupingVariables_biotic.
    )
    # Get the variables to add from the AbundanceData, which are the Abundance, the surveyDefinition, the vertical dimension, and the acoustic grouping variables:
    # Abundance/Survey/MinLayerDepth/MaxLayerDepth/Beam/Frequency
    variablesToGetFromAbundanceData <- c(
        "Abundance", 
        getDataTypeDefinition(dataType = "AbundanceData", elements = c("surveyDefinition", "verticalLayerDimension", "groupingVariables_acoustic"), unlist = TRUE)
    )
    # Add the mergeBy since these are needed in the merging:
    variablesToGetFromAbundanceData <- unique(c(variablesToGetFromAbundanceData, mergeBy))
    # Keep only variables present in SuperIndividualsData:
    variablesToGetFromAbundanceData <- intersect(variablesToGetFromAbundanceData, names(AbundanceData$Data))
    
    
    # Merge AbundanceData into the IndividualsData
    SuperIndividualsData <- merge(
        SuperIndividualsData, 
        AbundanceData$Data[, ..variablesToGetFromAbundanceData], 
        by = mergeBy, 
        allow.cartesian = TRUE, 
        ### all.y = TRUE # Keep all stata, even those with no acoustic data of the requested species. Using all.y = TRUE will however delete the individuals assigned to PSUs in those strata. We rather use all = TRUE and deal with the NAs (explain the NAs from the data):
        all = TRUE
    )
    
    # Append an individualCount to the SuperIndividualsData, representing the number of individuals in each category given by 'by':
    distributeAbundanceBy <- c(
        getDataTypeDefinition(dataType = "AbundanceData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable", "groupingVariables_acoustic"), unlist = TRUE), 
        "LengthGroup" # Here we use the temporary LengthGroup variable instead of the groupingVariables_biotic.
    )
    # Keep only the variables present in the AbundanceData, as AbundanceData is a flexible datatype which may or may not contain Beam and Frequency (i.e., the groupingVariables_acoustic):
    distributeAbundanceBy <- intersect(distributeAbundanceBy, names(AbundanceData$Data))
    
    # Distributing abundance equally between all individuals of each Stratum, Layer, SpeciesCategory and LengthGroup:
    if(DistributionMethod == "Equal") {
        SuperIndividualsData[, individualCount := as.double(.N), by = distributeAbundanceBy]
        SuperIndividualsData[, haulWeightFactor := 1]
    }
    else if(DistributionMethod == "HaulDensity") {
        
        # Give an error if the LengthDistributionType is "Percent" or "Standard":
        #validLengthDistributionType <- c("Normalized", "SweepWidthCompensatedNormalized", "SelectivityCompensatedNormalized")
        #if(! LengthDistributionData$LengthDistributionType[1] %in% validLengthDistributionType) {
        if(!endsWith(LengthDistributionData$LengthDistributionType[1], "Normalized")) {
            stop("The LengthDistributionType must be \"Normalized\" (ending with \"Normalized\")")
        }
        
        # Add length group IDs also in in LengthDistributionData:
        # Make sure the LengthDistributionData is proper data.table. Comment on 2021-03-18: Why is this needed????:
        LengthDistributionData <- data.table::setDT(LengthDistributionData)
        
        # Make sure to discard Hauls that are not present in the SuperIndividualsData (e.g. outside of any stratum). This is done in order to not try to fit lengths from Hauls that are not used, and that may not be present in the SuperIndividualsData, when creating length groups, as this may lead to errors when using findInterval() to get indices:
        LengthDistributionData <- subset(LengthDistributionData, Haul %in% SuperIndividualsData$Haul)
        
        addLengthGroup(data = LengthDistributionData, master = AbundanceData$Data)
        # We need to unique since there may have been multiple lines in the same length group:
        LengthDistributionData <- unique(LengthDistributionData)
        
        # Sum in each length group (in case lengths are grouped coearser than the original groups):
        haulGrouping <- c(
            "Haul", 
            getDataTypeDefinition(dataType = "SuperIndividualsData", elements = "categoryVariable", unlist = TRUE), 
            "LengthGroup"
        )
        # Add the haul density as the WeightedCount to the SuperIndividualsData (requiring Normalized LengthDistributionType):
        # Added allow.cartesian = TRUE on 2021-02-08 to make this work with acoustic trawl:
        SuperIndividualsData <- merge(
            SuperIndividualsData, 
            LengthDistributionData[, c(..haulGrouping, "WeightedCount")], 
            by = haulGrouping, 
            allow.cartesian = TRUE, 
            # Changed this onn 2021-02-09 to all.x = TRUE, as we only want to keep the individuals, and not add WeightedCount from hauls with no individuals present in the estimation:
            # all.y = TRUE
            all.x = TRUE
        )
        
        # Sum the haul densities (stored as WeightedCount) over all hauls of each Stratum/Layer/SpeciesCategory/LengthGroup/Frequency/Beam:
        # No no, this was certainly wrong, as WeightedCount could be duplicated within a Stratum for one length group:
        #SuperIndividualsData[, sumWeightedCount := sum(unique(WeightedCount), na.rm = TRUE), by = distributeAbundanceBy]
        # Sum the WeightedCount over all hauls of the columns specified by distributeAbundanceBy:
        SuperIndividualsData[, sumWeightedCount := sum(WeightedCount[!duplicated(Haul)], na.rm = TRUE), by = distributeAbundanceBy]
        
        # Get the Haul weight factor as the WeightedCount divided by sumWeightedCount:
        SuperIndividualsData[, haulWeightFactor := WeightedCount / sumWeightedCount]
        
        # Get the number of length measured individuals of the length group:
        SuperIndividualsData[, individualCount := .N, by = haulGrouping]
        
        # Remove WeightedCount and sumWeightedCount:
        SuperIndividualsData[, WeightedCount := NULL]
        SuperIndividualsData[, sumWeightedCount := NULL]
    }
    else{
        stop("Invalid DistributionMethod")
    }
    
    # Multiply by abundance weighting factors:
    SuperIndividualsData[, Abundance := Abundance * haulWeightFactor]
    
    # Divide by the number of individuals (regardless of DistributionMethod)
    SuperIndividualsData[, Abundance := Abundance / individualCount]
    
    # Add Biomass:
    SuperIndividualsData[, Biomass := Abundance * IndividualRoundWeight]
    
    # Format the output but keep all columns:
    #formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE)
    # Order the columns, but keep all columns. Also add the names of the MergeStoxBioticData as secondaryColumnOrder to tidy up by moving the Haul column (used as by in the merging) back into its original position:
    areKeys <- endsWith(names(SuperIndividualsData), "Key")
    keys <- names(SuperIndividualsData)[areKeys]
    #formatOutput(IndividualsData, dataType = "IndividualsData", keep.all = TRUE, secondaryColumnOrder = keys)
    formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE, secondaryColumnOrder = unlist(attr(IndividualsData, "stoxDataVariableNames")), secondaryRowOrder = keys)
    
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
    
    # Not needed here, since we only copy data: 
    #Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(SuperIndividualsData)
    
    return(SuperIndividualsData)
}


# Function to add length group IDs in two tables given the length groups in the second:
addLengthGroupOneSpecies <- function(
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
    
    # Not needed, as similar check is done in addLengthGroup():
    #if(length(atSpeciesInData) == 0 || length(atSpeciesInMaster) == 0) {
    #    stop("The species ", species, " is not present in both data and master.")
    #}
    
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
addLengthGroup <- function(
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
    speciesOnlyInMaster <- stats::na.omit(setdiff(speciesInMaster, speciesInData))
    if(length(speciesOnlyInMaster)) {
        warning("StoX: The species categories ", paste(speciesOnlyInMaster, collapse = ", "), " are present in the master but not in the data")
    }
    # If there are species in the data that are not in the master, report a warning:
    speciesOnlyInData <- stats::na.omit(setdiff(speciesInMaster, speciesInData))
    if(length(speciesOnlyInData)) {
        warning("StoX: The species categories ", paste(speciesOnlyInData, collapse = ", "), " are present in the data but not in the master. These species categories will be removed from the output.")
    }

    # Keep only the common species:    
    species <- intersect(speciesInData, speciesInMaster)
    
    if(!length(species)) {
        data[, LengthGroup := NA_integer_]
    }
    else {
        for(thisspecies in species) {
            addLengthGroupOneSpecies(
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
#' Impute missing individual data
#' 
#' This function identifies individuals with missing value in the column gievn by \code{ImputeAtMissing} (defaulted to "IndividualAge"), and imputes the variables specified by \code{ToImpute} by the method specified by \code{ImputationMethod} (defaulted to "RandomSampling", which currently is the only option) from individuals with values identical to the target individual in all of the columns specified by \code{ImputeByEqual} (defaulted to c("IndividualTotalLength", "SpeciesCategory")). In the imputation, individual to impute from are randomly selected from individuals at the same Haul, then the same Stratum if no individuals with identical values in \code{ImputeByEqual} are found, and finally the same Survey. If no individuals are found, the missing values are not imputed, but kept as missing values (currently with no warning).
#' 
#' @inheritParams ModelData
#' @param ImputationMethod The method to use for the imputation. Currently, only "RandomSampling" is implemented, but may be accompanied "Regression" in a coming release.
#' @param ImputeAtMissing The name of the variable identifying which individuals to impute data to. In StoX 3.0.0 and older, this was hard coded to IndividualAge.
#' @param ImputeByEqual The name of the variables identifying which individuals to impute data from. In StoX 3.0.0 and older, this was hard coded to IndividualTotalLength.
#' @param ToImpute The name of the variable(s) to impute. In StoX 3.0.0 and older, this was hard coded to all available variables of the BioticData contained in the \code{\link{SuperIndividualsData}}. The variable specified by \code{ImputeAtMissing} is always imputed, and need not be included in \code{ToImpute}.
#' @param Seed An integer giving the seed to use for the random sampling used to obtain the imputed data.
#' 
#' @return
#' An object of StoX data type \code{\link{SuperIndividualsData}}. 
#' 
#' @seealso \code{\link{SuperIndividuals}} for distributing Abundance to the Individuals.
#' 
#' @export
#' 
ImputeSuperIndividuals <- function(
    SuperIndividualsData, 
    ImputationMethod = c("RandomSampling"), 
    ImputeAtMissing = character(), 
    ImputeByEqual = character(), 
    ToImpute = character(), 
    Seed = 1
) {
    
    # Check that the length resolution is constant: 
    if(!allEqual(SuperIndividualsData$LengthResolution)) {
        stop("All individuals must have identical LengthResolution in the current version.")
    }
    
    if(!length(ToImpute)) {
        warning("StoX: Empty ToImpute is deprecated. Please select the variables to impute explicitely.")
        ToImpute <- getIndividualNames(SuperIndividualsData, ImputeByEqual,  tables = "Individual")
    }
    if(!ImputeAtMissing %in% ToImpute) {
        stop("Please specify the variable given by ImputeAtMissing (", ImputeAtMissing, ") in ToImpute. ")
    }
    # For safety uniquify:
    ToImpute <- unique(ToImpute)
    
    if(length(ImputeAtMissing) != 1) {
        stop("ImputeAtMissing must have length 1.")
    }
    
    # Impute the SuperIndividualsData:
    ImputeSuperIndividualsData <- ImputeData(
        data = SuperIndividualsData, 
        imputeAtMissing = ImputeAtMissing, 
        imputeByEqual = ImputeByEqual, 
        seed = Seed, 
        columnNames = ToImpute
    )
    
    # Re-calculate the Biomass:
    ImputeSuperIndividualsData[, Biomass := Abundance * IndividualRoundWeight]
    
    # Format the output but keep all columns:
    #formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE)
    # Order the columns, but keep all columns. Also add the names of the MergeStoxBioticData as secondaryColumnOrder to tidy up by moving the Haul column (used as by in the merging) back into its original position:
    areKeys <- endsWith(names(ImputeSuperIndividualsData), "Key")
    keys <- names(ImputeSuperIndividualsData)[areKeys]
    #formatOutput(IndividualsData, dataType = "IndividualsData", keep.all = TRUE, secondaryColumnOrder = keys)
    formatOutput(ImputeSuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE, secondaryColumnOrder = unlist(attr(SuperIndividualsData, "stoxDataVariableNames")), secondaryRowOrder = keys)
    
    # Not needed here, since we only copy data: 
    #Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(SuperIndividualsData)
    
    return(ImputeSuperIndividualsData)
}


ImputeData <- function(
    data, 
    imputeAtMissing = "IndividualAge", 
    imputeByEqual = c("IndividualTotalLength", "SpeciesCategory"), 
    seed = 1, 
    columnNames = NULL, 
    levels = list(
        "Haul", 
        "Stratum", 
        "Survey"
    )
) {
    
    # Get the data and add the RowIndex for use when identifying which rows to impute from:
    dataCopy <- data.table::copy(data)
    #RowIndex <- seq_len(nrow(dataCopy))
    #dataCopy[, RowIndex := ..RowIndex]
    
    # Introduce an Individual index for use in the sorted sampling:
    dataCopy[, IndividualIndex := as.numeric(as.factor(Individual))]
    
    ## Add an AllStrata column to the data to facilitate the AllStrata level: 
    #dataCopy[, AllStrata := "AllStrata"]
    ##setcolorder(dataCopy, neworder = "AllStrata")
    
    # Get a vector with the seed of each level:
    seedVector <- structure(as.list(getSeedVector(size = length(levels), seed = seed)), names = levels)
    
    dataCopy[, ReplaceIndividualIndex := NA_integer_]
    dataCopy[, ReplaceLevel := NA_character_]
    
    for(level in levels) {
        # Get the vector of columns to impute by:
        by <- c(level, imputeByEqual)
        
        # Get the table of seeds for each unique combination of the columns defined by 'by':
        seedTable <- unique(dataCopy[, ..by])
        data.table::setorder(seedTable)
        seedTable <- data.table::data.table(
            seedTable, 
            imputeSeed = getSeedVector(size = nrow(seedTable), seed = seedVector[[level]])
        )
        
        # Add the seeds to the data (recycled). Use all.x = TRUE as there is no need to include any unwanted rows from the seedTable (althouhg this iss unlikely to happen):
        dataCopy <- merge(dataCopy, seedTable, by = by, all.x = TRUE, sort = FALSE)
        
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
    
    # Reset to original order:
    data.table::setorderv(dataCopy, "IndividualIndex")
    
    # Get the ReplaceIndividual:
    dataCopy[, ReplaceIndividual := Individual[match(ReplaceIndividualIndex, IndividualIndex)]]
    
    # Delete the IndividualIndex and ReplaceIndividualIndex:
    dataCopy[, IndividualIndex := NULL]
    #dataCopy[, RowIndex := NULL]
    dataCopy[, ReplaceIndividualIndex := NULL]
    #dataCopy[, AllStrata := NULL]
    
    return(dataCopy)
}

# Function to get the imputation row indices of one level ("Haul", "Stratum", NULL). This function is applied using for loop over the levels:
getImputeRowIndicesOneLevel <- function(
    dataCopy, 
    imputeAtMissing = "Age", 
    level = "Haul", 
    by
) {
    # Get the row indices to replace data from by applying the function getImputeRowIndicesOneGroup by the level (one of Haul, Stratum, NULL) and the imputeByEqual input. 
    .SDcols <- c(imputeAtMissing, "ReplaceIndividualIndex", "ReplaceLevel", "IndividualIndex", "imputeSeed")
    
    dataCopy[, 
         c("ReplaceIndividualIndex", "ReplaceLevel") := getImputeRowIndicesOneGroup(
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
    
    # Get the super individuals with missing data (and which have not been given ReplaceIndividualIndex):
    missingData <- dataCopyOneGroup[, is.na(get(imputeAtMissing)) & is.na(ReplaceIndividualIndex)]
    # This is assuming unique individuals for each Stratum, Layer, SpeciesCategory and length group:
    presentData <- dataCopyOneGroup[, !is.na(get(imputeAtMissing))]
    
    # Get the number of missing and present rows:
    NMissingRows <- sum(missingData)
    NPresentRows <- sum(presentData)
    
    # We choose (as it may be cleaner) to create the output row indices as a vector of NAs, instead of using data.table:
    ReplaceIndividualIndex <- dataCopyOneGroup$ReplaceIndividualIndex
    ReplaceLevel <- dataCopyOneGroup$ReplaceLevel
    
    if(NMissingRows > 0 && NPresentRows > 0) {
        # Sample the rows with present data:
        #sampleIndexInPresent <- sample.int(NPresentRows, NMissingRows, replace = TRUE)
        ### sampleIndexInPresent <- sampleSorted(
        ###     #dataCopyOneGroup[!missingData, IndividualIndex], 
        ###     dataCopyOneGroup[presentData, IndividualIndex], 
        ###     size = NMissingRows, 
        ###     seed = dataCopyOneGroup$imputeSeed[1], 
        ###     replace = TRUE, 
        ###     index.out = TRUE, 
        ###     redraw.seed = TRUE
        ### )
        ### ReplaceRowIndex[missingData] <- dataCopyOneGroup[presentData, RowIndex][sampleIndexInPresent]
        
        ReplaceIndividualIndex[missingData] <- sampleSorted(
            #dataCopyOneGroup[!missingData, IndividualIndex], 
            dataCopyOneGroup[presentData, IndividualIndex], 
            size = NMissingRows, 
            seed = dataCopyOneGroup$imputeSeed[1], 
            replace = TRUE, 
            #index.out = TRUE, 
            index.out = FALSE#, 
            #redraw.seed = TRUE
        )
        # Add also the replace level:
        ReplaceLevel[missingData] <- level
    }
    
    return(
        list(
            ReplaceIndividualIndex = ReplaceIndividualIndex, 
            ReplaceLevel = ReplaceLevel
        )
    )
}



replaceMissingData <- function(x, columnNames) {
    
    # Get the matrix indices of data to replace, which are those that are missing in the rows impute:
    #rowsToImpute <- x[!is.na(ReplaceRowIndex), RowIndex]
    #rowsToImputeFrom <- x[!is.na(ReplaceRowIndex), ReplaceRowIndex]
    # Changed on 2021-02-09 to match the IndividualIndex and ReplaceIndividualIndex:
    
    # Get the rows to impute, i.e., those with non-missing ReplaceIndividualIndex:
    rowsToImpute <- x[, which(!is.na(ReplaceIndividualIndex))]
    # Get the rows to replace from, by matching the ReplaceIndividualIndex with the IndividualIndex, and keep only those to impute:
    rowsToImputeFrom <- match(x$ReplaceIndividualIndex, x$IndividualIndex)
    rowsToImputeFrom <- rowsToImputeFrom[rowsToImpute]
    
    # Loop through the columns and replace missing data:
    namesx <- names(x)
    if(!length(columnNames)) {
        columnNames <- namesx
    }
    
    for(columnName in columnNames) {
        if(columnName %in% namesx) {
            # Locate the inidices at which the data in the column given by columnName is NA in the rows to imput and not NA in the rows to impute from:
            atReplacement <- x[rowsToImpute, is.na(get(columnName))] & x[rowsToImputeFrom, !is.na(get(columnName))]
            if(any(atReplacement)) {
                atMissing <- rowsToImpute[atReplacement]
                atPresent <- rowsToImputeFrom[atReplacement]
                replacement <- x[atPresent, get(columnName)]
                x[atMissing, eval(columnName) := replacement]
            }
        }
    }
    
    return(x)
}





