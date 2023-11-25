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
#' For acoustic-trawl estimates the abundance is calculated by Stratum, Layer, Beam, SpeciesCategory and IndividualTotalLength.
#' In swept-area estimates the abundance is calculated by Stratum, Layer, SpeciesCategory and IndividualTotalLength 
#' 
#' For swept-area estimates the density calculated by \code{\link{SweptAreaDensity}} can be given as "AreaWeightDensity" (as indicated by the column \code{DensityType}). In this case the \code{Biomass} column and not the \code{Abundance} column is populated in the output \code{\link{QuantityData}} from this function. The biomass is given in kilogram in this case, as Density is in kilogram per square nautical mile if \code{DensityType} = "AreaWeightDensity". This is different from the \code{Biomass} column of \code{\link{SuperIndividualsData}}, which is in gram, as it is generated from IndividualRoundWeight which is in gram.
#' 
#' 
#' 
#' @return
#' An object of StoX data type \code{\link{QuantityData}}. The \code{\link{QuantityData}} contains both an Abundance and a Biomass column, but only one of these can be populated.
#' 
#' @seealso \code{\link{SuperIndividuals}} for distributing Abundance to individuals.
#' 
#' @export
#' 
Quantity <- function(
    MeanDensityData, 
    StratumAreaData
) {
    
    # Merge the stratum area with the DensityData to an QuantityData (remove the area at the end of the function):
    QuantityData <- data.table::copy(MeanDensityData)
    # Added all.x for StoX 3.2.0, as the default all = FALSE drops strata not present in StratumAreaData (particularly NA stratum):
    QuantityData$Data <- merge(QuantityData$Data, StratumAreaData, by ="Stratum", all.x = TRUE)
    
    # Multiply the area and the density:
    if(MeanDensityData$Data$DensityType[1] == "AreaNumberDensity") {
        QuantityData$Data[, Abundance := Area * Density]
        QuantityData$Data[, Biomass := NA_real_]
    }
    else if(MeanDensityData$Data$DensityType[1] == "AreaWeightDensity") {
        QuantityData$Data[, Abundance := NA_real_]
        QuantityData$Data[, Biomass := Area * Density]
    }
    
    # Format the output:
    # Changed added on 2020-10-16, where the datatypes DensityData and QuantityData are now considered non-rigid:
    #formatOutput(QuantityData, dataType = "QuantityData", keep.all = FALSE)
    formatOutput(QuantityData, dataType = "QuantityData", keep.all = FALSE, allow.missing = TRUE)
    
    return(QuantityData)
}


##################################################
##################################################
#' Indivduals to distribute abundance to to create super-individuals
#' 
#' This function defines and returns the individuals used in the estimation model, to which to distribute the abundance to create super-individuals.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param QuantityType The type of abundance, one of "Acoustic" and "SweptArea".
#' 
#' @details 
#' Individuals are retrieved from only Hauls that contribute to the Abundance. For QuantityType = "Acoustic" these are the Hauls that have positive WeightingFactor in the BioticAssignment input, and for QuantityType = "SweptArea" the Hauls that have positive WeightedNumber.
#' 
#' The \code{\link{IndividualsData}} contains variables from \code{\link{StoxBioticData}} in addition to the columns Stratum and Layer. The Stratum column is not necessarily the actual stratum containing the Haul in which an individual was sampled, but rather the stratum linked to the haul via \code{\link{DefineBioticAssignment}} for acoustic-trawl models and \code{\link{MeanLengthDistribution}} for swept-area models. In detail:
#' * DefineAcousticPSU(): Defines acoustic PSUs, which are assigned to a stratum, and that containns EDSUs, possibly from other strata than the assigned stratum.
#' * DefineBioticPSU(): Defines biotic PSUs, which are assigned to a stratum, and that conntainns Stations, possibly from other strata than the assigned stratum.
#' * DefineBioticAssignment(): Assignes hauls to acoustic PSUs, possibly from other strata than the stratum assigned to the acoustic PSU. In this datatype it is possible that EDSUs of an acoustic PSU are located in different strata, or even that all the EDSUs of the acoustic PSU are located in another stratum than the assigned stratum; and that the hauls assigned to the acoustic PSU are located in yet another stratum.
#' * In Individuals() the StoxBioticData are merged with BioticAssignment in the case of acoustic-trawl models and with MeanLengthDistributionData in the case of swept-area models, by the Haul identifier stored in the StoxBioticData, the BioticAssignment, and in the Resolution table of the MeanLengthDistributionData. As the hauls may be linked to a different statum than the one containing the haul, as per the description of DefineBioticAssignment() above, the Stratum column of the IndividualsData may not correspond to the actual stratum of the haul.
#' @md
#' 
#' @return 
#' An object of StoX datatype \code{\link{IndividualsData}}.
#' 
#' @seealso \code{\link{SuperIndividuals}} for distributing Abundance to the Individuals.
#' 
#' @export
#' 
Individuals <- function(
    StoxBioticData, 
    QuantityType = c("Acoustic", "SweptArea"), 
    BioticAssignment, 
    MeanLengthDistributionData
) {

    # Get the DefinitionMethod:
    QuantityType <- RstoxData::match_arg_informative(QuantityType)
    
    # Merge StoxBtiotic:
    MergeStoxBioticData <- RstoxData::MergeStoxBiotic(StoxBioticData)
    
    # Get the resolution variables of QuantityData:
    abundanceResolutionVariables <- getResolutionVariables("QuantityData")
    
    # Get all hauls of each Stratum and Layer:
    if(QuantityType == "Acoustic") {
        # Get the hauls with positive WeightingFactor:
        usedHauls <- BioticAssignment[WeightingFactor > 0, .(Haul = unique(Haul)), by = abundanceResolutionVariables]
    }
    else if(QuantityType == "SweptArea") {
        # Get the PSUs that have positive WeightedNumber (changed on 2021-03-09):
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
    
    # Add the attribute 'stoxDataVariableNames':
    #setattr(
    #    IndividualsData, 
    #    "stoxDataVariableNames",
    #    attr(MergeStoxBioticData, "stoxDataVariableNames")
    #)
    attr(IndividualsData, "stoxDataVariableNames") <- attr(MergeStoxBioticData, "stoxDataVariableNames")
    
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
#' @param DistributionMethod The method used for distributing the abundance, one of "Equal" for equal abundance to all individuals of each Stratum, Layer, SpeciesCategory and length group, and "HaulDensity" to weight by the haul density. For \code{DistributionMethod} = "HaulDensity" the \code{LengthDistributionData} must be given. It is recommended to use the same \code{\link{LengthDistributionData}} that was used to produce the \code{\link{QuantityData}} (via \code{\link{DensityData}}). If the length resolution is not the same in the \code{\link{QuantityData}} and \code{\link{LengthDistributionData}}, an error will be thrown.
#' 
#' @details The \code{\link{SuperIndividualsData}} contains the variables \code{Abundance} and \code{Biomass}. The \code{Biomass} is given in gram, as it is generated from IndividualRoundWeight which is in gram. This is different from the \code{Biomass} column of \code{\link{QuantityData}}, which is in kilogram, as it is originates from \code{CatchFractionWeight} in \code{\link{StoxBioticData}}, which is in kilogram.
#' 
#' @seealso \code{\link{Individuals}}, \code{\link{Quantity}} and \code{\link{LengthDistribution}} for generating the input to this function.
#' 
#' @export
#' 
SuperIndividuals <- function(
    IndividualsData, 
    QuantityData, 
    DistributionMethod = c("Equal", "HaulDensity"), 
    LengthDistributionData
) {
    
    # Get the DistributionMethod:
    DistributionMethod <- RstoxData::match_arg_informative(DistributionMethod)
    
    # Make a copy of the IndividualsData:
    SuperIndividualsData <- data.table::copy(IndividualsData)
    # Make sure the QuantityData is proper data.table. Comment on 2021-03-18: Why is this needed????:
    QuantityData$Data <- data.table::setDT(QuantityData$Data)
    
    #### This is a potentially backward reproducibility breaking change, scheduled for 4.0.0: ####
    # Subset to the positive data, as this can discard length groups that have been filtered out in Indivduals when computing the PSUs:
    # In Individuals() only Hauls with BioticAssignment$WeightingFactor > 0 for QuantityType "Acoustic" and only Abundance > 0 for "SweptArea":
    #QuantityData$Data <- subset(QuantityData$Data, Abundance > 0 | is.na(Abundance))
    # Rather we should maybe change bootstrapping to exclude PSUs that are not resampled, instead of including them with 0 in the datavariable.
    
    # Add length groups to SuperIndividualsData, based on the lengths and resolutions of the QuantityData:
    addLengthGroup(data = SuperIndividualsData, master = QuantityData$Data, warn = FALSE)
    # Add length groups also to the QuantityData:
    addLengthGroup(data = QuantityData$Data, master = QuantityData$Data, warn = FALSE)
    
    # Merge the QuantityData into the SuperIndividualsData, by the resolution and category variables of the QuantityData and the TempLengthGroupUsedInSuperIndividuals introduced in addLengthGroups().
    # Stratum/Layer/SpeciesCategory/TempLengthGroupUsedInSuperIndividuals
    mergeBy <- c(
        getDataTypeDefinition(dataType = "QuantityData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable"), unlist = TRUE), 
        "TempLengthGroupUsedInSuperIndividuals" # Here we use the temporary LengthGroup variable instead of the groupingVariables_biotic.
    )
    # Get the variables to add from the QuantityData, which are the Abundance, the surveyDefinition, the vertical dimension, and the acoustic grouping variables:
    # Abundance/Survey/MinLayerDepth/MaxLayerDepth/Beam/Frequency
    variablesToGetFromQuantityData <- c(
        "Abundance", 
        getDataTypeDefinition(dataType = "QuantityData", elements = c("surveyDefinition", "verticalLayerDimension", "groupingVariables_acoustic"), unlist = TRUE)
    )
    # Add the mergeBy since these are needed in the merging:
    variablesToGetFromQuantityData <- unique(c(variablesToGetFromQuantityData, mergeBy))
    # Keep only variables present in SuperIndividualsData:
    variablesToGetFromQuantityData <- intersect(variablesToGetFromQuantityData, names(QuantityData$Data))
    
    # Merge QuantityData into the IndividualsData:
    SuperIndividualsData <- merge(
        SuperIndividualsData, 
        QuantityData$Data[, ..variablesToGetFromQuantityData], 
        by = mergeBy, 
        allow.cartesian = TRUE, 
        ### all.y = TRUE # Keep all data, even those with no acoustic data of the requested species. Using all.y = TRUE will however delete the individuals assigned to PSUs in those strata. We rather use all = TRUE and deal with the NAs (explain the NAs from the data):
        # Changed this onn 2021-02-09 to all.x = TRUE, as we only want to keep the individuals, and not add WeightedNumber from hauls with no individuals present in the estimation (after bootstrapping):
        # all.y = TRUE
        
        # Reverting to all = TRUE, since all.x = TRUE had the unwanted effect that the behavior of seed changed due to the weak code in ImputeData where data.table::data.table is used to cbind uniqueKeys, and imputeSeed, whereas a seed per pasted unique combination of the keys, treated as factor with platform independent sorting should be use. Also, we are not adding Abundance to the individuals, but rather distributing ALL OF THE Abundance to inidividuals:
        #all.x = TRUE
        all = TRUE
    )
    
    
    # Append an individualNumber to the SuperIndividualsData, representing the number of individuals in each category given by 'by':
    distributeQuantityBy <- c(
        getDataTypeDefinition(dataType = "QuantityData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable", "groupingVariables_acoustic"), unlist = TRUE), 
        "TempLengthGroupUsedInSuperIndividuals" # Here we use the temporary LengthGroup variable instead of the groupingVariables_biotic.
    )
    # Keep only the variables present in the QuantityData, as QuantityData is a flexible datatype which may or may not contain Beam and Frequency (i.e., the groupingVariables_acoustic):
    distributeQuantityBy <- intersect(distributeQuantityBy, names(QuantityData$Data))
    
    # Distributing abundance equally between all individuals of each Stratum, Layer, SpeciesCategory and TempLengthGroupUsedInSuperIndividuals:
    if(DistributionMethod == "Equal") {
        SuperIndividualsData[, individualNumber := as.double(.N), by = distributeQuantityBy]
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
        
        # This test was too hard, as bootstrapping can prooduce different length groups in QuantityData:
        # # Require the exact same length resolutions in the LengthDistributionData as in the QuantityData$Data:
        # lengthGroups_LengthDistributionData <- unique(LengthDistributionData[, c("IndividualTotalLength", "LengthResolution")])
        # lengthGroups_LengthDistributionData <- subset(lengthGroups_LengthDistributionData, !is.na(IndividualTotalLength))
        # lengthGroups_LengthDistributionData <- lengthGroups_LengthDistributionData[, do.call(paste, .SD)]
        # lengthGroups_QuantityData <- unique(QuantityData$Data[, c("IndividualTotalLength", "LengthResolution")])
        # lengthGroups_QuantityData <- subset(lengthGroups_QuantityData, !is.na(IndividualTotalLength))
        # lengthGroups_QuantityData <- lengthGroups_QuantityData[, do.call(paste, .SD)]
        # if(!all(lengthGroups_QuantityData %in% lengthGroups_LengthDistributionData)) {
        #     stop("The length resolution of the LengthDistributionData must match that of the QuantityData.")
        # }
        
        # Add the TempLengthGroupUsedInSuperIndividuals variable:
        addLengthGroup(data = LengthDistributionData, master = QuantityData$Data, warn = FALSE)
        
        # We need to unique since there may have been multiple lines in the same length group:
        LengthDistributionData <- unique(LengthDistributionData)
        
        # Sum in each length group (in case lengths are grouped coarser than the original groups):
        haulGrouping <- c(
            "Haul", 
            getDataTypeDefinition(dataType = "SuperIndividualsData", elements = "categoryVariable", unlist = TRUE), 
            "TempLengthGroupUsedInSuperIndividuals"
        )
        # Add the haul density as the WeightedNumber to the SuperIndividualsData (requiring Normalized LengthDistributionType):
        # Added allow.cartesian = TRUE on 2021-02-08 to make this work with acoustic trawl:
        SuperIndividualsData <- merge(
            SuperIndividualsData, 
            LengthDistributionData[, c(..haulGrouping, "WeightedNumber")], 
            by = haulGrouping, 
            allow.cartesian = TRUE, 
            # Changed this onn 2021-02-14 to all.x = TRUE, as we only want to keep the individuals, and not add WeightedNumber from hauls with no individuals present in the estimation:
            # all.y = TRUE
            all.x = TRUE
        )
        
        # Check that no Abundance is linked to a species for missing individuals (missing length group, hence missing WeightedNumber):
        atMissisngLengthButPresentAbundance <- SuperIndividualsData[, 
            which(
                Abundance > 0 & 
                is.na(WeightedNumber) & 
                !is.na(SpeciesCategory)
            )]
        if(length(atMissisngLengthButPresentAbundance)) {
            warning("StoX: There are rows of the QuantityData with positive Abundance for a species that cannot be matched to any individuals of the IndividualsData. The reason for this may be that there are samples in the StoxBioticData used as input to the LengthDistribution process earlier in the Baseline model that have positive SampleNumber but no individuals. The consequence is that Abundance is set to NA in SuperIndividualsData for the following Stratum Layer and SpeciesCategory, resulting in loss of Abundance: ", paste(SuperIndividualsData[atMissisngLengthButPresentAbundance, paste0("Stratum: ", Stratum, ", Layer: ", Layer, ", SpeciesCategory: ", SpeciesCategory)], collapse = "\n"))
        }
        
        # Sum the haul densities (stored as WeightedNumber) over all hauls of each Stratum/Layer/SpeciesCategory/TempLengthGroupUsedInSuperIndividuals/Frequency/Beam:
        SuperIndividualsData[, sumWeightedNumber := sum(WeightedNumber[!duplicated(Haul)], na.rm = FALSE), by = distributeQuantityBy]
        
        # Get the Haul weight factor as the WeightedNumber divided by sumWeightedNumber:
        SuperIndividualsData[, haulWeightFactor := WeightedNumber / sumWeightedNumber]
        
        # Do not scale Abundance for missing Haul, as the whole point of the DistributionMethod == "HaulDensity" is to weight by the haul density, which is not relevant for missing Haul:
        SuperIndividualsData[is.na(Haul), haulWeightFactor := 1]
        
        # Count the length measured individuals of each Haul, species, beam and length group:
        #SuperIndividualsData[, individualNumber := .N, by = haulGrouping] # This was an error, since it did not take different beams into account.
        countGrouping <- c(
            getDataTypeDefinition(dataType = "QuantityData", elements = c("horizontalResolution", "verticalResolution"), unlist = TRUE), 
            "Haul", 
            getDataTypeDefinition(dataType = "SuperIndividualsData", elements = "categoryVariable", unlist = TRUE), 
            getDataTypeDefinition(dataType = "QuantityData", elements = "groupingVariables_acoustic", unlist = TRUE), 
            "TempLengthGroupUsedInSuperIndividuals" # Here we use the temporary LengthGroup variable instead of the groupingVariables_biotic.
        )
        # Keep only the variables present in the QuantityData, as QuantityData is a flexible datatype which may or may not contain Beam and Frequency (i.e., the groupingVariables_acoustic):
        countGrouping <- intersect(countGrouping, names(SuperIndividualsData))
        SuperIndividualsData[, individualNumber := .N, by = countGrouping]
        
        # Remove WeightedNumber and sumWeightedNumber:
        removeColumnsByReference(
            data = SuperIndividualsData, 
            toRemove = c("WeightedNumber", "sumWeightedNumber")
        )
    }
    else{
        stop("Invalid DistributionMethod")
    }
    
    
    # Generate the weighting factors:
    SuperIndividualsData[, individualWeightFactor := haulWeightFactor / individualNumber]
    
    # ... and check that they sum to 1:
    SuperIndividualsData[, sumIndividualWeightFactor := sum(individualWeightFactor), by = distributeQuantityBy]
    if(!testEqualTo1(SuperIndividualsData$sumIndividualWeightFactor)) {
        stop("An error occurred causing weights to not sum to 1. This could be due to mismatch between the LengthDistributionData used in SuperIndividuals() and the LengthDistributionData used to derive the QuantityData.")
    }
    SuperIndividualsData[, sumIndividualWeightFactor := NULL]
    
    # Multiply by abundance weighting factors:
    #SuperIndividualsData[, Abundance := Abundance * haulWeightFactor]
    
    # Divide by the number of individuals (regardless of DistributionMethod)
    #SuperIndividualsData[, Abundance := Abundance / individualNumber]
    SuperIndividualsData[, Abundance := Abundance * individualWeightFactor]
    
    # Test whether the sum of the Abundance is equal to that of the QuantityData. Note that if all SuperIndividualsData$Abundance and all QuantityData$Data$Abundance are NA, the totalAbundanceInSuperIndividualsData and totalAbundanceInQuantityData will be 0 due to the na.rm = TRUE. Before, testEqualTo1() was used here, but this returned NA for 0 / 0 = NaN, and the condition in the test failed. Thus the testEqual() is used instead:
    totalAbundanceInSuperIndividualsData <- sum(SuperIndividualsData$Abundance, na.rm = TRUE)
    totalAbundanceInQuantityData <- sum(QuantityData$Data$Abundance, na.rm = TRUE)
    if(!testEqual(totalAbundanceInSuperIndividualsData, totalAbundanceInQuantityData)) {
        stop("Sum of Abundance not equal in QuantityData (", totalAbundanceInQuantityData, ") and SuperIndividualsData (", totalAbundanceInSuperIndividualsData, ").")
    }
    
    SuperIndividualsData[, individualWeightFactor := NULL]
    
    
    # Add Biomass, where missing weigth is accepted if Abundane is 0:
    SuperIndividualsData[, Biomass := ifelse(Abundance %in% 0, 0, Abundance * IndividualRoundWeight)]
    
    # Format the output but keep all columns:
    #formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE)
    # Order the columns, but keep all columns. Also add the names of the MergeStoxBioticData as secondaryColumnOrder to tidy up by moving the Haul column (used as by in the merging) back into its original position:
    areKeys <- endsWith(names(SuperIndividualsData), "Key")
    keys <- names(SuperIndividualsData)[areKeys]
    #formatOutput(IndividualsData, dataType = "IndividualsData", keep.all = TRUE, secondaryColumnOrder = keys)
    formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE, secondaryColumnOrder = unlist(attr(IndividualsData, "stoxDataVariableNames")), secondaryRowOrder = keys)
    
    # Remove the columns "individualNumber" and "abundanceWeightFactor", manually since the data type SuperIndividualsData is not uniquely defined (contains all columns of StoxBiotic):
    #SuperIndividualsData[, haulWeightFactor := NULL]
    #SuperIndividualsData[, individualNumber := NULL]
    ##SuperIndividualsData[, abundanceWeightFactor := NULL]
    #SuperIndividualsData[, TempLengthGroupUsedInSuperIndividuals := NULL]
    removeColumnsByReference(
        data = SuperIndividualsData, 
        toRemove = c("haulWeightFactor", "individualNumber", "TempLengthGroupUsedInSuperIndividuals")
    )
    
    ## Order the rows:
    #orderDataByReference(SuperIndividualsData, "SuperIndividualsData")
    
    
    # Add the attribute 'variableNames':
    setattr(
        SuperIndividualsData, 
        "stoxDataVariableNames",
        attr(IndividualsData, "stoxDataVariableNames")
    )
    
    return(SuperIndividualsData)
}


testEqual <- function(x, y, tol = sqrt(.Machine$double.eps)) {
    notNA <- !is.na(x) & !is.na(y)
    if(any(notNA)) {
        ratio <- y[notNA] / x[notNA]
        all(ratio > 1 - tol & ratio < 1 + tol | (x[notNA] == 0 & y[notNA] == 0))
    }
    else {
        NA
    }
}

testEqualTo1 <- function(x, tol = sqrt(.Machine$double.eps)) {
    notNA <- !is.na(x)
    if(any(notNA)) {
        all(x[notNA] > 1 - tol & x[notNA] < 1 + tol)
    }
    else {
        NA
    }
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
    speciesVar <- getDataTypeDefinition(dataType = "QuantityData", elements = "categoryVariable", unlist = TRUE)
    atSpeciesInData <- which(data[[speciesVar]] %in% species)
    atSpeciesInMaster <- which(master[[speciesVar]] %in% species)
    
    # Not needed, as similar check is done in addLengthGroup():
    #if(length(atSpeciesInData) == 0 || length(atSpeciesInMaster) == 0) {
    #    stop("The species ", species, " is not present in both data and master.")
    #}
    
    # Get the unique length intervals of the master:
    uniqueLengthGroups <- unique(master[atSpeciesInMaster, c(..lengthVar, ..resolutionVar)])
    
    if(all(is.na(uniqueLengthGroups))) {
        # Add the TempLengthGroupUsedInSuperIndividuals as NA:
        data[atSpeciesInData, TempLengthGroupUsedInSuperIndividuals := NA_integer_]
        return(FALSE)
    }
    
    # Order the unique length intervals:
    uniqueLengthGroups <- uniqueLengthGroups[order(uniqueLengthGroups[[lengthVar]]), ]
    
    # (1) Match the length groups exactly:
    data[atSpeciesInData, TempLengthGroupUsedInSuperIndividuals := match(
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
            data[missingLength, TempLengthGroupUsedInSuperIndividuals := ..atMissingLengthGroup]
        }
    }
    
    # (3) For the length intervals not exactly matched in the master, find the appropriate intervals. The intervalVector may contain intervals that are not present in the data (intervals between present intervals):
    #data[, is.na(TempLengthGroupUsedInSuperIndividuals)]
    notExactlyMatched <- atSpeciesInData[is.na(data$TempLengthGroupUsedInSuperIndividuals[atSpeciesInData])]
    startLength <- uniqueLengthGroups$IndividualTotalLength
    endLength <- startLength + uniqueLengthGroups$LengthResolution
    data[notExactlyMatched, TempLengthGroupUsedInSuperIndividuals := findInterval_disjoint(
        IndividualTotalLength, 
        start = startLength, 
        end = endLength, 
        out = "index"
    )]
    
    
    hasInvalidLength <- data[atSpeciesInData, is.na(TempLengthGroupUsedInSuperIndividuals)]
    if(any(hasInvalidLength)) {
        if("Individual" %in% names(data)) {
            invalidIndividuals <- do.call(paste, c(subset(data[atSpeciesInData, ], hasInvalidLength, select = c("Individual", "IndividualTotalLength")), list(sep = ", ")))
            #warning("StoX: There are Individuals in the IndividualsData with IndividualTotalLength that does not match any of the length intervals of the QuantityData, ((", paste(startLength, endLength, sep = ", ", collapse = "), ("), ")). This can happen if a length group is only present in Hauls that are all assigned to AcousticPSUs with missing NASC (no acoustic data). The result is only that there will be rows in SuperIndividualsData with missing IndividualTotalLength for the following Individuals, IndividualTotalLength:\n", RstoxData::printErrorIDs(invalidIndividuals))
            warning("StoX: There are Individuals in the IndividualsData with IndividualTotalLength that does not match any of the length intervals of the QuantityData, ((", paste(startLength, endLength, sep = ", ", collapse = "), ("), ")). This can happen if a length group is only present in Hauls that are all assigned to AcousticPSUs with missing NASC (no acoustic data). The result is only that there will be rows in SuperIndividualsData with missing IndividualTotalLength")
        }
        else if("Haul" %in% names(data)) {
            invalidHauls <-data[atSpeciesInData[hasInvalidLength], Haul]
            warning("StoX: There are Hauls in the LengthDistributionData with IndividualTotalLength that does not match any of the length intervals of the QuantityData, ((", paste(startLength, endLength, sep = ", ", collapse = "), ("), ")). This involves the following Hauls:\n", RstoxData::printErrorIDs(invalidHauls))
        }
  }
    
    # Also replace the lengthVar and resolutionVar by those in the master:
    data[atSpeciesInData, `:=`(c(lengthVar), uniqueLengthGroups[[lengthVar]][TempLengthGroupUsedInSuperIndividuals])]
    data[atSpeciesInData, `:=`(c(resolutionVar), uniqueLengthGroups[[resolutionVar]][TempLengthGroupUsedInSuperIndividuals])]
    
    return(TRUE)
}

# Function to find intervals from start and end points:
findInterval_disjoint <- function(x, start, end, out = c("index", "start", "end"), right.closed = FALSE) {
    out <- match.arg(out)
    if(length(start) != length(end)) {
        stop("start and end must have the same length!")
    }
    output <- rep(NA, length(x))
    for(ind in seq_along(start)) {
        if(right.closed) {
            at <- x >= start[ind] & x <= end[ind]
        }
        else {
            at <- x >= start[ind] & x < end[ind]
        }
        if(any(at, na.rm = TRUE)) {
            if(out == "index") {
                output[at] <- ind
            }
            else if(out == "start") {
                output[at] <- start[ind]
            }
            else if(out == "end") {
                output[at] <- end[ind]
            }
        }
    }
    return(output)
}



# Function to add length group IDs in two tables given the length groups in the second:
addLengthGroup <- function(
    data, 
    master, 
    lengthVar = "IndividualTotalLength", 
    resolutionVar = "LengthResolution", 
    warn = TRUE
) {
    
    # Run a for loop through the common species:
    speciesVar <- getDataTypeDefinition(dataType = "QuantityData", elements = "categoryVariable", unlist = TRUE)
    speciesInData <- unique(data[[speciesVar]])
    speciesInMaster <- unique(master[[speciesVar]])
    
    # If there are species in the master that are not in the data, report a warning:
    speciesOnlyInMaster <- stats::na.omit(setdiff(speciesInMaster, speciesInData))
    if(warn && length(speciesOnlyInMaster)) {
        warning("StoX: The species categories ", paste(speciesOnlyInMaster, collapse = ", "), " are present in the master but not in the data")
    }
    # If there are species in the data that are not in the master, report a warning:
    speciesOnlyInData <- stats::na.omit(setdiff(speciesInData, speciesInMaster))
    if(warn && length(speciesOnlyInData)) {
        warning("StoX: The species categories ", paste(speciesOnlyInData, collapse = ", "), " are present in the data but not in the master. These species categories will be removed from the output.")
    }

    # Keep only the common species:
    species <- intersect(speciesInData, speciesInMaster)
    
    if(!length(species)) {
        data[, TempLengthGroupUsedInSuperIndividuals := NA_integer_]
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
#' Impute missing super-individual data
#'
#'  For each (target) individual with missing value in \code{ImputeAtMissing}, identify all (source) individuals in the haul for which \code{ImputeAtMissing} is non-missing and for which the values in \code{ImputeByEqual} are identical to the target individual. Then sample one of these source individuals, and copy values of \code{ToImpute} to the target individual. Only values that are non-missing are copied from the sampled individual, and  only missing values in the target individual are replaced. If no source individuals are found in the haul, expand the search to the stratum, and finally to the survey. If no source individuals are found in the survey, leave the target individual unchanged. 
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams DefineRegression
#' @param ImputationMethod The method to use for the imputation. Currently, only "RandomSampling" is implemented, but may be accompanied "Regression" in a coming release.
#' @param ImputeAtMissing A single string naming the variable which when missing identifies target individuals to input data \bold{to}. I.e., if \code{ImputeAtMissing} is missing for an individual, perform the imputation. In StoX 3.0.0 and older, \code{ImputeAtMissing} was hard coded to IndividualAge.
#' @param ImputeByEqual A vector of strings naming the variable(s) which, when identical to the target individual, identifies the source individuals to impute data \bold{from}. The source individuals need also to have non-missing \code{ImputeAtMissing}. In StoX 3.0.0 and older, \code{ImputeByEqual} was hard coded to c("SpeciesCategory","IndividualTotalLength").
#' @param ToImpute A vector of strings naming the variable(s) to impute (copy to the target individual). Values that are not missing are not imputed. Note that values are only imputed when \code{ImputeAtMissing} is missing, so including many variables in \code{ToImpute} is only recommended if all these are present for the individuals (see Details). In StoX 3.0.0 and older, \code{ToImpute} was hard coded to all available variables of the BioticData contained in the \code{\link{SuperIndividualsData}}. 
#' @param ImputationLevels A vector of strings  naming the levels at which to input, defaulted to c("Haul", "Stratum", "Survey"). To prevent imputation at the Survey level, use c("Haul", "Stratum").
#' @param Seed An integer giving the seed to use for the random sampling used to obtain the imputed data.
#' @param RegressionDefinition Character: A string naming the method to use, one of \code{FunctionParameter} to define the Regression on the fly in this function (using \code{GroupingVariables}, \code{RegressionModel} and \code{RegressionTable}), or \code{FunctionInput} to import Regression process data from a previously run process using the function
#' 
#' @details When \code{ToImpute} contains more variables than that given by \code{ImputeAtMissing} there is a risk that values remain missing even after successful imputation. E.g., if \code{ImputeAtMissing} is IndividualAge, and \code{ToImpute} includes IndividualRoundWeight, then the weight is only imputed when age is missing. Super-individuals with age but not weight will then still have missing weight. Variables that are naturally connected, such as IndividualRoundWeight and WeightMeasurement, or IndividualTotalLength and LengthResolution, should both be included in \code{ToImpute}.
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
    #ImputationMethod = "RandomSampling", 
    ImputationMethod = c("RandomSampling", "Regression"), 
    ImputeAtMissing = character(), 
    ImputeByEqual = character(), 
    ToImpute = character(), 
    ImputationLevels = c("Haul", "Stratum", "Survey"), 
    Seed = 1, 
    RegressionDefinition = c("FunctionParameter", "FunctionInput"), 
    GroupingVariables = character(), 
    RegressionModel = c("SimpleLinear", "Power"), 
    RegressionTable = data.table::data.table(), 
    Regression
    #RegroupIndividualTotalLength = FALSE, 
    #LengthInterval = numeric()
) {
    
    ImputationMethod <- RstoxData::match_arg_informative(ImputationMethod)
    
    if(ImputationMethod == "RandomSampling") {
        # Check that the length resolution is constant: 
        if("IndividualTotalLength" %in% ImputeByEqual && !allEqual(SuperIndividualsData$LengthResolution)) {
            stop("All individuals must have identical LengthResolution in the current version.")
        }
        
        if(!length(ToImpute)) {
            warning("StoX: Empty ToImpute is deprecated. Please select the variables to impute explicitely.")
            ToImpute <- getIndividualNames(SuperIndividualsData, ImputeByEqual,  tables = "Individual")
        }
        if(length(ImputeAtMissing) != 1) {
            stop("ImputeAtMissing must have length 1.")
        }
        else if(!ImputeAtMissing %in% ToImpute) {
            stop("Please specify the variable given by ImputeAtMissing (", ImputeAtMissing, ") in ToImpute. ")
        }
        # For safety uniquify:
        ToImpute <- unique(ToImpute)
        
        
        # Impute the SuperIndividualsData:
        ImputeSuperIndividualsData <- ImputeDataByRandomSampling(
            data = SuperIndividualsData, 
            imputeAtMissing = ImputeAtMissing, 
            imputeByEqual = ImputeByEqual, 
            seed = Seed, 
            columnNames = ToImpute, 
            levels = ImputationLevels#, 
            #lengthInterval  = if(RegroupIndividualTotalLength) LengthInterval else numeric()
        )
    }
    else if(ImputationMethod == "Regression") {
        
        RegressionDefinition <- RstoxData::match_arg_informative(RegressionDefinition)
        
        if(RegressionDefinition == "FunctionParameter") {
            Regression <- DefineRegression(
                DefinitionMethod = "Table",
                GroupingVariables = GroupingVariables, 
                RegressionModel = RegressionModel, 
                RegressionTable = RegressionTable
            )
        }
        
        # Initiate the output:
        ImputeSuperIndividualsData <- data.table::copy(SuperIndividualsData)
        
        for(rowInd in seq_len(NROW(Regression$RegressionTable))) {
            thisRegression <- Regression
            thisRegression$RegressionTable <- Regression$RegressionTable[rowInd, ]
            
            ImputeSuperIndividualsData <- ImputeDataByRegressionOneRow(
                data = ImputeSuperIndividualsData, 
                Regression = thisRegression
            )
        }
    
        }
    
    
    
    # Re-calculate the Biomass, where missing weigth is accepted if Abundane is 0:
    ImputeSuperIndividualsData[, Biomass := ifelse(Abundance %in% 0, 0, Abundance * IndividualRoundWeight)]
    
    # Format the output but keep all columns:
    #formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE)
    # Order the columns, but keep all columns. Also add the names of the MergeStoxBioticData as secondaryColumnOrder to tidy up by moving the Haul column (used as by in the merging) back into its original position:
    areKeys <- endsWith(names(ImputeSuperIndividualsData), "Key")
    keys <- names(ImputeSuperIndividualsData)[areKeys]
    #formatOutput(IndividualsData, dataType = "IndividualsData", keep.all = TRUE, secondaryColumnOrder = keys)
    formatOutput(ImputeSuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE, secondaryColumnOrder = unlist(attr(SuperIndividualsData, "stoxDataVariableNames")), secondaryRowOrder = keys)
    
    # Add the attribute 'variableNames':
    setattr(
        ImputeSuperIndividualsData, 
        "stoxDataVariableNames",
        attr(SuperIndividualsData, "stoxDataVariableNames")
    )
    
    return(ImputeSuperIndividualsData)
}


ImputeDataByRandomSampling <- function(
    data, 
    imputeAtMissing = "IndividualAge", 
    imputeByEqual = c("IndividualTotalLength", "SpeciesCategory"), 
    seed = 1, 
    columnNames = NULL, 
    lengthInterval = numeric(), 
    levels = c(
        "Haul", 
        "Stratum", 
        "Survey"
    )
) {
    
    # Get the data and add the RowIndex for use when identifying which rows to impute from:
    dataCopy <- data.table::copy(data)
    #RowIndex <- seq_len(nrow(dataCopy))
    #dataCopy[, RowIndex := ..RowIndex]
    
    # If specified, regroup the length intervals:
    if(length(lengthInterval) == 1L) {
        dataCopy <- RegroupLengthData(
            dataCopy, 
            lengthInterval = lengthInterval
        )
    }
    
    
    # Introduce an Individual index for use in the sorted sampling:
    dataCopy[, IndividualIndex := as.numeric(as.factor(Individual))]
    #dataCopy[, IndividualIndex := seq_along(Individual)]
    # Use the semi-numeric sorting proivded by RstoxData:
    #dataCopy[, IndividualIndex := as.numeric(factor(RstoxData::createOrderKey(Individual)))]
    
    # Missed attempt:
    #RstoxData::setorderv_numeric(dataCopy, by = getDataTypeDefinition("SuperIndividualsData", unlist = TRUE))
    #dataCopy[, IndividualIndex := seq_along(Individual)]
    
    # Missed attempt:
    #dataCopy[, IndividualExpanded := paste(Survey, Stratum, Layer, SpeciesCategory, Individual, sep = "-")]
    #dataCopy[, IndividualIndex := as.numeric(factor(RstoxData::createOrderKey(IndividualExpanded, split = c("/", "-"))))]
    
    
    
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
        uniqueKeys <- unique(dataCopy[, ..by])
        data.table::setorder(uniqueKeys)
        
        # This seed table will be dependent on whether there is a row of all NAs or not in the table of unique keys. As a consequence, a future review of how StoX includes NA rows will affect how seed works!!!
        seedTable <- data.table::data.table(
            uniqueKeys, 
            imputeSeed = getSeedVector(size = nrow(uniqueKeys), seed = seedVector[[level]])
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
    
    # Add a column ImputationMethod:
    dataCopy[!is.na(ReplaceIndividual), ImputationMethod := "RandomSampling"]
    
    # Delete the IndividualIndex and ReplaceIndividualIndex:
    removeColumnsByReference(
        data = dataCopy, 
        toRemove = c("IndividualIndex", "ReplaceIndividualIndex")
    )

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
        
        # Using the new sampleSorted() which is identical to sort() for non-character such as these integers to be sampled:
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



ImputeDataByRegressionOneRow <- function(
    data, 
    Regression
) {
    
    # Get the data and add the RowIndex for use when identifying which rows to impute from:
    dataCopy <- data.table::copy(data)
    
    # There should either be a demannd for only one row, or we should re-code to treating each row separately!!!!!!!!!!!!!!!!!!!!!!!!!111
    if(NROW(Regression$RegressionTable) > 1) {
        stop("Currently, only one row in the regression table is supported.")
    }
    
    # Store the variable names for convenience:
    DependentVariable <- Regression$RegressionTable$DependentVariable[1]
    IndependentVariable <- Regression$RegressionTable$IndependentVariable[1]
    DependentResolutionVariable <- Regression$RegressionTable$DependentResolutionVariable[1]
    IndependentResolutionVariable <- Regression$RegressionTable$IndependentResolutionVariable[1]
    checkWhetherVariableIsPresent(DependentVariable, dataCopy)
    checkWhetherVariableIsPresent(IndependentVariable, dataCopy)
    checkWhetherVariableIsPresent(DependentResolutionVariable, dataCopy)
    checkWhetherVariableIsPresent(IndependentResolutionVariable, dataCopy)
    
    
    # Merge in the regression parameters:
    mergeBy <- intersect(
        names(dataCopy), 
        names(Regression$RegressionTable)
    )
    
    tomerge <- setdiff(names(Regression$RegressionTable), c("DependentVariable", "IndependentVariable"))
    removeAfterwards <- setdiff(names(Regression$RegressionTable), c(names(dataCopy), c("DependentVariable", "IndependentVariable")))
    if(length(mergeBy) > 0) {
        # Stop if there are no intersecting variables to merge by:
        intersectingMergeBy <- fintersect(dataCopy[, mergeBy, with = FALSE], Regression$RegressionTable[, mergeBy, with = FALSE])
        if(!nrow(intersectingMergeBy)) {
            stop("There are no intersecting rows for the variables ", paste(mergeBy, collapse = ", "), ". No regression applied.")
            return(dataCopy)
        }
        dataCopy <- merge(dataCopy, Regression$RegressionTable[, ..tomerge], by = mergeBy, all.x = TRUE, sort = FALSE)
    }
    else {
        dataCopy <- cbind(dataCopy, Regression$RegressionTable[, ..tomerge])
    }

    # Add half of the resolution:
    #if(length(thisDependentResolutionVariable) && !is.na(thisDependentResolutionVariable)) {
        addHalfResolution(data = dataCopy, variable = DependentVariable, resolutionVariable = DependentResolutionVariable)
        on.exit(addHalfResolution(data = dataCopy, variable = DependentVariable, resolutionVariable = DependentResolutionVariable, reverse = TRUE))
        #dataCopy[, eval(thisDependentVariable) := get(thisDependentVariable) + eval(get(thisDependentResolutionVariable)) / 2]
    #}
    #if(length(thisIndependentResolutionVariable) && !is.na(thisIndependentResolutionVariable)) {
        addHalfResolution(data = dataCopy, variable = IndependentVariable, resolutionVariable = IndependentResolutionVariable)
        on.exit(addHalfResolution(data = dataCopy, variable = IndependentVariable, resolutionVariable = IndependentResolutionVariable, reverse = TRUE))
        #dataCopy[, eval(thisIndependentVariable) := get(thisIndependentVariable) + eval(get(thisIndependentResolutionVariable)) / 2]
    #}
    
    # Apply the regression model with the parameters:
    RegressionModel <- Regression$RegressionModel$RegressionModel
    modelParameters <- getRstoxBaseDefinitions("modelParameters")$Regression[[RegressionModel]]
    regressionFunction <- getRstoxBaseDefinitions("modelFunctions")[["Regression"]][[RegressionModel]]
    
    # Find rows with missing DependentVariable and present IndependentVariable:
    atImpute <- dataCopy[, which(is.na(get(DependentVariable)) & !is.na(get(IndependentVariable)))]
    # Run the regression:
    dataCopy[atImpute, eval(DependentVariable) := regressionFunction(get(eval(IndependentVariable)), .SD), .SDcols = modelParameters]
    
    # Add a column ImputationMethod:
    dataCopy[atImpute, ImputationMethod := "Regression"]
    
    # Remove columns:
    dataCopy[, (removeAfterwards) := NULL]
    
    
    return(dataCopy)
}

checkWhetherVariableIsPresent <- function(var, data) {
    # Using identical(var, "NA") for safety:
    if(length(var) && !is.na(var) && identical(var, "NA") && ! var %in% names(data)) {
        stop("The variable ", var, " given by ", deparse(substitute(var)), " is not present in the data.")
    } 
}

addHalfResolution <- function(data, variable, resolutionVariable, reverse = FALSE) {
    if(length(variable) && !is.na(variable) && length(resolutionVariable) && !is.na(resolutionVariable)) {
        if(! variable %in% names(data)) {
            stop("The value \"", variable, "\" for the argument ", deparse(substitute(variable)), " was not found as a column in the data. Use one of the following names: ", paste(names(data), collapse = ", "), ".")
        }
        if(! resolutionVariable %in% names(data)) {
            stop("The value \"", resolutionVariable, "\" for the argument ", deparse(substitute(resolutionVariable)), " was not found as a column in the data. Use one of the following names: ", paste(names(data), collapse = ", "), ".")
        }
        
        if(reverse) {
            data[, eval(variable) := get(variable) - eval(get(resolutionVariable)) / 2]    
        }
        else {
            data[, eval(variable) := get(variable) + eval(get(resolutionVariable)) / 2]
        }
    }
}
