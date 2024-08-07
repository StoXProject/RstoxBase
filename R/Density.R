
##################################################
##################################################
#' Calculate number density from NASC in length intervals
#' 
#' This function converts NASC to area number density for each species category based on the acoustic target strength as a function of length for each acoustic category.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param SpeciesLink A table of the two columns AcousticCategory and SpeciesCategory.
#' 
#' @details
#' The AcousticDensity function calculates length distributed densities as number of fish per square nautical mile by vertical layer. Length based density distributions are calculated for each NASC value of the input acoustic data set (MacLennan \emph{et al}, 2002) . Usually, the NASC values have a horizontal resolution at PSU level (e.g. transect). By combining a NASC value with a length distribution (usually a total combined length distribution derived from more than one biotic station) and applying a TS vslength relationship, a corresponding density length distribution can be calculated.
#' 
#' The horizontal resolution (EDSU, PSU or Stratum) of the NASC object determines the horizontal resolution of the densities. The same principal applies for the vertical layer resolution.
#' 
#' To combine a NASC value and a length distribution into a density distribution, the TS vs length relationship for the actual species and acoustic frequency, needs to be known.Constants in the TS vs length formula has to be given. If the vertical layer resolution is channels referred from surface and with a fixed thickness, the mean depth of each channel can be calculated and a depth dependent TS vs length formula may be applied. The calculation of densities by length from a NASC value and a corresponding length distribution (as percentage or proportion) is performed as follows (Ona, 2003, Simmond and MacLennan, 2005, Johnsen \emph{et al}, 2019):
#'
#' \deqn{TS_l = m \log_{10}{(l)}+a + d \log_{10}{(1 + \frac{r_y}{10})}}
#'
#' where:
#'
#' \eqn{TS_l} = target strength (dB re 1 \eqn{m^{2}}) of a fish with length l (cm)
#'
#' \eqn{m} = constant in the TS vs length relationship for the given
#' species
#'
#' \eqn{a} = constant in the TS vs length relationship for the given species
#'
#' \eqn{d} = constant in the TS vs length relationship related to depth dependent TS
#'
#' \eqn{l} = length of the fish (cm). Typically, the center length of a length group
#'
#' \eqn{r_y} = average depth (m) of the NASC channel \eqn{y}
#'
#' \deqn{\sigma_{bs,l} = 10^{  (  \frac{TS_l}{10}  )  }   }
#'
#' where:
#'
#' \eqn{\sigma_{bs,l}} = acoustic backscattering cross-section (\eqn{m^{2}}) for a fish of length \eqn{l}
#'
#' \deqn{NASC_l = NASC \frac{\sigma_{bs,l} p_l}{\sum_l{( \sigma_{bs,l} p_l )} } }
#'
#' where:
#'
#' \eqn{NASC} =  the total NASC which is used to calculate densities by length
#'
#' \eqn{NASC_l} = the proportion of the total \eqn{NASC} which can be attributed to length group \eqn{l}.  The sum of \eqn{NASC_l} for all length groups in the total length distribution is equal to \eqn{NASC}
#'
#' \eqn{p_l} = proportion of fish of length \eqn{l} in the input length distribution. Sum of all \eqn{p_l} is 1.
#'
#'
#' \deqn{\rho_l = \frac{NASC_l}{(4 \pi \sigma_{bs,l})}}
#'
#' where:
#'
#' \eqn{\rho_l} = area density of fish (ind. per sqare nautical mile) in length group \eqn{l}
#'
#' References:
#'
#' Johnsen, E,.Totland, A.,Skaalevik, A., et al., 2019, StoX: An open source software for marine survey analyses. Methods Ecol Evol. 2019;10:1523_1528. \doi{10.1111/2041-210X.13250}
#'
#' MacLennan, D. N., Fernandes, P. G., and Dalen, J. 2002. A consistent approach to definitions and symbols in fisheries acoustics. ICES Journal of Marine Science, 59: 365_369.
#'
#' Ona, E. 2003, An expanded target strength relationship for herring, ICES Journal of Marine Science, Volume 60, Issue 3, 2003, Pages 493_499, \doi{10.1016/S1054-3139(03)00031-6}
#'
#' Simmonds, J., and MacLennan, D. 2005. Fisheries Acoustics. Theory and Practice, Blackwell Science, Oxford. 437 pp.
#'
#' @return
#' An object of StoX data type \code{\link{DensityData}}. Note that the Density column of the Data table has unit nmi^-2 and DensityType "AreaNumberDensity".
#' 
#' @seealso See \code{\link{SweptAreaDensity}} for swept-area density. To define the acoustic target strength, see \code{\link{AcousticTargetStrength}}. The DensityData leads to the \code{\link{QuantityData}} in a typical survey estimation model.
#' 
#' @export
#' 
AcousticDensity <- function(
    MeanNASCData,
    AssignmentLengthDistributionData,
    AcousticTargetStrength,
    SpeciesLink = data.table::data.table()
) {
    # Check that the input SpeciesLink has the appropriate types (this function reads the functionName from the function in is placed in):
    checkTypes(table = SpeciesLink)
    
    # Define the resolution on which to distribute the NASC:
    # There was a serious bug in RstoxBase 1.3.1. The sum of the backscatter was taken over the horisontal and vertical resolution only, not including the SpeciesCategory (categoryVariable):
    
    #sumBy <- getDataTypeDefinition(dataType = "DensityData", elements = c("horizontalResolution", "verticalResolution"), unlist = TRUE)
    #
    ## Split the NASC by the AssignmentLengthDistributionData:
    #NASCData <- DistributeNASC(
    #    NASCData = MeanNASCData$Data, 
    #    AssignmentLengthDistributionData = AssignmentLengthDistributionData, 
    #    AcousticTargetStrength = AcousticTargetStrength, 
    #    SpeciesLink = SpeciesLink, 
    #    sumBy = sumBy
    #)
    
    
    sumBy <- getDataTypeDefinition(dataType = "DensityData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable", "groupingVariables_acoustic"), unlist = TRUE)
    # Split the NASC by the AssignmentLengthDistributionData:
    LengthDistributedMeanNASCData <- DistributeNASC(
        MeanNASCData = MeanNASCData$Data, 
        AssignmentLengthDistributionData = AssignmentLengthDistributionData, 
        AcousticTargetStrength = AcousticTargetStrength, 
        SpeciesLink = SpeciesLink, 
        sumBy = sumBy, 
        distributionType = "AcousticDensity"
    )
    
    # Convert NASC to density by dividing by the backscattering cross section of each species:
    DensityData <- NASCToDensity(LengthDistributedMeanNASCData)
    
    # Introduce the DensityWeight as a copy of the NASCWeight:
    DensityData[, DensityWeight := MeanNASCWeight]
    
    # Add the density type, which is always c for AcousticDensity():
    DensityData[, DensityType := "AreaNumberDensity"]
    
    # Add the Resolution table:
    DensityData <- list(
        Data = DensityData, 
        Resolution = MeanNASCData$Resolution
    )
    
    
    # Format the output:
    # Changed added on 2020-10-16, where the datatypes DensityData and QuantityData are now considered non-rigid:
    #formatOutput(DensityData, dataType = "DensityData", keep.all = FALSE)
    # 2020-12-02: This had keep.all = FALSE. Is this an error????????????????????? We should clearly describe the justifications for which data types are rigid and which are not rigid:
    # Changed to keep.all = TRUE on 2021-03-18 when introducing Data and Resolution for DensityData and onwards:
    #formatOutput(DensityData, dataType = "DensityData", keep.all = FALSE, allow.missing = TRUE)
    
    # Changed to keep.all = FALSE in StoX 3.2.0, as inlcuding all variables from MeanNASC seems like an error.
    #formatOutput(DensityData, dataType = "DensityData", keep.all = TRUE, allow.missing = TRUE)
    formatOutput(DensityData, dataType = "DensityData", keep.all = FALSE, allow.missing = TRUE)
    
    return(DensityData)
}


# This function distributes NASC to the different length groups for each species, given the species and length dependent acoustic target strength and the length distribution assigned to each AcousticPSU:
DistributeNASC <- function(
    MeanNASCData, 
    AssignmentLengthDistributionData, 
    AcousticTargetStrength, 
    SpeciesLink, 
    sumBy, 
    distributionType = c("AcousticDensity", "SplitNASC")
) {
    
    # Get the distributionType:
    distributionType <- RstoxData::match_arg_informative(distributionType)
    
    #############################
    ##### Initial warnings: #####
    #############################
    
    ## Initial warning 1: ##
    # Warning if the SpeciesLink does not contain all SpeciesCategory of the AssignmentLengthDistributionData:
    allSpeciesCategory <- unique(AssignmentLengthDistributionData$SpeciesCategory)
    if(!all(allSpeciesCategory %in% SpeciesLink$SpeciesCategory)) {
        notPresent <- setdiff(allSpeciesCategory, SpeciesLink$SpeciesCategory)
        notPresent <- notPresent[!is.na(notPresent)]
        if(length(notPresent)) {
            warning("StoX: The following SpeciesCategory are present in the AssignmentLengthDistributionData but not in the SpeciesLink: ", paste(notPresent, collapse = ", "), ".")
        }
    }
    ## Initial warning 2: ##
    # Warning if the AcousticTargetStrengthTable does not contain all AcousticCategory of the MeanNASCData:
    allAcousticCategory <- unique(MeanNASCData$AcousticCategory)
    if(!all(allAcousticCategory %in% AcousticTargetStrength$AcousticTargetStrengthTable$AcousticCategory)) {
        notPresent <- setdiff(allAcousticCategory, AcousticTargetStrength$AcousticTargetStrengthTable$AcousticCategory)
        notPresent <- notPresent[!is.na(notPresent)]
        if(length(notPresent)) {
            warning("StoX: The following AcousticCategory are present in the MeanNASCData but not in the AcousticTargetStrength: ", paste(notPresent, collapse = ", "), ".")
        }
    }
    ## Initial warning 3: ##
    # Warning if the SpeciesLink does not contain all AcousticCategory of the AcousticTargetStrengthTable:
    if(!all(allAcousticCategory %in% SpeciesLink$AcousticCategory)) {
        notPresent <- setdiff(allAcousticCategory, SpeciesLink$AcousticCategory)
        notPresent <- notPresent[!is.na(notPresent)]
        if(length(notPresent)) {
            warning("StoX: The following AcousticCategory are present in the MeanNASCData but not in the SpeciesLink: ", paste(notPresent, collapse = ", "), ".")
        }
    }
    #################################
    ##### End initial warnings: #####
    #################################
    
    
    
    ###############################
    ##### Merge input tables: #####
    ###############################
    
    ## Merge input tables 1: ##
    # Merge AcousticTargetStrength with SpeciesLink:
    # This merge is done in order to get the targets strength for each SepciesCategory (and not only AcousticCategory):
    AcousticTargetStrength$AcousticTargetStrengthTable <- merge(AcousticTargetStrength$AcousticTargetStrengthTable, SpeciesLink, by = "AcousticCategory", all = TRUE, allow.cartesian = TRUE)
    
    ## Merge input tables 2: ##
    # Merge AcousticTargetStrength into the MeanNASCData:
    # This merge adds the parameters of the target strength to length relationship. This step is important, as merging is done by the AcousticCategory, Frequency and possibly other grouping columns.:
    # Take special care of AcousticTargetStrengthModels that are tables of length instead of functions, in which we apply constant interpolation to the lengths in the data here, to facilitate correct merging:
    if(getRstoxBaseDefinitions("modelTypes")$AcousticTargetStrength[[AcousticTargetStrength$AcousticTargetStrengthModel$AcousticTargetStrengthModel]] == "Table") {
        AcousticTargetStrength$AcousticTargetStrengthTable <- getTargetStrengthByLengthFunction(
            AcousticTargetStrength$AcousticTargetStrengthTable, 
            method = "constant", 
            rule = 2
        )
    }
    # Merge by intersecting columns: 
    mergeBy <- intersect(
        names(MeanNASCData), 
        names(AcousticTargetStrength$AcousticTargetStrengthTable)
    )
    if(length(mergeBy) > 0) {
        MeanNASCData <- merge(MeanNASCData, AcousticTargetStrength$AcousticTargetStrengthTable, by = mergeBy, all.x = TRUE)
    }
    else {
        MeanNASCData <- cbind(MeanNASCData, AcousticTargetStrength$AcousticTargetStrengthTable)
    }
    
    ## Merge input tables 3: ##
    # Merge AssignmentLengthDistributionData into the MeanNASCData:
    # This merge adds the length distribution:
    # This is "Stratum", "PSU", "Layer":
    mergeBy <- getDataTypeDefinition("AssignmentLengthDistributionData", elements = c("horizontalResolution", "verticalResolution", "categoryVariable"), unlist = TRUE)
    mergeBy <- intersect(mergeBy, intersect(names(MeanNASCData), names(AssignmentLengthDistributionData)))
    # Do the merge:
    MeanNASCData <- merge(MeanNASCData, AssignmentLengthDistributionData, by = mergeBy, all.x = TRUE, allow.cartesian = TRUE)
    
    ###################################
    ##### End merge input tables: #####
    ###################################
    
    
    
    #######################
    ##### Exceptions: #####
    #######################
    
    ##### Exception 1: 
    # Find PSUs with both missing and non-missing WeightedNumber, indicating that there are species present in the MeanNASCData that have a length distribution in the AssignmentLengthDistributionData, and at the same time there are species in the MeanNASCData that are not present in the AssignmentLengthDistributionData. The WeightedNumber for the latter species should be 0 and not NA (the merge fills with NA) This is done at the end of the function:
    # AssignmentLengthDistributionData does not contain Layer yet, as it is not supported to define different assignment per Layer, so we use only the horizontal resolution here:
    horizontalResolution <- getDataTypeDefinition("MeanNASCData", elements = c("horizontalResolution"), unlist = TRUE)
    # The is.na(WeightedNumber) finds rows with NA WeightedNumber
    # The any(!is.na(WeightedNumber)) requires also that there are any non missing WeightedNumber in the PSU
    MeanNASCData[, missingSpecies := is.na(WeightedNumber) & any(!is.na(WeightedNumber)), by = horizontalResolution]
    
    ##############################
    ##### End of exceptions: #####
    ##############################
    
    
    
    #####################
    ##### Warnings: #####
    #####################
    
    ## Warning 1: ##
    # Warning if all length distribution frequencies are missing, indicating that the inputs do not match:
    allNAWeightedNumber <- MeanNASCData[, all(is.na(WeightedNumber))]
    if(allNAWeightedNumber) {
        warning("StoX: The MeanNASCData and AssignmentLengthDistributionData have no intersecting values for the columns: ", paste0(mergeBy, collapse = ", "), ". A possible reason is that the LayerDefinition differs between the MeanNASCData and the AssignmentLengthDistributionData. In that case rerun BioticAssignment process data with the same Layer definition as used in the process using the function MeanNASC(). Another reason may be that the AcousticCategory of the AcousticTargetStrength process data and the parameter SpeciesLink of the AcousticDensity function do not match.")
    }
    
    
    
    
    
    
    
    # And check whether there are PSU/Layer win only one non-empty assigned haul:
    # This was a bug, as it failed to detect NumberOfAssignedHauls = 1 when NumberOfAssignedHauls was NA in the first row of a resolution:
    #withOnlyOneHaul <- subset(unique(MeanNASCData, by = resolution), NumberOfAssignedHauls == 1)
    #withOnlyOneHaul <- unique(subset(MeanNASCData, NumberOfAssignedHauls == 1), by = resolution)
    
    #if(NROW(withOnlyOneHaul)) {
    #    # Get the unique invalid hauls:
    #    withOnlyOneHaul <- withOnlyOneHaul[, Reduce(function(...) paste(..., sep = ", "), .SD), .SDcols = resolution]
    #    warning(paste("StoX: There are Stratum,PSU that have assigned ONLY ONE haul. This is in conflict with the principle of bootstrapping, and can lead to underestimation of variance in reports from bootstrap. The following Stratum,PSU have only one assigned haul:", RstoxData::printErrorIDs(withOnlyOneHaul)))
    #}
    
    # Add a warning if any WeightedNumber are NA while NASC > 0:
    MeanNASCData[, missingAssignment := all(is.na(WeightedNumber) & NASC > 0), by = sumBy]
    unassignedPSUs <- unique(MeanNASCData[missingAssignment == TRUE, PSU])
    
    unassignedPSUs <- setdiff(unassignedPSUs, NA)
    MeanNASCData[, missingAssignment := NULL]
    if(length(unassignedPSUs)) {
        if(distributionType == "AcousticDensity") {
            warning("StoX: SEVERE WARNING: There are positive NASC values with no assignment length distribution! \n If this warning occurs in Baseline, it can lead to missing (NA) acoustic density! Please make sure that biotic hauls containing the relevant species are assigned to all acoustic PSUs to avoid this (e.g. using FilterSttoxBiotic with FilterUpwards to remove Hauls without the target species). \nIf this warning occurs in a Bootstrap process, it implies that there are AcousticPSUs where none of the assigned hauls were resampled in one or more bootstrap replicates. This causes missing values in the AcousticDensityData, QuantityData and SuperIndividualsData, with the result that the only way to get non-missing values in ReportBootstrap is to use RemoveMissingValues = TRUE, which directly leads to under-estimation!!!! This can occur if ResampleBioticAssignmentByStratum is used in the BootstrapMethodTable when there are Hauls assigned differently for the different AcousticPSUs in a Stratum (e.g., using DefinitionMethod = \"Radius\"). In this case it is adviced to use ResampleBioticAssignmentByPSU instead, which resamples only the Hauls assigned to each individual AccousticPSU.")
        }
        else if(distributionType == "SplitNASC") {
            resolution <- getDataTypeDefinition(dataType = "DensityData", elements = "horizontalResolution", unlist = TRUE)
            unassignedStratumPSUs <- unique(subset(MeanNASCData, PSU %in% unassignedPSUs, select = c("Stratum", "PSU")))
            unassignedStratumPSUs <- unassignedStratumPSUs[, Reduce(function(...) paste(..., sep = ", "), .SD), .SDcols = resolution]
            warning("StoX: There are positive NASC values with no assignment length distribution!. This can lead to un-split acoustic categories in SplitNASC(). Please make sure that biotic hauls containing the relevant species are assigned to all acoustic PSUs to avoid this. \nMissing length distribution was found for the following PSUs:\n", RstoxData::printErrorIDs(unassignedStratumPSUs))
        }
        
        ###warning("StoX: There are positive NASC values with no assignment length distribution!\n\nIf this warning occurs in a Bootstrap process, it implies that there are AcousticPSUs where none of the assigned hauls were resampled in one or more bootstrap replicates. This causes missing values in the AcousticDensityData, QuantityData and SuperIndividualsData, with the result that the only way to get non-missing values in ReportBootstrap is to use RemoveMissingValues = TRUE, which directly leads to under-estimation!!!! This can occur if Hauls are assigned differently for the different AcousticPSUs in a stratum (e.g., using DefinitionMethod = \"Radius\") and ResampleBioticAssignmentByStratum is used as the ResampleFunction for the DefineBioticAssignment process in the BootstrapMethodTable of the Bootstrap function. In this case it is adviced to use ResampleBioticAssignmentByPSU as the ResampleFunction instead, which resamples only the Hauls assigned to each AccousticPSU.\n\nIf this warning occurs in Baseline, it can lead to un-split acoustic categories in SplitNASC() and missing (NA) acoustic density. Please make sure that biotic hauls containing the relevant species are assigned to all acoustic PSUs to avoid this.", "Missing length distribution was found for the following PSUs:\n", paste("\t", unassignedPSUs, collapse = "\n"))
        #stop("StoX: There are positive NASC values with no assignment length distibution. This is considered to be an error as of StoX 4.0.0. If this error occurres in a Bootstrap process, it implies that none of the assigned hauls of the acoustic PSUs were resampled in a bootstrap replicate. This is known to occur if Hauls are assigned differently for the different AcousticPSUs in a stratum (e.g., using DefinitionMethod = \"Radius\") and ResampleBioticAssignmentByStratum is used as the ResampleFunction of the DefineBioticAssignment process in the BootstrapMethodTable of the Bootstrap function. This induces in missing values in the  AcousticDensityData and consequently in the QuantityData and SuperIndividualsData, with the result that the only way to get non-missing values in ReportBootstrap is to use RemoveMissingValues = TRUE, which directly causes under-estimation! If Hauls are assigned differently for the different AcousticPSUs in a stratum it is adivced to use the ResampleBioticAssignmentByPSU as the ResampleFunction, which resamples only the Hauls assigned to each AccousticPSU, which will never cause missing assignment length distibution.")
    }
    
    # Calculate the target strength of each length group:
    getTargetStrength(MeanNASCData, AcousticTargetStrengthModel = AcousticTargetStrength$AcousticTargetStrengthModel$AcousticTargetStrengthModel)
    
    # Get backscattering cross section:
    MeanNASCData[, backscatteringCrossSection := targetStrengthToBackscatteringCrossSection(TargetStrength)]
    
    # Get the representative backscattering cross section of each length group as the product of backscatteringCrossSection and the length distribution from the AssignmentLengthDistributionData:
    MeanNASCData[, representativeBackscatteringCrossSection := backscatteringCrossSection * WeightedNumber]
    # Divide by the sum of the representativeBackscatteringCrossSection for each PSU/Layer:
    # If the length distribution is misssing (NA) this will be NA/sum(NA, na.rm = TRUE) = NA, as expected.
    MeanNASCData[, representativeBackscatteringCrossSectionNormalized := representativeBackscatteringCrossSection / sum(representativeBackscatteringCrossSection, na.rm = TRUE), by = sumBy]
    #MeanNASCData[is.na(representativeBackscatteringCrossSectionNormalized), representativeBackscatteringCrossSectionNormalized := 0]
    
    
    # Distribute the NASC by the representativeBackscatteringCrossSectionNormalized:
    MeanNASCData[, NASC := ifelse(NASC == 0, 0, NASC * representativeBackscatteringCrossSectionNormalized)]
    #MeanNASCData[, NASC := ifelse(
    #    NASC == 0 | is.na(representativeBackscatteringCrossSectionNormalized), 
    #    0, 
    #    NASC * representativeBackscatteringCrossSectionNormalized
    #)]
    
    
    # Apply Exception 1:
    MeanNASCData[missingSpecies == TRUE, NASC := 0]
    
    # Return the MeanNASCData where NASC is distributed to length groups:
    return(MeanNASCData[])
}


NASCToDensity <- function(NASCData) {
    # Get the density by dividing by the cross section (4 * pi * backscatteringCrossSection):
    DensityData <- data.table::copy(NASCData)
    DensityData[, crossSection := 4 * pi * backscatteringCrossSection]
    DensityData[, Density := NASC / crossSection]
    
    return(DensityData[])
}


checkIntersect <- function(x, y, by = NULL) {
    if(length(by)) (
        FALSE
    )
    else {
        lapply(by, function(thisBy) length(intersect(x[[thisBy]], y[[thisBy]])) > 0)
    }
}

# Convert a table of length and TS to a funciton:
getTargetStrengthByLengthFunction <- function(AcousticTargetStrengthTable, method = "constant", rule = 2) {
    
    # Define the columns to modify:
    functionColumns <- c("TotalLength", "TargetStrength")
    by <- setdiff(names(AcousticTargetStrength), functionColumns)
    
    # Add mid points to the AcousticTargetStrength to facilitate use of approxfun with method = "constant":
    AcousticTargetStrengthTable  <- expandTargetStrengthTable(AcousticTargetStrengthTable, by = by)
    
    # Get one function for each combination of the columns given by 'by':
    AcousticTargetStrengthWithFunction <- AcousticTargetStrengthTable[, .(TargetStrengthFunction = getTargetStrengthByLengthFunctionOne(.SD, by = by, method = method, rule = rule)), by = by]
    
    return(AcousticTargetStrengthWithFunction)
}


# Define the function to get the target strength function, using one TargetStrength (a subset of the TargetStrength):
getTargetStrengthByLengthFunctionOne <- function(AcousticTargetStrengthTable, by, method = "constant", rule = 2) {
    
    # Define the target strength function as a function of length and length interval:
    targetStrengthByLengthFunctionOne <- function(TotalLength) {
        output <- stats::approx(
            x = AcousticTargetStrengthTable$TotalLength, 
            y = AcousticTargetStrengthTable$TargetStrength, 
            xout = TotalLength, 
            method = method, 
            rule = rule
        )$y
        
        return(output)
    }
    
    # Save the function in the package environment:
    #functionName <- paste0("StrengthByLengthFunction_", paste(by, TargetStrength[1, ..by], sep = "_", collapse = "_"))
    functionName <- paste0("StrengthByLengthFunction_UNIXTime", unclass(Sys.time()))
    assign(
        x = functionName, 
        value = targetStrengthByLengthFunctionOne, 
        envir = get("RstoxBaseEnv")
    )
    
    # Return the function:
    return(functionName)
}


# Function to prepare a TargetStrength for approxfun():
expandTargetStrengthTable <- function(AcousticTargetStrengthTable, by) {
    # Add mid points of the lengths between the first and last:
    AcousticTargetStrengthTable[, .(
        TotalLength = c(
            utils::head(TotalLength, 1), 
            TotalLength[-1] - diff(TotalLength) / 2, 
            utils::tail(TotalLength, 1)
        ), 
        TargetStrength = c(
            TargetStrength, 
            utils::tail(TargetStrength, 1)
        )), 
        by = by]
}


###########################################################
# Define a function to add TS according to selected model #
###########################################################
getTargetStrength <- function(Data, AcousticTargetStrengthModel) {
    
    # Get the length interval mid points:
    Data[, midIndividualTotalLength := getMidIndividualTotalLength(.SD)]
    
    # Check wich model is selected
    if(grepl("LengthDependent", AcousticTargetStrengthModel, ignore.case = TRUE)) {
        # Apply the LengthDependent equation: 
        Data[, TargetStrength := getRstoxBaseDefinitions("modelFunctions")$AcousticTargetStrength$LengthDependent(
            midIndividualTotalLength, 
            TargetStrength0 = TargetStrength0, 
            LengthExponent = LengthExponent
        )]
    }
    else if(grepl("LengthAndDepthDependent", AcousticTargetStrengthModel, ignore.case = TRUE)) {
        # Add the the depth:
        verticalLayerDimension <- getDataTypeDefinition(dataType = "MeanNASCData", elements = "verticalLayerDimension", unlist = TRUE)
        Data[, Depth := rowMeans(.SD), .SDcols = verticalLayerDimension]
        
        # Apply the LengthAndDepthDependent equation: 
        Data[, TargetStrength := getRstoxBaseDefinitions("modelFunctions")$AcousticTargetStrength$LengthAndDepthDependent(
            midIndividualTotalLength, 
            TargetStrength0 = TargetStrength0, 
            LengthExponent = LengthExponent, 
            DepthExponent = DepthExponent, 
            Depth = Depth
        )]
    }
    else if(grepl("LengthExponent", AcousticTargetStrengthModel, ignore.case = TRUE)) {
        # Apply only the LengthExponent: 
        Data[, TargetStrength := getRstoxBaseDefinitions("modelFunctions")$AcousticTargetStrength$LengthExponent(
            midIndividualTotalLength, 
            LengthExponent = LengthExponent
        )]
    }
    else if(grepl("TargetStrengthByLength", AcousticTargetStrengthModel, ignore.case = TRUE)) {
        # Apply the TargetStrengthByLengthTargetStrengthByLength equations: 
        Data[, TargetStrength := 
                 if(!is.na(TargetStrengthFunction)) 
                     get("RstoxBaseEnv")[[TargetStrengthFunction]](midIndividualTotalLength)
             else 
                 NA_real_, 
             by = seq_len(nrow(Data)
             )]
    }
    else{
        warning("StoX: Invalid AcousticTargetStrengthModel (", paste(names(getRstoxBaseDefinitions("modelParameters")$AcousticTargetStrength), collapse = ", "), " currently implemented)")
    }
}





# Function to convert from target strength (TS) to backscattering cross section (sigma_bs)
targetStrengthToBackscatteringCrossSection <- function(targetStrength) {
    10^(targetStrength/10)
}

# Function to get mid points of length intervals of e.g. LengthDistributionData:
getMidIndividualTotalLength <- function(x) {
    x[, IndividualTotalLength + LengthResolution / 2]
}


##################################################
##################################################
#' Swept-area density
#' 
#' This function calculates the area density of fish as number of individuals or weight (kg) per square nautical mile, as determined by the \code{DensityType}.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param SweptAreaDensityMethod The method to use for the swept-area calculation, one of \"LengthDistributed\" for calculating density from the length distribution (\code{\link{MeanLengthDistributionData}}), and \"TotalCatch\" for calculating density from the total catch (\code{\link{MeanSpeciesCategoryCatchData}}).
#' @param SweepWidthMethod The method for calculating the sweep width. Possible options are (1) "Constant", which requires \code{SweepWidth} to be set as the constant sweep width, and (2) "PreDefined", impying that the sweep width is already incorporated in the \code{WeightedNumber} in the \code{MeanLengthDistributionData} using \code{link{GearDependentLengthDistributionCompensation}} or \code{link{LengthDependentLengthDistributionCompensation}}, or in the \code{MeanSpeciesCategoryCatchData} using \code{link{GearDependentSpeciesCategoryCatchCompensation}}.
#' @param SweepWidth The constant sweep width in meters.
#' @param DensityType The requested density type, currently only "AreaNumberDensity" is supported for SweptAreaDensityMethod = "LengthDistributed", and one of "AreaNumberDensity" and "AreaWeightDensity" (kg) for SweptAreaDensityMethod = "TotalCatch". All area densities are given per square nautical mile.
#' 
#' @return
#' An object of StoX data type \code{\link{DensityData}}. Note that the Density column of the Data table has unit kg nmi^-2 if SweptAreaDensityMethod is "TotalCatch" and DensityType is "AreaWeightensity". See also \code{\link{QuantityData}}
#' 
#' @seealso See \code{\link{AcousticDensity}} for acoustic density. The DensityData leads to the \code{\link{QuantityData}} in a typical survey estimation model.
#' 
#' @export
#' 
SweptAreaDensity <- function(
    SweptAreaDensityMethod = c("LengthDistributed", "TotalCatch"), 
    MeanLengthDistributionData, 
    MeanSpeciesCategoryCatchData, 
    SweepWidthMethod = c("Constant", "PreDefined"), 
    SweepWidth = double(), 
    DensityType = character()
) {
    
    ## Get the DefinitionMethod:
    SweepWidthMethod <- RstoxData::match_arg_informative(SweepWidthMethod)
    SweptAreaDensityMethod <- RstoxData::match_arg_informative(SweptAreaDensityMethod)
    
    # Get the input data type:
    if(SweptAreaDensityMethod == "LengthDistributed") {
        Data <- data.table::copy(MeanLengthDistributionData$Data)
        Resolution <- data.table::copy(MeanLengthDistributionData$Resolution)
        InputDataType <- "MeanLengthDistributionData"
    }
    else if(SweptAreaDensityMethod == "TotalCatch") {
        Data <- data.table::copy(MeanSpeciesCategoryCatchData$Data)
        Resolution <- data.table::copy(MeanSpeciesCategoryCatchData$Resolution)
        InputDataType <- "MeanSpeciesCategoryCatchData"
    }
    
    # Get the types:
    typeVariableName <- getDataTypeDefinition(dataType = InputDataType, elements = "type", unlist = TRUE)
    weightingVariableName <-getDataTypeDefinition(dataType = InputDataType, elements = "weighting", unlist = TRUE)
    
    # Require normalized type:
    if(!endsWith(firstNonNA(Data[[typeVariableName]]), "Normalized")) {
        stop("The ", InputDataType, " must be normalized, i.e. divided by towed distance (", typeVariableName, " ending with \"Normalized\")")
    }
    
    # Introduce the DensityWeight as a copy of the MeanLengthDistributionWeight:
    Data[, DensityWeight := get(weightingVariableName)]
    
    # Add the density type:
    if(!startsWith(DensityType, "Area")) {
        stop("Only area density is currently implemented in StoX.")
    }
    # SweptAreaDensityMethod == "LengthDistributed" only permits number density:
    if(SweptAreaDensityMethod == "LengthDistributed") {
        if(!endsWith(DensityType, "NumberDensity")) {
            stop("Only number density is available for SweptAreaDensityMethod = \"LengthDistributed\".")
        }
    }
    Data[, DensityType := ..DensityType]
    
    # Get the data variable depending on the SweptAreaDensityMethod and the DensityType:
    dataVariables <- getDataTypeDefinition(dataType = InputDataType, elements = "data", unlist = TRUE)
    if(SweptAreaDensityMethod == "LengthDistributed") {
        dataVariable <- dataVariables
    }
    else if(SweptAreaDensityMethod == "TotalCatch") {
        # Select weight or number density:
        if(DensityType == "AreaWeightDensity") {
            dataVariable <- dataVariables["Weight"]
        }
        else if(DensityType == "AreaNumberDensity") {
            dataVariable <- dataVariables["Number"]
        }
    }
    
    if(Data[, all(is.na(get(dataVariable)))]) {
        warning("StoX: All ", dataVariable, " are NA. The selected DensityType (", DensityType, ") results in missing density.")
    }
    
    ## Do we need to account for the sweep width?:
    #if(startsWith(firstNonNA(Data[[typeVariableName]]), "SweepWidthCompensated")) {
    #    if(SweepWidthMethod == "PreDefined") {
    #        # The data are already in area density when the type is SweepWidthCompensatedNormalized, which is generated by *Compensation():
    #        Data[, Density := get(dataVariable)]
    #    }
    #    else {
    #        stop("SweepWidthMethod must be \"PreDefined\" if the length distribution in ", InputDataType, " is sweep width compensated (", typeVariableName, " starting with \"SweepWidthCompensated\")")
    #    }
    #}
    #else {
    #    # Use a constant sweep width for all data by default:
    #    if(SweepWidthMethod == "Constant") {
    #        if(length(SweepWidth) == 0) {
    #            stop("SweepWidth must be given when SweepWidthMethod == \"Constant\"")
    #        }
    #        
    #        # Convert WeightedNumber to density:
    #        sweepWidthInNauticalMiles <- SweepWidth / getRstoxBaseDefinitions("nauticalMileInMeters")
    #        
    #        Data[, Density := get(dataVariable) / sweepWidthInNauticalMiles]
    #    }
    #    else {
    #        stop("SweepWidthMethod must be \"Constant\" if ", InputDataType, " is not sweep width compensated (", typeVariableName, " not starting with \"SweepWidthCompensated\")")
    #    }
    #}
    
    
    # Convert to density:
    dataVariableToDensity(
        Data, 
        dataVariable = dataVariable, 
        typeVariableName = typeVariableName, 
        InputDataType = InputDataType, 
        SweepWidthMethod = SweepWidthMethod, 
        SweepWidth = SweepWidth
    ) 
    
    
    
    
    # Remove the dataVariables:
    Data[, (dataVariables) := NULL]
    # Remove the weighting and type:
    Data[, (typeVariableName) := NULL]
    Data[, (weightingVariableName) := NULL]
    
    # Add the Resolution table:
    DensityData <- list(
        Data = Data, 
        Resolution = Resolution
    )
    
    # Format the output:
    # Changed to keep.all = TRUE on 2021-03-18 when introducing Data and Resolution for DensityData and onwards:
    #formatOutput(DensityData, dataType = "DensityData", keep.all = FALSE)
    formatOutput(DensityData, dataType = "DensityData", keep.all = TRUE, allow.missing = TRUE)
    
    return(DensityData)
}


# Function to calculate swept-area density from e.g. WeightedNumber:
dataVariableToDensity <- function(Data, dataVariable, typeVariableName, InputDataType, SweepWidthMethod, SweepWidth) {
    
    # Do we need to account for the sweep width?:
    if(startsWith(firstNonNA(Data[[typeVariableName]]), "SweepWidthCompensated")) {
        if(SweepWidthMethod == "PreDefined") {
            # The data are already in area density when the type is SweepWidthCompensatedNormalized, which is generated by *Compensation():
            Data[, Density := get(dataVariable)]
        }
        else {
            stop("SweepWidthMethod must be \"PreDefined\" if the length distribution in ", InputDataType, " is sweep width compensated (", typeVariableName, " starting with \"SweepWidthCompensated\")")
        }
    }
    else {
        # Use a constant sweep width for all data by default:
        if(SweepWidthMethod == "Constant") {
            if(length(SweepWidth) == 0) {
                stop("SweepWidth must be given when SweepWidthMethod == \"Constant\"")
            }
            
            # Convert WeightedNumber to density:
            sweepWidthInNauticalMiles <- SweepWidth / getRstoxBaseDefinitions("nauticalMileInMeters")
            
            Data[, Density := get(dataVariable) / sweepWidthInNauticalMiles]
        }
        else {
            stop("SweepWidthMethod must be \"Constant\" if ", InputDataType, " is not sweep width compensated (", typeVariableName, " not starting with \"SweepWidthCompensated\")")
        }
    }
}



##################################################
##################################################
#' Mean density in each stratum
#' 
#' This function calculates the weighted average of the density in each Stratum/Layer/SpeciesCategory/Beam. The weights are summed effective log distance for each acoustic PSU for acoustic density, and number of stations per biotic PSU for swept area density.
#' 
#' @inheritParams ModelData
#' 
#' @seealso See \code{\link{AcousticDensity}} for acoustic density and  \code{\link{SweptAreaDensity}} for swept-area density.
#' 
#' @export
#' 
MeanDensity <- function(
    DensityData
) {
    
    # Get the mean of the density data:
    MeanDensityData <- data.table::copy(DensityData)
    
    # Issue a warning if there are NA in Beam with positive DensityWeight, which will represent a separate Beam, and will thus exlcude those PSUs from the mean:
    if("Beam" %in% names(MeanDensityData$Data)) {
        NABeamPositiveDensityWeight <- MeanDensityData$Data[, is.na(Beam) & DensityWeight >  0]
        if(any(NABeamPositiveDensityWeight, na.rm = TRUE)) {
            warning("StoX: The following acoustic PSUs contain missing Beam with positive DensityWeight, which is an indication that the Beam of interest was not recording or that it for some other reason was non inlcuded in the acoustic file for some or all of the EDSUs of the PSU. This will have the effect that the mean density for the Beam of interes does not cover those PSUs, and that the acoustic sampling therefore does not cover the entire stratum:\n", paste("\t", stats::na.omit(MeanDensityData$Data$PSU[NABeamPositiveDensityWeight]), collapse = "\n"))
        }
    }
    
    
    MeanDensityData$Data <- applyMeanToData(data = MeanDensityData$Data, dataType = "DensityData", targetResolution = "Stratum")
    
    # Format the output:
    # Use keep.all = FALSE, as the difference between acoustic and swept area density is sorted out in the DensityData:
    formatOutput(MeanDensityData, dataType = "MeanDensityData", keep.all = FALSE, allow.missing = TRUE)
    
    return(MeanDensityData)
}


