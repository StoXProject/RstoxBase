
##################################################
##################################################
#' Calculate number density from NASC in length intervals
#' 
#' This function converts NASC to number density for each species category based on the acoustic target strength as a function of length for each acoustic category.
#' 
#' @inheritParams ProcessData
#' @inheritParams ModelData
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
#' Johnsen, E,.Totland, A.,Skaalevik, A., et al., 2019, StoX: An open source software for marine survey analyses. Methods Ecol Evol. 2019;10:1523_1528.  \url{https://doi.org/10.1111/2041-210X.13250}
#'
#' MacLennan, D. N., Fernandes, P. G., and Dalen, J. 2002. A consistent approach to definitions and symbols in fisheries acoustics. ICES Journal of Marine Science, 59: 365_369.
#'
#' Ona, E. 2003, An expanded target strength relationship for herring, ICES Journal of Marine Science, Volume 60, Issue 3, 2003, Pages 493_499, \url{https://doi.org/10.1016/S1054-3139(03)00031-6}
#'
#' Simmonds, J., and MacLennan, D. 2005. Fisheries Acoustics. Theory and Practice, Blackwell Science, Oxford. 437 pp.
#'
#' @return
#' An object of StoX data type \code{\link{DensityData}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso To modify AcousticCategory use \code{\link[RstoxData]{DefineStoxAcousticVariableConversion}} and \code{\link[RstoxData]{ConvertStoxAcousticVariables}} and to modify SpeciesCategory use \code{\link[RstoxData]{DefineStoxBioticVariableConversion}} and \code{\link[RstoxData]{ConvertStoxBioticVariables}}. See \code{\link{SweptAreaDensity}} for swept-area density.
#' 
#' @export
#' 
AcousticDensity <- function(
    MeanNASCData,
    AssignmentLengthDistributionData,
    AcousticTargetStrength,
    SpeciesLink
) {
    
    # Check that the input SpeciesLink has the appropriate types:
    checkTypes(table = SpeciesLink)
    
    # Merge TargetStrength with SpeciesLink in order to get the targets strengt for each SepciesCategory (and not only AcousticCategory):
    AcousticTargetStrength$TargetStrengthTable <- merge(AcousticTargetStrength$TargetStrengthTable, SpeciesLink, by='AcousticCategory', all = TRUE, allow.cartesian = TRUE)
    # Define the resolution on which to distribute the NASC:
    resolution <- getDataTypeDefinition(dataType = "DensityData", elements = c("horizontalResolution", "verticalResolution"), unlist = TRUE)
    
    # Convert NASC to number density using the length distribution coupled with the target strength:
    DensityData <- NASCToDensity(
        NASCData = MeanNASCData$Data, 
        AssignmentLengthDistributionData = AssignmentLengthDistributionData, 
        AcousticTargetStrength = AcousticTargetStrength, 
        resolution = resolution
    )
    
    # Introduce the DensityWeight as a copy of the NASCWeight:
    DensityData[, DensityWeight := MeanNASCWeight]
    
    # Keep only the releavnt columns:
    #keepOnlyRelevantColumns(DensityData, "DensityData")
    formatOutput(DensityData, dataType = "DensityData", keep.all = FALSE)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(DensityData)
    
    return(DensityData)
}

NASCToDensity <- function(NASCData, AssignmentLengthDistributionData, AcousticTargetStrength, resolution) {
    # Merge the TargetStrength into the NASCData to form the DensityData. This adds the parameters of the target strength to length relationship. This step is important, as merging is done by the AcousticCategory, Frequency and possibly other grouping columns.:
    
    
    # Take special care of TargetStrengthMethods that are tables of length instead of functions, in which we apply constant interpolation to the lengths in the data here, to facilitate correct merging:
    if(getRstoxBaseDefinitions("targetStrengthMethodTypes")[[AcousticTargetStrength$TargetStrengthMethod$TargetStrengthMethod]] == "Table") {
        AcousticTargetStrength$TargetStrengthTable <- getTargetStrengthByLengthFunction(
            AcousticTargetStrength$TargetStrengthTable, 
            method = "constant", 
            rule = 2
        )
    }
    
    #mergeBy <- getDataTypeDefinition(dataType = "NASCData", elements = c("categoryVariable", "groupingVariables"), unlist = TRUE)
    mergeBy <- intersect(
        names(NASCData), 
        names(AcousticTargetStrength$TargetStrengthTable)
    )
    ## Subtract the columns defined as parameters to the target strength function:
    #mergeBy <- setdiff(
    #    mergeBy, 
    #    getRstoxBaseDefinitions("targetStrengthParameters")[[AcousticTargetStrength$TargetStrengthMethod]]
    #)
    
    if(length(mergeBy) > 0) {
        DensityData <- merge(NASCData, AcousticTargetStrength$TargetStrengthTable, by = mergeBy, all.x = TRUE)
    }
    else {
        DensityData <- cbind(NASCData, AcousticTargetStrength$TargetStrengthTable)
    }
    
    # Merge the AssignmentLengthDistributionData into the DensityData. This adds the length distribution:
    mergeBy <- intersect(names(DensityData), names(AssignmentLengthDistributionData))
    DensityData <- merge(DensityData, AssignmentLengthDistributionData, by = mergeBy, all.x = TRUE)
    
    # Calculate the target strength of each length group:
    getTargetStrength(DensityData, TargetStrengthMethod = AcousticTargetStrength$TargetStrengthMethod$TargetStrengthMethod)
    
    # Get backscattering cross section:
    DensityData[, backscatteringCrossSection := targetStrengthToBackscatteringCrossSection(TargetStrength)]
    
    # Get the representative backscattering cross section of each length group as the product of backscatteringCrossSection and the length distribution from the AssignmentLengthDistributionData:
    DensityData[, representativeBackscatteringCrossSection := backscatteringCrossSection * WeightedCount]
    # Divide by the sum of the representativeBackscatteringCrossSection for each PSU/Layer:
    DensityData[, representativeBackscatteringCrossSectionNormalized := representativeBackscatteringCrossSection / sum(representativeBackscatteringCrossSection, na.rm = TRUE), by = resolution]
    
    # Distribute the NASC by the representativeBackscatteringCrossSectionNormalized:
    DensityData[, NASCDistributed := NASC * representativeBackscatteringCrossSectionNormalized]
    # and get the density by dividing by the cross section (4 * pi * backscatteringCrossSection):
    DensityData[, crossSection := 4 * pi * backscatteringCrossSection]
    DensityData[, Density := NASCDistributed / crossSection]
    
    return(DensityData[])
}


# Convert a table of lenght and TS to a funciton:
getTargetStrengthByLengthFunction <- function(TargetStrengthTable, method = "constant", rule = 2) {
    
    # Define the columns to modify:
    functionColumns <- c("TotalLength", "TargetStrength")
    by <- setdiff(names(TargetStrength), functionColumns)
    
    # Add mid points to the TargetStrength to facilitate use of approxfun with method = "constant":
    TargetStrengthTable  <- expandTargetStrengthTable(TargetStrengthTable, by = by)
    
    # Get one function for each combination of the columns given by 'by':
    TargetStrengthWithFunction <- TargetStrengthTable[, .(TargetStrengthFunction = getTargetStrengthByLengthFunctionOne(.SD, by = by, method = method, rule = rule)), by = by]
    
    return(TargetStrengthWithFunction)
}


# Define the function to get the target strength function, using one TargetStrength (a subset of the TargetStrength):
getTargetStrengthByLengthFunctionOne <- function(TargetStrengthTable, by, method = "constant", rule = 2) {
    
    # Define the target strength function as a function of length and length interval:
    targetStrengthByLengthFunctionOne <- function(TotalLength) {
        output <- approx(
            x = TargetStrengthTable$TotalLength, 
            y = TargetStrengthTable$TargetStrength, 
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
expandTargetStrengthTable <- function(TargetStrengthTable, by) {
    # Add mid points of the lengths between the first and last:
    TargetStrengthTable[, .(
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
getTargetStrength <- function(Data, TargetStrengthMethod) {
    
    # Get the length interval mid points:
    Data[, midIndividualTotalLength := getMidIndividualTotalLength(.SD)]
    
    # Check wich model is selected
    if(grepl("LengthDependent", TargetStrengthMethod, ignore.case = TRUE)) {
        # Apply the LengthDependent equation: 
        Data[, TargetStrength := getRstoxBaseDefinitions("TargetStrengthFunction_LengthDependent")(
            midIndividualTotalLength, 
            TargetStrength0 = TargetStrength0, 
            LengthExponent = LengthExponent
        )]
    }
    else if(grepl("LengthAndDepthDependent", TargetStrengthMethod, ignore.case = TRUE)) {
        # Add the the depth:
        verticalLayerDimension <- getDataTypeDefinition(dataType = "LengthDistributionData", elements = "verticalLayerDimension", unlist = TRUE)
        Data[, midDepth := rowMeans(.SD), .SDcols = verticalLayerDimension]
        
        # Apply the LengthAndDepthDependent equation: 
        Data[, TargetStrength := getRstoxBaseDefinitions("TargetStrengthFunction_LengthAndDepthDependent")(
            midIndividualTotalLength, 
            TargetStrength0 = TargetStrength0, 
            LengthExponent = LengthExponent, 
            Depth = Depth
        )]
    }
    else if(grepl("LengthExponent", TargetStrengthMethod, ignore.case = TRUE)) {
        # Apply only the LengthExponent: 
        Data[, TargetStrength := getRstoxBaseDefinitions("TargetStrengthFunction_LengthExponent")(
            midIndividualTotalLength, 
            LengthExponent = LengthExponent
        )]
    }
    else if(grepl("TargetStrengthByLength", TargetStrengthMethod, ignore.case = TRUE)) {
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
        warning("StoX: Invalid TargetStrengthMethod (", paste(getRstoxBaseDefinitions("targetStrengthParameters"), collapse = ", "), " currently implemented)")
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
#' This function calculates the area density of fish as number of individuals per square nautical mile.
#' 
#' @inheritParams ModelData
#' @param SweepWidthMethod The method for calculating the sweep width to multiply the \code{WeightedCount} in the \code{LengthDistributionData} with. Possible options are (1) "Constant", which requires \code{SweepWidth} to be set as the constant sweep width, (2) "PreDefined", impying that the sweep width is already incorporated in the \code{WeightedCount} in the \code{LengthDistributionData}, by using LengthDistributionType == "SweepWidthCompensatedNormalized" in the function \code{\link{LengthDependentCatchCompensation}}, and (3) "CruiseDependent", which requires the \code{SweepWidth} to be given.
#' @param SweepWidth The constant sweep width in meters.
#' @param SweepWidth A table of two columns, \code{Cruise} and \code{SweepWidth}, giving the sweep width for each cruise.
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
#' @seealso See \code{\link{AcousticDensity}} for acoustic density.
#' 
#' @export
#' 
SweptAreaDensity <- function(
    MeanLengthDistributionData, 
    SweepWidthMethod = c("Constant", "PreDefined", "CruiseDependent"), 
    SweepWidth = double(), 
    SweepWidthByCruise = data.table::data.table()
) {
	
    ## Get the DefinitionMethod:
    SweepWidthMethod <- match.arg(SweepWidthMethod)
    
    # Get the length distribution type:
    LengthDistributionType <- utils::head(MeanLengthDistributionData$Data$LengthDistributionType, 1)
    
    # Issue an error if the LengthDistributionType is "Percent" or "Standard":
    validLengthDistributionType <- c("Normalized", "SweepWidthCompensatedNormalized", "SelectivityCompensatedNormalized")
    if(! LengthDistributionType %in% validLengthDistributionType) {
        stop("The LengthDistributionType of the input MeanLengthDistributionData must be one of ", paste(validLengthDistributionType, collapse = ", "))
    }
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    DensityData = data.table::copy(MeanLengthDistributionData$Data)
    
    # Introduce the DensityWeight as a copy of the MeanLengthDistributionWeight:
    DensityData[, DensityWeight := MeanLengthDistributionWeight]
    
    # If LengthDistributionType is "NormalizedLengthDistribution", the effectivev towed distance has been accounted for, but there is still need to account for the sweep width:
    if(LengthDistributionType %in% c("Normalized", "SelectivityCompensatedNormalized")) {
        
        # Use a constant sweep width for all data by default:
        if(SweepWidthMethod == "Constant") {
            if(length(SweepWidth) == 0) {
                stop("SweepWidth must be given when SweepWidthMethod == \"Constant\"")
            }
            
            # Convert WeightedCount to density:
            sweepWidthInNauticalMiles <- SweepWidth / getRstoxBaseDefinitions("nauticalMileInMeters")
            
            DensityData[, Density := WeightedCount / sweepWidthInNauticalMiles]
        }
        else if(SweepWidthMethod == "CruiseDependent") {
            if(length(SweepWidthByCruise) == 0) {
                stop("SweepWidthByCruise must be given when SweepWidthMethod == \"CruiseDependent\"")
            }
            
            # Merge in the SweepWidth:
            DensityData <- merge(DensityData, SweepWidthByCruise, by = "Cruise")
            # Convert sweep width to nautical miles:
            DensityData[, SweepWidthNauticalMile := SweepWidth  / getRstoxBaseDefinitions("nauticalMileInMeters")]
            # Divide by the sweep width:
            DensityData[, Density := WeightedCount / SweepWidthNauticalMile, by = "Cruise"]
        }
        else {
            stop("SweepWidthMethod must be \"Constant\" or \"CruiseDependent\" if LengthDistributionType is \"Normalized\" or \"SelectivityCompensatedNormalized\" in the MeanLengthDistributionData")
        }
        
    }
    # If LengthDistributionType == "SweepWidthCompensatedNormalized" the sweep width is already accounted for, in meters:
    else if(LengthDistributionType == "SweepWidthCompensatedNormalized") {
        if(SweepWidthMethod == "PreDefined") {
            # WeightedCount is already in area density when LengthDistributionType is SweepWidthCompensatedNormalized, which is generated by LengthDependentCatchCompensation():
            DensityData[, Density := WeightedCount]
        }
        else {
            stop("SweepWidthMethod must be \"PreDefined\" if LengthDistributionType is \"SweepWidthCompensatedNormalized\" in the MeanLengthDistributionData")
        }
    }
    else {
        # In StoX 2.7 and earlier, LengthDistType = \"LengthDist\" was allowed, where the distance was multiplied with the weighted count in SweptAreaDensity.
        stop("Invalid LengthDistributionType.")
    }
    
    # Convert to density per nautical mile atwarthship:
    #DensityData[, Density := Density * lengthOfOneNauticalMile]
    
    # Extract the relevant variables only:
    #relevantVariables <- getAllDataTypeVariables(dataType = "DensityData")
    #DensityData <- DensityData[, ..relevantVariables]
    formatOutput(DensityData, dataType = "DensityData", keep.all = FALSE)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(DensityData)
    
    return(DensityData)
}



##################################################
##################################################
#' Mean density in each stratum
#' 
#' This function calculates the weighted avverage of the density in each stratum (or possibly in each PSU, which impies returning the input DensityData unchanged). The weights are effective log distance for acoustic density and number of hauls ??????????????????? per PSU for swept area density.
#' 
#' @inheritParams ProcessData
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
#' @seealso See \code{\link{AcousticDensity}} for acoustic density and  \code{\link{SweptAreaDensity}} for swept-area density.
#' 
#' @export
#' 
MeanDensity <- function(
    DensityData
) {
    MeanDensityData <- applyMeanToData(data = DensityData, dataType = "DensityData", targetResolution = "Stratum")
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(MeanDensityData)
    
    return(MeanDensityData)
}


##################################################
##################################################
#' Table of biotic stations as rows with station info and catch of all species categories as columns
#' 
#' This function organizes the catch of all species in the input \code{StoxBioticData} into a table with biotic stations as rows, and the catch in count or weight (kilogram) of each species category in columns.
#' 
#' @inheritParams ModelData
#' @param CatchVariable Specifies whether to output catch or weight (kilogram).
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @export
#' 
SpeciesCategoryCatch <- function(
    StoxBioticData, 
    CatchVariable = c("Count", "Weight")
) {
    
    # Get the DensityUnit and DensityType:
    CatchVariable <- match.arg(CatchVariable)
    
    # Merge Station, ..., Sample table:
    StoxBioticDataMerged <- MergeStoxBiotic(StoxBioticData, TargetTable = "Sample")
    #Cruise_Station <- MergeStoxBiotic(StoxBioticData, TargetTable = "Station")
    Cruise_Station_Haul <- MergeStoxBiotic(StoxBioticData, TargetTable = "Haul")
    
    # Sum the CatchFractionWeight for each Haul:
    CatchVariableName <- paste0("CatchFraction", CatchVariable)
    categoryVariable <- getDataTypeDefinition(dataType = "DensityData", elements = "categoryVariable", unlist = TRUE)
    sumBy <- c("Haul", categoryVariable)
    StoxBioticDataMerged[, eval(CatchVariableName) := sum(get(CatchVariableName)), by = sumBy]
    
    # Warning if there are species ccategories which are empty string:
    emptyString <- StoxBioticDataMerged[, nchar(get(categoryVariable))] == 0
    if(any(emptyString, na.rm = TRUE)) {
        warning("StoX: There are empty strings for the ", categoryVariable, ". These will be included in the column V1 in the SpeciesCategoryCatch table.")
    }
    
    # Create the SpeciesCategoryDensity as a table with species categories in the columns:
    SpeciesCategoryCatch <- data.table::dcast(
        StoxBioticDataMerged, 
        formula = Haul ~ get(categoryVariable), 
        value.var = CatchVariableName, 
        fun.aggregate = sum
        )
    
    SpeciesCategoryCatchData <- list(
        HaulInfo = Cruise_Station_Haul, 
        SpeciesCategoryCatch = SpeciesCategoryCatch
    )
    
    #SpeciesCategoryCatchData <- merge(Cruise_Station_Haul, SpeciesCategoryCatchData, by = "Station")
    
    # Not needed here, since we only copy data: 
    #Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(SpeciesCategoryCatchData)
    
    return (SpeciesCategoryCatchData)
}
