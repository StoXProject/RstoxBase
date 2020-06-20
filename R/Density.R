
##################################################
##################################################
#' Calculate number density from NASC in length intervals
#' 
#' This function takes NASC and 
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
AcousticDensity <- function(
    NASCData,
    AssignmentLengthDistributionData,
    AcousticTargetStrength,
    SpeciesLinkTable){
    
    # Check that the input SpeciesLinkTable has the appropriate types:
    checkTypes(table = SpeciesLinkTable)
    
    # Check that the NASCData has PSU and Layer resolution:
    checkResolutionPSU_Layer(NASCData, "NASCData")
    
    # Merge TargetStrengthTable with SpeciesLinkTable in order to get the targets trengt for each SepciesCategory (and not only AcousticCategory):
    AcousticTargetStrength$TargetStrengthTable <- merge(AcousticTargetStrength$TargetStrengthTable, SpeciesLinkTable, by='AcousticCategory', all = TRUE, allow.cartesian = TRUE)
    # Define the resolution on which to distribute the NASC:
    resolution <- getDataTypeDefinition(dataType = "DensityData", elements = c("horizontalResolution", "verticalResolution"), unlist = TRUE)
    
    # Convert NASC to number density using the length distribution coupled with the target strength:
    DensityData <- NASCToDensity(
        NASCData = NASCData, 
        LengthDistributionData = AssignmentLengthDistributionData, 
        AcousticTargetStrength = AcousticTargetStrength, 
        resolution = resolution
    )
    
    # Introduce the DensityWeight as a copy of the NASCWeight:
    DensityData[, DensityWeight := NASCWeight]
    
    # Keep only the releavnt columns:
    #keepOnlyRelevantColumns(DensityData, "DensityData")
    formatOutput(DensityData, dataType = "DensityData", keep.all = FALSE)
    
    return(DensityData)
}

NASCToDensity <- function(NASCData, LengthDistributionData, AcousticTargetStrength, resolution) {
    # Merge the TargetStrengthTable into the NASCData to form the DensityData. This adds the parameters of the target strength to length relationship. This step is important, as merging is done by the AcousticCategory, Frequency and possibly other grouping columns.:
    
    
    # Take special care of TargetStrengthMethods that are tables of length instead of functions, in which we apply constant interpolation to the lengths in the data here, to facilitate correct merging:
    if(getRstoxBaseDefinitions("targetStrengthMethodTypes")[[AcousticTargetStrength$TargetStrengthMethod$TargetStrengthMethod]] == "Table") {
        #AcousticTargetStrength$TargetStrengthFunction <- getTargetStrengthFunction(
        #    TargetStrengthTable = AcousticTargetStrength$TargetStrengthTable, 
        #    LengthData = LengthDistributionData
        #)
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
    
    # Merge the LengthDistributionData into the DensityData. This adds the length distribution:
    mergeBy <- intersect(names(DensityData), names(LengthDistributionData))
    DensityData <- merge(DensityData, LengthDistributionData, by = mergeBy, all.x = TRUE)
    
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

## Convert a table of lenght and TS to a funciton:
#getTargetStrengthFunctionOld <- function(TargetStrengthTable, LengthData, method = "constant", rule = 2) {
#    # Approximate the TargetStrength to the mid lengths of the length interavls of the LengthData (such as length distribution). Take a #copy first since the IndividualTotalLengthCentimeter is replaced by reference using getLengthIntervalMidPoints with replace.IndividualTo#talLengthCentimeter = TRUE:
#    LengthDataCopy <- data.table::copy(LengthData)
#    LengthData[, IndividualTotalLengthCentimeter := getLengthIntervalMidPoints(.SD)]
#    
#    # Since approxfun with method "constant" defines values to span to the next value, we need to modify the TargetStrengthTable to have #the first point followed by the mid points (in x), and then followed by the last point:
#    TargetStrengthTable <- addMidPointsOfTargetStrengthTable(TargetStrengthTable)
#    
#    # Create the function to use in getTargetStrength():
#    approxfun(
#        x = TargetStrengthTable$IndividualTotalLengthCentimeter, 
#        y = TargetStrengthTable$TargetStrength, 
#        xout = sort(unique(LengthData$IndividualTotalLengthCentimeter)), 
#        method = method, 
#        rule = rule
#    )
#}


# Convert a table of lenght and TS to a funciton:
getTargetStrengthByLengthFunction <- function(TargetStrengthTable, method = "constant", rule = 2) {
    
    # Define the columns to modify:
    functionColumns <- c("IndividualTotalLengthCentimeter", "TargetStrength")
    by <- setdiff(names(TargetStrengthTable), functionColumns)
    
    # Add mid points to the TargetStrengthTable to facilitate use of approxfun with method = "constant":
    TargetStrengthTable  <- expandTargetStrengthTable(TargetStrengthTable, by = by)
    
    # Get one function for each combination of the columns given by 'by':
    TargetStrengthTableWithFunction <- TargetStrengthTable[, .(TargetStrengthFunction = getTargetStrengthByLengthFunctionOne(.SD, by = by, method = method, rule = rule)), by = by]
    
    return(TargetStrengthTableWithFunction)
}


# Define the function to get the target strength function, using one TargetStrengthTable (a subset of the TargetStrengthTable):
getTargetStrengthByLengthFunctionOne <- function(TargetStrengthTable, by, method = "constant", rule = 2) {
    
    # Define the target strength function as a function of length and length interval:
    targetStrengthByLengthFunctionOne <- function(midIndividualTotalLengthCentimeter) {
        output <- approx(
            x = TargetStrengthTable$IndividualTotalLengthCentimeter, 
            y = TargetStrengthTable$TargetStrength, 
            xout = midIndividualTotalLengthCentimeter, 
            method = method, 
            rule = rule
        )$y
        
        return(output)
    }
    
    # Save the function in the package environment:
    #functionName <- paste0("StrengthByLengthFunction_", paste(by, TargetStrengthTable[1, ..by], sep = "_", collapse = "_"))
    functionName <- paste0("StrengthByLengthFunction_UNIXTime", unclass(Sys.time()))
    assign(
        x = functionName, 
        value = targetStrengthByLengthFunctionOne, 
        envir = get("RstoxBaseEnv")
    )
    
    # Return the function:
    return(functionName)
}






## Convert a table of lenght and TS to a funciton:
#getTargetStrengthFunctionOne <- function(TargetStrengthTable, LengthData, method = "constant", rule = 2) {
#    
#    # Define the columns to modify:
#    functionColumns <- c("IndividualTotalLengthCentimeter", "TargetStrength")
#    by <- setdiff(names(TargetStrengthTable), functionColumns)
#    
#    # Add mid points to the TargetStrengthTable to facilitate use of approxfun with method = "constant":
#    TargetStrengthTable  <- addMidPointsOfTargetStrengthTable(TargetStrengthTable, by = by)
#    
#}

# Function to prepare a TargetStrengthTable for approxfun():
expandTargetStrengthTable <- function(TargetStrengthTable, by) {
    # Add mid points of the lengths between the first and last:
    TargetStrengthTable[, .(
        IndividualTotalLengthCentimeter = c(
            utils::head(IndividualTotalLengthCentimeter, 1), 
            IndividualTotalLengthCentimeter[-1] - diff(IndividualTotalLengthCentimeter) / 2, 
            utils::tail(IndividualTotalLengthCentimeter, 1)
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
getTargetStrength <- function(Data, TargetStrengthMethod){
    
    # Get the length interval mid points:
    Data[, midIndividualTotalLengthCentimeter := getMidIndividualTotalLengthCentimeter(.SD)]
    
    # Check wich model is selected
    if(grepl("LengthDependent", TargetStrengthMethod, ignore.case = TRUE)){
        # Apply the LengthDependent equation: 
        Data[, TargetStrength := getRstoxBaseDefinitions("TargetStrengthFunction_LengthDependent")(
            midIndividualTotalLengthCentimeter = midIndividualTotalLengthCentimeter, 
            TargetStrength0 = TargetStrength0, 
            LengthExponent = LengthExponent
        )]
    }
    else if(grepl("LengthAndDepthDependent", TargetStrengthMethod, ignore.case = TRUE)){
        # Add the the depth:
        verticalLayerDimension <- getDataTypeDefinition(dataType = "LengthDistributionData", elements = "verticalLayerDimension", unlist = TRUE)
        Data[, midDepthMeter := rowMeans(.SD), .SDcols = verticalLayerDimension]
        
        # Apply the LengthAndDepthDependent equation: 
        Data[, TargetStrength := getRstoxBaseDefinitions("TargetStrengthFunction_LengthAndDepthDependent")(
            midIndividualTotalLengthCentimeter = midIndividualTotalLengthCentimeter, 
            TargetStrength0 = TargetStrength0, 
            LengthExponent = LengthExponent, 
            DepthMeter = DepthMeter
        )]
    }
    else if(grepl("LengthExponent", TargetStrengthMethod, ignore.case = TRUE)){
        # Apply only the LengthExponent: 
        Data[, TargetStrength := getRstoxBaseDefinitions("TargetStrengthFunction_LengthExponent")(
            midIndividualTotalLengthCentimeter = midIndividualTotalLengthCentimeter, 
            LengthExponent = LengthExponent
        )]
    }
    else if(grepl("TargetStrengthByLength", TargetStrengthMethod, ignore.case = TRUE)){
        # Apply the TargetStrengthByLengthTargetStrengthByLength equations: 
        Data[, TargetStrength := 
            if(!is.na(TargetStrengthFunction)) 
                    get("RstoxBaseEnv")[[TargetStrengthFunction]](midIndividualTotalLengthCentimeter)
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
getMidIndividualTotalLengthCentimeter <- function(x) {
    x[, IndividualTotalLengthCentimeter + LengthResolutionCentimeter / 2]
}


##################################################
##################################################
#' Swept-area density
#' 
#' This function calculates the area density of fish as number of individuals per square nautical mile.
#' 
#' @inheritParams RegroupLengthDistribution
#' @param SweepWidthMethod The method for calculating the sweep width to multiply the \code{WeightedCount} in the \code{LengthDistributionData} with. Possible options are (1) "Constant", which requires \code{SweepWidth} to be set as the constant sweep width, (2) "PreDefined", impying that the sweep width is already incorporated in the \code{WeightedCount} in the \code{LengthDistributionData}, by using LengthDistributionType == "SweepWidthCompensatedNormalized" in the function \code{\link{LengthDependentCatchCompensation}}, and (3) "CruiseDependent", which requires the \code{SweepWidthTable} to be given.
#' @param SweepWidth The constant sweep width of the project.
#' @param SweepWidthTable A table of two columns, \code{Cruise} and \code{SweepWidth}, giving the sweep width for each cruise.
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
SweptAreaDensity <- function(
    LengthDistributionData, 
    SweepWidthMethod = c("Constant", "PreDefined", "CruiseDependent"), 
    SweepWidth = integer(), 
    SweepWidthTable = data.table::data.table()
) {
	
    ## Get the DefinitionMethod:
    SweepWidthMethod <- match.arg(SweepWidthMethod)
    
    # Check the horizontal and vertical resolution of the input LengthDistributionData:
    if(any(!is.na(LengthDistributionData$Station)) && any(!is.na(LengthDistributionData$Haul))) {
        stop("The horizontal and vertical resolution of the input LengthDistributionData must be PSU (identified by only NAs in the Station column) and Layer (identified by only NAs in the Haul column)")
    }
    else if(any(!is.na(LengthDistributionData$Station))) {
        stop("The horizontal resolution of the input LengthDistributionData must be PSU (identified by only NAs in the Station column)")
    }
    else if(any(!is.na(LengthDistributionData$Haul))) {
        stop("The vertical resolution of the input LengthDistributionData must be Layer (identified by only NAs in the Haul column)")
    }
    
    # Get the length distribution type:
    LengthDistributionType <- utils::head(LengthDistributionData$LengthDistributionType, 1)
    
    # Issue an error if the LengthDistributionType is "Percent" or "Standard":
    validLengthDistributionType <- c("Normalized", "SweepWidthCompensatedNormalized", "SelectivityCompensatedNormalized")
    if(! LengthDistributionType %in% validLengthDistributionType) {
        stop("The LengthDistributionType of the input LengthDistributionData must be one of ", paste(validLengthDistributionType, collapse = ", "))
    }
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    DensityData = data.table::copy(LengthDistributionData)
    
    # Introduce the DensityWeight as a copy of the LengthDistributionWeight:
    DensityData[, DensityWeight := LengthDistributionWeight]
    
    # If LengthDistributionType is "NormalizedLengthDistribution", the effectivev towed distance has been accounted for, but there is still need to account for the sweep width:
    if(LengthDistributionType %in% c("Normalized", "SelectivityCompensatedNormalized")) {
        
        # Use a constant sweep width for all data by default:
        if(SweepWidthMethod == "Constant") {
            if(length(SweepWidth) == 0) {
                stop("SweepWidth must be given when SweepWidthMethod == \"Constant\"")
            }
            
            # Convert WeightedCount to density:
            #sweepWidthInNauticalMiles <- SweepWidth / 1852
            sweepWidthInNauticalMiles <- SweepWidth / getRstoxBaseDefinitions("nauticalMileInMeters")
            
            DensityData[, Density := WeightedCount / sweepWidthInNauticalMiles]
        }
        else if(SweepWidthMethod == "CruiseDependent") {
            stop("SweepWidthMethod = \"CruiseDependent\" is not yet implemented")
            if(length(SweepWidthTable) == 0) {
                stop("SweepWidthTable must be given when SweepWidthMethod == \"CruiseDependent\"")
            }
        }
        else {
            stop("SweepWidthMethod must be \"Constant\" or \"CruiseDependent\" if LengthDistributionType is \"Normalized\" or \"SelectivityCompensatedNormalized\" in the LengthDistributionData")
        }
        
    }
    # If LengthDistributionType == "SweepWidthCompensatedNormalized" the sweep width is already accounted for, in meters:
    else if(LengthDistributionType == "SweepWidthCompensatedNormalized") {
        if(SweepWidthMethod == "PreDefined") {
            # WeightedCount is already in area density when LengthDistributionType is SweepWidthCompensatedNormalized, which is generated by LengthDependentCatchCompensation():
            DensityData[, Density := WeightedCount]
        }
        else {
            stop("SweepWidthMethod must be \"PreDefined\" if LengthDistributionType is \"SweepWidthCompensatedNormalized\" in the LengthDistributionData")
        }
    }
    else {
        # In StoX 2.7 and earlier, LengthDistType = \"LengthDist\" was allowed, where the distance was multiplied with the weighted count in SweptAreaDensity.
        stop("Invalid LengthDistributionType.")
    }
    
    # Convert to density per nautical mile atwarthship:
    #DensityData[, Density := Density * lengthOfOneNauticalMile]
    
    # Extract the relevant variables only:
    relevantVariables <- getAllDataTypeVariables(dataType = "DensityData")
    DensityData <- DensityData[, ..relevantVariables]
    
    return(DensityData)
}



##################################################
##################################################
#' Mean density in each stratum
#' 
#' This function calculates the weighted avverage of the density in each stratum (or possibly in each PSU, which impies returning the input DensityData unchanged). The weights are effective log distance for acoustic density and number of hauls ??????????????????? per PSU for swept area density.
#' 
#' @param DensityData An object of \code{\link{DensityData}} data.
#' @param TargetResolution The vertical resolution of the output.
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
MeanDensity <- function(DensityData, TargetResolution = "Stratum") {
    meanData(DensityData, dataType = "DensityData", targetResolution = TargetResolution)
}


##################################################
##################################################
#' Table of biotic stations as rows with station info and density of all species categories as columns
#' 
#' This function organizes density of all species in the input Density into a table with biotic stations as rows, and the density of each species category in columns. The function requires each swept-area PSUs to contain only one station.
#' 
#' @inheritParams MeanDensity
#' @inheritParams DefineSweptAreaPSU
#' @inheritParams LengthDistribution
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @export
#' 
SpeciesCategoryDensity <- function(DensityData, StoxBioticData, SweptAreaPSU) {
    
    # Check that there is only one Station per PSU in SweptAreaPSU$Station_PSU: 
    NumberOfStationsPerPSU <- SweptAreaPSU$Station_PSU[, .N, by = "PSU"]
    PSUsWithMoreStations <- NumberOfStationsPerPSU[N>1]
    nrOfRows <- PSUsWithMoreStations[,.N]
    psuCol <- PSUsWithMoreStations[,PSU]

    if(nrOfRows == 1) {
        if(!is.na(psuCol)) {
            stop("PSU ", psuCol, " is assigned more than one station.")
        }
    }
    else if(nrOfRows > 1) {
        stop("PSUs ", paste(psuCol, collapse = ", ") , " are assigned more than one station.")
    }
    
    # Sum the DensityData vertically and across length groups:
    horizontalResolution <- getDataTypeDefinition(dataType = "DensityData", elements = "horizontalResolution", unlist = TRUE)
    categoryVariable <- getDataTypeDefinition(dataType = "DensityData", elements = "categoryVariable", unlist = TRUE)
    #data <- getDataTypeDefinition(dataType = "DensityData", elements = "data", unlist = TRUE)
    
    sumBy <- c(horizontalResolution, categoryVariable)
    DensityData <- DensityData[, .(Density = sum(Density)), by = sumBy]
    
    # Add the column Station to the DensityData by merging DensityData and SweptAreaPSU$Station_PSU by PSU:
    DensityData = merge(DensityData, SweptAreaPSU$Station_PSU, by="PSU")

    # Create the SpeciesCategoryDensity as a table with species categories in the columns: 
    SpeciesCategoryDensityData <- data.table::dcast(
        DensityData, 
        formula = Station ~ get(categoryVariable), 
        value.var = "Density")
    
    # Get the Station information by merging StoxBioticData$Cruise and StoxBioticData$Station into Cruise_Station: 
    #Cruise_Station <- merge(StoxBioticData$Cruise, StoxBioticData$Station)
    Cruise_Station_Haul <- MergeStoxBiotic(StoxBioticData, TargetTable = "Haul")
    
    # Merge SpeciesCategoryDensity with Cruise_Station_Haul by Station:
    SpeciesCategoryDensityData <- merge(Cruise_Station_Haul, SpeciesCategoryDensityData, by="Station")
    
    return (SpeciesCategoryDensityData)
}

##################################################
##################################################
#' Table of biotic stations as rows with station info and catch of all species categories as columns
#' 
#' This function organizes the catch of all species in the input \code{StoxBioticData} into a table with biotic stations as rows, and the catch in count or weight (kilogram)   of each species category in columns.
#' 
#' @inheritParams DefineSweptAreaPSU
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
SpeciesCategoryCatch <- function(StoxBioticData, CatchVariable = c("Count", "WeightKilogram")) {
    
    # Get the DensityUnit and DensityType:
    CatchVariable <- match.arg(CatchVariable)
    
    # Merge Station, ..., Sample table:
    StoxBioticDataMerged <- MergeStoxBiotic(StoxBioticData, TargetTable = "Sample")
    #Cruise_Station <- MergeStoxBiotic(StoxBioticData, TargetTable = "Station")
    Cruise_Station_Haul <- MergeStoxBiotic(StoxBioticData, TargetTable = "Haul")
    
    # Sum the CatchFractionWeightKilogram for each Haul:
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
    
    return (SpeciesCategoryCatchData)
}
