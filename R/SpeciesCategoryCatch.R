##################################################
##################################################
#' Table of total catch weight and number per Haul and SpeciesCategory
#' 
#' This function sums the catch fraction weight and number per Haul and SpeciesCategory.
#' 
#' @inheritParams ModelData
#' @param SpeciesCategoryCatchType The type of the total catch variables, either "Standard" for the raw total catch and "Normalized" for normalizing by tow distance by dividing by the EffectiveTowDistance in nautical miles.
#' 
#' @return An object of StoX data type \code{\link{SpeciesCategoryCatchData}}.
#' 
#' @export
#' 
SpeciesCategoryCatch <- function(
    StoxBioticData, 
    SpeciesCategoryCatchType = c("Normalized", "Standard")
) {
    
    # Store the sample names to remove them as columns in the end of the function:
    sampleNames <- names(StoxBioticData$Sample)
    sampleNamesToRemove <- c(sampleNames[!endsWith(sampleNames, "Key")], "SampleKey")
    
    # Get the DensityUnit and DensityType:
    SpeciesCategoryCatchType <- RstoxData::match_arg_informative(SpeciesCategoryCatchType)
    
    # Merge Station, ..., Sample table:
    SpeciesCategoryCatchData <- RstoxData::MergeStoxBiotic(StoxBioticData, TargetTable = "Sample")
    
    # Sum the CatchFractionWeight and CatchFractionNumber (separately) for each Haul (verticalResolution) and SpeciesCategory (categoryVariable):
    dataVariables <- getDataTypeDefinition(dataType = "SpeciesCategoryCatchData", elements = "data", unlist = TRUE)
    CatchFractionVariables <- sub("TotalCatch", "CatchFraction", dataVariables)
    sumBy <-  getDataTypeDefinition(
        dataType = "SpeciesCategoryCatchData", 
        elements = c("verticalResolution", "categoryVariable"), 
        unlist = TRUE
    )
    # Sum for weight and number separately:
    for(ind in seq_along(dataVariables)) {
        # Make sure that these are numeric, as data.table somehow performs an integer operation when normalizing below (which does not happen in the console, but for some reason here):
        SpeciesCategoryCatchData[, eval(dataVariables[ind]) := sum(get(CatchFractionVariables[ind])), by = sumBy]
        #SpeciesCategoryCatchData[, eval(dataVariables[ind]) := as.numeric(sum(get(CatchFractionVariables[ind]))), by = sumBy]
    }
    # Uniquify, as the above sum is by reference, and thus keeps all rows:
    SpeciesCategoryCatchData <- unique(SpeciesCategoryCatchData, by = sumBy)
    
    # Normalize if requested:
    if(SpeciesCategoryCatchType == "Normalized") {
        for(ind in seq_along(dataVariables)) {
            SpeciesCategoryCatchData[, eval(dataVariables[ind]) := get(dataVariables[ind]) / EffectiveTowDistance, by = sumBy]
        }
    }
    
    # Add SpeciesCategoryCatchType and SpeciesCategoryCatchWeight:
    SpeciesCategoryCatchData[, SpeciesCategoryCatchType := ..SpeciesCategoryCatchType]
    SpeciesCategoryCatchData[, SpeciesCategoryCatchWeight := 1]
    
    # Discard the variables in the Sample table, since we have summed over samples:
    SpeciesCategoryCatchData[, (sampleNamesToRemove) := NULL]

    # Format the output:
    # 2024-08-20: We keep all possible variables from StoxBiotic here excluding the keys:
    formatOutput(SpeciesCategoryCatchData, dataType = "SpeciesCategoryCatchData", keep.all = TRUE, removeStoXKeys = TRUE)
    
    return (SpeciesCategoryCatchData)
}

# ##################################################
# ##################################################
# #' Table of total catch weight and number per Haul and SpeciesCategory
# #' 
# #' This function sums the catch fraction weight and number per Haul and SpeciesCategory.
# #' 
# #' @inheritParams ModelData
# #' @param SpeciesCategoryCatchType The type of the total catch variables, either "Standard" for the raw total catch and "Normalized" for normalizing by tow distance # by dividing by the EffectiveTowDistance in nautical miles.
# #' 
# #' @export
# #' 
# SpeciesCategoryCatch_Old <- function(
#     StoxBioticData, 
#     SpeciesCategoryCatchType = c("Normalized", "Standard")
# ) {
#     
#     # Store the sample names to remove them as columns in the end of the function:
#     sampleNames <- names(StoxBioticData$Sample)
#     sampleNamesToRemove <- c(sampleNames[!endsWith(sampleNames, "Key")], "SampleKey")
#     
#     # Get the DensityUnit and DensityType:
#     SpeciesCategoryCatchType <- RstoxData::match_arg_informative(SpeciesCategoryCatchType)
#     
#     # Merge Station, ..., Sample table:
#     SpeciesCategoryCatchData <- RstoxData::MergeStoxBiotic(StoxBioticData, TargetTable = "Sample")
#     
#     # Sum the CatchFractionWeight for each Haul (verticalResolution) and SpeciesCategory (categoryVariable):
#     dataVariables <- getDataTypeDefinition(dataType = "SpeciesCategoryCatchData", elements = "data", unlist = TRUE)
#     CatchFractionVariables <- sub("TotalCatch", "CatchFraction", dataVariables)
#     sumBy <-  getDataTypeDefinition(
#         dataType = "SpeciesCategoryCatchData", 
#         elements = c("verticalResolution", "categoryVariable"), 
#         unlist = TRUE
#     )
#     # Sum for weight and number separately:
#     for(ind in seq_along(dataVariables)) {
#         SpeciesCategoryCatchData[, eval(dataVariables[ind]) := sum(get(CatchFractionVariables[ind])), by = sumBy]
#     }
#     
#     # Replicate the old bug where subsamples were multiplied by the number of sumbsamples:
#     SpeciesCategoryCatchData <- unique(SpeciesCategoryCatchData, by = c("Sample", sumBy))
#     SpeciesCategoryCatchData[, NumberOfSamples := .N, by = sumBy]
#     
#     # Uniquify, as the above sum is by reference, and thus keeps all rows:
#     SpeciesCategoryCatchData <- unique(SpeciesCategoryCatchData, by = sumBy)
#     SpeciesCategoryCatchData[, eval(dataVariables[ind]) := get(dataVariables[ind]) * NumberOfSamples]
#     
#     # Normalize if requested:
#     if(SpeciesCategoryCatchType == "Normalized") {
#         for(ind in seq_along(dataVariables)) {
#             SpeciesCategoryCatchData[, eval(dataVariables[ind]) := get(dataVariables[ind]) / EffectiveTowDistance, by = sumBy]
#         }
#     }
#     
#     # Add SpeciesCategoryCatchType and SpeciesCategoryCatchWeight:
#     SpeciesCategoryCatchData[, SpeciesCategoryCatchType := ..SpeciesCategoryCatchType]
#     SpeciesCategoryCatchData[, SpeciesCategoryCatchWeight := 1]
#     
#     # Discard the variables in the Sample table, since we have summed over samples:
#     SpeciesCategoryCatchData[, (sampleNamesToRemove) := NULL]
#     
#     # Format the output:
#     formatOutput(SpeciesCategoryCatchData, dataType = "SpeciesCategoryCatchData", keep.all = TRUE)
#     
#     return (SpeciesCategoryCatchData)
# } 


##################################################
##################################################
#' Sum species category catch over Hauls for each Station
#' 
#' This function summes \code{link{SpeciesCategoryCatchData}} data vertically.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams SumLengthDistribution
#' @inheritParams DefineBioticLayer
#' 
#' @return
#' An \code{\link{SumSpeciesCategoryCatchData}} object.
#' 
#' @export
#' 
SumSpeciesCategoryCatch <- function(
    SpeciesCategoryCatchData, 
    LayerDefinition = c("FunctionParameter", "FunctionInput"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    BioticLayer = NULL
) {
    
    SumSpeciesCategoryCatchData<- sumRawResolutionData(
        data = SpeciesCategoryCatchData, dataType = "SpeciesCategoryCatchData", 
        LayerDefinition = LayerDefinition, 
        LayerProcessData = BioticLayer, 
        LayerDefinitionMethod = LayerDefinitionMethod, 
        Resolution = Resolution, 
        LayerTable = LayerTable, 
        LayerType = "Biotic"
    )
    
    # Format the output:
    formatOutput(SumSpeciesCategoryCatchData, dataType = "SumSpeciesCategoryCatchData", keep.all = FALSE)
    
    return(SumSpeciesCategoryCatchData)
}



##################################################
##################################################
#' Mean species category catch over Stations in each AcousticPSU
#' 
#' This function averages \code{link{SpeciesCategoryCatchData}} data horizontally, weighted by the effective towed distance.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams MeanLengthDistribution
#' @inheritParams DefineBioticPSU
#' 
#' @return
#' An \code{\link{MeanSpeciesCategoryCatchData}} object.
#' 
#' @export
#' 
MeanSpeciesCategoryCatch <- function(
    SpeciesCategoryCatchData, 
    SumSpeciesCategoryCatchData, 
    # Parameters of the sum part:
    # Layer: 
    LayerDefinition = c("FunctionParameter", "FunctionInput", "PreDefined"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    BioticLayer = NULL, 
    # Survey: 
    SurveyDefinition = c("FunctionParameter", "FunctionInput"), 
    SurveyDefinitionMethod = c("AllStrata", "Table"), 
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
    LayerDefinition <- RstoxData::match_arg_informative(LayerDefinition)
    if(LayerDefinition != "PreDefined") {
        SumSpeciesCategoryCatchData <- SumSpeciesCategoryCatch(
            SpeciesCategoryCatchData = SpeciesCategoryCatchData, 
            LayerDefinition = LayerDefinition, 
            LayerDefinitionMethod = LayerDefinitionMethod, 
            Resolution = Resolution, 
            LayerTable = LayerTable, 
            BioticLayer = BioticLayer
        )
    }
    
    # Convert the PSUDefinitionMethod to "Identity" if "EDSUToPSU":
    PSUDefinitionMethod <- RstoxData::match_arg_informative(PSUDefinitionMethod)
    if(grepl("StationToPSU", PSUDefinitionMethod, ignore.case = TRUE)) {
        PSUDefinitionMethod <- "Identity"
    }
    
    # Run the mean part:
    MeanSpeciesCategoryCatchData <- meanRawResolutionData(
        data = SumSpeciesCategoryCatchData, dataType = "SumSpeciesCategoryCatchData", 
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
    formatOutput(MeanSpeciesCategoryCatchData, dataType = "MeanSpeciesCategoryCatchData", keep.all = FALSE)
    
    return(MeanSpeciesCategoryCatchData)
}




##################################################
##################################################
#' Apply the sweep of different gear (and cruise)
#' 
#' This function multiplies the CatchFractionWeight and CatchFractionNumber of a SpeciesCategoryCatchData by the sweep width given by \code{CompensationTable}. The result is a sweep width compensated length distribution (SpeciesCategoryCatchType starting with "SweepWidthCompensated").
#' 
#' @inheritParams ModelData
#' @param CompensationMethod The method to use for the length dependent catch compensation, i.e. specifying which columns to provide the sweep width for.
#' @param CompensationTable A table of the sweep width per combination of the variables specified in \code{CompensationMethod}. Note that all combinations present in the data must be given in the table, as the output should be sweep width compensated for all rows with non-missing WeightedNumber.
#' 
#' @return
#' A \code{\link{SpeciesCategoryCatchData}} object.
#' 
#' @export
#' 
GearDependentSpeciesCategoryCatchCompensation <- function(
    SpeciesCategoryCatchData, 
    CompensationMethod = c("Gear", "Cruise", "GearAndCruise"), 
    CompensationTable = data.table::data.table()
) {
    
    SpeciesCategoryCatchData <- GearDependentCatchCompensation(
        InputDataType = "SpeciesCategoryCatchData",
        SpeciesCategoryCatchData = SpeciesCategoryCatchData, 
        CompensationMethod = CompensationMethod, 
        CompensationTable = CompensationTable, 
        keep.all = TRUE
    )
        
        return(SpeciesCategoryCatchData)
    }
    