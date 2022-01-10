##################################################
##################################################
#' Table of total catch weight and count per Haul and SpeciesCategory
#' 
#' This function sums the catch fraction weight and count per Haul and SpeciesCategory.
#' 
#' @inheritParams ModelData
#' @param SpeciesCategoryCatchType The type of the total catch variables, either "Standard" for the raw total catch and "Normalized" for normalizing by tow distance by dividing by the EffectiveTowDistance in nautical miles.
#' 
#' @export
#' 
SpeciesCategoryCatch <- function(
    StoxBioticData, 
    SpeciesCategoryCatchType = c("Normalized", "Standard")
) {
    
    # Get the DensityUnit and DensityType:
    SpeciesCategoryCatchType <- match.arg(SpeciesCategoryCatchType)
    
    # Merge Station, ..., Sample table:
    SpeciesCategoryCatchData <- RstoxData::MergeStoxBiotic(StoxBioticData, TargetTable = "Sample")
    
    # Sum the CatchFractionWeight for each Haul (verticalResolution) and SpeciesCategory (categoryVariable):
    dataVariables <- getDataTypeDefinition(dataType = "SpeciesCategoryCatchData", elements = "data", unlist = TRUE)
    CatchFractionVariables <- sub("TotalCatch", "CatchFraction", dataVariables)
    sumBy <-  getDataTypeDefinition(
        dataType = "SpeciesCategoryCatchData", 
        elements = c("verticalResolution", "categoryVariable"), 
        unlist = TRUE
    )
    # Sum for weight and count separately:
    for(ind in seq_along(dataVariables)) {
        SpeciesCategoryCatchData[, eval(dataVariables[ind]) := sum(get(CatchFractionVariables[ind])), by = sumBy]
    }
    # Uniquify, as the above sum is by reference, and thus keeps all rows:
    SpeciesCategoryCatchData <- unique(SpeciesCategoryCatchData, by = sumBy)
    
    # Normalize if requested:
    if(SpeciesCategoryCatchType == "Normalized") {
        for(ind in seq_along(dataVariables)) {
            SpeciesCategoryCatchData[, eval(dataVariables[ind]) := get(CatchFractionVariables[ind]) / EffectiveTowDistance, by = sumBy]
        }
    }
    
    # Add SpeciesCategoryCatchType and SpeciesCategoryCatchWeight:
    SpeciesCategoryCatchData[, SpeciesCategoryCatchType := ..SpeciesCategoryCatchType]
    SpeciesCategoryCatchData[, SpeciesCategoryCatchWeight := 1]
    
    # Format the output:
    formatOutput(SpeciesCategoryCatchData, dataType = "SpeciesCategoryCatchData", keep.all = FALSE)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(SpeciesCategoryCatchData)
    
    
    return (SpeciesCategoryCatchData)
}


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
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(SumSpeciesCategoryCatchData)
    
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
    LayerDefinition <- match.arg(LayerDefinition)
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
    PSUDefinitionMethod <- match.arg(PSUDefinitionMethod)
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
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(MeanSpeciesCategoryCatchData)
    
    return(MeanSpeciesCategoryCatchData)
}




##################################################
##################################################
#' Apply the sweep of different gear (and cruise)
#' 
#' This function multiplies the CatchFractionWeight and CatchFractionCount of a SpeciesCategoryCatchData by the sweep width given by \code{CompensationTable}. The result is a sweep width compensated length distribution (SpeciesCategoryCatchType starting with "SweepWidthCompensated").
#' 
#' @inheritParams ModelData
#' @param CompensationMethod The method to use for the length dependent catch compensation, i.e. specifying which columns to provide the sweep width for.
#' @param CompensationTable A table of the sweep width per combination of the variables specified in \code{CompensationMethod}. Note that all combinations present in the data must be given in the table, as the output should be sweep width compensated for all rows with non-missing WeightedCount.
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
        CompensationTable = CompensationTable
    )
        
        return(SpeciesCategoryCatchData)
    }
    