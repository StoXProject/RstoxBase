# The following functions adds resolution variables to different StoX datatypes:




##################################################
##################################################
#' Add Survey, Stratum and PSU to PreySpeciesCategoryCatchData
#' 
#' This function adds biotic PSU definitions (linked to Stratum) and Survey information to PreySpeciesCategoryCatchData.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams MeanLengthDistribution
#' @inheritParams DefineBioticPSU
#' 
#' @export
#' 
AddPSUToPreySpeciesCategoryCatch <- function(
        PreySpeciesCategoryCatchData,
        # PSU: 
        PSUDefinition = c("FunctionParameter", "FunctionInput"), 
        PSUDefinitionMethod = c("StationToPSU", "None"), 
        BioticPSU = NULL, 
        # Survey:
        SurveyDefinition = c("FunctionParameter", "FunctionInput"), 
        SurveyDefinitionMethod = c("AllStrata", "Table"), 
        SurveyTable = data.table::data.table(), 
        Survey = NULL, 
        # General:
        StratumPolygon = NULL
) {
    
    # Convert the PSUDefinitionMethod to "Identity" if "EDSUToPSU":
    PSUDefinitionMethod <- RstoxData::match_arg_informative(PSUDefinitionMethod)
    if(grepl("StationToPSU", PSUDefinitionMethod, ignore.case = TRUE)) {
        PSUDefinitionMethod <- "Identity"
    }
    
    # Add the PSU and Survey resolution:
    PreySpeciesCategoryCatchData <- addPSUResolution(
        PreySpeciesCategoryCatchData, dataType = "PreySpeciesCategoryCatchData", 
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
    
    formatOutput(PreySpeciesCategoryCatchData, dataType = "PreySpeciesCategoryCatchData", keep.all = TRUE, removeStoXKeys = TRUE, primaryColumnOrder = c("Survey", "Stratum", "PSU"))
    
    return(PreySpeciesCategoryCatchData)
}

