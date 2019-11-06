# A list of the attributes of the exported StoX functions:
#' 
#' @export
#' 
stoxFunctionAttributes <- list(
    
    # The format describes the actual content, such as catchabilityTable, filePath, filter, etc. These are used by StoX to choose action on these parameters.
    # The primitive type (one of integer, double, logical, character) will be interpreted in the process property functions from the type of the function input or parameter.
    
    # Read input biotic data:
    ReadBiotic = list(
        functionType = "modelData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "BioticData", 
        #functionInputs = NULL, 
        #functionParameters = c("FileNames"),  
        functionParameterFormats = list(FileNames = "filePaths"), 
        functionArgumentHierarchy = list(), 
        functionAlias = list(), 
        functionParameterAlias = list(), 
        functionParameterValueAilas = list()
    ), 
    
    # Read strata polygons:
    DefineStrata = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "StratumPolygon", 
        #functionInputs = NULL, 
        #functionParameters = c("FileName", "UseProcessData"),  
        functionParameterFormats = list(FileName = "filePath"), 
        functionParameterHierarchy = list(), 
        functionAlias = list(), 
        functionParameterAlias = list(), 
        functionParameterValueAilas = list()
    ), 
    
    # Calculate areas of strata polygons:
    StratumArea = list(
        functionType = "modelData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "StratumArea", 
        #functionInputs = c("StratumPolygon"), 
        #functionParameters = c("AreaMethod"),  
        functionParameterFormats = list(), 
        functionParameterHierarchy = list(), 
        functionAlias = list(), 
        functionParameterAlias = list(), 
        functionParameterValueAilas = list()
    )
)
