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
        functionParameterType = list(FileNames = "character"), 
        functionParameterFormat = list(FileNames = "filePaths"), 
        functionArgumentHierarchy = list(), 
        functionAlias = list(), 
        functionParameterAlias = list(), 
        functionParameterValueAlias = list()
    ), 
    
    # Read input biotic data:
    ReadAcoustic = list(
        functionType = "modelData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "AcousticData", 
        functionParameterType = list(FileNames = "character"), 
        functionParameterFormat = list(FileNames = "filePaths"), 
        functionArgumentHierarchy = list(), 
        functionAlias = list(), 
        functionParameterAlias = list(), 
        functionParameterValueAlias = list()
    ), 
    
    # Read strata polygons:
    DefineStrata = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "StratumPolygon", 
        functionParameterType = list(FileName = "character"), 
        functionParameterFormat = list(FileName = "filePath"), 
        functionParameterHierarchy = list(), 
        functionAlias = list(), 
        functionParameterAlias = list(), 
        functionParameterValueAlias = list()
    ), 
    
    # Calculate areas of strata polygons:
    StratumArea = list(
        functionType = "modelData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "StratumArea", 
        functionParameterType = list(StratumPolygon = "character"), 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list(), 
        functionAlias = list(), 
        functionParameterAlias = list(), 
        functionParameterValueAlias = list()
    ), 
    
    # Define acoustic PSUs:
    DefineAcousticPSU = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "AcousticPSU", 
        functionParameterType = list(
            StratumPolygon = "character", 
            StoxAcousticData = "character"), 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list(), 
        functionAlias = list(), 
        functionParameterAlias = list(), 
        functionParameterValueAlias = list()
    ), 
    
    # Define acoustic PSUs:
    DefineAcousticLayer = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "AcousticLayer", 
        functionParameterType = list(
            StoxAcousticData = "character"), 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list(), 
        functionAlias = list(), 
        functionParameterAlias = list(), 
        functionParameterValueAlias = list()
    )
)
