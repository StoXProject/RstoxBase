# A list of the attributes of the exported StoX functions:
# The format describes the actual content, such as catchabilityTable, filePath, filter, etc. These are used by StoX to choose action on these parameters.
# The primitive type (one of integer, double, logical, character) will be interpreted in the process property functions from the type of the function input or parameter.
#' 
#' @export
#' 
stoxFunctionAttributes <- list(
    
    # Read strata polygons:
    DefineStrata = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "StratumPolygon", 
        #functionParameterType = list(FileName = "character"), 
        functionParameterFormat = list(FileName = "filePath"), 
        functionParameterHierarchy = list()
    ), 
    
    # Calculate areas of strata polygons:
    StratumArea = list(
        functionType = "modelData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "StratumArea", 
        #functionParameterType = list(StratumPolygon = "character"), 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list()
    ), 
    
    # Define acoustic PSUs:
    DefineAcousticPSU = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "AcousticPSU", 
        #functionParameterType = list(
        #    StratumPolygon = "character", 
        #    StoxAcousticData = "character"), 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list()
    ), 
    
    # Define acoustic PSUs:
    DefineSweptAreaPSU = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "SweptAreaPSU", 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list()
    ), 
    
    # Define acoustic Layer:
    DefineAcousticLayer = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "AcousticLayer", 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list()
    ), 
    
    # Define swept-area Layer:
    DefineSweptAreaLayer = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "SweptAreaLayer", 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list()
    ), 
    
    # Define acoustic PSUs:
    DefineBioticAssignment = list(
        functionType = "processData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "BioticAssignment", 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list(
            StratumPolygon = list(
                DefinitionMethod = "Stratum"
            ), 
            AcousticData = list(
                DefinitionMethod = "Radius"
            ), 
            Radius = list(
                DefinitionMethod = "Radius"
            ), 
            MinNumStations  = list(
                DefinitionMethod = "EllipsoidalDistance"
            ), 
            RefGCDistance  = list(
                DefinitionMethod = "EllipsoidalDistance"
            ), 
            RefTime  = list(
                DefinitionMethod = "EllipsoidalDistance"
            ), 
            RefBotDepth  = list(
                DefinitionMethod = "EllipsoidalDistance"
            ), 
            RefLatitude  = list(
                DefinitionMethod = "EllipsoidalDistance"
            ), 
            RefLongitude  = list(
                DefinitionMethod = "EllipsoidalDistance"
            )
        )
    ), 
    
    # Calculate areas of strata polygons:
    LengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "LengthDistribution", 
        functionParameterFormat = list(), 
        functionParameterHierarchy = list()
    ), 
    
    # Calculate areas of strata polygons:
    NASC = list(
        functionType = "modelData", 
        functionCategory = "Baseline", 
        functionOutputDataType = "NASCData"
    )
)
