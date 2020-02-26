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
        functionCategory = "baseline", 
        functionOutputDataType = "StratumPolygon", 
        #functionParameterType = list(FileName = "character"), 
        functionParameterFormat = list(FileName = "filePath"), 
        functionArgumentHierarchy = list()
    ), 
    
    # Calculate areas of strata polygons:
    StratumArea = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "StratumArea", 
        #functionParameterType = list(StratumPolygon = "character"), 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list()
    ), 
    
    # Define acoustic PSUs:
    DefineAcousticPSU = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticPSU", 
        #functionParameterType = list(
        #    StratumPolygon = "character", 
        #    StoxAcousticData = "character"), 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list()
    ), 
    
    # Define acoustic PSUs:
    DefineSweptAreaPSU = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SweptAreaPSU", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list()
    ), 
    
    # Define acoustic Layer:
    DefineAcousticLayer = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticLayer", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list()
    ), 
    
    # Define swept-area Layer:
    DefineSweptAreaLayer = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SweptAreaLayer", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list()
    ), 
    
    # Define acoustic PSUs:
    DefineBioticAssignment = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticAssignment", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list(
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
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list()
    ), 
    
    # Calculate areas of strata polygons:
    RegroupLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list()
    ), 
    
    # Calculate areas of strata polygons:
    MeanLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list()
    ), 
    
    # Calculate areas of strata polygons:
    SumLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list()
    ), 
    
    # Calculate areas of strata polygons:
    LengthDependentCatchCompensation = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list(
            LengthDependentSweepWidthParameters = list(
                CompensationMethod = "LengthDependentSweepWidth"
            ), 
            LengthDependentSelectivityParameters = list(
                CompensationMethod = "LengthDependentSelectivity"
            )
        )
    ), 
    
    # Convvert to percent length distribution:
    RelativeLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData"
    ), 
    
    # Convvert to percent length distribution:
    AssignmentLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AssignmentLengthDistributionData"
    ), 
    
    # Calculate areas of strata polygons:
    NASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData"
    ), 
    
    # Calculate areas of strata polygons:
    SumNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData"
    ), 
    
    # Calculate areas of strata polygons:
    MeanNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData"
    ), 
    
    SweptAreaDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "DensityData", 
        functionArgumentHierarchy = list(
            SweepWidth = list(
                SweepWidthMethod = "Constant"
            ), 
            SweepWidthExpr = list(
                SweepWidthMethod = "CruiseDependent"
            )
        )
    ), 
    
    MeanDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "DensityData"
    ), 
    
    Abundance = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AbundanceData"
    ), 
    
    Individuals = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "IndividualsData"
    ), 
    
    SuperIndividuals = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SuperIndividualsData"
    ), 
    
    DefineAcousticTargetStrength = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticTargetStrength"
    )
    
)
