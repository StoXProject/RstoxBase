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
        functionArgumentHierarchy = list(
            Resolution = list(
                DefinitionMethod = "Resolution"
            ), 
            LayerTable = list(
                DefinitionMethod = "LayerTable"
            )
        )
    ), 
    
    # Define swept-area Layer:
    DefineSweptAreaLayer = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SweptAreaLayer", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list(
            Resolution = list(
                DefinitionMethod = "Resolution"
            ), 
            LayerTable = list(
                DefinitionMethod = "LayerTable"
            )
        )
    ), 
    
    # Define acoustic PSUs:
    DefineBioticAssignment = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticAssignment", 
        functionParameterFormat = list(
            EllipsoidalDistanceTable = "ellipsoidalDistanceTable"
        ), 
        functionArgumentHierarchy = list(
            StratumPolygon = list(
                DefinitionMethod = "Stratum"
            ), 
            StoxBioticData = list(
                DefinitionMethod = "Stratum"
            ), 
            StoxAcousticData = list(
                DefinitionMethod = "Radius"
            ), 
            Radius = list(
                DefinitionMethod = "Radius"
            ), 
            #MinNumStations = list(
            #    DefinitionMethod = "EllipsoidalDistance"
            #), 
            #RefGCDistance = list(
            #    DefinitionMethod = "EllipsoidalDistance"
            #), 
            #RefTime = list(
            #    DefinitionMethod = "EllipsoidalDistance"
            #), 
            #RefBotDepth = list(
            #    DefinitionMethod = "EllipsoidalDistance"
            #), 
            #RefLatitude = list(
            #    DefinitionMethod = "EllipsoidalDistance"
            #), 
            #RefLongitude = list(
            #    DefinitionMethod = "EllipsoidalDistance"
            #), 
            EllipsoidalDistanceTable = list(
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
        functionArgumentHierarchy = list(
            SweptAreaPSU = list(
                IncludePSU = TRUE
            ), 
            SweptAreaLayer = list(
                IncludeLayer = TRUE
            )
        )
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
        functionArgumentHierarchy = list(
            SweptAreaPSU = list(
                PSUDefinition = "FunctionInput"
            )
        )
    ), 
    
    # Calculate areas of strata polygons:
    SumLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
        functionParameterFormat = list(), 
        functionArgumentHierarchy = list(
            SweptAreaLayer = list(
                LayerDefinition = "FunctionInput"
            )
        )
    ), 
    
    # Calculate areas of strata polygons:
    LengthDependentCatchCompensation = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
        functionParameterFormat = list(
            LengthDependentSweepWidthParameters = "catchCompensationTable", 
            LengthDependentSelectivityParameters = "selectivityTable"
        ), 
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
        functionOutputDataType = "NASCData", 
        functionArgumentHierarchy = list(
            AcousticPSU = list(
                IncludePSU = TRUE
            ), 
            AcousticLayer = list(
                IncludeLayer = TRUE
            )
        )
    ), 
    
    # Calculate areas of strata polygons:
    SumNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData", 
        functionArgumentHierarchy = list(
            AcousticLayer = list(
                LayerDefinition = "FunctionInput"
            )
        )
    ), 
    
    # Calculate areas of strata polygons:
    MeanNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData", 
        functionArgumentHierarchy = list(
            AcousticPSU = list(
                PSUDefinition = "FunctionInput"
            )
        )
    ), 
    
    SweptAreaDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "DensityData", 
        functionArgumentHierarchy = list(
            SweepWidth = list(
                SweepWidthMethod = "Constant"
            ), 
            SweepWidthTable = list(
                SweepWidthMethod = "CruiseDependent"
            )
        )
    ), 
    
    AcousticDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "DensityData", 
        functionParameterFormat = list(
            SpeciesLinkTable = "speciesLinkTable"
        ),
        functionArgumentHierarchy = list()
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
        functionOutputDataType = "IndividualsData", 
        functionArgumentHierarchy = list(
            BioticAssignment = list(
                AbundanceType = "Acoustic"
            ), 
            SweptAreaPSU = list(
                AbundanceType = "SweptArea"
            ), 
            SweptAreaLayer = list(
                AbundanceType = "SweptArea"
            )
        )
    ), 
    
    SuperIndividuals = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SuperIndividualsData"
    ), 
    
    DefineAcousticTargetStrength = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticTargetStrength", 
        functionParameterFormat = list(
            ParameterTable = "acousticTargetStrengthTable"
        ), 
        functionArgumentHierarchy = list(
            ParameterTable = list(
                DefinitionMethod = "Table"
            ), 
            FileName = list(
                DefinitionMethod = "ResourceFile"
            )
        )
    )
    
)
