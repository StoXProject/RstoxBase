# A list of the attributes of the exported StoX functions:
# The format describes the actual content, such as catchabilityTable, filePath, filter, etc. These are used by StoX to choose action on these parameters.
# The primitive type (one of integer, double, logical, character) will be interpreted in the process property functions from the type of the function input or parameter.
#' 
#' @export
#' 
stoxFunctionAttributes <- list(
    
    # Read strata polygons:
    DefineStratumPolygon = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "StratumPolygon", 
        functionParameterFormat = list(
            FileName = "filePath"
        ), 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            FileName = list(
                DefinitionMethod = "ResourceFile", 
                UseProcessData = FALSE
            )
        )
    ), 
    
    # Calculate areas of strata polygons:
    StratumArea = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "StratumAreaData"
    ), 
    
    # Define acoustic PSUs:
    DefineAcousticPSU = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticPSU",
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            )
        )
    ), 
    
    # Define swetp-area PSUs:
    DefineSweptAreaPSU = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SweptAreaPSU", 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            )
        )
    ), 
    
    # Define acoustic Layer:
    DefineAcousticLayer = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticLayer", 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            Resolution = list(
                DefinitionMethod = "Resolution", 
                UseProcessData = FALSE
            ), 
            LayerTable = list(
                DefinitionMethod = "LayerTable", 
                UseProcessData = FALSE
            )
        )
    ), 
    
    # Define swept-area Layer:
    DefineSweptAreaLayer = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SweptAreaLayer", 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            Resolution = list(
                DefinitionMethod = "Resolution", 
                UseProcessData = FALSE
            ), 
            LayerTable = list(
                DefinitionMethod = "LayerTable", 
                UseProcessData = FALSE
            )
        )
    ), 
    
    # Define acoustic PSUs:
    DefineBioticAssignment = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticAssignment", 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            AcousticPSU = list(
                UseProcessData = FALSE
            ), 
            AcousticLayer = list(
                UseProcessData = FALSE
            ), 
            StratumPolygon = list(
                DefinitionMethod = "Stratum", 
                UseProcessData = FALSE
            ), 
            StoxBioticData = list(
                DefinitionMethod = "Stratum", 
                UseProcessData = FALSE
            ), 
            StoxAcousticData = list(
                DefinitionMethod = "Radius", 
                UseProcessData = FALSE
            ), 
            Radius = list(
                DefinitionMethod = "Radius", 
                UseProcessData = FALSE
            ), 
            MinNumberOfHauls = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            ), 
            DistanceNauticalMiles = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            ), 
            TimeDifferenceHours = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            ), 
            BottomDepthDifferenceMeters = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            ), 
            LongitudeDifferenceDegrees = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            ), 
            LatitudeDifferenceDegrees = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            )
        )
    ), 
    
    # Calculate areas of strata polygons:
    LengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
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
        functionOutputDataType = "LengthDistributionData"
    ), 
    
    # Calculate areas of strata polygons:
    MeanLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
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
        functionOutputDataType = "SuperIndividualsData", 
        functionArgumentHierarchy = list(
            LengthDistributionData = list(
                AbundWeightMethod = "HaulDensity"
            )
        )
    ), 
    
    DefineAcousticTargetStrength = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticTargetStrength", 
        functionParameterFormat = list(
            FileName = "filePath",
            LengthDependentTable = "lengthDependentTable",
            LengthAndDepthDependentTable = "lengthAndDepthDependentTable",
            TargetStrengthByLengthTable = "targetStrengthByLengthTable",
            LengthExponentTable = "lengthExponentTable"
        ), 
        functionArgumentHierarchy = list(
            TargetStrengthMethod = list(
                UseProcessData = FALSE
            ), 
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            LengthDependentTable = list(
                TargetStrengthMethod = "LengthDependent", 
                DefinitionMethod = "Table", 
                UseProcessData = FALSE
            ), 
            LengthAndDepthDependentTable = list(
                TargetStrengthMethod = "LengthAndDepthDependent", 
                DefinitionMethod = "Table", 
                UseProcessData = FALSE
            ), 
            TargetStrengthByLengthTable = list(
                TargetStrengthMethod = "TargetStrengthByLength", 
                DefinitionMethod = "Table", 
                UseProcessData = FALSE
            ), 
            LengthExponentTable = list(
                TargetStrengthMethod = "LengthExponent", 
                DefinitionMethod = "Table", 
                UseProcessData = FALSE
            ), 
            FileName = list(
                DefinitionMethod = "ResourceFile", 
                UseProcessData = FALSE
            )
        )
    ), 
    
    BioticAssignmentWeighting = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticAssignment", 
        functionParameterFormat = list(
            LengthExponentTable = "lengthExponentTable"
        ), 
        functionArgumentHierarchy = list(
            StoxBioticData = list(
                WeightingMethod = c("NumberOfLengthSamples", "NormalizedTotalWeight", "NormalizedTotalCount")
            ), 
            LengthDistributionData = list(
                WeightingMethod = c("SumWeightedCount", "InverseSumWeightedCount", "NASC")
            ), 
            MaxNumberOfLengthSamples = list(
                WeightingMethod = c("NumberOfLengthSamples")
            ), 
            StoxAcousticData = list(
                WeightingMethod = c("NASC")
            ), 
            Radius = list(
                WeightingMethod = c("NASC")
            ), 
            LengthExponentTable = list(
                WeightingMethod = c("NASC")
            )
        )
    ), 
    
    SpeciesCategoryDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SpeciesCategoryDensityData"
    ), 
    
    SpeciesCategoryCatch = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SpeciesCategoryCatchData"
    )
    
)

# Define the process property formats:
#' 
#' @export
#' 
processPropertyFormats <- list(
    filePath = list(
        title = "The path to a single file", 
        type = "single"
        
    ), 
    catchCompensationTable = list(
        title = "Define parameters for length dependent catch compensation", 
        type = "table", 
        info = data.table::data.table(
            name = c(
                "SpeciesCategory", 
                "Alpha", 
                "Beta", 
                "LMin", 
                "LMax"
            ), 
            type = c(
                "character", 
                "double", 
                "double", 
                "double", 
                "double"
            )
        )
    ), 
    selectivityTable = list(
        title = "Define parameters for length dependent selectivity", 
        type = "table", 
        info = data.table::data.table(
            name = c(
                "SpeciesCategory", 
                "Alpha", 
                "Beta", 
                "LMax"
            ), 
            type = c(
                "character", 
                "double", 
                "double", 
                "double"
            )
        )
    ), 
    speciesLinkTable = list(
        title = "Link acoustic categories and species categories", 
        type = "table", 
        info = data.table::data.table(
            name = c(
                "AcousticCategory",
                "SpeciesCategory"
            ), 
            type = c(
                "integer", # This is how it is defined in the XSD, see http://www.imr.no/formats/nmdechosounder/v1/nmdechosounderv1.xsd
                "character"
            )
        )
    ), 
    lengthDependentTable = list(
        title = "Define parameters of (logarithmic) acoustic target strength as a function of length (TargetStrength = Targetstrength0 + LengthExponent * log10(Length))", 
        type = "table", 
        info = data.table::data.table(
            name = c(
                "AcousticCategory", 
                "Frequency", 
                "TargetStrength0", 
                "LengthExponent"
            ), 
            type = c(
                "integer",
                "double",
                "double",
                "double"
            )
        )
    ), 
    lengthAndDepthDependentTable = list(
        title = "Define parameters of (logarithmic) acoustic target strength as a function of length (TargetStrength = Targetstrength0 + LengthExponent * log10(Length) + DepthExponent * log10(1 + DepthMeter/10))", 
        type = "table", 
        info = data.table::data.table(
            name = c(
                "AcousticCategory", 
                "Frequency", 
                "TargetStrength0", 
                "LengthExponent", 
                "DepthExponent"
            ), 
            type = c(
                "integer",
                "double",
                "double",
                "double",
                "double"
            )
        )
    ), 
    lengthExponentTable = list(
        title = "Define LengthExponent", 
        type = "table", 
        info = data.table::data.table(
            name = c(
                "AcousticCategory", 
                "Frequency", 
                "LengthExponent"
            ), 
            type = c(
                "integer",
                "double",
                "double"
            )
        )
    ), 
    targetStrengthByLengthTable = list(
        title = "Define a table of IndividualTotalLengthCentimeter and TargetStrength for each AcousticCategory and Frequency", 
        type = "table", 
        info = data.table::data.table(
            name = c(
                "AcousticCategory", 
                "Frequency", 
                "TotalLengthCentimeter", 
                "TargetStrength"
            ), 
            type = c(
                "integer",
                "double",
                "double",
                "double"
            )
        )
    ), 
    lengthExponentTable = list(
        title = "Define the LengthExponent used when distributing NASC in BioticAssignmentWeighting() when WeightingMethod = \"NASC\"",
        type = "table", 
        info = data.table::data.table(
            name = c(
                "SpeciesCategory", 
                "LengthExponent"
            ), 
            type = c(
                "character",
                "double"
            )
        )
    )
)
