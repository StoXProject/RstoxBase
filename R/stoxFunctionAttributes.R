# A list of the attributes of the exported StoX functions:
# The format describes the actual content, such as catchabilityTable, filePath, filter, etc. These are used by StoX to choose action on these parameters.
# The primitive type (one of integer, double, logical, character) will be interpreted in the process property functions from the type of the function input or parameter.
#' 
#' @export
#' 
stoxFunctionAttributes <- list(
    
    ##### Stratum polygons: #####
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
    StratumArea = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "StratumAreaData"
    ), 
    ##########
    
    
    ##### PSUs: #####
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
    DefineBioticPSU = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticPSU", 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            )
        )
    ), 
    ##########
    
    
    ##### Layers: #####
    DefineAcousticLayer = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticLayer", 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            Resolution = list(
                DefinitionMethod = "Resolution", 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            LayerTable = list(
                DefinitionMethod = "LayerTable", 
                UseProcessData = FALSE
            )
        )
    ), 
    DefineBioticLayer = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticLayer", 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            Resolution = list(
                DefinitionMethod = "Resolution", 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            LayerTable = list(
                DefinitionMethod = "LayerTable", 
                UseProcessData = FALSE
            )
        )
    ), 
    ##########
    
    
    ##### Biotic assignment: #####
    DefineBioticAssignment = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticAssignment", 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            StoxBioticData = list(
                UseProcessData = FALSE
            ), 
            AcousticPSU = list(
                UseProcessData = FALSE
            ), 
            LayerDefinition = list(
                UseProcessData = FALSE
            ), 
            AcousticLayer = list(
                LayerDefinition = "FunctionInput", 
                UseProcessData = FALSE
            ), 
            LayerDefinitionMethod = list(
                LayerDefinition = "FunctionParameter", 
                UseProcessData = FALSE
            ), 
            Resolution = list(
                LayerDefinitionMethod = "Resolution", 
                UseProcessData = FALSE
            ), 
            LayerTable = list(
                LayerDefinitionMethod = "LayerTable", 
                UseProcessData = FALSE
            ), 
            #AcousticLayer = list(
            #    UseProcessData = FALSE
            #), 
            # These two are joined with AND, and must both be fulfilled:
            StratumPolygon = list(
                DefinitionMethod = "Stratum", 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            StoxAcousticData = list(
                DefinitionMethod = c("Radius", "EllipsoidalDistance"), 
                UseProcessData = FALSE
            ), 
            StoxAcousticData = list(
                LayerDefinition = "FunctionParameter", 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            Radius = list(
                DefinitionMethod = "Radius", 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            MinNumberOfHauls = list(
                DefinitionMethod = c("Radius", "EllipsoidalDistance"), 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            Distance = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            TimeDifference = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            BottomDepthDifference = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            LongitudeDifference = list(
                DefinitionMethod = "EllipsoidalDistance", 
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            LatitudeDifference = list(
                DefinitionMethod = "EllipsoidalDistance", 
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
                # The WeightingMethod can be any of these:
                WeightingMethod = c("NumberOfLengthSamples", "NormalizedTotalWeight", "NormalizedTotalCount")
            ), 
            LengthDistributionData = list(
                # The WeightingMethod can be any of these:
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
    AssignmentLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AssignmentLengthDistributionData"
    ), 
    ##########
    
    
    ##### Length distribution #####
    LengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData"
    ), 
    RegroupLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData"
    ), 
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
    RelativeLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData"
    ), 
    SumLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SumLengthDistributionData", 
        functionArgumentHierarchy = list(
            functionArgumentHierarchy = list(
                # Layer: 
                LayerDefinitionMethod = list(
                    LayerDefinition = "FunctionParameter"
                ), 
                Resolution = list(
                    LayerDefinitionMethod = "Resolution"
                ), 
                LayerTable = list(
                    LayerDefinitionMethod = "LayerTable"
                ), 
                LayerProcessData = list(
                    LayerDefinition = "FunctionInput"
                )
            )
        )
    ), 
    MeanLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "MeanLengthDistributionData", 
        functionArgumentHierarchy = list(
            # Layer: 
            LengthDistributionData = list(
                # The LayerDefinition can be any of these:
                LayerDefinition = c(
                    "FunctionInput", 
                    "FunctionParameter"
                )
            ), 
            SumLengthDistributionData = list(
                LayerDefinition = "PreDefined"
            ), 
            BioticLayer = list(
                LayerDefinition = "FunctionInput"
            ),
            LayerDefinitionMethod = list(
                LayerDefinition = "FunctionParameter"
            ), 
            Resolution = list(
                LayerDefinitionMethod = "Resolution"
            ), 
            LayerTable = list(
                LayerDefinitionMethod = "LayerTable"
            ), 
            # PSU: 
            BioticPSU = list(
                PSUDefinition = "FunctionInput"
            ), 
            PSUDefinitionMethod = list(
                PSUDefinition = "FunctionParameter"
            ), 
            StratumPolygon = list(
                PSUDefinitionMethod = "StationToPSU"
            )
        )
    ), 
    ##########
    
    
    ##### NASC: #####
    NASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData"
    ), 
    SumNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SumNASCData", 
        functionArgumentHierarchy = list(
            AcousticLayer = list(
                LayerDefinition = "FunctionInput"
            ), 
            LayerDefinitionMethod = list(
                LayerDefinition = "FunctionParameter"
            ), 
            Resolution = list(
                LayerDefinitionMethod = "Resolution"
            ), 
            LayerTable = list(
                LayerDefinitionMethod = "LayerTable"
            )
        )
    ),
    MeanNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "MeanNASCData", 
        functionArgumentHierarchy = list(
            # Layer: 
            NASCData = list(
                # The LayerDefinition can be any of these:
                LayerDefinition = c(
                    "FunctionInput", 
                    "FunctionParameter"
                )
            ), 
            SumNASCData = list(
                LayerDefinition = "PreDefined"
            ), 
            AcousticLayer = list(
                LayerDefinition = "FunctionInput"
            ),
            LayerDefinitionMethod = list(
                LayerDefinition = "FunctionParameter"
            ), 
            Resolution = list(
                LayerDefinitionMethod = "Resolution"
            ), 
            LayerTable = list(
                LayerDefinitionMethod = "LayerTable"
            ), 
            # PSU: 
            AcousticPSU = list(
                PSUDefinition = "FunctionInput"
            ), 
            PSUDefinitionMethod = list(
                PSUDefinition = "FunctionParameter"
            ), 
            StratumPolygon = list(
                PSUDefinitionMethod = "EDSUToPSU"
            )
        )
    ),
    ##########
    
    
    ##### Density: #####
    SweptAreaDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "DensityData", 
        functionParameterFormat = list(
            SweepWidthTable = "sweepWidthTable"
        ), 
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
        functionOutputDataType = "MeanDensityData"
    ), 
    ##########
    
    
    ##### Abundance: #####
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
            MeanLengthDistributionData = list(
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
                DistributionMethod = "HaulDensity"
            )
        )
    ), 
    ImputeSuperIndividuals = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "ImputeSuperIndividualsData"
    ),
    ##########
    
    
    ##### Other: #####
    DefineAcousticTargetStrength = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticTargetStrength", 
        functionParameterFormat = list(
            FileName = "filePath",
            TargetStrengthDefinitionTable = "targetStrengthDefinitionTable", 
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
            TargetStrengthDefinitionTable = list(
                DefinitionMethod = "Table", 
                UseProcessData = FALSE
            ), 
            FileName = list(
                DefinitionMethod = "ResourceFile", 
                UseProcessData = FALSE
            )
        )
    ), 
    
    ### 
    ### SpeciesCategoryDensity = list(
    ###     functionType = "modelData", 
    ###     functionCategory = "baseline", 
    ###     functionOutputDataType = "SpeciesCategoryDensityData"
    ### ), 
    
    SpeciesCategoryCatch = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SpeciesCategoryCatchData"
    ), 
    
    
    ReportImputeSuperIndividuals = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportImputeSuperIndividualsData", 
        # This is an example of using an expression to determine when to show a parameter:
        functionArgumentHierarchy = list(
            WeightingVariable = list(
                ReportFunction = expression(RstoxBase::getWeightingFunctions())
            )
        )
    ), 
    
    
    ReportBootstrap = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportBootstrapData", 
        # This is an example of using an expression to determine when to show a parameter:
        functionArgumentHierarchy = list(
            AggregationWeightingVariable = list(
                ReportFunction = expression(RstoxBase::getWeightingFunctions())
            ), 
            BootstrapReportWeightingVariable = list(
                ReportFunction = expression(RstoxBase::getWeightingFunctions())
            )
        )
    ), 
    
    
    ReportSpeciesCategoryCatch = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportSpeciesCategoryCatchData"
    )
    ##########
)

# Define the process property formats:
#' 
#' @export
#' 
processPropertyFormats <- list(
    filePath = list(
        class = "single", 
        title = "The path to a single file"
    ), 
    sweepWidthTable = list(
        class = "table", 
        title = "Define sweep width in meters for each cruise", 
        columnNames = c(
            "Cruise", 
            "SweepWidth"
        ), 
        variableTypes = c(
            "character", 
            "double"
        )
    ),
    catchCompensationTable = list(
        class = "table", 
        title = "Define parameters for length dependent catch compensation", 
        columnNames = c(
            "SpeciesCategory", 
            "Alpha", 
            "Beta", 
            "LMin", 
            "LMax"
        ), 
        variableTypes = c(
            "character", 
            "double", 
            "double", 
            "double", 
            "double"
        )
    ), 
    selectivityTable = list(
        class = "table", 
        title = "Define parameters for length dependent selectivity", 
        columnNames = c(
            "SpeciesCategory", 
            "Alpha", 
            "Beta", 
            "LMax"
        ), 
        variableTypes = c(
            "character", 
            "double", 
            "double", 
            "double"
        )
    ), 
    speciesLinkTable = list(
        class = "table", 
        title = "Link acoustic categories and species categories", 
        columnNames = c(
            "AcousticCategory",
            "SpeciesCategory"
        ), 
        variableTypes = c(
            #"integer", # This is how it is defined in the XSD, see http://www.imr.no/formats/nmdechosounder/v1/nmdechosounderv1.xsd
            # Changed on 2020-06-30 from integer to character. There is no need to bring the integer definition of LUF20 on to StoxAcoustic!:
            "character", 
            "character"
        )
    ), 
    targetStrengthDefinitionTable = list(
        class = "table", 
        title = function(TargetStrengthMethod = c("LengthDependent", "LengthAndDepthDependent", "TargetStrengthByLength", "LengthExponent")) {
            TargetStrengthMethod <- match.arg(TargetStrengthMethod)
            
            if(identical(TargetStrengthMethod, "LengthDependent")) {
                title <- "Define parameters of (logarithmic) acoustic target strength as a function of length (TargetStrength = Targetstrength0 + LengthExponent * log10(Length))"
            }
            else if(identical(TargetStrengthMethod, "LengthAndDepthDependent")) {
                title <- "Define parameters of (logarithmic) acoustic target strength as a function of length (TargetStrength = Targetstrength0 + LengthExponent * log10(Length) + DepthExponent * log10(1 + Depth/10))"
            }
            else if(identical(TargetStrengthMethod, "TargetStrengthByLength")) {
                title <- "Define a table of IndividualTotalLength and TargetStrength for each AcousticCategory and Frequency"
            }
            else if(identical(TargetStrengthMethod, "LengthExponent")) {
                title <- "Define LengthExponent"
            }
            else {
                stop("Wrong TargetStrengthMethod")
            }
            
            return(title)
        }, 
        columnNames = function(TargetStrengthMethod = c("LengthDependent", "LengthAndDepthDependent", "TargetStrengthByLength", "LengthExponent")) {
            TargetStrengthMethod <- match.arg(TargetStrengthMethod)
            
            if(identical(TargetStrengthMethod, "LengthDependent")) {
                columnNames <- c(
                    "AcousticCategory", 
                    "Frequency", 
                    "TargetStrength0", 
                    "LengthExponent"
                )
            }
            else if(identical(TargetStrengthMethod, "LengthAndDepthDependent")) {
                columnNames <- c(
                    "AcousticCategory", 
                    "Frequency", 
                    "TargetStrength0", 
                    "LengthExponent", 
                    "DepthExponent"
                )
            }
            else if(identical(TargetStrengthMethod, "TargetStrengthByLength")) {
                columnNames <- c(
                    "AcousticCategory", 
                    "Frequency", 
                    "TotalLength", 
                    "TargetStrength"
                )
            }
            else if(identical(TargetStrengthMethod, "LengthExponent")) {
                columnNames <- c(
                    "AcousticCategory", 
                    "Frequency", 
                    "LengthExponent"
                )
            }
            else {
                stop("Wrong TargetStrengthMethod")
            }
            
            return(columnNames)
        }, 
        variableTypes = function(TargetStrengthMethod = c("LengthDependent", "LengthAndDepthDependent", "TargetStrengthByLength", "LengthExponent")) {
            TargetStrengthMethod <- match.arg(TargetStrengthMethod)
            
            if(identical(TargetStrengthMethod, "LengthDependent")) {
                variableTypes <- c(
                    "character",
                    "double",
                    "double",
                    "double"
                )
            }
            else if(identical(TargetStrengthMethod, "LengthAndDepthDependent")) {
                variableTypes <- c(
                    "character",
                    "double",
                    "double",
                    "double",
                    "double"
                )
            }
            else if(identical(TargetStrengthMethod, "TargetStrengthByLength")) {
                variableTypes <- c(
                    "character",
                    "double",
                    "double",
                    "double"
                )
            }
            else if(identical(TargetStrengthMethod, "LengthExponent")) {
                variableTypes <- c(
                    "character",
                    "double",
                    "double"
                )
            }
            else {
                stop("Wrong TargetStrengthMethod")
            }
            
            return(variableTypes)
        }
    )
    
)
