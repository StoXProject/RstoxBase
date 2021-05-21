
getIndividualNames <- function(SuperIndividualsData, ImputeByEqual, tables = c("Individual", "SpeciesCategory"), removeKeys = TRUE) {
    individualNames <- unlist(attr(SuperIndividualsData, "stoxDataVariableNames")[tables])
    if(removeKeys) {
        individualNames <- individualNames[!endsWith(individualNames, "Key")]
    }
    # Remove the unique Individual ID:
    individualNames <- setdiff(individualNames, "Individual")
    # Remove the variables selected as ImputeByEqual:
    individualNames <- setdiff(individualNames, ImputeByEqual)
    
    return(individualNames)
}

#' A list of the attributes of the exported StoX functions:
#' The format describes the actual content, such as catchabilityTable, filePath, filter, etc. These are used by StoX to choose action on these parameters.
#' The primitive type (one of integer, double, logical, character) will be interpreted in the process property functions from the type of the function input or parameter.
#' 
#' @export
#' 
stoxFunctionAttributes <- list(
    
    ##### Survey: #####
    DefineSurvey = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "Survey",
        functionParameterFormat = list(
            SurveyTable = "surveyTable"
        ), 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            SurveyTable = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "SurveyTable"
            )
        )
    ), 
    ##########
    
    
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
                UseProcessData = FALSE, 
                DefinitionMethod = "ResourceFile"
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
            ), 
            # Require UseProcessData = FALSE OR UseProcessData = TRUE and SavePSUByTime = TRUE for StoxAcousticData to show:
            #StoxAcousticData = list(
            #    UseProcessData = FALSE
            #), 
            #StoxAcousticData = list(
            #    UseProcessData = TRUE, 
            #    SavePSUByTime = TRUE
            #), 
            StratumPolygon = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "EDSUToPSU"
            ), 
            #IntervalVariable = list(
            #    UseProcessData = FALSE, 
            #    DefinitionMethod = "Interval"
            #), 
            #Interval = list(
            #    UseProcessData = FALSE, 
            #    DefinitionMethod = "Interval"
            #), 
            AcousticPSU = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "PreDefined"
            )#, 
            #SavePSUByTime = list(
            #    UseProcessData = FALSE
            #)
        )
    ), 
    #ExtractAcousticPSUByTime = list(
    #    functionType = "modelData", 
    #    functionCategory = "baseline", 
    #    functionOutputDataType = "AcousticPSUByTime"
    #), 
    #BioticPSUByTime = list(
    #    functionType = "modelData", 
    #    functionCategory = "baseline", 
    #    functionOutputDataType = "AcousticPSUByTime"
    #), 
    #DefineAcousticPSUFromPSUByTime = list(
    #    functionType = "modelData", 
    #    functionCategory = "baseline", 
    #    functionOutputDataType = "AcousticPSU"
    #), 
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
        functionParameterFormat = list(
            LayerTable = "layerTable"
        ), 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            Resolution = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "Resolution"
            ), 
            # These two are joined with AND, and must both be fulfilled:
            LayerTable = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "LayerTable"
            )
        )
    ), 
    DefineBioticLayer = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticLayer", 
        functionParameterFormat = list(
            LayerTable = "layerTable"
        ), 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            # These two are joined with AND, and must both be fulfilled:
            Resolution = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "Resolution"
            ), 
            # These two are joined with AND, and must both be fulfilled:
            LayerTable = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "LayerTable"
            )
        )
    ), 
    ##########
    
    
    ##### Biotic assignment: #####
    DefineBioticAssignment = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticAssignment", 
        functionParameterFormat = list(
            LayerTable = "layerTable"
        ), 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            StoxBioticData = list(
                UseProcessData = FALSE
            ), 
            #AcousticPSU = list(
            #    UseProcessData = FALSE
            #), 
            LayerDefinition = list(
                #UseProcessData = FALSE
            ), 
            AcousticLayer = list(
                #UseProcessData = FALSE, 
                LayerDefinition = "FunctionInput"
            ), 
            LayerDefinitionMethod = list(
                #UseProcessData = FALSE, 
                LayerDefinition = "FunctionParameter"
            ), 
            Resolution = list(
                #UseProcessData = FALSE, 
                LayerDefinitionMethod = "Resolution"
            ), 
            LayerTable = list(
                #UseProcessData = FALSE, 
                LayerDefinitionMethod = "LayerTable"
            ), 
            #AcousticLayer = list(
            #    UseProcessData = FALSE
            #), 
            # These two are joined with AND, and must both be fulfilled:
            StratumPolygon = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "Stratum"
            ), 
            # These two are joined with AND, and must both be fulfilled:
            StoxAcousticData = list(
                #UseProcessData = FALSE, 
                DefinitionMethod = c("Radius", "EllipsoidalDistance")
            ), 
            StoxAcousticData = list(
                #UseProcessData = FALSE, 
                LayerDefinition = "FunctionParameter"
            ), 
            # These two are joined with AND, and must both be fulfilled:
            Radius = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "Radius"
            ), 
            # These two are joined with AND, and must both be fulfilled:
            MinNumberOfHauls = list(
                UseProcessData = FALSE, 
                DefinitionMethod = c("Radius", "EllipsoidalDistance")
            ), 
            # These two are joined with AND, and must both be fulfilled:
            Distance = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "EllipsoidalDistance"
            ), 
            # These two are joined with AND, and must both be fulfilled:
            TimeDifference = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "EllipsoidalDistance"
            ), 
            # These two are joined with AND, and must both be fulfilled:
            BottomDepthDifference = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "EllipsoidalDistance"
            ), 
            # These two are joined with AND, and must both be fulfilled:
            LongitudeDifference = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "EllipsoidalDistance"
            ), 
            # These two are joined with AND, and must both be fulfilled:
            LatitudeDifference = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "EllipsoidalDistance"
            )
        )
    ), 
    BioticAssignmentWeighting = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticAssignment", 
        functionParameterFormat = list(
            LengthExponent = "lengthExponentTable"
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
            LengthExponent = list(
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
    GearDependentCatchCompensation = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
        functionParameterFormat = list(
            CompensationTable = "gearCompensationTable"
        )
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
        functionParameterFormat = list(
            LayerTable = "layerTable"
        ), 
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
    ), 
    MeanLengthDistribution = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "MeanLengthDistributionData", 
        functionParameterFormat = list(
            LayerTable = "layerTable", 
            SurveyTable = "surveyTable"
        ), 
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
            ), 
            # Survey:
            Survey = list(
                SurveyDefinition = "FunctionInput"
            ), 
            SurveyDefinitionMethod = list(
                SurveyDefinition = "FunctionParameter"
            ), 
            SurveyTable = list(
                SurveyDefinition = "FunctionParameter", 
                SurveyDefinitionMethod = "SurveyTable"
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
        functionParameterFormat = list(
            LayerTable = "layerTable"
        ), 
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
        functionParameterFormat = list(
            LayerTable = "layerTable", 
            SurveyTable = "surveyTable"
        ), 
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
                LayerDefinition = "FunctionParameter", 
                LayerDefinitionMethod = "Resolution"
            ), 
            LayerTable = list(
                LayerDefinition = "FunctionParameter", 
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
            ), 
            # Survey:
            Survey = list(
                SurveyDefinition = "FunctionInput"
            ), 
            SurveyDefinitionMethod = list(
                SurveyDefinition = "FunctionParameter"
            ), 
            SurveyTable = list(
                SurveyDefinition = "FunctionParameter", 
                SurveyDefinitionMethod = "SurveyTable"
            )
        )
    ),
    
    SplitMeanNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData", 
        functionParameterFormat = list(
            SpeciesLink = "speciesLinkTable",
            AcousticCategoryLink = "acousticCategoryLinkTable"
        )
    ),
    
    NASCToStoxAcoustic = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "StoxAcousticData"
    ),
    
    
    AppendNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData"
    ),
    ##########
    
    
    ##### Density: #####
    SweptAreaDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "DensityData", 
        functionArgumentHierarchy = list(
            SweepWidth = list(
                SweepWidthMethod = "Constant"
            )
        )
    ), 
    AcousticDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "DensityData", 
        functionParameterFormat = list(
            SpeciesLink = "speciesLinkTable"
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
        functionOutputDataType = "SuperIndividualsData", 
        functionParameterFormat = list(
            #ImputeAtMissing = "getIndividualVaiableNamesSingle", 
            ImputeAtMissing = "getIndividualVaiableNamesVector", 
            ImputeByEqual = "getIndividualAndSpeciesCategoryVaiableNamesVector", 
            ToImpute = "getIndividualVaiableNamesVector"
        )
    ),
    ##########
    
    
    ##### Other: #####
    DefineAcousticTargetStrength = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "AcousticTargetStrength", 
        functionParameterFormat = list(
            FileName = "filePath",
            TargetStrengthTable = "targetStrengthTable"
        ), 
        functionArgumentHierarchy = list(
            TargetStrengthMethod = list(
                UseProcessData = FALSE
            ), 
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            TargetStrengthTable = list(
                DefinitionMethod = "TargetStrengthTable", 
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
    
    
    ReportSuperIndividuals = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportSuperIndividualsData", 
        # This is an example of using an expression to determine when to show a parameter:
        functionParameterFormat = list(
            #TargetVariable = "targetVariable_ReportSuperIndividuals", 
            GroupingVariables = "groupingVariables_ReportSuperIndividuals"
        ), 
        functionArgumentHierarchy = list(
            WeightingVariable = list(
                ReportFunction = expression(RstoxBase::getWeightingFunctions())
            )
        )
    ), 
    
    ReportDensity = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportDensityData", 
        # This is an example of using an expression to determine when to show a parameter:
        functionParameterFormat = list(
            #TargetVariable = "targetVariable_ReportSuperIndividuals", 
            GroupingVariables = "groupingVariables_ReportDensity"
        ), 
        functionArgumentHierarchy = list(
            WeightingVariable = list(
                ReportFunction = expression(RstoxBase::getWeightingFunctions())
            )
        )
    ), 
    
    
    ReportSpeciesCategoryCatch = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportSpeciesCategoryCatchData"
    )#,
    
    
    #WriteStratumPolygon = list(
    #    functionType = "modelData", 
    #    functionCategory = "report", 
    #    functionOutputDataType = "WriteStratumPolygonData"
    #)
    ##########
)

#' Define the process property formats:
#' 
#' @export
#' 
processPropertyFormats <- list(
    filePath = list(
        class = "single", 
        title = "The path to a single file"
    ), 
    sweepWidthByCruiseTable = list(
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
        ), 
        possibleValues = function(LengthDistributionData) {
            if(!length(LengthDistributionData)) {
                return(vector("list", 5))
            }
            
            # Get all unique combinations:
            listOfUniqueCombinations <- as.list(unique(LengthDistributionData$SpeciesCategory))
            
            # Output must be an unnamed list:
            c(
                unname(listOfUniqueCombinations), 
                rep(list(list()), 4)
            )
        }
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
        ), 
        possibleValues = function(LengthDistributionData) {
            if(!length(LengthDistributionData)) {
                return(vector("list", 4))
            }
            
            # Get all unique combinations:
            listOfUniqueCombinations <- as.list(unique(LengthDistributionData$SpeciesCategory))
            
            # Output must be an unnamed list:
            c(
                unname(listOfUniqueCombinations), 
                rep(list(list()), 3)
            )
        }
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
        )#, 
        #possibleValues = function(AcousticTargetStrength, AssignmentLengthDistributionData) {
        #    # Must be an unnamed list:
        #    list(
        #        unique(AcousticTargetStrength$TargetStrengthTable$AcousticCategory), 
        #        unique(AssignmentLengthDistributionData$SpeciesCategory)
        #    )
        #}
    ), 
    
    acousticCategoryLinkTable = list(
        class = "table", 
        title = "Define acoustic categories to split mix categories into", 
        columnNames = c(
            "AcousticCategory",
            "SplitAcousticCategory"
        ), 
        variableTypes = c(
            "character", 
            "character"
        )
    ), 
    
    gearCompensationTable =  list(
        class = "table", 
        title = "Sweep width for all gear", 
        columnNames = function(CompensationMethod = c("Gear", "Cruise", "GearAndCruise")) {
            CompensationMethod <- match.arg(CompensationMethod)
            columnNames <- c(
                strsplit(CompensationMethod, "And")[[1]], 
                "SweepWidth"
            )
            
            return(columnNames)
        }, 
        variableTypes = function(CompensationMethod = c("Gear", "Cruise", "GearAndCruise")) {
            CompensationMethod <- match.arg(CompensationMethod)
            columnNames <- strsplit(CompensationMethod, "And")[[1]]
            variableTypes <- c(
                rep("character", length(columnNames)), 
                "double"
            )
            
            return(variableTypes)
        }, 
        possibleValues = function(LengthDistributionData, CompensationMethod = c("Gear", "Cruise", "GearAndCruise")) {
            CompensationMethod <- match.arg(CompensationMethod)
            CompensationMethod <- strsplit(CompensationMethod, "And")[[1]]
            
            if(!length(LengthDistributionData)) {
                return(vector("list", length(CompensationMethod) + 1))
            }
            
            # Get all unique combinations:
            listOfUniqueCombinations <- as.list(unique(LengthDistributionData[, ..CompensationMethod]))
            
            # Output must be an unnamed list:
            c(
                unname(listOfUniqueCombinations), 
                list(NULL)
            )
        }
    ), 
    
    targetStrengthTable = list(
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
    ), 
    
    targetVariable_ReportSuperIndividuals = list(
        class = "single", 
        possibleValues = function(SuperIndividualsData) {
            sort(names(SuperIndividualsData))
        }
    ), 
    
    groupingVariables_ReportSuperIndividuals = list(
        class = "vector", 
        title = "One or more variables to group super-individuals by when reporting SuperIndividualsData", 
        possibleValues = function(SuperIndividualsData) {
            sort(names(SuperIndividualsData))
        }, 
        variableTypes <- "character"
    ), 
    
    groupingVariables_ReportDensity = list(
        class = "vector", 
        title = "One or more variables to group by when reporting DensityData", 
        possibleValues = function(DensityData) {
            sort(names(DensityData$Data))
        }, 
        variableTypes <- "character"
    ), 
    
    surveyTable = list(
        class = "table", 
        title = "Link strata to different surveys", 
        columnNames = c(
            "Stratum",
            "Survey"
        ), 
        variableTypes = c(
            "character", 
            "character"
        ), 
        possibleValues = function(StratumPolygon) {
            if(!length(StratumPolygon)) {
                return(vector("list", 2))
            }
            # Must be an unnamed list:
            list(
                getStratumNames(StratumPolygon), # Stratum
                # This results in the JSON string "[]" as is expected by the GUI:
                list() # Survey
            )
        }
    ), 
    
    layerTable = list(
        class = "table", 
        title = "Define Layers by depth intervals", 
        columnNames = c(
            "Layer",
            "MinLayerDepth", 
            "MaxLayerDepth"
        ), 
        variableTypes = c(
            "character", 
            "double", 
            "double"
        )
    ), 
    
    getIndividualVaiableNamesSingle = list(
        class = "single", 
        title = "Select variable", 
        possibleValues = getIndividualNames
    ), 
    
    getIndividualAndSpeciesCategoryVaiableNamesVector = list(
        class = "vector", 
        title = "Select variables", 
        possibleValues = function(SuperIndividualsData, ImputeByEqual) {
            getIndividualNames(SuperIndividualsData, ImputeByEqual, tables = c("Individual", "SpeciesCategory"), removeKeys = TRUE) 
        }
    ), 
    
    getIndividualVaiableNamesVector = list(
        class = "vector", 
        title = "Select variables", 
        possibleValues = function(SuperIndividualsData, ImputeByEqual) {
            getIndividualNames(SuperIndividualsData, ImputeByEqual, tables = "Individual", removeKeys = TRUE) 
        }
    )
    
)
