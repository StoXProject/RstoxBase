isCategorical <- function(x) {
    is.character(x) || is.integer(x)
}

defaultPlotGeneralOptions <- list(
    # Options for the labels and other text:
    AxisTitleSize = 12, 
    AxisTickSize = 10, 
    LegendTitleSize = 12, 
    LegendTextSize = 10
)

defaultPlotFileOptions <- list(
    # Options for the output file:
    Format = "png", 
    # Using the ICES Journal og Marine Science recommendations (https://academic.oup.com/icesjms/pages/General_Instructions):
    Width = 17, 
    Height = 10, 
    DotsPerInch = 500
)


defaultMapPlotOptions <- list(
    # Options for the zoom and limits:
    Zoom = 1, 
    LongitudeCenter = 0.5, 
    LatitudeCenter = 0.5, 
    LandColor = "#FDFECC", # rgb(253, 254, 204, maxColorValue = 255), as specified in the StoX GUI
    BorderColor = "grey50", 
    OceanColor = "white", 
    GridColor = "#DFF2FF"# rgb(223, 242, 255, maxColorValue = 255), as specified in the StoX GUI
)

defaultMapPlotNASCOptions <- list(
    # Options for the colors:
    #PointColor = function(x) {
    #    if(isCategorical(x$SumNASCData$Data[[x$ColorVariable]])) {
    #        PointColor <- character()
    #    }
    #    else {
    #        PointColor <- "combined.color"
    #    }
    #    return(PointColor)
    #},
    # Use black vessel track:
    TrackColor = "black", 
    # Options for the point sizes and shapes:
    MaxPointSize = 10, 
    MinPointSize = 0.5, 
    TrackSize = 0.5
)

defaultAcousticPSUPlotOptions <- list(
    # Options for AcousticPSU:
    AcousticPSULabelSize = 4, 
    AcousticPSULabelColor = "black", 
    AcousticPSULabelPosition = "mean", 
    AcousticPSULabelHjust = 0.5, 
    AcousticPSULabelVjust = 0.5
)

#defaultStratumPolygonPlotOptions <- list(
#    StratumPolygonColor = character(), 
#    StratumPolygonLabelSize = numeric(), 
#    StratumPolygonLabelColor = character(), 
#)





defaultColorVariableOptions <- list(
    ColorVariable = "NASC"
)

getIndividualNames <- function(SuperIndividualsData, remove = NULL, tables = c("Individual", "SpeciesCategory"), removeKeys = TRUE) {
    individualNames <- unlist(attr(SuperIndividualsData, "stoxDataVariableNames")[tables])
    if(removeKeys) {
        individualNames <- individualNames[!endsWith(individualNames, "Key")]
    }
    # Remove the unique Individual ID:
    individualNames <- setdiff(individualNames, "Individual")
    # Remove the variables selected as remove:
    individualNames <- setdiff(individualNames, remove)
    
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
            SurveyTable = "surveyTable", 
            FileName = "filePath"
        ), 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            SurveyTable = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "Table"
            ), 
            FileName = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "ResourceFile"
            )#, 
            # # StratumPolygon is shown in the GUI either if DefinitionMethod is "AllStrata" or "Table":
            # StratumPolygon = list(
            #     UseProcessData = FALSE, 
            #     DefinitionMethod = "AllStrata"
            # ), 
            # StratumPolygon = list(
            #     UseProcessData = FALSE, 
            #     DefinitionMethod = "Table"
            # )
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
            ), 
            StratumNameLabel = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "ResourceFile"
            ), 
            SimplifyStratumPolygon = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "ResourceFile"
            ), 
            SimplificationFactor = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "ResourceFile", 
                SimplifyStratumPolygon = TRUE
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
        functionParameterFormat = list(
            FileName = "filePath"
        ), 
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
            ), 
            FileName = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "ResourceFile"
            )
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
        functionParameterFormat = list(
            FileName = "filePath"
        ), 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            StratumPolygon = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "StationToPSU"
            ), 
            FileName = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "ResourceFile"
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
                DefinitionMethod = "Table"
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
                DefinitionMethod = "Table"
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
            LayerTable = "layerTable", 
            FileName = "filePath"
        ), 
        functionArgumentHierarchy = list(
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            #StoxBioticData = list(
            #    UseProcessData = FALSE
            #), 
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
                LayerDefinitionMethod = "Table"
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
            ), 
            
            # These two are joined with AND, and must both be fulfilled:
            FileName = list(
                UseProcessData = FALSE, 
                DefinitionMethod = "ResourceFile"
            )
        )
    ), 
    BioticAssignmentWeighting = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "BioticAssignment", 
        functionParameterFormat = list(
            LengthExponent = "lengthExponentTable", 
            SpeciesLink = "speciesLinkTable_BioticAssignmentWeighting"
        ), 
        functionArgumentHierarchy = list(
            StoxBioticData = list(
                # The WeightingMethod can be any of these:
                WeightingMethod = c("NumberOfLengthSamples", "NormalizedTotalWeight", "NormalizedTotalNumber")
            ), 
            LengthDistributionData = list(
                # The WeightingMethod can be any of these:
                WeightingMethod = c("SumWeightedNumber", "InverseSumWeightedNumber", "AcousticDensity")
            ), 
            MaxNumberOfLengthSamples = list(
                WeightingMethod = c("NumberOfLengthSamples")
            ), 
            
            # Layer: 
            LayerDefinition = list(
                WeightingMethod = c("AcousticDensity")
            ), 
            NASCData = list(
                WeightingMethod = c("AcousticDensity"), 
                # The LayerDefinition can be any of these:
                LayerDefinition = c(
                    "FunctionInput", 
                    "FunctionParameter"
                )
            ), 
            # Layer:
            LayerDefinitionMethod = list(
                WeightingMethod = c("AcousticDensity"), 
                LayerDefinition = "FunctionParameter"
            ), 
            Resolution = list(
                WeightingMethod = c("AcousticDensity"), 
                LayerDefinition = "FunctionParameter", 
                LayerDefinitionMethod = "Resolution"
            ), 
            LayerTable = list(
                WeightingMethod = c("AcousticDensity"), 
                LayerDefinition = "FunctionParameter", 
                LayerDefinitionMethod = "Table"
            ), 
            AcousticLayer = list(
                WeightingMethod = c("AcousticDensity"), 
                LayerDefinition = "FunctionInput"
            ),
            # AcousticDensity:
            AcousticPSU = list(
                WeightingMethod = c("AcousticDensity")
            ), 
            AcousticTargetStrength = list(
                WeightingMethod = c("AcousticDensity")
            ), 
            SpeciesLink = list(
                WeightingMethod = c("AcousticDensity")
            ), 
            Radius = list(
                WeightingMethod = c("AcousticDensity")
            ), 
            MinNumberOfEDSUs = list(
                WeightingMethod = c("AcousticDensity")
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
    GearDependentSpeciesCategoryCatchCompensation = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SpeciesCategoryCatchData", 
        functionParameterFormat = list(
            CompensationTable = "gearCompensationTable_SpeciesCategoryCatchData"
        )
    ), 
    GearDependentLengthDistributionCompensation = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "LengthDistributionData", 
        functionParameterFormat = list(
            CompensationTable = "gearCompensationTable_LengthDistributionData"
        )
    ), 
    LengthDependentLengthDistributionCompensation = list(
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
                LayerDefinitionMethod = "Table"
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
                LayerDefinitionMethod = "Table"
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
            StratumPolygon = list(
                SurveyDefinitionMethod = "Table"
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
                SurveyDefinitionMethod = "Table"
            )
        )
    ), 
    ##########
    
    
    
    #########
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
    
    SumSpeciesCategoryCatch = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "SumSpeciesCategoryCatchData", 
        functionParameterFormat = list(
            LayerTable = "layerTable"
        ), 
        functionArgumentHierarchy = list(
            # Layer: 
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
                LayerDefinitionMethod = "Table"
            ), 
            LayerProcessData = list(
                LayerDefinition = "FunctionInput"
            )
        )
    ), 
    
    MeanSpeciesCategoryCatch = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "MeanSpeciesCategoryCatchData", 
        functionParameterFormat = list(
            LayerTable = "layerTable", 
            SurveyTable = "surveyTable"
        ), 
        functionArgumentHierarchy = list(
            # Layer: 
            SpeciesCategoryCatchData = list(
                # The LayerDefinition can be any of these:
                LayerDefinition = c(
                    "FunctionInput", 
                    "FunctionParameter"
                )
            ), 
            SumSpeciesCategoryCatchData = list(
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
                LayerDefinitionMethod = "Table"
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
            StratumPolygon = list(
                SurveyDefinitionMethod = "Table"
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
                SurveyDefinitionMethod = "Table"
            )
        )
    ), 
    #########
    
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
                LayerDefinitionMethod = "Table"
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
                LayerDefinitionMethod = "Table"
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
            StratumPolygon = list(
                SurveyDefinitionMethod = "Table"
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
                SurveyDefinitionMethod = "Table"
            )
        )
    ),
    
    SplitMeanNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData", 
        functionParameterFormat = list(
            SpeciesLink = "speciesLinkTable_AcousticDensity",
            AcousticCategoryLink = "acousticCategoryLinkTable"
        )
    ),
    
    SplitNASC = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "NASCData", 
        functionParameterFormat = list(
            SpeciesLink = "speciesLinkTable_Split",
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
    
    #PlotAcousticTrawlSurvey = list(
    #    functionType = "modelData", 
    #    functionCategory = "report", 
    #    functionOutputDataType = "PlotAcousticTrawlSurveyData", 
    #    functionParameterFormat = list(
    #        LayerTable = "layerTable"#, 
    #        #PointColor = "pointColor"
    #    ),
    #    functionArgumentHierarchy = list(
    #        # Options for the colors:
    #        #ColorVariable = list(
    #        #    UseDefaultColorSettings = FALSE
    #        #), 
    #        # PointColor cannot have a default, as it depends on whether the ColorVariable is categorical of continuous:
    #        #PointColor = list(
    #        #    UseDefaultColorSettings = FALSE
    #        #), 
    #        TrackColor = list(
    #            UseDefaultColorSettings = FALSE
    #        ), 
    #        LandColor = list(
    #            UseDefaultColorSettings = FALSE
    #        ), 
    #        BorderColor = list(
    #            UseDefaultColorSettings = FALSE
    #        ), 
    #        OceanColor = list(
    #            UseDefaultColorSettings = FALSE
    #        ), 
    #        GridColor = list(
    #            UseDefaultColorSettings = FALSE
    #        ), 
    #        # Options for the point sizes and shapes:
    #        MaxPointSize = list(
    #            UseDefaultSizeSettings = FALSE
    #        ), 
    #        MinPointSize = list(
    #            UseDefaultSizeSettings = FALSE
    #        ), 
    #        TrackSize = list(
    #            UseDefaultSizeSettings = FALSE
    #        ), 
    #        # Options for zoom and limits:
    #        Zoom = list(
    #            UseDefaultAspectSettings = FALSE
    #        ), 
    #        LongitudeMin = list(
    #            UseDefaultAspectSettings = FALSE
    #        ), 
    #        LongitudeMax = list(
    #            UseDefaultAspectSettings = FALSE
    #        ), 
    #        LatitudeMin = list(
    #            UseDefaultAspectSettings = FALSE
    #        ), 
    #        LatitudeMax = list(
    #            UseDefaultAspectSettings = FALSE
    #        ), 
    #        LongitudeCenter = list(
    #            UseDefaultAspectSettings = FALSE
    #        ), 
    #        LatitudeCenter = list(
    #            UseDefaultAspectSettings = FALSE
    #        ), 
    #        # Options for the labels and other text:
    #        Title = list(
    #            UseDefaultTextSettings = FALSE
    #        ), 
    #        AxisTitleSize = list(
    #            UseDefaultTextSettings = FALSE
    #        ), 
    #        AxisTickSize = list(
    #            UseDefaultTextSettings = FALSE
    #        ), 
    #        LegendTitleSize = list(
    #            UseDefaultTextSettings = FALSE
    #        ), 
    #        LegendTextSize = list(
    #            UseDefaultTextSettings = FALSE
    #        ), 
    #        # Options for the output file:
    #        Format = list(
    #            UseDefaultFileSettings = FALSE
    #        ), 
    #        Width = list(
    #            UseDefaultFileSettings = FALSE
    #        ), 
    #        Height = list(
    #            UseDefaultFileSettings = FALSE
    #        ), 
    #        DotsPerInch = list(
    #            UseDefaultFileSettings = FALSE
    #        ), 
    #        # Layer: 
    #        NASCData = list(
    #            # The LayerDefinition can be any of these:
    #            LayerDefinition = c(
    #                "FunctionInput", 
    #                "FunctionParameter"
    #            )
    #        ), 
    #        SumNASCData = list(
    #            LayerDefinition = "PreDefined"
    #        ), 
    #        AcousticLayer = list(
    #            LayerDefinition = "FunctionInput"
    #        ),
    #        LayerDefinitionMethod = list(
    #            LayerDefinition = "FunctionParameter"
    #        ), 
    #        Resolution = list(
    #            LayerDefinition = "FunctionParameter", 
    #            LayerDefinitionMethod = "Resolution"
    #        ), 
    #        LayerTable = list(
    #            LayerDefinition = "FunctionParameter", 
    #            LayerDefinitionMethod = "Table"
    #        ), 
    #        # AcousticPSU:
    #        AcousticPSU = list(
    #            UseAcousticPSU = TRUE
    #        ), 
    #        UseDefaultAcousticPSUSettings <- list(
    #            UseAcousticPSU = TRUE
    #        ), 
    #        AcousticPSULabelSize = list(
    #            UseAcousticPSU = TRUE, 
    #            UseDefaultAcousticPSUSettings = FALSE
    #        ), 
    #        AcousticPSULabelColor = list(
    #            UseAcousticPSU = TRUE, 
    #            UseDefaultAcousticPSUSettings = FALSE
    #        ), 
    #        AcousticPSULabelPosition = list(
    #            UseAcousticPSU = TRUE, 
    #            UseDefaultAcousticPSUSettings = FALSE
    #        ), 
    #        AcousticPSULabelHjust = list(
    #            UseAcousticPSU = TRUE, 
    #            UseDefaultAcousticPSUSettings = FALSE
    #        ), 
    #        AcousticPSULabelVjust = list(
    #            UseAcousticPSU = TRUE, 
    #            UseDefaultAcousticPSUSettings = FALSE
    #        )
    #    ), 
    #    functionParameterDefaults = c(
    #        # Default general options:
    #        defaultPlotGeneralOptions, 
    #        # Default file options:
    #        defaultPlotFileOptions, 
    #        # Default map plotting options:
    #        defaultMapPlotNASCOptions, 
    #        # Default NASC-plotting options:
    #        defaultMapPlotOptions, 
    #        # Defaults for the AcousticPSU (potting PSU names) text size and shift (from the mean EDSU position):
    #        defaultAcousticPSUPlotOptions, 
    #        # Defaults color variable:
    #        defaultColorVariableOptions
    #    )
    #),
    
    ##########
    
    
    ##### Density: #####
    SweptAreaDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "DensityData", 
        functionParameterFormat = list(
            DensityType = "densityType_SweptAreaDensity"
        ),
        functionArgumentHierarchy = list(
            MeanLengthDistributionData = list(
                SweptAreaDensityMethod = "LengthDistributed"
            ),
            MeanSpeciesCategoryCatchData = list(
                SweptAreaDensityMethod = "TotalCatch"
            ),
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
            SpeciesLink = "speciesLinkTable_AcousticDensity"
        )
    ), 
    MeanDensity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "MeanDensityData"
    ), 
    ##########
    
    
    ##### Quantity: #####
    DefineRegression = list(
        functionType = "processData", 
        functionCategory = "baseline", 
        functionOutputDataType = "Regression", 
        functionParameterFormat = list(
            FileName = "filePath",
            GroupingVariables = "groupingVariables",
            RegressionTable = "regressionTable"
        ), 
        functionArgumentHierarchy = list(
            RegressionModel = list(
                UseProcessData = FALSE
            ), 
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            GroupingVariables = list(
                DefinitionMethod = "Table", 
                UseProcessData = FALSE
            ), 
            RegressionTable = list(
                DefinitionMethod = "Table", 
                UseProcessData = FALSE
            ), 
            FileName = list(
                DefinitionMethod = "ResourceFile", 
                UseProcessData = FALSE
            )
        )
    ),
    
    
    EstimateBioticRegression = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "Regression", 
        functionParameterFormat = list(
            RegressionTable = "regressionTable", 
            DependentVariable = "dependentVariable_EstimateBioticRegression", 
            DependentResolutionVariable = "dependentResolutionVariable_EstimateBioticRegression", 
            IndependentVariable = "independentVariable_EstimateBioticRegression", 
            IndependentResolutionVariable = "independentResolutionVariable_EstimateBioticRegression", 
            GroupingVariables = "groupingVariables_EstimateBioticRegression"
        ), 
        functionArgumentHierarchy = list(
            IndividualsData = list(
                InputDataType = "IndividualsData"
            ), 
            SuperIndividualsData = list(
                InputDataType = "SuperIndividualsData"
            )
        )
    ),
    
    
    Quantity = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "QuantityData"
    ), 
    Individuals = list(
        functionType = "modelData", 
        functionCategory = "baseline", 
        functionOutputDataType = "IndividualsData", 
        functionArgumentHierarchy = list(
            BioticAssignment = list(
                QuantityType = "Acoustic"
            ), 
            MeanLengthDistributionData = list(
                QuantityType = "SweptArea"
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
            ImputeAtMissing = "getImputeAtMissing", 
            ImputeByEqual = "getImputeByEqual", 
            ToImpute = "getToImpute"
        ),
        functionArgumentHierarchy = list(
            Regression = list(
                ImputationMethod = "Regression"
            ), 
            ImputeAtMissing = list(
                ImputationMethod = "RandomSampling"
            ), 
            ImputeByEqual = list(
                ImputationMethod = "RandomSampling"
            ), 
            ToImpute = list(
                ImputationMethod = "RandomSampling"
            ), 
            Seed = list(
                ImputationMethod = "RandomSampling"
            )
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
            AcousticTargetStrengthTable = "targetStrengthTable"
        ), 
        functionArgumentHierarchy = list(
            AcousticTargetStrengthModel = list(
                UseProcessData = FALSE
            ), 
            DefinitionMethod = list(
                UseProcessData = FALSE
            ), 
            AcousticTargetStrengthTable = list(
                DefinitionMethod = "Table", 
                UseProcessData = FALSE
            ), 
            FileName = list(
                DefinitionMethod = "ResourceFile", 
                UseProcessData = FALSE
            )
        )
    ), 
    
    
    
    ReportSuperIndividuals = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportSuperIndividualsData", 
        # This is an example of using an expression to determine when to show a parameter:
        functionParameterFormat = list(
            TargetVariable = "targetVariable_ReportSuperIndividuals", 
            GroupingVariables = "groupingVariables_ReportSuperIndividuals", 
            InformationVariables = "informationVariables_ReportSuperIndividuals", 
            TargetVariableUnit = "targetVariableUnit_ReportSuperIndividuals", 
            WeightingVariable = "weightingVariable_ReportSuperIndividuals"
        ), 
        functionArgumentHierarchy = list(
            WeightingVariable = list(
                ReportFunction = expression(RstoxBase::getWeightingFunctions())
            )
        )
    ), 
    
    ReportQuantity = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportQuantityData", 
        # This is an example of using an expression to determine when to show a parameter:
        functionParameterFormat = list(
            GroupingVariables = "groupingVariables_ReportQuantity", 
            InformationVariables = "informationVariables_ReportQuantity", 
            TargetVariableUnit = "targetVariableUnit_ReportQuantity", 
            WeightingVariable = "weightingVariable_ReportQuantity"
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
            GroupingVariables = "groupingVariables_ReportDensity", 
            InformationVariables = "informationVariables_ReportDensity", 
            DensityUnit = "densityUnit", 
            WeightingVariable = "weightingVariable_ReportDensity"
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
        functionOutputDataType = "ReportSpeciesCategoryCatchData", 
        functionParameterFormat = list(
            ReportVariableUnit = "reportVariableUnit_ReportSpeciesCategoryCatch"
        )
    )
    
    
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
    
    dependentVariable_EstimateBioticRegression = list(
        class = "vector", 
        title = "Select DependentVariable for regression", 
        variableTypes = "character", 
        possibleValues = function(InputDataType, IndividualsData, SuperIndividualsData) {
            data <- get(InputDataType)
            intersect(names(data), attr(data, "stoxDataVariableNames")$Individual)
        }
    ), 
    
    dependentResolutionVariable_EstimateBioticRegression = list(
        class = "vector", 
        title = "Select DependentResolutionVariable for regression", 
        variableTypes = "character", 
        possibleValues = function(DependentVariable, InputDataType, IndividualsData, SuperIndividualsData) {
            data <- get(InputDataType)
            setdiff(intersect(names(data), attr(data, "stoxDataVariableNames")$Individual), DependentVariable)
        }
    ), 
    
    independentVariable_EstimateBioticRegression = list(
        class = "vector", 
        title = "Select IndependentVariable for regression", 
        variableTypes = "character", 
        possibleValues = function(DependentVariable, DependentResolutionVariable, InputDataType, IndividualsData, SuperIndividualsData) {
            data <- get(InputDataType)
            setdiff(intersect(names(data), attr(data, "stoxDataVariableNames")$Individual), c(DependentVariable, DependentResolutionVariable))
        }
    ), 
    
    independentResolutionVariable_EstimateBioticRegression = list(
        class = "vector", 
        title = "Select IndependentResolutionVariable for regression", 
        variableTypes = "character", 
        possibleValues = function(DependentVariable, DependentResolutionVariable, IndependentVariable, InputDataType, IndividualsData, SuperIndividualsData) {
            data <- get(InputDataType)
            setdiff(intersect(names(data), attr(data, "stoxDataVariableNames")$Individual), c(DependentVariable, DependentResolutionVariable, IndependentVariable))
        }
    ), 
    
    groupingVariables_EstimateBioticRegression = list(
        class = "vector", 
        title = "Select GroupingVariables for regression", 
        variableTypes = "character", 
        possibleValues = function(DependentVariable, DependentResolutionVariable, IndependentVariable, IndependentResolutionVariable, InputDataType, IndividualsData, SuperIndividualsData) {
            data <- get(InputDataType)
            setdiff(
                intersect(
                    names(data), 
                    c(
                        attr(data, "stoxDataVariableNames")$SpeciesCategory, 
                        attr(data, "stoxDataVariableNames")$Individual
                    )
                ), 
                c(DependentVariable, DependentResolutionVariable, IndependentVariable, IndependentResolutionVariable)
            )
        }
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
                list(unname(listOfUniqueCombinations)), 
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
                list(unname(listOfUniqueCombinations)), 
                rep(list(list()), 3)
            )
        }
    ), 
    speciesLinkTable_AcousticDensity = list(
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
        ), 
        possibleValues = function(MeanNASCData, AssignmentLengthDistributionData) {
            # Must be an unnamed list:
            list(
                unique(MeanNASCData$Data$AcousticCategory), 
                unique(AssignmentLengthDistributionData$SpeciesCategory)
            )
        }
    ), 
    speciesLinkTable_Split = list(
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
        ), 
        #possibleValues = function(NASCData, AssignmentLengthDistributionData) {
        possibleValues = function(AssignmentLengthDistributionData, AcousticCategoryLink) {
                # Must be an unnamed list:
            list(
                #unique(NASCData$AcousticCategory), 
                sort(unique(AcousticCategoryLink$SplitAcousticCategory)), 
                unique(AssignmentLengthDistributionData$SpeciesCategory)
            )
        }
    ), 
    speciesLinkTable_BioticAssignmentWeighting = list(
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
        ), 
        possibleValues = function(NASCData, LengthDistributionData) {
            # Must be an unnamed list:
            list(
                unique(NASCData$AcousticCategory), 
                unique(LengthDistributionData$SpeciesCategory)
            )
        }
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
    
    gearCompensationTable_LengthDistributionData =  list(
        class = "table", 
        title = "Sweep width for all gear", 
        columnNames = function(CompensationMethod = c("Gear", "Cruise", "GearAndCruise")) {
            CompensationMethod <- RstoxData::match_arg_informative(CompensationMethod)
            columnNames <- c(
                strsplit(CompensationMethod, "And")[[1]], 
                "SweepWidth"
            )
            
            return(columnNames)
        }, 
        variableTypes = function(CompensationMethod = c("Gear", "Cruise", "GearAndCruise")) {
            CompensationMethod <- RstoxData::match_arg_informative(CompensationMethod)
            columnNames <- strsplit(CompensationMethod, "And")[[1]]
            variableTypes <- c(
                rep("character", length(columnNames)), 
                "double"
            )
            
            return(variableTypes)
        }, 
        possibleValues = function(LengthDistributionData, CompensationMethod = c("Gear", "Cruise", "GearAndCruise")) {
            CompensationMethod <- RstoxData::match_arg_informative(CompensationMethod)
            Variables <- strsplit(CompensationMethod, "And")[[1]]
            
            if(!length(LengthDistributionData)) {
                return(vector("list", length(Variables) + 1))
            }
            
            # Get all unique combinations:
            #listOfUniqueCombinations <- as.list(unique(LengthDistributionData[, ..Variables]))
            listOfUniqueCombinations <- lapply(Variables, function(var) as.list(unique(LengthDistributionData[[var]])))
            
            # Output must be an unnamed list:
            c(
                #list(unname(listOfUniqueCombinations)), 
                listOfUniqueCombinations, 
                list(NULL)
            )
            
        }
    ), 
    
    gearCompensationTable_SpeciesCategoryCatchData =  list(
        class = "table", 
        title = "Sweep width for all gear", 
        columnNames = function(CompensationMethod = c("Gear", "Cruise", "GearAndCruise")) {
            CompensationMethod <- RstoxData::match_arg_informative(CompensationMethod)
            columnNames <- c(
                strsplit(CompensationMethod, "And")[[1]], 
                "SweepWidth"
            )
            
            return(columnNames)
        }, 
        variableTypes = function(CompensationMethod = c("Gear", "Cruise", "GearAndCruise")) {
            CompensationMethod <- RstoxData::match_arg_informative(CompensationMethod)
            columnNames <- strsplit(CompensationMethod, "And")[[1]]
            variableTypes <- c(
                rep("character", length(columnNames)), 
                "double"
            )
            
            return(variableTypes)
        }, 
        possibleValues = function(SpeciesCategoryCatchData, CompensationMethod = c("Gear", "Cruise", "GearAndCruise")) {
            CompensationMethod <- RstoxData::match_arg_informative(CompensationMethod)
            Variables <- strsplit(CompensationMethod, "And")[[1]]
            
            if(!length(SpeciesCategoryCatchData)) {
                return(vector("list", length(Variables) + 1))
            }
            
            # Get all unique combinations:
            #listOfUniqueCombinations <- as.list(unique(LengthDistributionData[, ..Variables]))
            listOfUniqueCombinations <- lapply(Variables, function(var) as.list(unique(SpeciesCategoryCatchData[[var]])))
            
            # Output must be an unnamed list:
            c(
                #list(unname(listOfUniqueCombinations)), 
                listOfUniqueCombinations, 
                list(NULL)
            )
            
        }
    ), 
    
    targetStrengthTable = list(
        class = "table", 
        title = function(AcousticTargetStrengthModel = c("LengthDependent", "LengthAndDepthDependent", "TargetStrengthByLength", "LengthExponent")) {
            AcousticTargetStrengthModel <- RstoxData::match_arg_informative(AcousticTargetStrengthModel)
            
            if(identical(AcousticTargetStrengthModel, "LengthDependent")) {
                title <- "Define parameters of (logarithmic) acoustic target strength as a function of length (TargetStrength = Targetstrength0 + LengthExponent * log10(Length))"
            }
            else if(identical(AcousticTargetStrengthModel, "LengthAndDepthDependent")) {
                title <- "Define parameters of (logarithmic) acoustic target strength as a function of length (TargetStrength = Targetstrength0 + LengthExponent * log10(Length) + DepthExponent * log10(1 + Depth/10))"
            }
            else if(identical(AcousticTargetStrengthModel, "TargetStrengthByLength")) {
                title <- "Define a table of IndividualTotalLength and TargetStrength for each AcousticCategory and Frequency"
            }
            else if(identical(AcousticTargetStrengthModel, "LengthExponent")) {
                title <- "Define LengthExponent"
            }
            else {
                stop("Wrong AcousticTargetStrengthModel")
            }
            
            return(title)
        }, 
        columnNames = function(AcousticTargetStrengthModel = c("LengthDependent", "LengthAndDepthDependent", "TargetStrengthByLength", "LengthExponent")) {
            AcousticTargetStrengthModel <- RstoxData::match_arg_informative(AcousticTargetStrengthModel)
            
            if(identical(AcousticTargetStrengthModel, "LengthDependent")) {
                columnNames <- c(
                    "AcousticCategory", 
                    "Frequency", 
                    "TargetStrength0", 
                    "LengthExponent"
                )
            }
            else if(identical(AcousticTargetStrengthModel, "LengthAndDepthDependent")) {
                columnNames <- c(
                    "AcousticCategory", 
                    "Frequency", 
                    "TargetStrength0", 
                    "LengthExponent", 
                    "DepthExponent"
                )
            }
            else if(identical(AcousticTargetStrengthModel, "TargetStrengthByLength")) {
                columnNames <- c(
                    "AcousticCategory", 
                    "Frequency", 
                    "TotalLength", 
                    "TargetStrength"
                )
            }
            else if(identical(AcousticTargetStrengthModel, "LengthExponent")) {
                columnNames <- c(
                    "AcousticCategory", 
                    "Frequency", 
                    "LengthExponent"
                )
            }
            else {
                stop("Wrong AcousticTargetStrengthModel")
            }
            
            return(columnNames)
        }, 
        variableTypes = function(AcousticTargetStrengthModel = c("LengthDependent", "LengthAndDepthDependent", "TargetStrengthByLength", "LengthExponent")) {
            AcousticTargetStrengthModel <- RstoxData::match_arg_informative(AcousticTargetStrengthModel
                                             )
            
            if(identical(AcousticTargetStrengthModel, "LengthDependent")) {
                variableTypes <- c(
                    "character",
                    "double",
                    "double",
                    "double"
                )
            }
            else if(identical(AcousticTargetStrengthModel, "LengthAndDepthDependent")) {
                variableTypes <- c(
                    "character",
                    "double",
                    "double",
                    "double",
                    "double"
                )
            }
            else if(identical(AcousticTargetStrengthModel, "TargetStrengthByLength")) {
                variableTypes <- c(
                    "character",
                    "double",
                    "double",
                    "double"
                )
            }
            else if(identical(AcousticTargetStrengthModel, "LengthExponent")) {
                variableTypes <- c(
                    "character",
                    "double",
                    "double"
                )
            }
            else {
                stop("Wrong AcousticTargetStrengthModel")
            }
            
            return(variableTypes)
        }
    ), 
    
    
    regressionTable = list(
        class = "table", 
        title = function(RegressionModel = c("SimpleLinear", "Power")) {
            
            RegressionModel <- RstoxData::match_arg_informative(RegressionModel)
            
            if(identical(RegressionModel, "SimpleLinear")) {
                title <- "Define parameters of the linear model ((DependentVariable + DependentResolutionVariable / 2) = Intercept + Slope * (IndependentVariable + IndependentResolutionVariable / 2))"
            }
            else if(identical(RegressionModel, "Power")) {
                title <- "Define parameters of the power model ((DependentVariable + DependentResolutionVariable / 2) = Factor * (IndependentVariable + IndependentResolutionVariable / 2) ^ Exponent)"
            }
            else {
                stop("Wrong RegressionModel")
            }
            
            return(title)
        }, 
        columnNames = function(RegressionModel = c("SimpleLinear", "Power"), GroupingVariables = character(), RegressionTable = data.table::data.table()) {
            
            RegressionModel <- RstoxData::match_arg_informative(RegressionModel)
            
            variableSpecification <- c(
                "DependentVariable", 
                "DependentResolutionVariable", 
                "IndependentVariable", 
                "IndependentResolutionVariable"
            )
            
            metaColumns <- c(
                "ResidualStandardError", 
                "EstimationMethod"
            )
            
            if(identical(RegressionModel, "SimpleLinear")) {
                specificRegressionTableColumns <- c(
                    "Intercept", 
                    "Slope"
                )
            }
            else if(identical(RegressionModel, "Power")) {
                specificRegressionTableColumns <- c(
                    "Factor", 
                    "Exponent"
                )
            }
            else {
                stop("Wrong RegressionModel")
            }
            
            #if(length(GroupingVariables) && nchar(GroupingVariables)) {
            GroupingVariables <- GroupingVariables[nchar(GroupingVariables) > 0]
                columnNames <- c(
                    GroupingVariables, 
                    variableSpecification, 
                    specificRegressionTableColumns, 
                    metaColumns
                )
            #}
            
            # Warninig if there are columns in the existing table that are no longer valid:
            invalidColumns <- c(
                setdiff(names(RegressionTable), columnNames), 
                setdiff(columnNames, names(RegressionTable))
            )
            if(NROW(RegressionTable) & length(invalidColumns)) {
                warning("StoX: RegressionTable no longer valid. Changing GroupingVariables or RegressionModel requires to create a new RegressionTable.")
            }
            
            return(columnNames)
        }, 
        variableTypes = function(RegressionModel = c("SimpleLinear", "Power"), GroupingVariables = character()) {
            RegressionModel <- RstoxData::match_arg_informative(RegressionModel)
            
            if(identical(RegressionModel, "SimpleLinear")) {
                variableTypes <- c(
                    "character",
                    "character",
                    "character",
                    "character",
                    "double",
                    "double",
                    "double",
                    "character"
                )
            }
            else if(identical(RegressionModel, "Power")) {
                variableTypes <- c(
                    "character",
                    "character",
                    "character",
                    "character",
                    "double",
                    "double",
                    "double",
                    "character"
                )
            }
            else {
                stop("Wrong RegressionModel")
            }
            
            if(length(GroupingVariables[nchar(GroupingVariables) > 0])) {
                variableTypes <- c(rep("character", length(GroupingVariables)), variableTypes)
            }
            
            return(variableTypes)
        }, 
        possibleValues = function(RegressionModel, GroupingVariables = character()) {
            
            # Get all unique combinations:
            if(RegressionModel == "SimpleLinear") {
                EstimationMethod <- "SimpleLinear"
            }
            else if(RegressionModel == "Power") {
                EstimationMethod <- c(
                    "LogLogLinear"#, 
                    #"NonLinear"
                )
            }
            
            # Output must be an unnamed list:
            c(
                rep(list(list()), 7 + as.numeric(length(GroupingVariables[nchar(GroupingVariables) > 0]))), 
                list(EstimationMethod)
            )
        }
    ), 
    
    densityType_SweptAreaDensity = list(
        class = "vector", # Should be changed to single when this is implemented in the GUI.
        title = "Select the type of swept area density.", 
        possibleValues = function(SweptAreaDensityMethod) {
            if(SweptAreaDensityMethod == "LengthDistributed") {
                "AreaNumberDensity"
            }
            else if(SweptAreaDensityMethod == "TotalCatch") {
                c("AreaNumberDensity", "AreaWeightDensity")
            }
        }
    ), 
    
    # ReportSuperIndividuals: 
    targetVariable_ReportSuperIndividuals = list(
        class = "vector", 
        title = "One variable to group report from SuperIndividualsData", 
        possibleValues = function(SuperIndividualsData) {
            sort(subset(names(SuperIndividualsData), sapply(SuperIndividualsData, class) == "numeric"))
        }, 
        variableTypes <- "character"
    ), 
    groupingVariables_ReportSuperIndividuals = list(
        class = "vector", 
        title = "One or more variables to group super-individuals by when reporting SuperIndividualsData", 
        possibleValues = function(SuperIndividualsData, TargetVariable) {
            setdiff(sort(names(SuperIndividualsData)), TargetVariable)
        }, 
        variableTypes <- "character"
    ), 
    informationVariables_ReportSuperIndividuals = list(
        class = "vector", 
        title = "One or more columns to inlcude as information in ReportSuperIndividualsData", 
        possibleValues = function(SuperIndividualsData, TargetVariable, GroupingVariables) {
            sort(setdiff(names(SuperIndividualsData), c(TargetVariable, GroupingVariables)))
        }, 
        variableTypes <- "character"
    ), 
    targetVariableUnit_ReportSuperIndividuals = list(
        class = "vector", 
        title = "Select Unit for the TargetVariable", 
        possibleValues = function(TargetVariable) {
            quantity <- getBaseUnit(dataType = "SuperIndividualsData", variableName = TargetVariable, element = "quantity")
            if(is.na(quantity)) {
                list()
            }
            else {
                RstoxData::getUnitOptions(quantity)
            }
        }
    ), 
    weightingVariable_ReportSuperIndividuals = list(
        class = "vector", 
        title = "Select weighting variable", 
        possibleValues = function(SuperIndividualsData, TargetVariable, GroupingVariables, InformationVariables) {
            sort(setdiff(names(SuperIndividualsData), c(TargetVariable, GroupingVariables, InformationVariables)))
        }, 
        variableTypes <- "character"
    ), 
    
    # ReportQuantity: 
    groupingVariables_ReportQuantity = list(
        class = "vector", 
        title = "One or more variables to group by when reporting QuantityData", 
        possibleValues = function(QuantityData) {
            sort(names(QuantityData$Data))
        }, 
        variableTypes <- "character"
    ), 
    informationVariables_ReportQuantity = list(
        class = "vector", 
        title = "One or more columns to inlcude as information in ReportQuantityData", 
        possibleValues = function(QuantityData, GroupingVariables) {
            sort(setdiff(names(QuantityData$Data), GroupingVariables))
        }, 
        variableTypes <- "character"
    ), 
    targetVariableUnit_ReportQuantity = list(
        class = "vector", 
        title = "Select Unit for the TargetVariable", 
        possibleValues = function(TargetVariable) {
            quantity <- getBaseUnit(dataType = "QuantityData", variableName = TargetVariable, element = "quantity")
            if(is.na(quantity)) {
                list()
            }
            else {
                RstoxData::getUnitOptions(quantity)
            }
        }
    ), 
    weightingVariable_ReportQuantity = list(
        class = "vector", 
        title = "Select weighting variable", 
        possibleValues = function(QuantityData, GroupingVariables, InformationVariables) {
            sort(setdiff(names(QuantityData), c(GroupingVariables, InformationVariables)))
        }, 
        variableTypes <- "character"
    ), 
    
    # ReportDensity: 
    groupingVariables_ReportDensity = list(
        class = "vector", 
        title = "One or more variables to group by when reporting DensityData", 
        possibleValues = function(DensityData) {
            sort(names(DensityData$Data))
        }, 
        variableTypes <- "character"
    ), 
    informationVariables_ReportDensity = list(
        class = "vector", 
        title = "One or more columns to inlcude as information in ReportDensityData", 
        possibleValues = function(DensityData, GroupingVariables) {
            sort(setdiff(names(DensityData$Data), GroupingVariables))
        }, 
        variableTypes <- "character"
    ), 
    densityUnit = list(
        class = "vector", 
        title = "Select unit for the Density", 
        possibleValues = function() {
            quantity <- getBaseUnit(dataType = "DensityData", variableName = "Density", element = "quantity")
            if(is.na(quantity)) {
                list()
            }
            else {
                RstoxData::getUnitOptions(quantity)
            }
        }
    ), 
    weightingVariable_ReportDensity = list(
        class = "vector", 
        title = "Select weighting variable", 
        possibleValues = function(DensityData, GroupingVariables, InformationVariables) {
            sort(setdiff(names(DensityData), c(GroupingVariables, InformationVariables)))
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
    
    #getIndividualVaiableNamesSingle = list(
    #    class = "single", 
    #    title = "Select variable", 
    #    possibleValues = getIndividualNames
    #), 
    
    getImputeAtMissing = list(
        class = "vector", 
        title = "Select a variable to impute", 
        possibleValues = function(SuperIndividualsData) {
            getIndividualNames(SuperIndividualsData, tables = "Individual", removeKeys = TRUE) 
        }
    ), 
    
    getImputeByEqual = list(
        class = "vector", 
        title = "Select a variable to impute", 
        possibleValues = function(SuperIndividualsData, ImputeAtMissing) {
            getIndividualNames(SuperIndividualsData, remove = ImputeAtMissing, tables = c("Individual", "SpeciesCategory"), removeKeys = TRUE) 
        }
    ), 
    
    getToImpute = list(
        class = "vector", 
        title = "Select variables to impute", 
        #possibleValues = function(SuperIndividualsData, ImputeByEqual, ImputeAtMissing) {
        #    getIndividualNames(SuperIndividualsData, remove = c(ImputeByEqual, ImputeAtMissing), tables = "Individual", removeKeys = TRUE) 
        #}
        possibleValues = function(SuperIndividualsData, ImputeByEqual) {
            getIndividualNames(SuperIndividualsData, remove = ImputeByEqual, tables = "Individual", removeKeys = TRUE) 
        }
    ), 
    
    groupingVariables = list(
        class = "vector", 
        title = "Select GroupingVariables for regression"
    ), 
    
    pointColor = list(
        class = "vector", 
        title = "Select color/oclor scale for the points", 
        possibleValues = function(NASCData, SumNASCData, ColorVariable) {
            if(missing(SumNASCData)) {
                var <- NASCData[[ColorVariable]]
            }
            else {
                var <- SumNASCData$Data[[ColorVariable]]
            }
            if(isCategorical(var)) {
                PointColor <- list()
            }
            else {
                PointColor <- "combined.color"
            }
            
            return(PointColor)
        }
    ), 
    
    
    
    
    
    reportVariableUnit_ReportSpeciesCategoryCatch = list(
        class = "vector", 
        title = "Select unit for the ReportVariable", 
        possibleValues = function(ReportVariable) {
            quantity <- getBaseUnit(dataType = "SpeciesCategoryCatchData", variableName = ReportVariable, element = "quantity")
            if(is.na(quantity)) {
                list()
            }
            else {
                RstoxData::getUnitOptions(quantity)
            }
        }
    )
)
