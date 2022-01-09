#' Backward compabitibility actions:
#' @export
backwardCompatibility <- list(
    renameFunction = list(
        list(
            changeVersion = "1.2.36", 
            functionName = "ReportBootstrap", 
            modelName = "report", 
            newFunctionName = "RstoxFramework::ReportBootstrap"
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "GearDependentCatchCompensation", 
            modelName = "baseline", 
            newFunctionName = "RstoxFramework::GearDependentLengthDistributionCompensation"
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "LengthDependentCatchCompensation", 
            modelName = "baseline", 
            newFunctionName = "RstoxFramework::LengthDependentLengthDistributionCompensation"
        )
    ), 
    
    removeParameter = list(
        list(
            changeVersion = "1.2.35", 
            functionName = "DefineAcousticPSU", 
            modelName = "baseline", 
            parameterName = "SavePSUByTime"
        ),  
        list(
            changeVersion = "1.3.6", 
            functionName = "SweptAreaDensity", 
            modelName = "baseline", 
            parameterName = "SweepWidthByCruise"
        ),  
        list(
            changeVersion = "1.5.2", 
            functionName = "SpeciesCategoryCatch", 
            modelName = "baseline", 
            parameterName = "CatchVariable"
        ),  
        list(
            changeVersion = "1.5.2", 
            functionName = "ReportSpeciesCategoryCatch", 
            modelName = "report", 
            parameterName = "Translation"
        )
    ),  
    
    addParameter = list(
        list(
            changeVersion = "1.3.16", 
            functionName = "ImputeSuperIndividuals", 
            modelName = "baseline", 
            parameterName = "ImputationMethod", 
            parameterValue = "RandomSampling"
        ), 
        list(
            changeVersion = "1.3.16", 
            functionName = "ImputeSuperIndividuals", 
            modelName = "baseline", 
            parameterName = "ImputeAtMissing", 
            parameterValue = "IndividualAge"
        ), 
        list(
            changeVersion = "1.3.16", 
            functionName = "ImputeSuperIndividuals", 
            modelName = "baseline", 
            parameterName = "ImputeByEqual", 
            parameterValue = c("SpeciesCategory", "IndividualTotalLength")
        ), 
        list(
            changeVersion = "1.4.7", 
            functionName = "DefineStratumPolygon", 
            modelName = "baseline", 
            parameterName = "StratumNameLabel", 
            parameterValue = "polygonName"
        ), 
        list(
            changeVersion = "1.4.7", 
            functionName = "DefineStratumPolygon", 
            modelName = "baseline", 
            parameterName = "SimplifyStratumPolygon", 
            parameterValue = "FALSE"
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "ReportSpeciesCategoryCatch", 
            modelName = "report", 
            parameterName = "ReportVariable"
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "SweptAreaDensity", 
            modelName = "baseline", 
            parameterName = "SweptAreaDensityType"
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "SweptAreaDensity", 
            modelName = "baseline", 
            parameterName = "MeanSpeciesCategoryCatchData"
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "SweptAreaDensity", 
            modelName = "baseline", 
            parameterName = "DensityType"
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "GearDependentLengthDistributionCompensation", 
            modelName = "baseline", 
            parameterName = "InputDataType", 
            parameterValue = "LengthDistributionData"
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "GearDependentLengthDistributionCompensation", 
            modelName = "baseline", 
            parameterName = "SpeciesCategoryCatchData"
        )
    ),  
    
    renameParameter = list(
        list(
            changeVersion = "1.2.43", 
            functionName = "DefineAcousticTargetStrength", 
            modelName = "baseline", 
            parameterName = "TargetStrengthDefinition",
            newParameterName = "TargetStrengthTable"
        ), 
        list(
            changeVersion = "1.2.44", 
            functionName = "ReportSpeciesCategoryCatch", 
            modelName = "report", 
            parameterName = "StoxBioticTranslation",
            newParameterName = "Translation"
        ), 
        list(
            changeVersion = "1.5.1", 
            functionName = "DefineAcousticTargetStrength", 
            modelName = "baseline", 
            parameterName = "TargetStrengthTable",
            newParameterName = "AcousticTargetStrengthTable"
        ), 
        list(
            changeVersion = "1.5.1", 
            functionName = "DefineAcousticTargetStrength", 
            modelName = "baseline", 
            parameterName = "TargetStrengthMethod",
            newParameterName = "AcousticTargetStrengthModel"
        )
    ),  
    
    translateParameter = list(
	    list(
	        changeVersion = "1.2.43", 
	        functionName = "DefineAcousticTargetStrength", 
	        modelName = "baseline", 
	        parameterName = "DefinitionMethod", 
	        value = "Table", 
	        newValue = "TargetStrengthTable"
	    ), 
	    list(
	        changeVersion = "1.2.43", 
	        functionName = "DefineStratumPolygon", 
	        modelName = "baseline", 
	        parameterName = "DefinitionMethod", 
	        value = "None", 
	        newValue = "Manual"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "DefineAcousticTargetStrength", 
	        modelName = "baseline", 
	        parameterName = "DefinitionMethod", 
	        value = "TargetStrengthTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "DefineSurvey", 
	        modelName = "baseline", 
	        parameterName = "DefinitionMethod", 
	        value = "SurveyTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "MeanLengthDistribution", 
	        modelName = "baseline", 
	        parameterName = "SurveyDefinitionMethod", 
	        value = "SurveyTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "MeanNASC", 
	        modelName = "baseline", 
	        parameterName = "SurveyDefinitionMethod", 
	        value = "SurveyTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "DefineBioticLayer", 
	        modelName = "baseline", 
	        parameterName = "DefinitionMethod", 
	        value = "LayerTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "DefineAcousticLayer", 
	        modelName = "baseline", 
	        parameterName = "DefinitionMethod", 
	        value = "LayerTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "DefineBioticAssignment", 
	        modelName = "baseline", 
	        parameterName = "LayerDefinitionMethod", 
	        value = "LayerTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "SumLengthDistribution", 
	        modelName = "baseline", 
	        parameterName = "LayerDefinitionMethod", 
	        value = "LayerTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "MeanLengthDistribution", 
	        modelName = "baseline", 
	        parameterName = "LayerDefinitionMethod", 
	        value = "LayerTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "SumNASC", 
	        modelName = "baseline", 
	        parameterName = "LayerDefinitionMethod", 
	        value = "LayerTable", 
	        newValue = "Table"
	    ), 
	    list(
	        changeVersion = "1.5.1", 
	        functionName = "MeanNASC", 
	        modelName = "baseline", 
	        parameterName = "LayerDefinitionMethod", 
	        value = "LayerTable", 
	        newValue = "Table"
	    )
	), 
    
    renameProcessData = list(
        list(
            changeVersion = "1.5.1", 
            functionName = "DefineAcousticTargetStrength", 
            modelName = "baseline", 
            processDataName = "TargetStrengthMethod",
            newProcessDataName = "AcousticTargetStrengthModel"
        ), 
        list(
            changeVersion = "1.5.1", 
            functionName = "DefineAcousticTargetStrength", 
            modelName = "baseline", 
            processDataName = "TargetStrengthTable",
            newProcessDataName = "AcousticTargetStrengthTable"
        )
    ), 
    
    renameColumInProcessDataTable = list(
        list(
            changeVersion = "1.5.1", 
            functionName = "DefineAcousticTargetStrength", 
            modelName = "baseline", 
            processDataName = "AcousticTargetStrengthModel",
            processDataColumnName = "TargetStrengthMethod",
            newProcessDataColumnName = "AcousticTargetStrengthModel"
        )
    )
)