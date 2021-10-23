#' Backward compabitibility actions:
#' @export
backwardCompatibility <- list(
    renameFunction = list(
        list(
            changeVersion = "1.2.36", 
            functionName = "ReportBootstrap", 
            modelName = "report", 
            newFunctionName = "RstoxFramework::ReportBootstrap"
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
	    )
	)
)