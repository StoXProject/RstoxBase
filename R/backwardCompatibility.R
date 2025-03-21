#' Backward compabitibility actions:
#' @export
backwardCompatibility_RstoxBase <- list(
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
            newFunctionName = "RstoxBase::GearDependentLengthDistributionCompensation"
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "LengthDependentCatchCompensation", 
            modelName = "baseline", 
            newFunctionName = "RstoxBase::LengthDependentLengthDistributionCompensation"
        ), 
        list(
            changeVersion = "1.5.4", 
            functionName = "Abundance", 
            modelName = "baseline", 
            newFunctionName = "RstoxBase::Quantity"
        ), 
        list(
            changeVersion = "1.5.4", 
            functionName = "ReportAbundance", 
            modelName = "baseline", 
            newFunctionName = "RstoxBase::ReportQuantity"
        ), 
        list(
            changeVersion = "1.11.4-9004", 
            functionName = "ImputeSuperIndividuals", 
            modelName = "baseline", 
            newFunctionName = "RstoxBase::ImputeSuperIndividuals_StoX3"
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
        ),  
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = c("StoxAcousticData", "LengthExponent")
        ),  
        list(
            changeVersion = "1.7.12", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "StratumPolygon"
        ),  
        list(
            changeVersion = "1.7.12", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "SurveyDefinition"
        ),  
        list(
            changeVersion = "1.7.12", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "SurveyDefinitionMethod"
        ),  
        list(
            changeVersion = "1.7.12", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "SurveyTable"
        ),  
        list(
            changeVersion = "1.7.12", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "Survey"
        ),  
        list(
            changeVersion = "1.10.3", 
            functionName = "ReportDensity", 
            modelName = "baseline", 
            parameterName = "TargetVariable"
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
            functionName = "SpeciesCategoryCatch", 
            modelName = "baseline", 
            parameterName = "SpeciesCategoryCatchType", 
            parameterValue = "Standard"
        ),  
        list(
            changeVersion = "1.5.2", 
            functionName = "ReportSpeciesCategoryCatch", 
            modelName = "report", 
            parameterName = "ReportVariable", 
            parameterValue = function(projectDescription, modelName, processIndex) {
                # Get the function input:
                thisProcess <- projectDescription[[modelName]][[processIndex]]
                functionInput <- thisProcess$functionInputs$SpeciesCategoryCatchData
                
                # Find the process that the function input points to:
                atThatProcess <- which(sapply(projectDescription[["baseline"]], "[[", "processName") == functionInput)
                thatProcess <- projectDescription[["baseline"]][[atThatProcess]]
                
                # Get the CatchVariable from the function input and use as ReportVariable:
                ReportVariable <- paste0("TotalCatch", thatProcess$functionParameters$CatchVariable)
                
                return(ReportVariable)
            }
        ), 
        list(
            changeVersion = "1.5.2", 
            functionName = "SweptAreaDensity", 
            modelName = "baseline", 
            parameterName = "SweptAreaDensityMethod", 
            parameterValue = "LengthDistributed"
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
            parameterName = "DensityType", 
            parameterValue = "AreaNumberDensity"
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
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "SumNASCData"
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "NASCData"
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "LayerDefinition"
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "LayerDefinitionMethod"
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "Resolution"
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "LayerTable"
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "AcousticLayer"
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "StratumPolygon"
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "AcousticTargetStrength"
        ), 
        list(
            changeVersion = "1.7.2", 
            functionName = "BioticAssignmentWeighting", 
            modelName = "baseline", 
            parameterName = "SpeciesLink"
        ), 
        list(
            changeVersion = "1.10.3", 
            functionName = "ReportSuperIndividuals", 
            modelName = "baseline", 
            parameterName = "TargetVariableUnit"
        ), 
        list(
            changeVersion = "1.10.3", 
            functionName = "ReportDensity", 
            modelName = "baseline", 
            parameterName = "DensityUnit"
        ), 
        list(
            changeVersion = "1.10.3", 
            functionName = "ReportQuantity", 
            modelName = "baseline", 
            parameterName = "TargetVariableUnit"
        ), 
        list(
            changeVersion = "1.10.3", 
            functionName = "ReportSpeciesCategoryCatch", 
            modelName = "baseline", 
            parameterName = "ReportVariableUnit"
        ), 
        list(
            changeVersion = "1.11.1", 
            functionName = "ImputeSuperIndividuals", 
            modelName = "baseline", 
            parameterName = "RegressionDefinition", 
            parameterValue = "FunctionInput"
        ), 
        list(
            changeVersion = "1.11.4-9002", 
            functionName = "ImputeSuperIndividuals", 
            modelName = "baseline", 
            parameterName = "ImputationLevels", 
            parameterValue = c("Haul", "Stratum", "Survey")
        ), 
        list(
            changeVersion = "1.11.4-9004", 
            functionName = "ImputeSuperIndividuals_StoX3", 
            modelName = "baseline", 
            parameterName = "ImputationLevels", 
            parameterValue = c("Haul", "Stratum", "Survey")
        ), 
        # ConditionOperator, ConditionValue and FractionOverVariable:
        list(
            changeVersion = "1.11.4-9007", 
            functionName = "ReportSuperIndividuals", 
            modelName = "report", 
            parameterName = "ConditionOperator"
        ), 
        list(
            changeVersion = "1.11.4-9007", 
            functionName = "ReportSuperIndividuals", 
            modelName = "report", 
            parameterName = "ConditionValue"
        ), 
        list(
            changeVersion = "1.11.4-9007", 
            functionName = "ReportSuperIndividuals", 
            modelName = "report", 
            parameterName = "FractionOverVariable"
        ), 
        list(
            changeVersion = "1.11.4-9007", 
            functionName = "ReportDensity", 
            modelName = "report", 
            parameterName = "ConditionOperator"
        ), 
        list(
            changeVersion = "1.11.4-9007", 
            functionName = "ReportDensity", 
            modelName = "report", 
            parameterName = "ConditionValue"
        ), 
        list(
            changeVersion = "1.11.4-9007", 
            functionName = "ReportDensity", 
            modelName = "report", 
            parameterName = "FractionOverVariable"
        ), 
        list(
            changeVersion = "1.11.4-9007", 
            functionName = "ReportQuantity", 
            modelName = "report", 
            parameterName = "ConditionOperator"
        ), 
        list(
            changeVersion = "1.11.4-9007", 
            functionName = "ReportQuantity", 
            modelName = "report", 
            parameterName = "ConditionValue"
        ), 
        list(
            changeVersion = "1.11.4-9007", 
            functionName = "ReportQuantity", 
            modelName = "report", 
            parameterName = "FractionOverVariable"
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
        ), 
        list(
            changeVersion = "1.5.4", 
            functionName = "SuperIndividuals", 
            modelName = "baseline", 
            parameterName = "AbundanceData",
            newParameterName = "QuantityData"
        ), 
        list(
            changeVersion = "1.5.4", 
            functionName = "Individuals", 
            modelName = "baseline", 
            parameterName = "AbundanceType",
            newParameterName = "QuantityType"
        ), 
        list(
            changeVersion = "1.5.4", 
            functionName = "ReportAbundance", 
            modelName = "baseline", 
            parameterName = "AbundanceData",
            newParameterName = "QuantityData"
        ), 
        list(
            changeVersion = "2.0.1-9002", 
            functionName = "ReportSpeciesCategoryCatch", 
            modelName = "report", 
            parameterName = "ReportVariable",
            newParameterName = "TargetVariable"
        ), 
        list(
            changeVersion = "2.0.1-9002", 
            functionName = "ReportSpeciesCategoryCatch", 
            modelName = "report", 
            parameterName = "ReportVariableUnit",
            newParameterName = "TargetVariableUnit"
        )#, 
        #list(
        #    changeVersion = "2.1.3-9001", 
        #    functionName = "LengthDependentLengthDistributionCompensation", 
        #    modelName = "baseline", 
        #    parameterName = "LengthDependentSweepWidthParameters",
        #    newParameterName = "LengthDependentHerdingParameters"
        #)
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
	    ), 
	    list(
	        changeVersion = "1.5.3", 
	        functionName = "BioticAssignmentWeighting", 
	        modelName = "baseline", 
	        parameterName = "WeightingMethod", 
	        value = "NormalizedTotalCount", 
	        newValue = "NormalizedTotalNumber"
	    ), 
	    list(
	        changeVersion = "1.5.3", 
	        functionName = "BioticAssignmentWeighting", 
	        modelName = "baseline", 
	        parameterName = "WeightingMethod", 
	        value = "SumWeightedCount", 
	        newValue = "SumWeightedNumber"
	    ), 
	    list(
	        changeVersion = "1.5.3", 
	        functionName = "BioticAssignmentWeighting", 
	        modelName = "baseline", 
	        parameterName = "WeightingMethod", 
	        value = "InverseSumWeightedCount", 
	        newValue = "InverseSumWeightedNumber"
	    ), 
	    list(
	        changeVersion = "1.5.3", 
	        functionName = "LengthDistribution", 
	        modelName = "baseline", 
	        parameterName = "RaisingFactorPriority", 
	        value = "Count", 
	        newValue = "Number"
	    ),
	    list(
	        changeVersion = "1.5.3", 
	        functionName = "ReportSpeciesCategoryCatch", 
	        modelName = "report", 
	        parameterName = "ReportVariable", 
	        value = "TotalCatchCount", 
	        newValue = "TotalCatchNumber"
	    ),
	    list(
	        changeVersion = "1.7.12", 
	        functionName = "BioticAssignmentWeighting", 
	        modelName = "baseline", 
	        parameterName = "WeightingMethod", 
	        value = "NASC", 
	        newValue = "AcousticDensity"
	    )#,
	    #list(
	    #    changeVersion = "2.1.3-9001", 
	    #    functionName = "LengthDependentLengthDistributionCompensation", 
	    #    modelName = "baseline", 
	    #    parameterName = "CompensationMethod", 
	    #    value = "LengthDependentSweepWidth", 
	    #    newValue = "LengthDependentHerding"
	    #)
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