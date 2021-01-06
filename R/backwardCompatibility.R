#' Backward compabitibility actions:
#' @export
backwardCompatibility <- list(
	removeParameter = list(
		list(
			changeVersion = "1.2.35", 
			functionName = "DefineAcousticPSU", 
			modelName = "baseline", 
			parameterName = "SavePSUByTime"
		)
	),  
	
	renameFunction = list(
	    list(
	        changeVersion = "1.2.36", 
	        functionName = "ReportBootstrap", 
	        modelName = "report", 
	        newFunctionName = "RstoxFramework::ReportBootstrap"
	    )
	)
)