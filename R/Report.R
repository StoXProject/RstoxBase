##################################################
##################################################
#' Report SuperIndividualsData
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{SuperIndividualsData}}.
#' 
#' @inheritParams ModelData
#' @inheritParams general_report_arguments
#' @param ReportFunction The function to apply, one of "summaryStox", "sum", "mean", "weighted.mean", "median", "min", "max", "sd", "var", "cv", "summary", "quantile", "percentile_5_95".
#' @param WeightingVariable The variable to weight by. Only relevant for \code{ReportFunction} "weighted.mean".
#'
#' @details This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLenght, or average IndividualTotalLenght for each IndiivdualAge and Stratum.
#' 
#' @return
#' A \code{\link{ReportSuperIndividualsData}} object.
#' 
#' @examples
#' 
#' @seealso 
#' 
#' @export
#' 
ReportSuperIndividuals <- function(
    SuperIndividualsData, 
    TargetVariable, 
    ReportFunction = getRstoxBaseDefinitions("reportFunctions")$functionName, 
    GroupingVariables = character(), 
    RemoveMissingValues = FALSE, 
    WeightingVariable = character()
) 
{
    aggregateBaselineDataOneTable(
        stoxData = SuperIndividualsData, 
        targetVariable = TargetVariable, 
        aggregationFunction = ReportFunction, 
        groupingVariables = GroupingVariables, 
        na.rm = RemoveMissingValues, 
        weightingVariable = WeightingVariable
    ) 
}



##################################################
##################################################
#' Report Bootstrap
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{BootstrapData}}.
#' 
##' @inheritParams ModelData
#' @inheritParams general_report_arguments
#' @param BaselineProcess A vector of character strings naming the baseline processes to report from the boostrap output.
#' @param AggregationFunction The function to apply to each bootstrap run. This must be a function returning a single value.
#' @param BootstrapReportFunction The function to apply across bootstrap run, such as "cv" or "stoxSummary".
#' @param AggregationWeightingVariable The variable to weight by in the \code{AggregationFunction}.
#' @param BootstrapReportWeightingVariable The variable to weight by in the \code{BootstrapReportFunction}.
#'
#' @details This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLenght, or average IndividualTotalLenght for each IndiivdualAge and Stratum.
#' 
#' @return
#' A \code{\link{ReportBootstrapData}} object.
#' 
#' @examples
#' 
#' @seealso 
#' 
#' @export
#' 
ReportBootstrap <- function(
    BootstrapData, 
    BaselineProcess, 
    TargetVariable, 
    AggregationFunction = getRstoxBaseDefinitions("reportFunctions")[multiple == FALSE, functionName], 
    BootstrapReportFunction = getRstoxBaseDefinitions("reportFunctions")$functionName, 
    GroupingVariables = character(), 
    RemoveMissingValues = FALSE, 
    AggregationWeightingVariable = character(), 
    BootstrapReportWeightingVariable = character()
) 
{
    # Run the initial aggregation (only applicable for single output functions):
    AggregationFunction <- match.arg(AggregationFunction)
    out <- aggregateBaselineDataOneTable(
        stoxData = BootstrapData[[BaselineProcess]], 
        targetVariable = TargetVariable, 
        aggregationFunction = AggregationFunction, 
        groupingVariables = c(GroupingVariables, "BootstrapID"), 
        na.rm = RemoveMissingValues, 
        weightingVariable = AggregationWeightingVariable
    )
    
    
    # Get the name of the new TargetVariable:
    TargetVariableAfterInitialAggregation <- getReportFunctionVariableName(
        functionName
        = AggregationFunction, 
        targetVariable = TargetVariable
    )
    
    # Run the report function of the bootstraps:
    BootstrapReportFunction <- match.arg(BootstrapReportFunction)
    out <- aggregateBaselineDataOneTable(
        stoxData = out, 
        targetVariable = TargetVariableAfterInitialAggregation, 
        aggregationFunction = BootstrapReportFunction, 
        groupingVariables = GroupingVariables, 
        na.rm = RemoveMissingValues, 
        weightingVariable = BootstrapReportWeightingVariable
    )
    
    return(out)
}



#' 
#' @export
#' 
aggregateBaselineDataOneTable <- function(
    stoxData, 
    targetVariable, 
    aggregationFunction = getRstoxBaseDefinitions("reportFunctions")$functionName, 
    subTable = character(), 
    groupingVariables = character(), 
    na.rm = FALSE, 
    weightingVariable = character()
)
{
    if(!length(stoxData)) {
        return(stoxData)
    }
    
    # Get the aggregation function:
    aggregationFunction <- match.arg(aggregationFunction)
    
    # Extract the sub table:
    if(length(subTable)) {
        subTable <- strsplit(subTable, "/")
        for(tableName in subTable) {
            stoxData <- stoxData[[tableName]]
        }
    }
    
    # Get the function to use:
    fun <- function(x) {
        # Create the list of inputs to the function:
        args <- list(
            x[[targetVariable]], 
            na.rm = na.rm
        )
        # Add weightin to the list of inputs to the function:
        if(isWeightingFunction(aggregationFunction)) {
            if(!length(weightingVariable)) {
                stop("WeightingVariable must be given.")
            }
            args[[getWeightingParameter(aggregationFunction)]] = x[[weightingVariable]]
        }
        # Call the function in the appropriate enivronment:
        out <- do.call(
            aggregationFunction, 
            args, 
            envir = as.environment(paste("package", getReportFunctionPackage(aggregationFunction), sep = ":"))
        )
        
        # Add the function name as names if the function does not name the output:
        names(out) <- getReportFunctionVariableName(aggregationFunction, targetVariable)
        # Convert to list to insert each element to a named column of the data table:
        out <- as.list(out)
        
        return(out)
    }
    
    outputData <- stoxData[, fun(.SD), by = groupingVariables]
    
    # Order by the grouping variables:
    data.table::setorderv(outputData, groupingVariables)
    
    
    return(outputData)
}


# Get/define the report function result variable name suffix:
getReportFunctionOutputNames <- function(functionName, packageName) {
    result <- names(
        do.call(
            functionName, 
            args = list(0), 
            envir = as.environment(paste("package", packageName, sep = ":"))
        )
    )
    if(!length(result)) {
        result <- functionName
    }
    
    return(result)
}

getReportFunctionVariableName <- function(functionName, targetVariable) {
    suffix <- getReportFunctionOutputNames(
        functionName = functionName, 
        packageName = getReportFunctionPackage(functionName)
    )
    paste(targetVariable, suffix, sep = "_")
}


isWeightingFunction <- function(x) {
    getRstoxBaseDefinitions("reportFunctions")[functionName == x, weighted]
}

#' 
#' @export
#'
getWeightingFunctions <- function() {
    getRstoxBaseDefinitions("reportFunctions")[weighted == TRUE, functionName]
}


getWeightingParameter <- function(x) {
    getRstoxBaseDefinitions("reportFunctions")[functionName == x, weightingParameter]
}

getReportFunctionPackage <- function(x) {
    getRstoxBaseDefinitions("reportFunctions")[functionName == x, packageName]
}





##################################################
##################################################
#' Report SpeciesCategoryCatch
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{SpeciesCategoryCatch}}.
#' 
#' @inheritParams ModelData
#' @param TargetVariable The variable to report.
#' @param ReportFunction The function to apply, one of "summaryStox", "sum", "mean", "weighted.mean", "median", "min", "max", "sd", "var", "cv", "summary", "quantile", "percentile_5_95".
#' @param GroupingVariables The variables to report by, e.g. "Stratum" or "SpeciesCategory".
#' @param RemoveMissingValues Logical: If TRUE, remove missing values (NAs). The default (FALSE) implies to report NA if at least one of the values used in the \code{ReportFunction} is NA.  
#' @param WeightingVariable The variable to weight by. Only relevant for \code{ReportFunction} "weighted.mean".
#'
#' @details This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLenght, or average IndividualTotalLenght for each IndiivdualAge and Stratum.
#' 
#' @return
#' A \code{\link{ReportSpeciesCategoryCatchData}} object.
#' 
#' @examples
#' 
#' @seealso 
#' 
#' @export
#' 
ReportSpeciesCategoryCatch <- function(
    SpeciesCategoryCatchData, 
    StoxBioticTranslation
) 
{
    
    # Add a warining if there are empty cells in the NewValue column of the StoxBioticTranslation table:
    ValueWithEmptyNewValue <- StoxBioticTranslation[nchar(NewValue) == 0, Value]
    if(length(ValueWithEmptyNewValue)) {
        warning("StoX: The following Values had empty NewValue in the StoxBioticTranslation, and were removed from the report: ", paste(ValueWithEmptyNewValue, collapse = ", "), ".")
        SpeciesCategoryCatchData$SpeciesCategoryCatch[, V1 := NULL]
    }
    
    ValueNotPresentInStoxBioticTranslation <- setdiff(
        setdiff(names(SpeciesCategoryCatchData$SpeciesCategoryCatch), "Haul"), 
        StoxBioticTranslation$NewValue
    )
    if(length(ValueNotPresentInStoxBioticTranslation)) {
        warning("StoX: The following SpeciesCategories were not found in the NewValue column of the StoxBioticTranslation, and were removed from the report: ", paste(ValueNotPresentInStoxBioticTranslation, collapse = ", "), ".")
        SpeciesCategoryCatchData$SpeciesCategoryCatch[, (ValueNotPresentInStoxBioticTranslation) := NULL]
    }
    
    ReportSpeciesCategoryCatchData <- RstoxData::mergeDataTables(SpeciesCategoryCatchData, output.only.last = TRUE)
    
    return(ReportSpeciesCategoryCatchData)
}




