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
        TargetVariable = TargetVariable, 
        aggregationFunction = ReportFunction, 
        GroupingVariables = GroupingVariables, 
        na.rm = RemoveMissingValues, 
        WeightingVariable = WeightingVariable
    ) 
}


##################################################
#' Function to aggregate baseline data
#' 
#' @param stoxData Output from any StoX function.
#' @inheritParams general_report_arguments
#' @param aggregationFunction The function to apply, one of "summaryStox", "sum", "mean", "weighted.mean", "median", "min", "max", "sd", "var", "cv", "summary", "quantile", "percentile_5_95".
#' @param subTable The name of the sub table to aggregate on, if \code{stoxData} is a list of tables.
#' @param na.rm Used in the function specified by \code{aggregationFunction}.
#' @inheritParams ReportSuperIndividuals
#'
#' @return
#' An aggregated version of the input \code{stoxData}.
#' 
#' @seealso 
#' \code{\link{ReportSuperIndividuals}}
#' 
#' @export
#' 
aggregateBaselineDataOneTable <- function(
    stoxData, 
    TargetVariable, 
    aggregationFunction = getRstoxBaseDefinitions("reportFunctions")$functionName, 
    subTable = character(), 
    GroupingVariables = character(), 
    na.rm = FALSE, 
    WeightingVariable = character()
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
            x[[TargetVariable]], 
            na.rm = na.rm
        )
        # Add weightin to the list of inputs to the function:
        if(isWeightingFunction(aggregationFunction)) {
            if(!length(WeightingVariable)) {
                stop("WeightingVariable must be given.")
            }
            args[[getWeightingParameter(aggregationFunction)]] = x[[WeightingVariable]]
        }
        # Call the function in the appropriate enivronment:
        out <- do.call(
            aggregationFunction, 
            args, 
            envir = as.environment(paste("package", getReportFunctionPackage(aggregationFunction), sep = ":"))
        )
        
        # Add the function name as names if the function does not name the output:
        names(out) <- getReportFunctionVariableName(aggregationFunction, TargetVariable)
        # Convert to list to insert each element to a named column of the data table:
        out <- as.list(out)
        
        return(out)
    }
    
    outputData <- stoxData[, fun(.SD), by = GroupingVariables]
    
    # Order by the grouping variables:
    data.table::setorderv(outputData, GroupingVariables)
    
    
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


##################################################
#' Get the name of the target variable after aggregating.
#' 
#' @inheritParams general_report_arguments
#' @param functionName The aggregation function name.
#'
#' @export
#' 
getReportFunctionVariableName <- function(functionName, TargetVariable) {
    suffix <- getReportFunctionOutputNames(
        functionName = functionName, 
        packageName = getReportFunctionPackage(functionName)
    )
    paste(TargetVariable, suffix, sep = "_")
}


isWeightingFunction <- function(x) {
    getRstoxBaseDefinitions("reportFunctions")[functionName == x, weighted]
}



#' List weighting functions
#' 
#' @export
#' 
getWeightingFunctions <- function() {
    getRstoxBaseDefinitions("reportFunctions")[weighted == TRUE, functionName]
}

#' List weighting parameters
#' 
#' @param x The name of the report function for which to list parameters.
#' 
#' @export
#' 
getWeightingParameter <- function(x) {
    getRstoxBaseDefinitions("reportFunctions")[functionName == x, weightingParameter]
}

#' List report function packages
#' 
#' @param x The name of the report function for which to return package name.
#' 
#' @export
#' 
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
#' @param StoxBioticTranslation The \code{\link[RstoxData]{StoxBioticTranslation}} process data.
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




