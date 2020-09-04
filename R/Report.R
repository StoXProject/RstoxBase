##################################################
##################################################
#' Report ImputeSuperIndividualsData
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{ImputeSuperIndividualsData}}.
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
#' A \code{\link{ReportImputeSuperIndividualsData}} object.
#' 
#' @examples
#' 
#' @seealso 
#' 
#' @export
#' 
ReportImputeSuperIndividuals <- function(
    ImputeSuperIndividualsData, 
    TargetVariable, 
    ReportFunction = getRstoxBaseDefinitions("reportFunctions")$functionName, 
    GroupingVariables = character(), 
    RemoveMissingValues = FALSE, 
    WeightingVariable = character()
) 
{
    aggregateBaselineDataOneTable(
        stoxData = ImputeSuperIndividualsData, 
        targetVariable = TargetVariable, 
        aggregationFunction = ReportFunction, 
        groupingVariables = GroupingVariables, 
        na.rm = RemoveMissingValues, 
        weightingVariable = WeightingVariable
    ) 
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
        do.call(
            aggregationFunction, 
            args, 
            envir = as.environment(paste("package", getReportFunctionPackage(aggregationFunction), sep = ":"))
        )
    }
    
    stoxDataCopy <<- data.table::copy(stoxData)
    stoxDataCopy[, fun(.SD), by = groupingVariables]
    
    return(stoxDataCopy)
}


isWeightingFunction <- function(x) {
    getRstoxBaseDefinitions("reportFunctions")[functionName == x, weighted]
}

getWeightingParameter <- function(x) {
    getRstoxBaseDefinitions("reportFunctions")[functionName == x, weightingParameter]
}

getReportFunctionPackage <- function(x) {
    getRstoxBaseDefinitions("reportFunctions")[functionName == x, packageName]
}
