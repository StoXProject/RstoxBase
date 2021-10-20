##################################################
##################################################
#' Report SuperIndividualsData
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{SuperIndividualsData}}.
#' 
#' @inheritParams ModelData
#' @inheritParams general_report_arguments
#' @param ReportFunction The function to apply, see RstoxBase::getRstoxBaseDefinitions("reportFunctions")$functionName.
#' @param WeightingVariable The variable to weight by. Only relevant for \code{ReportFunction} "weighted.mean".
#'
#' @details This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLength, or average IndividualTotalLength for each IndiivdualAge and Stratum.
#' 
#' @return
#' A \code{\link{ReportSuperIndividualsData}} object.
#' 
#' @export
#' 
ReportSuperIndividuals <- function(
    SuperIndividualsData, 
    TargetVariable, 
    ReportFunction = getReportFunctions(getMultiple = FALSE), 
    GroupingVariables = character(), 
    RemoveMissingValues = FALSE, 
    WeightingVariable = character()
) 
{
    
    # Issue a warning if RemoveMissingValues = TRUE:
    if(isTRUE(RemoveMissingValues)) {
        warning(getRstoxBaseDefinitions("RemoveMissingValuesWarning"))
    }
    
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
##################################################
#' Report DensityData
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{DensityData}}.
#' 
#' @inheritParams ModelData
#' @inheritParams general_report_arguments
#' @inheritParams ReportSuperIndividuals
#' 
#' @return
#' A \code{\link{ReportDensityData}} object.
#' 
#' @export
#' 
ReportDensity <- function(
    DensityData, 
    TargetVariable, 
    ReportFunction = getReportFunctions(getMultiple = FALSE), 
    GroupingVariables = character(), 
    RemoveMissingValues = FALSE, 
    WeightingVariable = character()
) 
{
    # Issue a warning if RemoveMissingValues = TRUE:
    if(isTRUE(RemoveMissingValues)) {
        warning(getRstoxBaseDefinitions("RemoveMissingValuesWarning"))
    }
    
    aggregateBaselineDataOneTable(
        stoxData = DensityData$Data, 
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
#' @param aggregationFunction The function to apply, see RstoxBase::getRstoxBaseDefinitions("reportFunctions")$functionName.
#' @param subTable The name of the sub table to aggregate on, if \code{stoxData} is a list of tables.
#' @param na.rm Used in the function specified by \code{aggregationFunction}.
#' @param padWithZerosOn Character vector giving the variables for which missing values should be padded with zeros. This is used particularly for bootstrapping, where a fish length missing in a bootstrap run should be considered as samples with zero individuals, and not missing, so that summary statistics end up taking all bootstrap replicates into account (if not a mean would be overestimated). When padWithZerosOn has positive length, padding with zeros is applied to this variable and to the \code{GroupingVariables}. 
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
    aggregationFunction = getReportFunctions(), 
    subTable = character(), 
    GroupingVariables = character(), 
    na.rm = FALSE, 
    padWithZerosOn = character(), 
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
    
    # Add a CJ operation here like in StoX 2.7 (function reportAbundanceAtLevel). This needs an option, so that it is only used across bootstrap iterations:
    if(length(padWithZerosOn)) {
        # Add NAs for missing combinations of the GroupingVariables:
        #stoxData <- stoxData[do.call(CJ, lapply(GroupingVariables, unique)), allow.cartesian = TRUE]
        paddingVariables <- c(GroupingVariables, padWithZerosOn)
        grid <- do.call(data.table::CJ, lapply(stoxData[, ..paddingVariables], unique))
        grid$index_ <- seq.int(nrow(grid))
        
        # Save indices in the grid at which there are NAs in the data:
        arePresent <- grid[stoxData[, ..paddingVariables], on = paddingVariables]$index_
        areNA <- arePresent[is.na(stoxData[[TargetVariable]])]
        
        stoxData <- stoxData[grid[, ..paddingVariables], on = paddingVariables]
        # Convert the NAs to 0 for the abundance and biomass columns:
        abudanceVariables <- setdiff(names(stoxData), paddingVariables)
        # Convvert NA to 0 only for Biomass or Abundance:
        abudanceVariableKeys <- getDataTypeDefinition("SuperIndividualsData", subTable = "Data", elements = "data", unlist = TRUE)
        isAbudanceVariable <- rowSums(outer(abudanceVariables, abudanceVariableKeys, startsWith)) > 0
        abudanceVariables <- abudanceVariables[isAbudanceVariable]
        
        if(length(abudanceVariables)) {
            # Set all NA to 0, both those from the original stoxData and those introduced by the grid:
            replaceNAByReference(stoxData, cols = abudanceVariables, replacement = 0)
            # Restore the NAs from the original st  oxData:
            stoxData[areNA, eval(TargetVariable) := NA]
        }
    }
    
    outputData <- stoxData[, fun(.SD), by = GroupingVariables]
    
    # Order by the grouping variables:
    if(length(GroupingVariables)) {
        data.table::setorderv(outputData, GroupingVariables)
    }
    
    # Set the number of digits. Added on 2021-03-04:
    RstoxData::setRstoxPrecisionLevel(outputData)
    
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
#' @param Translation The \code{\link[RstoxData]{Translation}} process data.
#' 
#' @details This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLenght, or average IndividualTotalLenght for each IndiivdualAge and Stratum.
#' 
#' @return
#' A \code{\link{ReportSpeciesCategoryCatchData}} object.
#' 
#' @export
#' 
ReportSpeciesCategoryCatch <- function(
    SpeciesCategoryCatchData, 
    Translation
) 
{
    
    # Add a warining if there are empty cells in the NewValue column of the Translation table:
    ValueWithEmptyNewValue <- Translation[nchar(NewValue) == 0, Value]
    if(length(ValueWithEmptyNewValue)) {
        warning("StoX: The following Values had empty NewValue in the Translation, and were removed from the report: ", paste(ValueWithEmptyNewValue, collapse = ", "), ".")
        SpeciesCategoryCatchData$SpeciesCategoryCatch[, V1 := NULL]
    }
    
    ValueNotPresentInTranslation <- setdiff(
        setdiff(names(SpeciesCategoryCatchData$SpeciesCategoryCatch), "Haul"), 
        Translation$NewValue
    )
    if(length(ValueNotPresentInTranslation)) {
        warning("StoX: The following SpeciesCategories were not found in the NewValue column of the Translation, and were removed from the report: ", paste(ValueNotPresentInTranslation, collapse = ", "), ".")
        SpeciesCategoryCatchData$SpeciesCategoryCatch[, (ValueNotPresentInTranslation) := NULL]
    }
    
    ReportSpeciesCategoryCatchData <- RstoxData::mergeDataTables(SpeciesCategoryCatchData, output.only.last = TRUE)
    
    return(ReportSpeciesCategoryCatchData)
}




##################################################
##################################################
#' Write StratumPolygon
#' 
#' Writes a StratumPolygon to GeoJSON, StoX_WKT or shapefile.
#' 
#' @inheritParams ModelData
#' @param FileFormat The format of the files to write the StratumPolygon to. \code{\link{StoX_multipolygon_WKT}} is the table of stratum name and WKT multipolygon used by StoX. 
#' 
#' @details The actual writing takes place in RstoxFramework. This function only converts the data to appropriate classes interpreted by RstoxFramework.
#' 
#' @return
#' A \code{\link{WriteStratumPolygonData}} object.
#' 
#' @noRd
# #' @export
# #' 
WriteStratumPolygon <- function(
    StratumPolygon, 
    FileFormat = c("GeoJSON", "StoX_multipolygon_WKT", "StoX_shapefile")
) 
{
    
    FileFormat <- match.arg(FileFormat)
    output <- StratumPolygon
    
    if(FileFormat == "GeoJSON") {
        # Do nonthing, as RstoxFramework wrirtes GeoJSON from SpatialPolygonsDataFrame.
    }
    else if(FileFormat == "StoX_multipolygon_WKT") {
        # Convert to WKT character vector:
        StoX_WKT <- sf::st_as_text(sf::st_as_sfc(StratumPolygon, forceMulti = TRUE))
        # Add stratum names in a matrix:
        output <- cbind(
            getStratumNames(StratumPolygon), 
            StoX_WKT
        )
        class(output) <- FileFormat
    }
    else if(FileFormat == "StoX_shapefile") {
        class(output) <- FileFormat
    }
    else {
        stop("Wrong FileFormat")
    }
    
    return(output)
}





