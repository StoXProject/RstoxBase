##################################################
##################################################
#' Report SuperIndividualsData
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{SuperIndividualsData}}.
#' 
#' @inheritParams ModelData
#' @inheritParams general_report_arguments
#' @param TargetVariableUnit The unit to use for the \code{TargetVariable}. See RstoxData::StoxUnits for possible units (look for the appropriate quantity, e.g. "length" for IndividualTotalLength, and use the shortname in the \code{TargetVariableUnit}).
#' @param ReportFunction The function to apply, see RstoxBase::getRstoxBaseDefinitions("reportFunctions")$functionName.
#' @param WeightingVariable The variable to weight by. Only relevant for \code{ReportFunction} "weighted.mean".
#'
#' @details This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLength, or average IndividualTotalLength for each IndividualAge and Stratum.
#' 
#' @return
#' A \code{\link{ReportSuperIndividualsData}} object.
#' 
#' @export
#' 
ReportSuperIndividuals <- function(
    SuperIndividualsData, 
    TargetVariable = character(), 
    TargetVariableUnit = character(), 
    ReportFunction = getReportFunctions(getMultiple = FALSE), 
    GroupingVariables = character(), 
    InformationVariables = character(), 
    RemoveMissingValues = FALSE, 
    WeightingVariable = character()
) 
{
    # Issue a warning if RemoveMissingValues = TRUE:
    if(isTRUE(RemoveMissingValues) && any(is.na(SuperIndividualsData[[TargetVariable]]))) {
        warning(getRstoxBaseDefinitions("RemoveMissingValuesWarning")(TargetVariable))
    }
    
    SuperIndividualsData[[TargetVariable]] <- setUnitRstoxBase(
        SuperIndividualsData[[TargetVariable]], 
        dataType =  "SuperIndividualsData", 
        variableName = TargetVariable, 
        unit = TargetVariableUnit
    )
    
    output <- aggregateBaselineDataOneTable(
        stoxData = SuperIndividualsData, 
        TargetVariable = TargetVariable, 
        aggregationFunction = ReportFunction, 
        GroupingVariables = GroupingVariables, 
        InformationVariables = InformationVariables, 
        na.rm = RemoveMissingValues, 
        WeightingVariable = WeightingVariable
    )
    
    if(RstoxData::hasUnit(SuperIndividualsData[[TargetVariable]], property = "shortname")) {
        unit <- RstoxData::getUnit(SuperIndividualsData[[TargetVariable]], property = "shortname")
        output <- cbind(output, Unit = unit)
    }
    
    return(output)
}


##################################################
##################################################
#' Report DensityData
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{DensityData}}.
#' 
#' @inheritParams ModelData
#' @inheritParams general_report_arguments
#' @param DensityUnit The unit to use for the \code{Density}. See subset(RstoxData::StoxUnits, quantity == "area_number_density") for possible units (use the shortname in the \code{DensityUnit}).
#' @inheritParams ReportSuperIndividuals
#' 
#' @return
#' A \code{\link{ReportDensityData}} object.
#' 
#' @export
#' 
ReportDensity <- function(
    DensityData, 
    DensityUnit = character(), 
    #TargetVariable = character(), 
    ReportFunction = getReportFunctions(getMultiple = FALSE), 
    GroupingVariables = character(), 
    InformationVariables = character(), 
    RemoveMissingValues = FALSE, 
    WeightingVariable = character()
) 
{
    # Only Density is relevant here:
    TargetVariable <- "Density"
    
    # Issue a warning if RemoveMissingValues = TRUE:
    if(isTRUE(RemoveMissingValues) && any(is.na(DensityData[[TargetVariable]]))) {
        warning(getRstoxBaseDefinitions("RemoveMissingValuesWarning")(TargetVariable))
    }
    
    DensityData$Data[[TargetVariable]] <- setUnitRstoxBase(
        DensityData$Data[[TargetVariable]], 
        dataType =  "DensityData", 
        variableName = TargetVariable, 
        unit = DensityUnit
    )
    
    output <- aggregateBaselineDataOneTable(
        stoxData = DensityData$Data, 
        TargetVariable = TargetVariable, 
        aggregationFunction = ReportFunction, 
        GroupingVariables = GroupingVariables, 
        InformationVariables = InformationVariables, 
        na.rm = RemoveMissingValues, 
        WeightingVariable = WeightingVariable
    )
    
    if(RstoxData::hasUnit(DensityData$Data[[TargetVariable]], property = "shortname")) {
        unit <- RstoxData::getUnit(DensityData$Data[[TargetVariable]], property = "shortname")
        output <- cbind(output, Unit = unit)
    }
    
    return(output)
}


##################################################
##################################################
#' Report QuantityData
#' 
#' Reports the sum, mean or other functions on a variable of the \code{\link{QuantityData}}.
#' 
#' @inheritParams ModelData
#' @inheritParams general_report_arguments
#' @param TargetVariableUnit The unit to use for the \code{TargetVariable}. For possible units, see subset(RstoxData::StoxUnits, quantity == "cardinality") for TargetVariable = "Abundance" and subset(RstoxData::StoxUnits, quantity == "mass") for TargetVariable = "Biomass".
#' @inheritParams ReportSuperIndividuals
#' 
#' @return
#' A \code{\link{ReportQuantityData}} object.
#' 
#' @export
#' 
ReportQuantity <- function(
    QuantityData, 
    TargetVariable = c("Abundance", "Biomass"), 
    TargetVariableUnit = character(), 
    ReportFunction = getReportFunctions(getMultiple = FALSE), 
    GroupingVariables = character(), 
    InformationVariables = character(), 
    RemoveMissingValues = FALSE, 
    WeightingVariable = character()
) 
{
    # Issue a warning if RemoveMissingValues = TRUE:
    if(isTRUE(RemoveMissingValues) && any(is.na(QuantityData[[TargetVariable]]))) {
        warning(getRstoxBaseDefinitions("RemoveMissingValuesWarning")(TargetVariable))
    }
    
    QuantityData$Data[[TargetVariable]] <- setUnitRstoxBase(
        QuantityData$Data[[TargetVariable]], 
        dataType =  "QuantityData", 
        variableName = TargetVariable, 
        unit = TargetVariableUnit
    )
    
    output <- aggregateBaselineDataOneTable(
        stoxData = QuantityData$Data, 
        TargetVariable = TargetVariable, 
        aggregationFunction = ReportFunction, 
        GroupingVariables = GroupingVariables, 
        InformationVariables = InformationVariables, 
        na.rm = RemoveMissingValues, 
        WeightingVariable = WeightingVariable
    )
    
    if(RstoxData::hasUnit(QuantityData$Data[[TargetVariable]], property = "shortname")) {
        unit <- RstoxData::getUnit(QuantityData$Data[[TargetVariable]], property = "shortname")
        output <- cbind(output, Unit = unit)
    }
    
    return(output)
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
    InformationVariables = character(), 
    na.rm = FALSE, 
    padWithZerosOn = character(), 
    WeightingVariable = character()
)
{
    if(!length(stoxData)) {
        return(stoxData)
    }
    
    # Get the aggregation function:
    aggregationFunction <- RstoxData::match_arg_informative(aggregationFunction)
    
    # Extract the sub table:
    if(length(subTable)) {
        subTable <- strsplit(subTable, "/")
        for(tableName in subTable) {
            stoxData <- stoxData[[tableName]]
        }
    }
    
    # Define constants outside of the fun to save time:
    # Whether we are using a function with weighting:
    weighting <- isWeightingFunction(aggregationFunction)
    if(weighting && !length(WeightingVariable)) {
        stop("WeightingVariable must be given.")
    }
    weightingParameter <- getWeightingParameter(aggregationFunction)
    # Define the environment to run the function in:
    funEnvir <- as.environment(paste("package", getReportFunctionPackage(aggregationFunction), sep = ":"))
    # Get the reportFunctionVariableName
    reportFunctionVariableName <- getReportFunctionVariableName(aggregationFunction, TargetVariable)
    
    
    # Get the function to use:
    fun <- function(x) {
        # Create the list of inputs to the function:
        args <- list(
            x[[TargetVariable]], 
            na.rm = na.rm
        )
        # Add weighting to the list of inputs to the function:
        if(weighting) {
            args[[weightingParameter]] = x[[WeightingVariable]]
        }
        # Call the function in the appropriate environment:
        out <- do.call(
            aggregationFunction, 
            args, 
            envir = funEnvir
        )
        
        # Add the function name as names if the function does not name the output:
        names(out) <- reportFunctionVariableName
        # Convert to list to insert each element to a named column of the data table:
        out <- as.list(out)
        
        return(out)
    }
    
    # Add a CJ operation here like in StoX 2.7 (function reportQuantityAtLevel). This needs an option, so that it is only used across bootstrap iterations:
    if(length(padWithZerosOn)) {
        # Attempt to generalize the creation of the grid and filling in the data, intended for use here and for an ExpandNASC function, but the latter was abandoned, so no need to generalize:
        ### dimensionVariables <- c(GroupingVariables, padWithZerosOn)
        ### informationVariables = setdiff(names(stoxData), c(dimensionVariables, TargetVariable))
        ### stoxData2 <- data.table::copy(stoxData)
        ### expandStoxData(
        ###     stoxData = stoxData2, 
        ###     dimensionVariables = dimensionVariables, 
        ###     targetVariable = TargetVariable, 
        ###     informationVariables = informationVariables
        ### )
        
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
        # Convert NA to 0 only for Biomass or Abundance:
        abudanceVariableKeys <- getDataTypeDefinition("SuperIndividualsData", subTable = "Data", elements = "data", unlist = TRUE)
        isAbudanceVariable <- rowSums(outer(abudanceVariables, abudanceVariableKeys, startsWith)) > 0
        abudanceVariables <- abudanceVariables[isAbudanceVariable]
        
        if(length(abudanceVariables)) {
            # Set all NA to 0, both those from the original stoxData and those introduced by the grid:
            replaceNAByReference(stoxData, cols = abudanceVariables, replacement = 0)
            # Restore the NAs from the original stoxData:
            stoxData[areNA, eval(TargetVariable) := NA]
        }
    }
    
    outputData <- stoxData[, fun(.SD), by = GroupingVariables]
    
    # Order by the grouping variables:
    if(length(GroupingVariables)) {
        data.table::setorderv(outputData, GroupingVariables)
    }
    
    # Add the InformationVariables: 
    if(length(InformationVariables)) {
        if(any(InformationVariables %in% GroupingVariables)) {
            warning("StoX: Removing the following InformationVariables that are present also in GroupingVariables: ", paste(intersect(InformationVariables, GroupingVariables), collapse = ", "), ".")
            InformationVariables <- setdiff(InformationVariables, GroupingVariables)
        }
        if(length(InformationVariables)) {
            toAdd <- unique(stoxData[, c(GroupingVariables, InformationVariables), with = FALSE])
            nUniqueLevelsOfGroupingVariables <- nrow(unique(outputData[, GroupingVariables, with = FALSE]))
            nUniqueLevelsOfInformationVariables <- nrow(toAdd)
            if(nUniqueLevelsOfInformationVariables > nUniqueLevelsOfGroupingVariables) {
                stop("The InformationVariables cannot contain more unique combinations than the GroupingVariables.")
            }
            outputData <- merge(
                outputData, 
                toAdd,  
                all.x = TRUE, 
                by = GroupingVariables)
        }
    }
    
    
    # Set the number of digits. Added on 2021-03-04:
    #RstoxData::setRstoxPrecisionLevel(outputData)
    
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
#' @param ReportVariable The column to report.
#' @param ReportVariableUnit The unit to use for the \code{ReportVariable}. See RstoxData::StoxUnits for possible units (look for the appropriate quantity, e.g. "length" for IndividualTotalLength, and use the shortname in the \code{ReportVariableUnit}).
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
    ReportVariable = c("TotalCatchNumber", "TotalCatchWeight"), 
    ReportVariableUnit = character()
){
    
    # Get the ReportVariable:
    ReportVariable <- RstoxData::match_arg_informative(ReportVariable)
    
    # Warning if there are species categories which are empty string:
    categoryVariable <- getDataTypeDefinition(dataType = "DensityData", elements = "categoryVariable", unlist = TRUE)
    emptyString <- SpeciesCategoryCatchData[, nchar(get(categoryVariable))] == 0
    if(any(emptyString, na.rm = TRUE)) {
        warning("StoX: There are empty strings for the ", categoryVariable, ". These will be included in the column V1 in the SpeciesCategoryCatch table.")
    }
    
    SpeciesCategoryCatchData[[ReportVariable]] <- setUnitRstoxBase(
        SpeciesCategoryCatchData[[ReportVariable]], 
        dataType =  "SpeciesCategoryCatchData", 
        variableName = ReportVariable, 
        unit = ReportVariableUnit
    )
    
    # Create the table with species categories in the columns:
    ReportSpeciesCategoryCatchData <- data.table::dcast(
        SpeciesCategoryCatchData, 
        formula = Haul ~ get(categoryVariable), 
        value.var = ReportVariable, 
        fun.aggregate = sum
    )
    
    # Add haul info as the unique table of all variables except the category and data variables:
    dataVariables <- getDataTypeDefinition(dataType = "SpeciesCategoryCatchData", elements = "data", unlist = TRUE)
    haulInfo <- unique(SpeciesCategoryCatchData[, !c(categoryVariable, dataVariables), with = FALSE], by = "Haul")
    
    ReportSpeciesCategoryCatchData <- merge(
        haulInfo, 
        ReportSpeciesCategoryCatchData, 
        by = "Haul"
    )
    
    if(RstoxData::hasUnit(SpeciesCategoryCatchData[[ReportVariable]], property = "shortname")) {
        unit <- RstoxData::getUnit(SpeciesCategoryCatchData[[ReportVariable]], property = "shortname")
        ReportSpeciesCategoryCatchData <- cbind(ReportSpeciesCategoryCatchData, Unit = unit)
    }
    
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
    
    FileFormat <- RstoxData::match_arg_informative(FileFormat)
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





