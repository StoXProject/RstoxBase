##################################################
##################################################
#' Impute missing super-individual data
#'
#' WARNING, DEPRECATED FUNCTION:
#' This is the old imputation function used in StoX 3.0.0 through 3.6.2. The function contains a weakness when hauls are assigned to AcousticPSUs in more than one stratum in BioticAssignment. The resulting SuperIndividuals will then have duplicated individuals and consequently non-unique values in the Individual column, which are used to identify rows to impute from in this function. The result is that values are imputed only from the first of the rows with duplicated Individual, so that information in the other rows are not available, which may lead to incomplete imputation. 
#' 
#' For this reason the function is deprecated and the function \code{\link{ImputeSuperIndividuals}}, which considers the unique Individual column when imputing, should be used instead. However, due to the difference in the imputation method the results will differ between the two functions even when all Individual are unique. Existing StoX projects saved with StoX <= 3.6.2 will be changed to using ImputeSuperIndividuals_StoX3 when opening in StoX >= 4.0.0, but the recommendation is to change these projects to using ImputeSuperIndividuals instead.
#'
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams DefineRegression
#' @param ImputationMethod The method to use for the imputation. Currently, only "RandomSampling" is implemented, but may be accompanied "Regression" in a coming release.
#' @param ImputeAtMissing A single string naming the variable which when missing identifies target individuals to input data \bold{to}. I.e., if \code{ImputeAtMissing} is missing for an individual, perform the imputation. In StoX 3.0.0 and older, \code{ImputeAtMissing} was hard coded to IndividualAge.
#' @param ImputeByEqual A vector of strings naming the variable(s) which, when identical to the target individual, identifies the source individuals to impute data \bold{from}. The source individuals need also to have non-missing \code{ImputeAtMissing}. In StoX 3.0.0 and older, \code{ImputeByEqual} was hard coded to c("SpeciesCategory","IndividualTotalLength").
#' @param ToImpute A vector of strings naming the variable(s) to impute (copy to the target individual). Values that are not missing are not imputed. Note that values are only imputed when \code{ImputeAtMissing} is missing, so including many variables in \code{ToImpute} is only recommended if all these are present for the individuals (see Details). In StoX 3.0.0 and older, \code{ToImpute} was hard coded to all available variables of the BioticData contained in the \code{\link{SuperIndividualsData}}. 
#' @param ImputationLevels A vector of strings  naming the levels at which to input, defaulted to c("Haul", "Stratum", "Survey"). To prevent imputation at the Survey level, use c("Haul", "Stratum").
#' @param Seed An integer giving the seed to use for the random sampling used to obtain the imputed data.
#' @param RegressionDefinition Character: A string naming the method to use, one of \code{FunctionParameter} to define the Regression on the fly in this function (using \code{GroupingVariables}, \code{RegressionModel} and \code{RegressionTable}), or \code{FunctionInput} to import Regression process data from a previously run process using the function
#' 
#' @details 
#' For each (target) individual with missing value in \code{ImputeAtMissing}, identify all (source) individuals in the haul for which \code{ImputeAtMissing} is non-missing and for which the values in \code{ImputeByEqual} are identical to the target individual. Then sample one of these source individuals, and copy values of \code{ToImpute} to the target individual. Only values that are non-missing are copied from the sampled individual, and  only missing values in the target individual are replaced. If no source individuals are found in the haul, expand the search to the stratum, and finally to the survey. If no source individuals are found in the survey, leave the target individual unchanged. 
#' 
#' When \code{ToImpute} contains more variables than that given by \code{ImputeAtMissing} there is a risk that values remain missing even after successful imputation. E.g., if \code{ImputeAtMissing} is IndividualAge, and \code{ToImpute} includes IndividualRoundWeight, then the weight is only imputed when age is missing. Super-individuals with age but not weight will then still have missing weight. Variables that are naturally connected, such as IndividualRoundWeight and WeightMeasurement, or IndividualTotalLength and LengthResolution, should both be included in \code{ToImpute}.
#' 
#' @return
#' An object of StoX data type \code{\link{SuperIndividualsData}}. 
#' 
#' @seealso \code{\link{SuperIndividuals}} for distributing Abundance to the Individuals.
#' 
#' @export
#' 
ImputeSuperIndividuals_StoX3 <- function(
        SuperIndividualsData, 
        #ImputationMethod = "RandomSampling", 
        ImputationMethod = c("RandomSampling", "Regression"), 
        ImputeAtMissing = character(), 
        ImputeByEqual = character(), 
        ToImpute = character(), 
        ImputationLevels = c("Haul", "Stratum", "Survey"), 
        Seed = 1, 
        RegressionDefinition = c("FunctionParameter", "FunctionInput"), 
        GroupingVariables = character(), 
        RegressionModel = c("SimpleLinear", "Power"), 
        RegressionTable = data.table::data.table(), 
        Regression
        #RegroupIndividualTotalLength = FALSE, 
        #LengthInterval = numeric()
) {
    
    warning("The function ImputeSuperIndividuals_StoX3 is deprecated. It will be kept for backward compatibility until further notice.")
    
    ImputationMethod <- RstoxData::match_arg_informative(ImputationMethod)
    
    if(ImputationMethod == "RandomSampling") {
        # Check that the length resolution is constant: 
        if("IndividualTotalLength" %in% ImputeByEqual && !allEqual(SuperIndividualsData$LengthResolution)) {
            stop("All individuals must have identical LengthResolution in the current version.")
        }
        
        if(!length(ToImpute)) {
            warning("StoX: Empty ToImpute is deprecated. Please select the variables to impute explicitely.")
            ToImpute <- getIndividualNames(SuperIndividualsData, ImputeByEqual,  tables = "Individual")
        }
        if(length(ImputeAtMissing) != 1) {
            stop("ImputeAtMissing must have length 1.")
        }
        else if(!ImputeAtMissing %in% ToImpute) {
            stop("Please specify the variable given by ImputeAtMissing (", ImputeAtMissing, ") in ToImpute. ")
        }
        # For safety uniquify:
        ToImpute <- unique(ToImpute)
        
        
        # Impute the SuperIndividualsData:
        ImputeSuperIndividualsData <- ImputeDataByRandomSampling_StoX3(
            data = SuperIndividualsData, 
            imputeAtMissing = ImputeAtMissing, 
            imputeByEqual = ImputeByEqual, 
            seed = Seed, 
            columnNames = ToImpute, 
            levels = ImputationLevels#, 
            #lengthInterval  = if(RegroupIndividualTotalLength) LengthInterval else numeric()
        )
    }
    else if(ImputationMethod == "Regression") {
        
        RegressionDefinition <- RstoxData::match_arg_informative(RegressionDefinition)
        
        if(RegressionDefinition == "FunctionParameter") {
            Regression <- DefineRegression(
                DefinitionMethod = "Table",
                GroupingVariables = GroupingVariables, 
                RegressionModel = RegressionModel, 
                RegressionTable = RegressionTable
            )
        }
        
        # Initiate the output:
        ImputeSuperIndividualsData <- data.table::copy(SuperIndividualsData)
        
        for(rowInd in seq_len(NROW(Regression$RegressionTable))) {
            thisRegression <- Regression
            thisRegression$RegressionTable <- Regression$RegressionTable[rowInd, ]
            
            ImputeSuperIndividualsData <- ImputeDataByRegressionOneRow(
                data = ImputeSuperIndividualsData, 
                Regression = thisRegression
            )
        }
        
    }
    
    
    
    # Re-calculate the Biomass, where missing weigth is accepted if Abundane is 0:
    ImputeSuperIndividualsData[, Biomass := ifelse(Abundance %in% 0, 0, Abundance * IndividualRoundWeight)]
    
    # Format the output but keep all columns:
    #formatOutput(SuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE)
    # Order the columns, but keep all columns. Also add the names of the MergeStoxBioticData as secondaryColumnOrder to tidy up by moving the Haul column (used as by in the merging) back into its original position:
    areKeys <- endsWith(names(ImputeSuperIndividualsData), "Key")
    keys <- names(ImputeSuperIndividualsData)[areKeys]
    #formatOutput(IndividualsData, dataType = "IndividualsData", keep.all = TRUE, secondaryColumnOrder = keys)
    formatOutput(ImputeSuperIndividualsData, dataType = "SuperIndividualsData", keep.all = TRUE, allow.missing = TRUE, secondaryColumnOrder = unlist(attr(SuperIndividualsData, "stoxDataVariableNames")), secondaryRowOrder = keys)
    
    # Add the attribute 'variableNames':
    setattr(
        ImputeSuperIndividualsData, 
        "stoxDataVariableNames",
        attr(SuperIndividualsData, "stoxDataVariableNames")
    )
    
    return(ImputeSuperIndividualsData)
}






ImputeDataByRandomSampling_StoX3 <- function(
        data, 
        imputeAtMissing, 
        imputeByEqual, 
        seed = 1, 
        columnNames = NULL, 
        lengthInterval = numeric(), 
        levels = c(
            "Haul", 
            "Stratum", 
            "Survey"
        )
) {
    
    # Get the data and add the RowIndex for use when identifying which rows to impute from:
    dataCopy <- data.table::copy(data)
    #RowIndex <- seq_len(nrow(dataCopy))
    #dataCopy[, RowIndex := ..RowIndex]
    
    # If specified, regroup the length intervals:
    if(length(lengthInterval) == 1L) {
        dataCopy <- RegroupLengthData(
            dataCopy, 
            lengthInterval = lengthInterval
        )
    }
    
    # A test for duplicated Individual ID for the rows to be imputed. If there are duplicates, the new method of using Individual to identify rows to impute from may differ from the old method using Individual:
    duplicatedIndividual <- dataCopy[is.na(get(imputeAtMissing)) & !is.na(Individual), duplicated(Individual)]
    if(any(duplicatedIndividual)) {
        warning("StoX: There are duplicated entries in the Individual column which imples that individuals were used in multiple Strata. Due to a bug in the function ImputeSuperIndividuals() in StoX <= 3.6.2, which has been renamed to the deprecated ImputeSuperIndividuals_StoX3(), this may result in non-imputed rows. When you see this warning it is advised to use the new ImputeSuperIndividuals() instead.")
    }
    
    
    # Introduce an Individual index for use in the sorted sampling, as a factor sorted as "en_US_POSIX":
    dataCopy[, IndividualIndex := as.numeric(factor(Individual, levels = stringi::stri_sort(unique(Individual), locale = "en_US_POSIX")))]
    
    
    # Get a vector with the seed of each level:
    seedVector <- structure(as.list(getSeedVector(size = length(levels), seed = seed)), names = levels)
    
    dataCopy[, ReplaceIndividualIndex := NA_integer_]
    dataCopy[, ReplaceLevel := NA_character_]
    
    for(level in levels) {
        # Get the vector of columns to impute by:
        by <- c(level, imputeByEqual)
        
        # Get the table of seeds for each unique combination of the columns defined by 'by':
        uniqueKeys <- unique(dataCopy[, ..by])
        data.table::setorder(uniqueKeys)
        
        # This seed table will be dependent on whether there is a row of all NAs or not in the table of unique keys. As a consequence, a future review of how StoX includes NA rows will affect how seed works!!!
        seedTable <- data.table::data.table(
            uniqueKeys, 
            imputeSeed = getSeedVector(size = nrow(uniqueKeys), seed = seedVector[[level]])
        )
        
        # Add the seeds to the data (recycled). Use all.x = TRUE as there is no need to include any unwanted rows from the seedTable (althouhg this iss unlikely to happen):
        dataCopy <- merge(dataCopy, seedTable, by = by, all.x = TRUE, sort = FALSE)
        
        # Perform the imputation:
        getImputeRowIndicesOneLevel_StoX3(
            dataCopy, 
            imputeAtMissing = imputeAtMissing, 
            level = level, 
            by = by
        )
        
        # Remove the imputeSeed column:
        dataCopy[, imputeSeed := NULL]
    }
    # Perform the imputation:
    dataCopy <- replaceMissingData_StoX3(dataCopy, columnNames = columnNames)
    
    # Reset to original order:
    data.table::setorderv(dataCopy, "IndividualIndex")
    
    # Get the ReplaceIndividual:
    dataCopy[, ReplaceIndividual := Individual[match(ReplaceIndividualIndex, IndividualIndex)]]
    
    # Add a column ImputationMethod:
    dataCopy[!is.na(ReplaceIndividual), ImputationMethod := "RandomSampling"]
    
    # Delete the IndividualIndex and ReplaceIndividualIndex:
    removeColumnsByReference(
        data = dataCopy, 
        toRemove = c("IndividualIndex", "ReplaceIndividualIndex")
    )
    
    return(dataCopy)
}








# Function to get the imputation row indices of one level ("Haul", "Stratum", NULL). This function is applied using for loop over the levels:
getImputeRowIndicesOneLevel_StoX3 <- function(
        dataCopy, 
        imputeAtMissing, 
        level = "Haul", 
        by
) {
    
    # Get the row indices to replace data from by applying the function getImputeRowIndicesOneGroup by the level (one of Haul, Stratum, NULL) and the imputeByEqual input. 
    .SDcols <- c(imputeAtMissing, "ReplaceIndividualIndex", "ReplaceLevel", "IndividualIndex", "imputeSeed")
    
    dataCopy[, 
             c("ReplaceIndividualIndex", "ReplaceLevel") := getImputeRowIndicesOneGroup_StoX3(
                 .SD, 
                 imputeAtMissing = imputeAtMissing, 
                 level = level
             ), 
             .SDcols = .SDcols, 
             by = by]
}

# Function to get the imputation row indices of one table of one level ("Haul", "Stratum", NULL). This function is applied using data table with 'by':
getImputeRowIndicesOneGroup_StoX3 <- function(
        dataCopyOneGroup, 
        imputeAtMissing, 
        level
) {
    # Get the super individuals with missing data (and which have not been given ReplaceIndividualIndex):
    missingData <- dataCopyOneGroup[, is.na(get(imputeAtMissing)) & is.na(ReplaceIndividualIndex)]
    # This is assuming unique individuals for each Stratum, Layer, SpeciesCategory and length group:
    presentData <- dataCopyOneGroup[, !is.na(get(imputeAtMissing))]
    
    # Get the number of missing and present rows:
    NMissingRows <- sum(missingData)
    NPresentRows <- sum(presentData)
    
    # We choose (as it may be cleaner) to create the output row indices as a vector of NAs, instead of using data.table:
    ReplaceIndividualIndex <- dataCopyOneGroup$ReplaceIndividualIndex
    ReplaceLevel <- dataCopyOneGroup$ReplaceLevel
    
    if(NMissingRows > 0 && NPresentRows > 0) {
        # Using the new sampleSorted() which is identical to sort() for non-character such as these integers to be sampled:
        ReplaceIndividualIndex[missingData] <- sampleSorted(
            #dataCopyOneGroup[!missingData, IndividualIndex], 
            dataCopyOneGroup[presentData, IndividualIndex], 
            size = NMissingRows, 
            seed = dataCopyOneGroup$imputeSeed[1], 
            replace = TRUE, 
            #index.out = TRUE, 
            index.out = FALSE#, 
            #redraw.seed = TRUE
        )
        # Add also the replace level:
        ReplaceLevel[missingData] <- level
    }
    
    return(
        list(
            ReplaceIndividualIndex = ReplaceIndividualIndex, 
            ReplaceLevel = ReplaceLevel
        )
    )
}



replaceMissingData_StoX3 <- function(x, columnNames) {
    
    # Get the matrix indices of data to replace, which are those that are missing in the rows impute:
    #rowsToImpute <- x[!is.na(ReplaceRowIndex), RowIndex]
    #rowsToImputeFrom <- x[!is.na(ReplaceRowIndex), ReplaceRowIndex]
    # Changed on 2021-02-09 to match the IndividualIndex and ReplaceIndividualIndex:
    
    # Get the rows to impute, i.e., those with non-missing ReplaceIndividualIndex:
    rowsToImpute <- x[, which(!is.na(ReplaceIndividualIndex))]
    # Get the rows to replace from, by matching the ReplaceIndividualIndex with the IndividualIndex, and keep only those to impute:
    rowsToImputeFrom <- match(x$ReplaceIndividualIndex, x$IndividualIndex)
    rowsToImputeFrom <- rowsToImputeFrom[rowsToImpute]
    
    # Loop through the columns and replace missing data:
    namesx <- names(x)
    if(!length(columnNames)) {
        columnNames <- namesx
    }
    
    for(columnName in columnNames) {
        if(columnName %in% namesx) {
            # Locate the indices at which the data in the column given by columnName is NA in the rows to impute and not NA in the rows to impute from:
            atReplacement <- x[rowsToImpute, is.na(get(columnName))] & x[rowsToImputeFrom, !is.na(get(columnName))]
            if(any(atReplacement)) {
                atMissing <- rowsToImpute[atReplacement]
                atPresent <- rowsToImputeFrom[atReplacement]
                replacement <- x[atPresent, get(columnName)]
                x[atMissing, eval(columnName) := replacement]
            }
        }
    }
    
    return(x)
}

