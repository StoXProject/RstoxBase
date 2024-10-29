##################################################
##################################################
#' Table of total catch weight and number per Haul and SpeciesCategory
#' 
#' This function sums the catch fraction weight and number per Haul and SpeciesCategory.
#' 
#' @inheritParams ModelData
#' 
PreySpeciesCategoryCatch <- function(StoxBioticData) {
    
    # Export this function when prey is official
    
    # Store the sample names to remove them as columns in the end of the function:
    preySampleNames <- names(StoxBioticData$PreySample)
    preySampleNamesToRemove <- c(preySampleNames[!endsWith(preySampleNames, "Key") & preySampleNames != "PreyCatchFractionWeightResolution"], "PreySampleKey")
    
    # Merge Station, ..., PreySample table:
    PreySpeciesCategoryCatchData <- RstoxData::MergeStoxBiotic(StoxBioticData, TargetTable = "PreySample")
    
    # Sum the CatchFractionWeight for each Individual and PreySpeciesCategory:
    dataVariables <- getDataTypeDefinition(dataType = "PreySpeciesCategoryCatchData", elements = "data", unlist = TRUE)
    CatchFractionVariables <- sub("TotalPreyCatch", "PreyCatchFraction", dataVariables)
    sumBy <-  getDataTypeDefinition(
        dataType = "PreySpeciesCategoryCatchData", 
        elements = c("verticalResolution", "categoryVariable"), 
        unlist = TRUE
    )
    # Sum for weight and number separately:
    for(ind in seq_along(dataVariables)) {
        # Make sure that these are numeric, as data.table somehow performs an integer operation when normalizing below (which does not happen in the console, but for some reason here):
        PreySpeciesCategoryCatchData[, eval(dataVariables[ind]) := sum(get(CatchFractionVariables[ind])), by = sumBy]
        #SpeciesCategoryCatchData[, eval(dataVariables[ind]) := as.numeric(sum(get(CatchFractionVariables[ind]))), by = sumBy]
    }
    # Uniquify, as the above sum is by reference, and thus keeps all rows:
    PreySpeciesCategoryCatchData <- unique(PreySpeciesCategoryCatchData, by = sumBy)
    
    
    # Add PreySpeciesCategoryCatchWeightingFactor:
    PreySpeciesCategoryCatchData[, PreySpeciesCategoryCatchWeightingFactor := 1]
    
    # Discard the variables in the Sample table, since we have summed over samples:
    PreySpeciesCategoryCatchData[, (preySampleNamesToRemove) := NULL]

    # Format the output:
    formatOutput(PreySpeciesCategoryCatchData, dataType = "PreySpeciesCategoryCatchData", keep.all = TRUE, removeStoXKeys = TRUE)
    
    return (PreySpeciesCategoryCatchData)
}
