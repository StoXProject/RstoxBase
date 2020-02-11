##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
Abundance <- function(DensityData, StratumArea) {
	
    # Merge the stratum area with the DensityData to an AbundanceData (remove the area at the end of the function):
    AbundanceData <- merge(DensityData, StratumArea, by ="Stratum")
    
    # Multiply the area and the density:
    AbundanceData[, Abundance := Area * Density]
    
    ## Remove the Area:
    #AbundanceData[, Area := NULL]
    
    # Keep only the releavnt columns:
    AbundanceData <- keepOnlyRelevantColumns(AbundanceData, "AbundanceData")
    
    return(AbundanceData)
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
Individuals <- function(StoxBioticData, DensityType = c("Acoustic", "SweptArea"), BioticAssignment, LengthDistributionData) {
    
    # Get the DefinitionMethod:
    DensityType <- match.arg(DensityType)
    
    # Merge StoxBtiotic:
    MergedStoxBioticData <- RstoxData::MergeStoxBiotic(StoxBioticData)
    
    # Get the resolution variables of AbundanceData:
    abundanceResolutionVariables <- getAllResolutionVariables("AbundanceData")
    
    # Get all hauls of each Stratum and Layer:
    if(DensityType == "Acoustic") {
        usedHauls <- BioticAssignment[, .(Haul = unique(Haul)), by = abundanceResolutionVariables]
    }
    else if(DensityType == "SweptArea") {
        usedHauls <- LengthDistributionData[, .(Haul = unique(Haul)), by = abundanceResolutionVariables]
    }
    else {
        stop("Invalid DensityType")
    }
    
    # Expand the vectors of hauls:
    usedHauls <- expandDT(usedHauls)
    
    # Merge individuals into the Stratum, Layer, Haul table:
    IndividualsData <- merge(usedHauls, MergedStoxBioticData, by = "Haul", allow.cartesian = TRUE)
    
    return(IndividualsData)
}




##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
SuperIndividuals <- function(IndividualsData, AbundanceData, AbundWeightMethod = c("Equal", "StationDensity"), LengthDistributionData) {
    # Get the AbundWeightMethod:
    AbundWeightMethod <- match.arg(AbundWeightMethod)
    
    # Get all hauls of each Stratum and Layer:
    if(AbundWeightMethod == "Equal") {
        # Append the abundance to the IndividualsData:
        SuperIndividualsData <- merge(IndividualsData, AbundanceData, by = intersect(names(IndividualsData), names(AbundanceData)))
        
        # Divide the Abundance by the number of apparences in the SuperIndividualsData, equivvalent to distributing the abundance equally amongst the individuals of each Stratum, Layer, SpeciesCategory, IndividualTotalLengthCentimeter and LengthResolutionCentimeter:
        by <- getAllDataTypeVariables(c("horizontalResolution", "verticalResolution", "categoryVariable", "groupingVariables"))
        
        # Add the haul density, only accepting the LengthDistributionType ends with "Normalized":
        if(! endsWith(LengthDistributionData$LengthDistributionType[1], "Normalized") ) {
            stop("LengthDistributionType must be normalized (\"Normalized\", \"SweepWidthCompensatedNormalized\" or \"SelectivityCompensatedNormalized\")")
        }
        
        SuperIndividualsData <- merge(SuperIndividualsData, LengthDistributionData[, c("Haul", "WeightedCount")], by = "Haul")
        
        # For each stratum, add the sum of the WeightedCount:
        #SuperIndividualsData[, SummedWeightedCount := sum(WeightedCount), by = c("Stratum", "Layer", "SpeciesCategory", ) ]
        
    }
    else if(AbundWeightMethod == "StationDensity") {
        stop("Not yet implemented")
    }
    else {
        stop("Invalid AbundWeightMethod")
    }
}


