
##################################################
##################################################
#' Calculate number density from NASC in length intervals
#' 
#' This function takes NASC and 
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
AcousticDensity <- function(NASCData, m = 20, a = -70, d = double()) {
    # Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
    
    
    
    
    # TS_l = m*log10(l) +a+d*log10(1+r_y/10)
    #Where:
    # TS_l = target strength (dB re 1 m2) of a fish with length l (cm)
    # m = constant in the TS vs length relationship for the given species (user input)
    # a = constant in the TS vs length relationship for the given species (user input)
    # d = constant in the TS vs length relationship related to depth dependent TS (user input)
    # l = length of the fish (cm). Typically, the center length of a length group (data input)
    # ry = average depth (m) of the NASC channel y (data input)
    
    # depth_info <- NASCData[,c('Channel','MinRange','MaxRange')]
    TS_l = c()
    
    
    #In linear domain
    sigma_bs_l = 10**(TS_l/10)
    # where
    #sigma_bs_l = acoustic backscattering cross-section (m2) for a fish of length l
    
    # NASC_l = NASC *(sigma_bs_l*p_l)/(sum(sigma_bs_l*p_l))
    # where:
    # NASC = the total NASC which is used to calculate densities by length
    # NASCl = the proportion of the total NASC which can be attributed to length group l. The sum of
    # NASCl for all length groups in the total length distribution is equal to NASC
    # pl = proportion of fish of length l in the input length distribution. Sum of all pl is 1.
    
    
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
StationSpecCatDensity <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


##################################################
##################################################
#' Swept-area density
#' 
#' This function calculates the area density of fish as number of individuals per square nautical mile.
#' 
#' @inheritParams RegroupLengthDistribution
#' @param SweptAreaMethod The method used to calculate the density, one of "LengthDependent" and "TotalCatch" (the latter is not )
#' @param SweepWidthMethod Parameter descrption.
#' @param SweepWidthSweepWidth Parameter descrption.
#' @param SweepWidthExpr Parameter descrption.
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
SweptAreaDensity <- function(
    LengthDistributionData, 
    SweptAreaMethod = c("LengthDependent", "TotalCatch"), 
    SweepWidthMethod = c("Constant", "CruiseDependent"), 
    SweepWidth = integer(), 
    SweepWidthExpr = character(), 
    CatchVariable = c("Weight", "Count")
) {
	
    # Get the DefinitionMethod:
    SweptAreaMethod <- match.arg(SweptAreaMethod)
    # Get the DefinitionMethod:
    SweepWidthMethod <- match.arg(SweepWidthMethod)
    
    # This method may require defining an additional data type in StoX:
    if(SweptAreaMethod == "TotalCatch") {
        stop("SweptAreaMethod = \"TotalCatch\" not yet implemented")
    }
    
    
    # Check the horizontal and vertical resolution of the input LengthDistributionData:
    if(any(!is.na(LengthDistributionData$Station)) && any(!is.na(LengthDistributionData$Haul))) {
        stop("The horizontal and vertical resolution of the input LengthDistributionData must be PSU (identified by only NAs in the Station column) and Layer (identified by only NAs in the Haul column)")
    }
    else if(any(!is.na(LengthDistributionData$Station))) {
        stop("The horizontal resolution of the input LengthDistributionData must be PSU (identified by only NAs in the Station column)")
    }
    else if(any(!is.na(LengthDistributionData$Haul))) {
        stop("The vertical resolution of the input LengthDistributionData must be Layer (identified by only NAs in the Haul column)")
    }
    
    
    # Get the length distribution type:
    LengthDistributionType <- utils::head(LengthDistributionData$LengthDistributionType, 1)
    
    # Issue an error if the LengthDistributionType is "PercentLengthDistribution" or "LengthDistribution":
    validLengthDistributionType <- c("NormalizedLengthDistribution", "SweepWidthCompensatedLengthDistribution")
    if(! LengthDistributionType %in% validLengthDistributionType) {
        stop("The LengthDistributionType of the input LengthDistributionData must be one of ", paste(validLengthDistributionType, collapse = " and "))
    }
    
    
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    DensityData = data.table::copy(LengthDistributionData)
    
    # Introduce the DensityWeight as a copy of the LengthDistributionWeight:
    DensityData[, DensityWeight := LengthDistributionWeight]
    
    
    # If LengthDistributionType is "NormalizedLengthDistribution", the effectivev towed distance has been accounted for, but there is still need to account for the sweep width:
    if(LengthDistributionType == "NormalizedLengthDistribution") {
        
        # Use a constant sweep width for all data by default:
        if(SweepWidthMethod == "Constant") {
            if(length(SweepWidth) == 0) {
                stop("SweepWidth must be given when SweepWidthMethod == \"Constant\"")
            }
            
            # Convert WeightedCount to density:
            DensityData[, Density := WeightedCount / SweepWidth]
        }
        else if(SweepWidthMethod == "CruiseDependent") {
            stop("SweepWidthMethod = \"CruiseDependent\" is not yet implemented")
            if(length(SweepWidthExpr) == 0) {
                stop("SweepWidthExpr must be given when SweepWidthMethod == \"CruiseDependent\"")
            }
        }
        
    }
    # If LengthDistributionType == "SweepWidthCompensatedLengthDistribution" the sweep width is already accounted for, in meters, so multiplying with the length of one nautical mile is required:
    else if(LengthDistributionType == "SweepWidthCompensatedLengthDistribution") {
        # Copy the WeightedCount to the Density:
        DensityData[, Density := WeightedCount]
    }
    
    # Convert to density per nautical mile atwarthship:
    lengthOfOneNauticalMile <- 1852
    DensityData[, Density := Density * lengthOfOneNauticalMile]
    
    # Extract the relevant variables only:
    relevantVariables <- getAllDataTypeVariables(dataType = "DensityData")
    DensityData <- DensityData[, ..relevantVariables]
    
    return(DensityData)
}



