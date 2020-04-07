
##################################################
##################################################
#' NASC
#' 
#' NASC function converts the StoxAcousticData into
#' a NASCData format
#' 
#' @param StoxAcousticData input in a StoxAcoustcdata format
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{NASCData}} object.
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
NASC <- function(StoxAcousticData = NULL, AcousticLayer = NULL, AcousticPSU = NULL) {
    
    # Merge the StoxAcousticData:
    NASC <- mergeDataTables(StoxAcousticData)$NASC
    # Check that the input StoxAcousticData has the same ChannelReferenceType:
    dataTypeDefinition <- getDataTypeDefinition(dataType = "NASCData")
    ChannelReferenceType <- NASC[[dataTypeDefinition$type]]
    if(!all(ChannelReferenceType == ChannelReferenceType[1])) {
        stop("The StoxAcousticData must have only one ", dataTypeDefinition$type, " in the NASC function. This can be obtained in FilterStoxAcoustic.")
    }
    
    # Add weights:
    NASC[, NASCWeight := EffectiveLogDistance]
    
    # Insert the Stratum and PSU column by the AcousticPSU input, and otherwise by NAs:
    NASC <- addPSUDefinition(NASC, PSUDefinition = AcousticPSU)
    
    # Insert the Layer column by the AcousticLayer input, and otherwise by NAs:
    NASC <- addLayerDefinition(NASC, dataType = "NASCData", layerDefinition = AcousticLayer)
    ## Rename "MinRange" and "MaxRange" to "MinChannelRange" and "MaxChannelRange":
    #setnames(NASC, old=c("MinRange", "MaxRange"), new=c("MinChannelRange", "MaxChannelRange"))
    
    # Extract the relevant variables:
    validVariables <- getAllDataTypeVariables(dataType = "NASCData")
    NASC <- NASC[, ..validVariables]
    
    return(NASC)
}


##################################################
#' Sum NASC 
#' 
#' This function sums NASC data vertically into layer resolution
#' 
#' @inheritParams SumLengthDistribution
#' @param NASCData The NASC data.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{NASCData}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{NASC}} and \code{\link{SumNASC}}.
#' 
#' @export
#' @import data.table
#' 
SumNASC <- function(NASCData, TargetResolution = "Layer") {
    sumData(NASCData, dataType = "NASCData", targetResolution = TargetResolution)
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
CombineNASC <- function(NASCData) {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}



##################################################
#' (Weighted) Average NASC 
#' 
#' This function averages NASC data horizontally, weighted by the log distance.
#' 
#' @inheritParams SumNASC
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{NASCData}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{NASC}} and \code{\link{MeanNASC}}.
#' 
#' @export
#' @import data.table
#' 
MeanNASC <- function(NASCData, TargetResolution = "PSU") {
    meanData(NASCData, dataType = "NASCData", targetResolution = TargetResolution)
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
NASCToAcousticData <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
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
SplitNASC <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
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
SplitNASCAssignment <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


