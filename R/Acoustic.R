
##################################################
##################################################
#' NASC
#' 
#' NASC function converts the StoxAcousticData into
#' a NASCData format
#' 
#' @inheritParams DefineBioticAssignment
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
NASC <- function(
    StoxAcousticData = NULL, 
    IncludePSU = FALSE, 
    IncludeLayer = FALSE, 
    AcousticLayer = NULL, 
    AcousticPSU = NULL
    ) {
    
    # Merge the StoxAcousticData:
    NASC <- RstoxData::mergeDataTables(StoxAcousticData)$NASC
    # Check that the input StoxAcousticData has the same ChannelReferenceType:
    dataTypeDefinition <- getDataTypeDefinition(dataType = "NASCData")
    ChannelReferenceType <- NASC[[dataTypeDefinition$type]]
    if(!all(ChannelReferenceType == ChannelReferenceType[1])) {
        stop("The StoxAcousticData must have only one ", dataTypeDefinition$type, " in the NASC function. This can be obtained in FilterStoxAcoustic.")
    }
    
    # Insert the Stratum and PSU column by the AcousticPSU input, and otherwise by NAs:
    NASC <- addPSUProcessData(NASC, dataType = "NASCData", PSUProcessData = AcousticPSU)
    
    ### # Add also the number of Stations in each PSU, and the number of PSUs in each Stratum:
    ### PSUSize <- AcousticPSU$EDSU_PSU[, .(PSUSize = length(EDSU)), by = "PSU"]
    ### StratumSize <- AcousticPSU$Stratum_PSU[, .(StratumSize = length(PSU)), by = "Stratum"]
    ### # Merge the PSUSize and StratumSize into the LengthDistributionData:
    ### NASC <- merge(NASC, PSUSize, by = "PSU")
    ### NASC <- merge(NASC, StratumSize, by = "Stratum")
    
    # Insert the Layer column by the AcousticLayer input, and otherwise by NAs:
    NASC <- addLayerProcessData(NASC, dataType = "NASCData", layerProcessData = AcousticLayer)
    ## Rename "MinRange" and "MaxRange" to "MinChannelRange" and "MaxChannelRange":
    #setnames(NASC, old=c("MinRange", "MaxRange"), new=c("MinChannelRange", "MaxChannelRange"))
    
    # Add weights:
    NASC[, NASCWeight := EffectiveLogDistance]
    
    # Add the sum of the weigths over the stations of each PSU:
    NASC[, SummedWeights := sum(NASCWeight), by = "PSU"]
    
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
#' @param NASCData The \code{\link{NASC}} data.
#' @param LayerDefinition A string naming the method to use for defining the Layers, one of "PreDefined", if the Layer column is already populated in the \code{NASCData}, or "FunctionInput" to provide the Layers in the input \code{AcousticLayer}
#' @param AcousticLayer \code{\link{AcousticLayer}} data.
#' @param TargetResolution The vertical resolution of the output.
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
SumNASC <- function(NASCData, LayerDefinition = c("PreDefined", "FunctionInput"), AcousticLayer = NULL, TargetResolution = "Layer") {
    sumData(NASCData, dataType = "NASCData", LayerDefinition = LayerDefinition, LayerProcessData = AcousticLayer, targetResolution = TargetResolution)
}




##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @inheritParams SumNASC
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
#' @param NASCData The \code{\link{NASC}} data.
#' @param PSUDefinition A string naming the method to use for defining the PSUs, one of "PreDefined", if the PSU column is already populated in the \code{NASCData}, or "FunctionInput" to provide the PSUs in the input \code{AcousticPSU}
#' @param AcousticPSU \code{\link{AcousticPSU}} data.
#' @param TargetResolution The vertical resolution of the output.
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
MeanNASC <- function(NASCData, PSUDefinition = c("PreDefined", "FunctionInput"), AcousticPSU = NULL, TargetResolution = "PSU") {
    meanData(NASCData, dataType = "NASCData", PSUDefinition = PSUDefinition, PSUProcessData = AcousticPSU, targetResolution = TargetResolution)
}








