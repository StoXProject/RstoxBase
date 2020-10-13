
##################################################
##################################################
#' NASC
#' 
#' NASC function converts the StoxAcousticData into NASCData format
#' 
#' @inheritParams ProcessData
#' @inheritParams ModelData
#' @param IncludePSU Logical: If TRUE the \code{AcousticLayer} is added to the NASC data
#' @param IncludeLayer input in a StoxAcoustcdata format
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
    StoxAcousticData = NULL
) {
    
    # Merge the StoxAcousticData:
    NASCData <- RstoxData::MergeStoxAcoustic(StoxAcousticData)
    
    # Check that the input StoxAcousticData has the same ChannelReferenceType throughout:
    dataTypeDefinition <- getDataTypeDefinition(dataType = "NASCData")
    ChannelReferenceType <- NASCData[[dataTypeDefinition$type]]
    if(!allEqual(ChannelReferenceType, na.rm = TRUE)) {
        stop("The StoxAcousticData must have only one ", dataTypeDefinition$type, " in the NASC function. This can be obtained in FilterStoxAcoustic.")
    }
    
    # Interpret the ChannelDepths:
    getChannelDepth(NASCData)
    
    # Add weights:
    NASCData[, NASCWeight := EffectiveLogDistance]
    
    # Set the order of the columns:
    formatOutput(NASCData, dataType = "NASCData", keep.all = FALSE)
    
    # Not needed here, since we only copy data: 
    #Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(NASCData)
    
    return(NASCData)
}


getChannelDepth <- function(NASC, force = FALSE) {
    if(force || !any(c("MinChannelDepth", "MaxChannelDepth") %in% names(NASC))) {
        NASC[, MinChannelDepth := getDepth(ChannelReferenceDepth, MinChannelRange, ChannelReferenceOrientation) ]
        NASC[, MaxChannelDepth := getDepth(ChannelReferenceDepth, MaxChannelRange, ChannelReferenceOrientation) ]
    }
}

getDepth <- function(depth0, range, angle) {
    depth0 + range * (-cos(angle * pi / 180))
}


##################################################
#' Sum NASC 
#' 
#' This function summes NASC data vertically.
#' 
#' @inheritParams MeanNASC
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{SumNASCData}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{NASC}} and \code{\link{MeanNASC}}.
#' 
#' @export
#' 
SumNASC <- function(
    NASCData, 
    LayerDefinition = c("FunctionParameter", "FunctionInput"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    AcousticLayer = NULL
) {
    
    sumRawResolutionData(
        data = NASCData, dataType = "NASCData", 
        LayerDefinition = LayerDefinition, 
        LayerProcessData = AcousticLayer, 
        LayerDefinitionMethod = LayerDefinitionMethod, 
        Resolution = Resolution, 
        LayerTable = LayerTable, 
        LayerType = "Acoustic"
    )
}


##################################################
#' Mean NASC 
#' 
#' This function summes NASC data vertically.
#' 
#' @inheritParams MeanNASC
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{SumNASCData}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{NASC}} and \code{\link{MeanNASC}}.
#' 
#' @export
#' 
MeanNASC <- function(
    NASCData, 
    SumNASCData, 
    # Parameters of the sum part:
    LayerDefinition = c("FunctionParameter", "FunctionInput", "PreDefined"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    AcousticLayer = NULL, 
    # Parameters of the mean part:
    PSUDefinition = c("FunctionParameter", "FunctionInput"), 
    PSUDefinitionMethod = c("EDSUToPSU"), 
    StratumPolygon = NULL, 
    AcousticPSU = NULL
) {
    
    # Skip the sum part if predefined:
    LayerDefinition <- match.arg(LayerDefinition)
    if(LayerDefinition != "PreDefined") {
        SumNASCData <- SumNASC(
            NASCData = NASCData, 
            LayerDefinition = LayerDefinition, 
            LayerDefinitionMethod = LayerDefinitionMethod, 
            Resolution = Resolution, 
            LayerTable = LayerTable, 
            AcousticLayer = AcousticLayer
        )
    }
    
    # Convert the PSUDefinitionMethod to "Identity" if "EDSUToPSU":
    PSUDefinitionMethod <- match.arg(PSUDefinitionMethod)
    if(grepl("EDSUToPSU", PSUDefinitionMethod, ignore.case = TRUE)) {
        PSUDefinitionMethod <- "Identity"
    }
    
    # Run the mean part:
    meanRawResolutionData(
        data = SumNASCData, dataType = "SumNASCData", 
        PSUDefinition = PSUDefinition, 
        PSUProcessData = AcousticPSU, 
        PSUDefinitionMethod = PSUDefinitionMethod, 
        StratumPolygon = StratumPolygon, 
        PSUType = "Acoustic"
    )
}
