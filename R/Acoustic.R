
##################################################
##################################################
#' NASC
#' 
#' NASC function converts the StoxAcousticData into NASCData format
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
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
    
    # Format the output:
    formatOutput(NASCData, dataType = "NASCData", keep.all = FALSE)
    
    # Not needed here, since we only copy data: 
    #Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(NASCData)
    
    return(NASCData)
}


getChannelDepth <- function(NASC, force = FALSE) {
    if(force || !any(c("MinChannelDepth", "MaxChannelDepth") %in% names(NASC))) {
        NASC[, MinChannelDepth := getDepth(ChannelReferenceDepth, MinChannelRange, ChannelReferenceTilt) ]
        NASC[, MaxChannelDepth := getDepth(ChannelReferenceDepth, MaxChannelRange, ChannelReferenceTilt) ]
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
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param LayerDefinition The method to use for defining the Layers, one of \code{FunctionParameter} to define the Layers on the fly in this function, or \code{FunctionInput} to import Layer process data from a previously run process by \code{AcousticLayer}.
#' @param LayerDefinitionMethod See \code{\link{DefineAcousticLayer}}
#' @inheritParams DefineAcousticLayer
#' 
#' @return
#' A \code{\link{SumNASCData}} object.
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
    
    SumNASCData <- sumRawResolutionData(
        data = NASCData, dataType = "NASCData", 
        LayerDefinition = LayerDefinition, 
        LayerProcessData = AcousticLayer, 
        LayerDefinitionMethod = LayerDefinitionMethod, 
        Resolution = Resolution, 
        LayerTable = LayerTable, 
        LayerType = "Acoustic"
    )
    
    # Format the output:
    formatOutput(SumNASCData, dataType = "SumNASCData", keep.all = FALSE)
    
    # Not needed here, since we only aggregate data: 
    #Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(SumNASCData)
    
    return(SumNASCData)
}


##################################################
#' Mean NASC 
#' 
#' This function summes NASC data vertically.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param LayerDefinition The method to use for defining the Layers, one of \code{FunctionParameter} to define the Layers on the fly in this function, \code{FunctionInput} to import Layer process data from a previously run process by the input \code{AcousticLayer}, or \code{PreDefined} whihc requires \code{SumNASCData} as input.
#' @param LayerDefinitionMethod See \code{\link{DefineAcousticLayer}}
#' @inheritParams DefineAcousticLayer
#' @param SurveyDefinition The method to use for defining the Survey, one of \code{FunctionParameter} to define the Survey on the fly in this function, or \code{FunctionInput} to import Survey process data from a previously run process by the input \code{Survey}.
#' @param SurveyDefinitionMethod See \code{\link{DefineSurvey}}
#' @inheritParams DefineSurvey
#' @param PSUDefinition The method to use for defining the PSUs, one of \code{FunctionParameter} to define the PSUs on the fly in this function, or \code{FunctionInput} to import PSU process data from a previously run process by \code{AcousticPSU}.
#' @param PSUDefinitionMethod See \code{\link{DefineAcousticPSU}}
#' @inheritParams DefineAcousticPSU
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @seealso \code{\link{NASC}} and \code{\link{MeanNASC}}.
#' 
#' @export
#' 
MeanNASC <- function(
    NASCData, 
    SumNASCData, 
    # Parameters of the sum part:
    # Layer: 
    LayerDefinition = c("FunctionParameter", "FunctionInput", "PreDefined"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    AcousticLayer = NULL, 
    # Survey: 
    SurveyDefinition = c("FunctionParameter", "FunctionInput"), 
    SurveyDefinitionMethod = c("AllStrata", "SurveyTable"), 
    SurveyTable = data.table::data.table(), 
    Survey = NULL, 
    # Parameters of the mean part:
    # PSU: 
    PSUDefinition = c("FunctionParameter", "FunctionInput"), 
    PSUDefinitionMethod = c("EDSUToPSU"), 
    StratumPolygon = NULL, 
    AcousticPSU = NULL
) {
    
    # Get the layer definition:
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
    
    SurveyDefinition <- match.arg(SurveyDefinition)
    PSUDefinition <- match.arg(PSUDefinition)
    # Convert the PSUDefinitionMethod to "Identity" if "EDSUToPSU":
    if(PSUDefinition == "FunctionParameter") {
        PSUDefinitionMethod <- match.arg(PSUDefinitionMethod)
        if(grepl("EDSUToPSU", PSUDefinitionMethod, ignore.case = TRUE)) {
            PSUDefinitionMethod <- "Identity"
        }
    }
    
    # Run the mean part:
    MeanNASCData <- meanRawResolutionData(
        data = SumNASCData, dataType = "SumNASCData", 
        # PSU:
        PSUDefinition = PSUDefinition, 
        PSUProcessData = AcousticPSU, 
        PSUDefinitionMethod = PSUDefinitionMethod, 
        # Survey:
        SurveyDefinition = SurveyDefinition, 
        SurveyProcessData = Survey, 
        SurveyDefinitionMethod = SurveyDefinitionMethod, 
        SurveyTable = SurveyTable, 
        # General:
        StratumPolygon = StratumPolygon, 
        PSUType = "Acoustic"
    )
    
    # Format the output:
    formatOutput(MeanNASCData, dataType = "MeanNASCData", keep.all = FALSE)
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    RstoxData::setRstoxPrecisionLevel(MeanNASCData)
    
    return(MeanNASCData)
}

#' 
#' @export
#'
SplitMeanNASC <- function(
    MeanNASCData, 
    AssignmentLengthDistributionData, 
    AcousticTargetStrength, 
    SpeciesLink, 
    AcousticCategoryLink
    ) {
    
    # Require full resolution vertically and horizontally:
    numberofUniquePSU_Layer <- nrow(unique(MeanNASCData$Resolution[!is.na(PSU), c("PSU", "Layer")]))
    numberofUniqueEDSU_Channel <- nrow(unique(MeanNASCData$Resolution[!is.na(PSU), c("EDSU", "Channel")]))
    
    if(numberofUniquePSU_Layer != numberofUniqueEDSU_Channel) {
        stop("The MeanNASCData must have maximum horizontal and vertical resolution for use in SplitMeanNASC")
    }
    
    # Find the mix categories in the MeanNASCData.
    allAcousticCategory <- unique(MeanNASCData$Data$AcousticCategory)
    presentMixAcousticCategory <- AcousticCategoryLink$AcousticCategory %in% allAcousticCategory
    if(any(!presentMixAcousticCategory)) {
        warning("The following mix AcousticCategory are not present in the MeanNASCData.")
    }
    # Keep only rows with mix categories present in the data:
    AcousticCategoryLink <- subset(AcousticCategoryLink, AcousticCategory %in% allAcousticCategory)
    # Add all species that will not be split to the AcousticCategoryLink:
    AcousticCategoryNotToBeSplit <- setdiff(
        unique(MeanNASCData$Data$AcousticCategory), 
        unique(AcousticCategoryLink$AcousticCategory)
    )
    AcousticCategoryLink <- rbind(
        AcousticCategoryLink, 
        data.table::data.table(
            MixAcousticCategory = AcousticCategoryNotToBeSplit, 
            SplitAcousticCategory = AcousticCategoryNotToBeSplit
        )
    )
    
    # Copy the NASC from the MixAcousticCategory to the SplitAcousticCategory, and remove the MixAcousticCategory:
    MeanNASCDataToSplit <- subset(MeanNASCData$Data, AcousticCategory %in% AcousticCategoryLink$AcousticCategory)
    MeanNASCDataNotToSplit <- subset(MeanNASCData$Data, ! AcousticCategory %in% AcousticCategoryLink$AcousticCategory)
    
    # Add the SplitAcousticCategory:
    MeanNASCDataToSplit <- merge(MeanNASCDataToSplit, AcousticCategoryLink, all = TRUE, allow.cartesian = TRUE, sort = FALSE)
    # Replace the AcousticCategory column by the SplitAcousticCategory column:
    MeanNASCDataToSplit[, AcousticCategory := SplitAcousticCategory][, SplitAcousticCategory := NULL]
    
    # Define the resolution on which to distribute the NASC:
    resolution <- getDataTypeDefinition(dataType = "DensityData", elements = c("horizontalResolution", "verticalResolution"), unlist = TRUE)
    # Split the NASC by the AssignmentLengthDistributionData:
    MeanNASCDataSplit <- DistributeNASC(
        NASCData = MeanNASCDataToSplit, 
        AssignmentLengthDistributionData = AssignmentLengthDistributionData, 
        AcousticTargetStrength = AcousticTargetStrength, 
        SpeciesLink = SpeciesLink, 
        sumBy = resolution
    )
    
    # Then add to the MeanNASCDataNotToSplit and sum for each species of each Stratum, PSU and Layer:
    columnsToKeep <- names(MeanNASCDataNotToSplit)
    MeanNASCData$Data <- rbind(
        MeanNASCDataNotToSplit, 
        MeanNASCDataSplit[, ..columnsToKeep]
    )
    # Sum the NASC:
    sumBy <- c(resolution, "AcousticCategory")
    MeanNASCData$Data <- MeanNASCData$Data[, NASC := sum(NASC), by = sumBy]
    # Uniquify:
    MeanNASCData$Data <- unique(MeanNASCData$Data, by = sumBy)
    
    # Keep only the AcousticCategory specified in SpeciesLink$AcousticCategory
    MeanNASCData$Data <- subset(MeanNASCData$Data, AcousticCategory %in% SpeciesLink$AcousticCategory)
    
    ### # Convert from MeanNASCData to NASCData, assuming full resolution:
    ### NASCData <-  merge(MeanNASCData,  MeanNASCData$Resolution, by = c("PSU", "Layer"))
    
    
    return(MeanNASCData)
}


