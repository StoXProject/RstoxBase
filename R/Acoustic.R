
##################################################
##################################################
#' NASC
#' 
#' NASC function converts the StoxAcousticData into NASCData format
#' 
#' @inheritParams ModelData
#' 
#' @return A \code{\link{NASCData}} object.
#' 
#' @export
NASC <- function(
    StoxAcousticData
) {
    
    # Merge the StoxAcousticData:
    NASCData <- RstoxData::MergeStoxAcoustic(StoxAcousticData)
    
    # Merging StoxAcousticData may result in rows with present AcousticCategory but NA for all variables to the right of this variable. This happens if there are ChannelReferenceType that are not present while AcousticCategory is present on an EDSU. This can occur e.g. if an NMDEchosounder file contains ChannelReferenceType = "B" and "P", but "P" is not present on all EDSUs where "B" is present. This should not be possible in LSSS, and should be considered to be an error. However, the opposite case is valid, and should lead to missing AcousticCategory. The cause of these issues is that the table hierarchy of NMDEchosounder and StoxAcoustic differ. Specifically, in NMDEchosounder the level ch_type precedes sa_by_acocat, whereas in StoxAcoustic the level AcousticCategory precedes ChannelReferenceType. 
    presentAcousticCategory <- NASCData[, !is.na(AcousticCategory)]
    colsToCheckForNA <- names(NASCData)[seq(which(names(NASCData) == "AcousticCategory") + 1, length(NASCData))]
    missingNASCAndChannelDepth <- rowSums(NASCData[, lapply(.SD, is.na), .SDcols = colsToCheckForNA]) == length(colsToCheckForNA)
    presentAcousticCategoryButMissingNASCAndChannelDepth <- which(presentAcousticCategory & missingNASCAndChannelDepth)
    if(length(presentAcousticCategoryButMissingNASCAndChannelDepth)) {
        NASCData[presentAcousticCategoryButMissingNASCAndChannelDepth, AcousticCategory := NA_character_]
    }
    
    # Check that the input StoxAcousticData has the same ChannelReferenceType throughout:
    type <- getDataTypeDefinition(dataType = "NASCData", elements = "type", unlist = TRUE)
    ChannelReferenceType <- NASCData[[type]]
    if(!allEqual(ChannelReferenceType, na.rm = TRUE) && NROW(NASCData)) {
        stop("The StoxAcousticData must have only one ", type, " in the NASC function. This can be obtained in the function FilterStoxAcoustic (in RstoxData).")
    }
    # Added a warning for multiple Beam with the same frequency, which may lead to over-estimation:
    numberOfBeamsPerFrequency <- NASCData[, .(numberOfBeams  = length(unique(Beam))), by = "Frequency"]
    numberOfBeamsPerFrequency <- subset(numberOfBeamsPerFrequency, numberOfBeams > 1)
    if(NROW(numberOfBeamsPerFrequency)) {
        warning("StoX: SEVERE WARNING: There are multiple Beams for the same frequency, which can lead to over-estimation, as AcousticDensity considers Frequency only and not Beam. If NMDEchosounder data were used to produce the NASCData, this can be an indication that different 'transceiver' ID is used for the same frequency, which can occur if two platforms have different frequency as its lowest frequency (e.g. that one of the platforms lack 18 kHz). Please translate the Beam to a common value to avoid this:\n", "Frequency: ", paste(numberOfBeamsPerFrequency$Frequency, collapse = ","), ". Number of Beams: ", paste(numberOfBeamsPerFrequency$numberOfBeams, collapse = ","))
    }
    
    # Interpret the ChannelDepths:
    getChannelDepth(NASCData)
    
    # Add weights:
    NASCData[, NASCWeight := EffectiveLogDistance]
    
    # Format the output:
    formatOutput(NASCData, dataType = "NASCData", keep.all = FALSE)
    
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


# We considered a function ExpandNASC() which should fill in all available AcousticCategory for each EDSU, thus avoiding the problem that a row with NA in AcousticCategory and other variables is generated for each EDSU which has not been scrutinized. However, an issue was how to treat channels. Should all possible channels be generated? And what if the channel definition changes in the cruise, will we then need to generate overlapping channels with 0 NASC? Also, could there be potential effects of removing the NA AcousticCategory, e.g. with respsect to order, sampling etc?
#
# Knowing that ICESAcoustic actually requires at least one AcousticCategory per EDSU, the problem of reports cluttered with AcousticCategory = NA can be avoided by moving to that input format instead. 
#
# A better suggestion was to add a FilterReport function which can actually filter away unwanted NAs. Then that choice is documented in the project.json file in a clear way.



##################################################
#' Sum NASC 
#' 
#' This function summes \code{\link{NASCData}} vertically.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param LayerDefinition The method to use for defining the Layers, one of \code{FunctionParameter} to define the Layers on the fly in this function, or \code{FunctionInput} to import Layer process data generated using the function \code{DefineAcousticLayer}.
#' @param LayerDefinitionMethod See the argument \code{DefinitionMethod} in \code{\link{DefineBioticLayer}}.
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
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
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
    
    return(SumNASCData)
}


##################################################
#' Mean NASC 
#' 
#' This function averages \code{\link{SumNASCData}} horizontally to the acoustic PSU resolution, weighted by the log distance. Optionally, \code{\link{NASCData}} can be used as input, in which case \code{\link{SumNASC}} is run first. 
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param LayerDefinition The method to use for defining the Layers, one of \code{FunctionParameter} to define the Layers on the fly in this function, \code{FunctionInput} to import Layer process data generated using the function \code{DefineAcousticLayer}, or \code{PreDefined} whihc requires \code{SumNASCData} as input.
#' @param LayerDefinitionMethod See the argument \code{DefinitionMethod} in \code{\link{DefineBioticLayer}}.
#' @inheritParams DefineAcousticLayer
#' @param SurveyDefinition The method to use for defining the Survey, one of \code{FunctionParameter} to define the Survey on the fly in this function, or \code{FunctionInput} to import Survey process data from a previously run process by the input \code{Survey}.
#' @param SurveyDefinitionMethod See \code{\link{DefineSurvey}}
#' @inheritParams DefineSurvey
#' @param PSUDefinition The method to use for defining the PSUs, one of \code{FunctionParameter} to define the PSUs on the fly in this function, or \code{FunctionInput} to import PSU process data from a previously run process by \code{AcousticPSU}.
#' @param PSUDefinitionMethod Character: A string naming the automatic PSU definition method to use. Currently only the option "EDSUToPSU" is supported, which sets each EDSU as a PSU. See \code{\link{DefineAcousticPSU}} for details.
#' @inheritParams DefineAcousticPSU
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
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    AcousticLayer = NULL, 
    # Survey: 
    SurveyDefinition = c("FunctionParameter", "FunctionInput"), 
    SurveyDefinitionMethod = c("AllStrata", "Table"), 
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
    LayerDefinition <- RstoxData::match_arg_informative(LayerDefinition)
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
    
    SurveyDefinition <- RstoxData::match_arg_informative(SurveyDefinition)
    PSUDefinition <- RstoxData::match_arg_informative(PSUDefinition)
    # Convert the PSUDefinitionMethod to "Identity" if "EDSUToPSU":
    if(PSUDefinition == "FunctionParameter") {
        PSUDefinitionMethod <- RstoxData::match_arg_informative(PSUDefinitionMethod)
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
    
    return(MeanNASCData)
}



##################################################
#' Split MeanNASCData to NASCData (deprecated)
#' 
#' This function splits \code{\link{NASCData}} of specific acoustic categories into other categories based on the acoustic target strength of these categories and the length distribution of corresponding species categories.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams AcousticDensity
#' @param AcousticCategoryLink A table linking the acoustic categories to split and those to split into.
#' 
#' @seealso This functions uses similar methods as \code{\link{AcousticDensity}}. Convert back to \code{\link[RstoxData]{StoxAcousticData}} with \code{\link{NASCToStoxAcoustic}}.
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
    
    warning("StoX: The function SplitMeanNASC is deprecated, and will be removevd in StoX 3.7.0. Use SplitNASC instead.")
    
    # Require full resolution vertically and horizontally:
    numberOfUniquePSU_Layer <- nrow(unique(MeanNASCData$Resolution[!is.na(PSU), c("PSU", "Layer")]))
    numberOfUniqueEDSU_Channel <- nrow(unique(MeanNASCData$Resolution[!is.na(PSU), c("EDSU", "Channel")]))
    
    if(numberOfUniquePSU_Layer < numberOfUniqueEDSU_Channel) {
        stop("The MeanNASCData must have maximum horizontal and vertical resolution for use in SplitMeanNASC")
    }
    
    # Find the mix categories in the MeanNASCData.
    allAcousticCategory <- unique(MeanNASCData$Data$AcousticCategory)
    presentMixAcousticCategory <- AcousticCategoryLink$AcousticCategory %in% allAcousticCategory
    if(any(!presentMixAcousticCategory)) {
        warning("StoX: The following mix AcousticCategory are not present in the MeanNASCData.")
    }
    # Keep only rows with mix categories present in the data:
    AcousticCategoryLink <- subset(AcousticCategoryLink, AcousticCategory %in% allAcousticCategory)
    # Add all species that will not be split to the AcousticCategoryLink:
    AcousticCategoryNotToBeSplit <- setdiff(
        unique(MeanNASCData$Data$AcousticCategory), 
        unique(AcousticCategoryLink$AcousticCategory)
    )
    if(length(AcousticCategoryNotToBeSplit)) {
        AcousticCategoryLink <- rbind(
            AcousticCategoryLink, 
            data.table::data.table(
                AcousticCategory = AcousticCategoryNotToBeSplit, 
                SplitAcousticCategory = AcousticCategoryNotToBeSplit
            )
        )
    }
    
    # Copy the NASC from the MixAcousticCategory to the SplitAcousticCategory, and remove the MixAcousticCategory:
    MeanNASCDataToSplit <- subset(MeanNASCData$Data, AcousticCategory %in% AcousticCategoryLink$AcousticCategory)
    MeanNASCDataNotToSplit <- subset(MeanNASCData$Data, ! AcousticCategory %in% AcousticCategoryLink$AcousticCategory)
    
    # Add the SplitAcousticCategory:
    MeanNASCDataToSplit <- merge(MeanNASCDataToSplit, AcousticCategoryLink, all = TRUE, allow.cartesian = TRUE, sort = FALSE)
    # Replace the AcousticCategory column by the SplitAcousticCategory column:
    MeanNASCDataToSplit[, AcousticCategory := SplitAcousticCategory][, SplitAcousticCategory := NULL]
    
    # Define the resolution on which to distribute the NASC:
    resolution <- getDataTypeDefinition(dataType = "DensityData", subTable = "Data", elements = c("horizontalResolution", "verticalResolution"), unlist = TRUE)
    # Split the NASC by the AssignmentLengthDistributionData:
    MeanNASCDataSplit <- DistributeNASC(
        MeanNASCData = MeanNASCDataToSplit, 
        AssignmentLengthDistributionData = AssignmentLengthDistributionData, 
        AcousticTargetStrength = AcousticTargetStrength, 
        SpeciesLink = SpeciesLink, 
        sumBy = resolution, 
        distributionType = "SplitNASC"
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
    
    ### # Keep only the AcousticCategory specified in SpeciesLink$AcousticCategory
    ### MeanNASCData$Data <- subset(MeanNASCData$Data, AcousticCategory %in% SpeciesLink$AcousticCategory)
    # Convert from MeanNASCData to NASCData, assuming full resolution:
    if(sum(is.na(MeanNASCData$Data$PSU))) {
        stop("All EDSUs must be inside a stratum.")
    }
    NASCData <-  merge(MeanNASCData$Data ,  MeanNASCData$Resolution, by = c("Stratum", "PSU", "Layer"))
    
    
    # Rename MinChannelDepth to MinChannelRange:
    data.table::setnames(NASCData, c("MinLayerDepth", "MaxLayerDepth", "MeanNASCWeight"), c("MinChannelDepth", "MaxChannelDepth", "NASCWeight"))
    
    # Format the output:
    formatOutput(NASCData, dataType = "NASCData", keep.all = FALSE)
    
    
    return(NASCData)
}





##################################################
#' Split NASCData
#' 
#' This function splits NASCData of specific acoustic categories into other categories based on the acoustic target strength of these categories and the length distribution of corresponding species categories.
#' 
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams AcousticDensity
#' @param AcousticCategoryLink A table linking the acoustic categories to split and those to split into.
#' 
#' @seealso This functions uses similar methods as \code{\link{AcousticDensity}}. Convert back to \code{\link[RstoxData]{StoxAcousticData}} with \code{\link{NASCToStoxAcoustic}}.
#' 
#' @export
#'
SplitNASC <- function(
    NASCData, 
    AcousticPSU, 
    AssignmentLengthDistributionData, 
    AcousticTargetStrength, 
    AcousticCategoryLink, 
    SpeciesLink
) {
    
    # Add PSUs. This makes a copy, also:
    MeanNASCData <- merge(NASCData, merge(AcousticPSU$Stratum_PSU, AcousticPSU$EDSU_PSU, all = TRUE), by = "EDSU", )
    
    # Only split rows with non-missing PSU and positive NASC (this also discards splitting of NA NASC):
    # # First consider only the rows with non-missing PSU, which are those that will be split:
    rowToBeSplit <- !is.na(MeanNASCData$PSU) & (MeanNASCData$NASC > 0) %in% TRUE
    
    # Then find the mix categories in the MeanNASCData.
    allAcousticCategory <- unique(MeanNASCData$AcousticCategory[rowToBeSplit])
    missingMixAcousticCategory <- setdiff(AcousticCategoryLink$AcousticCategory, allAcousticCategory)
    if(length(missingMixAcousticCategory)) {
        warning("StoX: The following mix AcousticCategory are not present in the NASCData, and will not be split: ", paste(missingMixAcousticCategory, collapse = ", "))
        
    }
    # Keep only rows with mix categories present in the data:
    AcousticCategoryLink <- subset(AcousticCategoryLink, AcousticCategory %in% allAcousticCategory)
    if(!NROW(AcousticCategoryLink)) {
        warning("No AcousticCategory given by AcousticCategoryLink that are actually present in the data. Returning the NASCData unchanged")
        return(NASCData)
    }
    
    
    # Fake a complete MeanNASCData table:
    data.table::setnames(MeanNASCData, c("MinChannelDepth", "MaxChannelDepth"), c("MinLayerDepth", "MaxLayerDepth"))
    # ... and fake Layers by simply copying Channel to Layer:
    MeanNASCData[, Layer := Channel]
    
    
    # Set aside the rows that will not be split:
    rowToBeSplit <- rowToBeSplit & MeanNASCData$AcousticCategory %in% AcousticCategoryLink$AcousticCategory
    MeanNASCDataNotSplit <- subset(MeanNASCData, !rowToBeSplit)
    
    # Define the resolution on which to distribute the NASC. This is not including AcousticCategory, so that DistributeNASC() distributes among all acoustic categories as the NASC is repeated to all acoustic categories in splitOneAcousticCategory():
    resolution <- getDataTypeDefinition(dataType = "NASCData", subTable = "Data", elements = c("horizontalResolution", "verticalResolution", "groupingVariables"), unlist = TRUE) # "EDSU", "Channel", "Beam", "Frequency"
    
    # Split the NASC by the AssignmentLengthDistributionData, but first remove the column Layer from AssignmentLengthDistributionData so that the fake layers which are copied from channels are not matched with the Layer from the assignment, which we   require to be WaterColumn in the current version:
    if(!all(AssignmentLengthDistributionData$Layer == "WaterColumn")) {
        stop("All Layer in AssignmentLengthDistributionData must be \"WaterColumn\". This can be set in the function DefineBioticAssignment.")
    }
    AssignmentLengthDistributionData[, Layer := NULL]
    
    # Split one mix acoustic category at the time:
    MeanNASCDataSplit <- lapply(
        unique(AcousticCategoryLink$AcousticCategory), 
        splitOneAcousticCategory, 
        MeanNASCData = MeanNASCData, 
        AssignmentLengthDistributionData = AssignmentLengthDistributionData, 
        AcousticTargetStrength = AcousticTargetStrength, 
        AcousticCategoryLink = AcousticCategoryLink, 
        SpeciesLink = SpeciesLink, 
        rowToBeSplit = rowToBeSplit, 
        splitResolution = resolution
    )
    MeanNASCDataSplit <- data.table::rbindlist(MeanNASCDataSplit)
    
    # Then add to the MeanNASCDataNotToSplit:
    columnsToKeep <- names(MeanNASCDataNotSplit)
    MeanNASCData <- rbind(
        MeanNASCDataNotSplit, 
        MeanNASCDataSplit[, ..columnsToKeep]
    )
    
    # Sum the NASC over length groups:
    sumBy <- c(resolution, "AcousticCategory")
    MeanNASCData <- MeanNASCData[, NASC := sum(NASC, na.rm = FALSE), by = sumBy]
    # Uniquify:
    MeanNASCData <- unique(MeanNASCData, by = sumBy)
    
    # Revert to channels:
    data.table::setnames(MeanNASCData, c("MinLayerDepth", "MaxLayerDepth"), c("MinChannelDepth", "MaxChannelDepth"))
    
    # Format the output to a NASCData (not MeanNASCData as is used in the splitting):
    formatOutput(MeanNASCData, dataType = "NASCData", keep.all = FALSE)
    
    
    return(MeanNASCData)
}

splitOneAcousticCategory <- function(mixAcousticCategory, MeanNASCData, AssignmentLengthDistributionData, AcousticTargetStrength, AcousticCategoryLink, SpeciesLink, rowToBeSplit, splitResolution) {
    
    # Extract the mix acoustic category :
    AcousticCategoryLink <- subset(AcousticCategoryLink, AcousticCategory == mixAcousticCategory)
    # Extract the MeanNASCData for the AcousticCategory to be split:
    doSplit <- rowToBeSplit & MeanNASCData$AcousticCategory %in% AcousticCategoryLink$AcousticCategory
    MeanNASCDataToBeSplit <- subset(MeanNASCData, doSplit)
    
    # Add the SplitAcousticCategory. This repeats the NASC to each of the split categories:
    MeanNASCDataToBeSplit <- RstoxData::mergeByIntersect(MeanNASCDataToBeSplit, AcousticCategoryLink, all = TRUE, allow.cartesian = TRUE, sort = FALSE)
    # Replace the AcousticCategory column by the SplitAcousticCategory column:
    MeanNASCDataToBeSplit[, AcousticCategory := SplitAcousticCategory][, SplitAcousticCategory := NULL]
    #  Remove the mixAcousticCategory, so that we properly REPLACE the mixAcousticCategory by the SplitAcousticCategory.:
    MeanNASCDataToBeSplit <- subset(MeanNASCDataToBeSplit, ! AcousticCategory %in% mixAcousticCategory)
    
    #  Split the NASC:
    MeanNASCDataSplit <- DistributeNASC(
        MeanNASCData = MeanNASCDataToBeSplit, 
        AssignmentLengthDistributionData = AssignmentLengthDistributionData, 
        AcousticTargetStrength = AcousticTargetStrength, 
        SpeciesLink = SpeciesLink, 
        sumBy = splitResolution, 
        distributionType = "SplitNASC"
    )
    
    # Check whether there are cells of the splitResolution ("EDSU", "Channel", "Beam", "Frequency") that are all NA in NASC (all length groups have missing NASC), and then add the MixAcousticCategory for these cells, so that the NASC is restored:
    allNA <- MeanNASCDataSplit[, .(allNA = all(is.na(NASC))), by = splitResolution]
    if(allNA[, any(allNA)]) {
        # Keep only the rows with all NAs in NASC:
        allNA <- subset(allNA, allNA == TRUE)
        # Merge with the MeanNASCData, to create a table to add to the MeanNASCDataSplit:
        MeanNASCDataToAddForAllNA <- merge(allNA, MeanNASCData, by = splitResolution, all.x = TRUE)
        
        # Add the restored NASC to the MeanNASCDataSplit (splitted MeanNASCData), but only keeping the rows with non-NA NASC in MeanNASCDataSplit. This is to prevent rows which are generated from missing AssignmentLengthDistribution (e.g., PSUs with no assigned hauls):
        MeanNASCDataSplit <- rbind(
            subset(MeanNASCDataSplit, !is.na(NASC)), 
            MeanNASCDataToAddForAllNA[, intersect(names(MeanNASCDataSplit), names(MeanNASCDataToAddForAllNA)), with = FALSE], 
            fill = TRUE
        )
    }
    
    return(MeanNASCDataSplit)
}

##################################################
#' Convert NASCData to StoxAcousticData
#' 
#' @inheritParams ModelData
#' 
#' @seealso \code{\link{NASCData}} and \code{\link[RstoxData]{StoxAcousticData}}.
#'  
#' @return A \code{\link{NASCData}} object.
#' 
#' @export
#'
NASCToStoxAcoustic <- function(NASCData, StoxAcousticData) {
    
    # Check that the StoxAcousticData are the same that were used to produce the MeanNASCData:
    if(!all(NASCData$EDSU %in% StoxAcousticData$Log$EDSU)) {
        stop("The StoxAcousticData are not compatible with the MeanNASCData. Please use the same StoxAcousticData that were used to generate the NASCData and further the MeanNASCData")
    }
    
    # Create BeamKey, AcousticCategoryKey, ChannelReferenceKey and NASCKey:
    NASCData[, CruiseKey := Cruise]
    # +2 and mot +1 due to the "/":
    NASCData[, LogKey := substr(EDSU, nchar(Cruise) + 2, nchar(EDSU))]
    NASCData[, BeamKey := Beam]
    NASCData[, AcousticCategoryKey := AcousticCategory]
    NASCData[, ChannelReferenceKey := ChannelReferenceType]
    NASCData[, NASCKey := Channel]
    
    # Rename MinChannelDepth to MinChannelRange:
    data.table::setnames(NASCData, c("MinChannelDepth", "MaxChannelDepth"), c("MinChannelRange", "MaxChannelRange"))
    
    
    # Keep only the data in the NASCData:
    StoxAcousticDataOut <- data.table::copy(StoxAcousticData)
    StoxAcousticDataOut$Cruise <- subset(StoxAcousticDataOut$Cruise, CruiseKey %in% NASCData$CruiseKey)
    
    StoxAcousticDataOut$Log <- subset(StoxAcousticDataOut$Log, LogKey %in% NASCData$LogKey)
    
    StoxAcousticDataOut$Beam <- extractColumnsOfDataTableAndUniquify(NASCData, names(StoxAcousticDataOut$Beam))
    StoxAcousticDataOut$AcousticCategory <- extractColumnsOfDataTableAndUniquify(NASCData, names(StoxAcousticDataOut$AcousticCategory))
    StoxAcousticDataOut$ChannelReference <- extractColumnsOfDataTableAndUniquify(NASCData, names(StoxAcousticDataOut$ChannelReference))
    StoxAcousticDataOut$NASC <- extractColumnsOfDataTableAndUniquify(NASCData, names(StoxAcousticDataOut$NASC))
    
    # Set the column order of the output StoxAcousticData based on the input StoxAcousticData:
    mapply(data.table::setcolorder, StoxAcousticDataOut, lapply(StoxAcousticData, names))
    
    # Order rows as in RstoxData::StoxAcoustic():
    RstoxData::orderRowsByKeys(StoxAcousticDataOut)
    
    return(StoxAcousticDataOut)
}


extractColumnsOfDataTableAndUniquify <- function(x, cols) {
    cols <- intersect(names(x), cols)
    unique(x[, ..cols])
}


##################################################
#' Append to NASCData 
#' 
#' This function converts a StoxAcousticData to NASCData and appends to another NASCData.
#' 
#' @inheritParams ModelData
#' 
#' @seealso \code{\link{NASC}} and \code{\link[RstoxData]{StoxAcousticData}}.
#'  
#' @export
#'
AppendNASC <- function(NASCData, StoxAcousticData) {
    
    # Convert the StoxAcousticData to NASCData:
    NSACData2 <- NASC(StoxAcousticData)
    
    # Append to the input NASCData and delete duplicated rows:
    NASCData <- rbind(NASCData, NSACData2)
    NASCData <- unique(NASCData)
    
    return(NASCData)
}
    
    






