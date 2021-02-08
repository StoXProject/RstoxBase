##################################################
##################################################
#' Define PSU
#' 
#' Underlying function for \code{\link{DefineBioticPSU}} and \code{\link{DefineAcousticPSU}}.
#' 
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @param StoxData Either \code{\link[RstoxData]{StoxBioticData}} or \code{\link[RstoxData]{StoxAcousticData}} data.
#' @param MergedStoxDataStationLevel The merged StoxData at the station level. Used in \code{meanRawResolutionData}.
#' @param DefinitionMethod A string naming the method to use, see \code{\link{DefineBioticPSU}} and \code{\link{DefineAcousticPSU}}.
#' @param SavePSUByTime Logical: If TRUE save the start and end times of sequences of EDSUs or Stations for each PSU.
#' @param PSUProcessData Previously generated PSU process data, one of \code{\link{AcousticPSU}} or \code{\link{BioticPSU}}.
#' 
#' @return
#' An list of two objects, Stratum_PSU and SSU_PSU.
#' 
#' @seealso \code{\link{DefineAcousticPSU}} and \code{\link{DefineBioticPSU}}..
#' 
#' @export
#' 
DefinePSU <- function(
    processData, UseProcessData = FALSE, 
    StratumPolygon, 
    StoxData = NULL, 
    MergedStoxDataStationLevel = NULL, 
    DefinitionMethod = c("Manual", "Identity", "DeleteAllPSUs", "PreDefined"), 
    #IntervalVariable = character(),
    #Interval = double(), 
    PSUType = c("Acoustic", "Biotic"), 
    SavePSUByTime = FALSE, 
    PSUProcessData
) {
    
    # Get the DefinitionMethod and PSUType:
    DefinitionMethod <- match.arg(DefinitionMethod)
    PSUType <- match.arg(PSUType)
    
    # Get the MergedStoxDataStationLevel if not given directly:
    if(!length(MergedStoxDataStationLevel)) {
        if(length(StoxData)) {
            MergedStoxDataStationLevel <- RstoxData::mergeDataTables(
                StoxData, 
                tableNames = c(
                    "Cruise", 
                    getRstoxBaseDefinitions("getStationLevel")(PSUType)
                ), 
                output.only.last = TRUE, 
                all = TRUE
            )
        }
        else {
            stop("One of MergedStoxDataStationLevel and StoxData must be given")
        }
    }
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        if(SavePSUByTime) {
            processData$PSUByTime <- getPSUByTime(
                PSUProcessData = processData, 
                MergedStoxDataStationLevel = MergedStoxDataStationLevel, 
                PSUType = PSUType
            )
        }
        return(processData)
    }
    
    # Define the PSU prefix and the SSU label, which is the name of the SSU column:
    prefix <- getRstoxBaseDefinitions("getPSUPrefix")(PSUType)
    SSULabel <- getRstoxBaseDefinitions("getSSULabel")(PSUType)
    
    # Make sure that there is only one row per SSU:
    notDuplicatedSSUs <- !duplicated(MergedStoxDataStationLevel[[SSULabel]])
    MergedStoxDataStationLevel <- MergedStoxDataStationLevel[notDuplicatedSSUs, ]
    
    # And order the SSUs by time:
    data.table::setorderv(MergedStoxDataStationLevel, "DateTime")
    
    # Get SSUs:
    SSU <- MergedStoxDataStationLevel[[SSULabel]]
    
    # Use each SSU as a PSU:
    if(grepl("Identity", DefinitionMethod, ignore.case = TRUE)) {
        
        # Define PSUIDs and PSUNames:
        PSUID <- seq_along(SSU)
        PSUName <- getPSUName(PSUID, prefix)
        
        # Set each SSU as a PSU:
        SSU_PSU <- data.table::data.table(
            SSU = SSU, 
            PSU = PSUName
        )
        
        # Find the stratum of each PSU:
        Stratum_PSU <- getStratumOfPSUs(SSU_PSU, MergedStoxDataStationLevel, StratumPolygon, SSULabel, StationLevel)
    }
    # Use each SSU as a PSU:
    else if(grepl("PreDefined", DefinitionMethod, ignore.case = TRUE)) {
        
        # Interpret middle times:
        MergedStoxDataStationLevel <- StoxDataStartMiddleStopDateTime(MergedStoxDataStationLevel)
        
        # Rename the SSULabel to "SSU":
        MergedStoxDataStationLevel <- renameSSUToSSULabelInTable(MergedStoxDataStationLevel, PSUType = PSUType, reverse = TRUE)
        
        # Get the SSU indices for each PSU:
        Stratum_PSU_SSU <- PSUProcessData$PSUByTime[, data.table::data.table(
            Stratum, 
            PSU, 
            Cruise, 
            # Use closed interval on both sides here to allow for time points and not only time interavls:
            SSUIndex = which(
                MergedStoxDataStationLevel$MiddleDateTime >= StartDateTime & 
                MergedStoxDataStationLevel$MiddleDateTime <= StopDateTime &
                MergedStoxDataStationLevel$Cruise == Cruise
                )
            ), 
            by = seq_len(nrow(PSUProcessData$PSUByTime))]
        
        # Remove PSUs with no SSUs:
        Stratum_PSU_SSU <- Stratum_PSU_SSU[!is.na(SSUIndex), ]
        
        # Add the SSUs:
        Stratum_PSU_SSU[, SSU := MergedStoxDataStationLevel$SSU[SSUIndex]]
        
        # Split into Stratum_PSU and SSU_PSU:
        Stratum_PSU <- unique(Stratum_PSU_SSU[, c("Stratum", "PSU")])
        SSU_PSU <- unique(Stratum_PSU_SSU[, c("SSU", "PSU")])
        
        # Add all SSUs:
        SSU_PSU <- merge(MergedStoxDataStationLevel[, "SSU"], SSU_PSU, all = TRUE)
        
        # Restore SSULabel:
        MergedStoxDataStationLevel <- renameSSUToSSULabelInTable(MergedStoxDataStationLevel, PSUType = PSUType)
    }
    
    #else if(grepl("Interval", DefinitionMethod, ignore.case = TRUE)) {
    #    
    #    # Find intervals:
    #    # Extract the interavl axis variable, such as DateTime or Log:
    #    IntervalAxis <- MergedStoxDataStationLevel[[IntervalVariable]]
    #    # Define the breaks, covering the range of the IntervalAxis, by steps defined by Interval:
    #    IntervalBreaks <- seq(
    #        Interval * floor(min(IntervalAxis/Interval)), 
    #        Interval * ceiling(max(IntervalAxis/Interval)), 
    #        Interval
    #    )
    #    # Find which intervals the IntervalAxis falls inside:
    #    intervals <- findInterval(IntervalAxis, IntervalBreaks)
    #    # Convert the intervals to 1, 2, 3, ...:
    #    intervals <- match(intervals, unique(intervals))
    #    # - and use these to define PSUs:
    #    PSU <- getPSUName(intervals, prefix)
    #    
    #    # Return the PSU definition with empty stratum links:
    #    SSU_PSU <- data.table::data.table(
    #        SSU = SSU, 
    #        PSU = PSU
    #    )
    #    
    #    # Find the stratum of each PSU:
    #    Stratum_PSU <- getStratumOfPSUs(SSU_PSU, MergedStoxDataStationLevel, StratumPolygon, SSULabel, Stat#ionLevel)
    #}
    
    
    # Otherwise return empty Stratum_PSU and SSU_PSU with all SSUs and empty string as PSU:
    else if(grepl("DeleteAllPSUs", DefinitionMethod, ignore.case = TRUE)) {
        
        SSU_PSU <- data.table::data.table(
            SSU = SSU, 
            PSU = NA_character_
        )
        Stratum_PSU <- data.table::data.table()
    }
    else if(grepl("Manual", DefinitionMethod, ignore.case = TRUE)) {
        if(length(processData)) {
            return(processData)
        }
        else {
            SSU_PSU <- data.table::data.table(
                SSU = SSU, 
                PSU = NA_character_
            )
            Stratum_PSU <- data.table::data.table()
        }
    }
    else {
        stop("Inavlid DefinitionMethod")
    }
    
    # Define the PSUProcessData:
    PSUProcessData <- list(
        Stratum_PSU = Stratum_PSU, 
        SSU_PSU = SSU_PSU
    )
    # Remove PSUs that do not have a stratum:
    PSUProcessData <- removePSUsWithMissingStratum(PSUProcessData)
    
    # Rename the data according to the model type:
    PSUProcessData <- renameSSULabelInPSUProcessData(PSUProcessData = PSUProcessData, PSUType = PSUType)
    
    # Add the PSU time information:
    if(SavePSUByTime) {
        PSUProcessData$PSUByTime <- getPSUByTime(
            PSUProcessData = PSUProcessData, 
            MergedStoxDataStationLevel = MergedStoxDataStationLevel, 
            PSUType = PSUType
        )
    }
    
    return(PSUProcessData)
}

# Function to get the stratum of each PSU, taken as the most frequent Stratum in which the PSU i loacted geographically:
getStratumOfPSUs <- function(SSU_PSU, MergedStoxDataStationLevel, StratumPolygon, SSULabel, StationLevel) {
    # Get unique PSUs:
    allPSUs <- unique(SSU_PSU$PSU)
    allPSUs <- allPSUs[!is.na(allPSUs)]
    # Get the strata:
    Stratum_PSU <- data.table::rbindlist(
        lapply(
            X = allPSUs, 
            FUN = getStratumOfPSU, 
            SSU_PSU = SSU_PSU, 
            MergedStoxDataStationLevel = MergedStoxDataStationLevel, 
            StratumPolygon = StratumPolygon, 
            SSULabel = SSULabel, 
            StationLevel = StationLevel
        )
    )
    
    return(Stratum_PSU)
}
getStratumOfPSU <- function(thisPSU, SSU_PSU, MergedStoxDataStationLevel, StratumPolygon, SSULabel, StationLevel) {
    
    # Get the MergedStoxDataStationLevel of the specified PSU:
    SSUs <- SSU_PSU[PSU == thisPSU, SSU]
    pos <- MergedStoxDataStationLevel[get(SSULabel) %in% SSUs, c("Longitude", "Latitude")]
    SpatialPSUs <- sp::SpatialPoints(pos)
    # Det the default projection to the points:
    sp::proj4string(SpatialPSUs) <- getRstoxBaseDefinitions("proj4string")
    
    # Find the stratum of each PSU:
    StratumNames <- sp::over(SpatialPSUs, StratumPolygon)
    # Select the most frequent:
    MostFrequentStratumName <- names(which.max(table(StratumNames)))
    
    # Create the Stratum_PSU data.table:
    Stratum_PSU <- data.table::data.table(
        #Stratum = NonEmptyStrata, 
        Stratum = if(length(MostFrequentStratumName)) MostFrequentStratumName else NA, 
        PSU = thisPSU
    )
    
    return(Stratum_PSU)
}

# Function to remove PSUs with missing Stratum:
removePSUsWithMissingStratum <- function(PSUProcessData) {
    
    validPSUs <- unique(PSUProcessData$Stratum_PSU$PSU[!is.na(PSUProcessData$Stratum_PSU$Stratum)])
    if(length(validPSUs)) {
        PSUProcessData$Stratum_PSU <- PSUProcessData$Stratum_PSU[ PSU %in% validPSUs ]
        PSUProcessData$SSU_PSU[! PSU %in% validPSUs, PSU := NA_character_]
    }
    
    return(PSUProcessData)
}

renameSSULabelInPSUProcessData <- function(PSUProcessData, PSUType = c("Acoustic", "Biotic"), reverse = FALSE) {
    PSUType <- match.arg(PSUType)
    SSULabel <- getRstoxBaseDefinitions("getSSULabel")(PSUType)
    
    if(reverse) {
        names(PSUProcessData) <- sub(SSULabel, "SSU", names(PSUProcessData))
        PSUProcessData$SSU_PSU <- renameSSUToSSULabelInTable(PSUProcessData$SSU_PSU, PSUType = PSUType, reverse = TRUE)
        #data.table::setnames(PSUProcessData$SSU_PSU, SSULabel, "SSU")
    }
    else {
        PSUProcessData$SSU_PSU <- renameSSUToSSULabelInTable(PSUProcessData$SSU_PSU, PSUType = PSUType, reverse = FALSE)
        #data.table::setnames(PSUProcessData$SSU_PSU, "SSU", SSULabel)
        names(PSUProcessData) <- sub("SSU", SSULabel, names(PSUProcessData))
    }
    
    
    return(PSUProcessData)
}


renameSSUToSSULabelInTable <- function(table, PSUType = c("Acoustic", "Biotic"), reverse = FALSE) {
    PSUType <- match.arg(PSUType)
    SSULabel <- getRstoxBaseDefinitions("getSSULabel")(PSUType)
    
    # Make a copy, since we are using setnames:
    table <- data.table::copy(table)
    if(reverse) {
        data.table::setnames(table, SSULabel, "SSU")
    }
    else {
        data.table::setnames(table, "SSU", SSULabel)
    }
    
    return(table)
}

# Function to paste PSU prefix and integer index to a PSU name:
getPSUName <- function(ind, prefix) {
    paste0(prefix, formatC(ind, width = max(nchar(ind)), format = "d", flag = "0"))
}

##################################################
##################################################
#' Biotic PSU
#' 
#' This function defines the \code{\link{BioticPSU}} process data, linking strata, biotic PSUs and Stations 
#' 
#' @inheritParams general_arguments
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param DefinitionMethod Character: A string naming the method to use, one of "StationToPSU", which sets each Station as a PSU, and "DeleteAllPSUs" to delete all PSUs.
#' 
#' @return
#' An object of StoX data type \code{\link{BioticPSU}}.
#' 
#' @seealso Acousic PSUs are generated using \code{\link{DefineAcousticPSU}}. For the vertical resolution (Layer) see \code{\link{DefineBioticLayer}} and \code{\link{DefineAcousticLayer}}.
#' 
#' @export
#' 
DefineBioticPSU <- function(
    processData, UseProcessData = FALSE, 
    StratumPolygon, 
    StoxBioticData, 
    DefinitionMethod = c("Manual", "StationToPSU", "DeleteAllPSUs")
) {
    
    # Get the DefinitionMethod:
    #DefinitionMethod <- match.arg(DefinitionMethod)
    DefinitionMethod <- if(isEmptyString(DefinitionMethod)) "" else match.arg(DefinitionMethod)
    if(grepl("StationToPSU", DefinitionMethod, ignore.case = TRUE)) {
        DefinitionMethod <- "Identity"
    }
    
    # Define the PSUs:
    BioticPSU <- DefinePSU(
        processData = processData, 
        UseProcessData = UseProcessData, 
        StratumPolygon = StratumPolygon, 
        StoxData = StoxBioticData, 
        DefinitionMethod = DefinitionMethod, 
        PSUType = "Biotic"
    )
    
    return(BioticPSU)
}


##################################################
##################################################
#' Acoustic PSU
#' 
#' Defines the \code{\link{AcousticPSU}} process data, linking strata, acoustic PSUs and EDSUs. 
#' 
#' @inheritParams general_arguments
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams DefinePSU
#' @param DefinitionMethod  Character: A string naming the method to use, one of "EDSUToPSU", which sets each EDSU as a PSU, and "DeleteAllPSUs" to delete all PSUs.
#' 
#' @return
#' An object of StoX data type \code{\link{AcousticPSU}}.
#' 
#' @seealso Biotic PSUs are generated using \code{\link{DefineBioticPSU}}. For the vertical resolution (Layer) see \code{\link{DefineBioticLayer}} and \code{\link{DefineAcousticLayer}}.
#' 
#' @export
#' 
DefineAcousticPSU <- function(
    processData, UseProcessData = FALSE, 
    StratumPolygon, 
    StoxAcousticData, 
    #DefinitionMethod = c("EDSUToPSU", "DeleteAllPSUs", "Interval", "ByTime"), 
    DefinitionMethod = c("Manual", "EDSUToPSU", "DeleteAllPSUs", "PreDefined"), 
    #IntervalVariable = character(),
    #Interval = double(), 
    #SavePSUByTime = FALSE, 
    AcousticPSU
) {
    
    # Get the DefinitionMethod:
    #DefinitionMethod <- match.arg(DefinitionMethod)
    DefinitionMethod <- if(isEmptyString(DefinitionMethod)) "" else match.arg(DefinitionMethod)
    if(grepl("EDSUToPSU", DefinitionMethod, ignore.case = TRUE)) {
        DefinitionMethod <- "Identity"
    }
    
    # Define the PSUs:
    AcousticPSU <- DefinePSU(
        processData = processData, 
        UseProcessData = UseProcessData, 
        StratumPolygon = StratumPolygon, 
        StoxData = StoxAcousticData, 
        DefinitionMethod = DefinitionMethod, 
        #IntervalVariable = IntervalVariable, 
        #Interval = Interval, 
        PSUType = "Acoustic", 
        SavePSUByTime = TRUE, 
        PSUProcessData = AcousticPSU
    )
    
    # Format the output:
    formatOutput(AcousticPSU, dataType = "AcousticPSU", keep.all = FALSE)
    
    return(AcousticPSU)
}




getPSUByTime <- function(
    PSUProcessData, 
    MergedStoxDataStationLevel, 
    PSUType
) {
    # Get the times of the PSUs:
    PSUByTime <- getPSUStartStopDateTime(
        PSUProcessData = PSUProcessData, 
        MergedStoxDataStationLevel = MergedStoxDataStationLevel, 
        PSUType = PSUType
    )
    
    return(PSUByTime)
}


# Function to get the start and stop time:
getPSUStartStopDateTime <- function(PSUProcessData, MergedStoxDataStationLevel, PSUType) {
    
    # Rename to the general SSU label:
    PSUProcessData <- renameSSULabelInPSUProcessData(PSUProcessData, PSUType = PSUType, reverse = TRUE)
    
    # Interpret start and end times:
    StationLevel <- getRstoxBaseDefinitions("getStationLevel")(PSUType)
    #StoxData <- RstoxData::mergeDataTables(
    #    StoxData, 
    #    tableNames = c("Cruise", StationLevel), 
    #    output.only.last = FALSE, 
    #    all = TRUE
    #)
    MergedStoxDataStationLevel <- StoxDataStartMiddleStopDateTime(MergedStoxDataStationLevel, type = PSUType)
    StationTable <- data.table::copy(MergedStoxDataStationLevel)
    
    SSULabel <- getRstoxBaseDefinitions("getSSULabel")(PSUType)
    data.table::setnames(StationTable, SSULabel, "SSU")
    
    # Split the SSU_PSU table into PSUs:
    SSU_PSU_ByPSU <- PSUProcessData$SSU_PSU[!is.na(PSU) & nchar(PSU) > 0]
    if(!nrow(SSU_PSU_ByPSU)) {
        # Create an empty table with the names defined in dataTypeDefinition:
        PSUStartStopDateTime <- data.table(1)[,`:=`(unlist(getRstoxBaseDefinitions("PSUByTime")), NA)][, V1 := NULL][.0]
        return(PSUStartStopDateTime)
    }
    SSU_PSU_ByPSU <- split(SSU_PSU_ByPSU, by = "PSU")
    
    # Get the table of start and stop times of each PSUs and combine to a table:
    PSUStartStopDateTime <- lapply(
        X = names(SSU_PSU_ByPSU), 
        FUN = getPSUStartStopDateTimeByPSU, 
        # Parameters of getPSUStartStopDateTimeByPSU(): 
        SSU_PSU_ByPSU = SSU_PSU_ByPSU, 
        StationTable = StationTable
    )
    PSUStartStopDateTime <- data.table::rbindlist(PSUStartStopDateTime)
    
    # Add the Stratum:
    PSUStartStopDateTime <- RstoxData::mergeByIntersect(PSUProcessData$Stratum_PSU, PSUStartStopDateTime)
    
    return(PSUStartStopDateTime)
}

# Function to get the start and end times of one acoustic PSU:
getPSUStartStopDateTimeByPSU <- function(PSU, SSU_PSU_ByPSU, StationTable) {
    
    # For convenience extract the SSUs of the current PSU:
    thisSSU_PSU <- SSU_PSU_ByPSU[[PSU]]
    
    # Match the SSUs of the PSUProcessData with SSUs of the StationTable:
    # Order both since it may happen that the EDSUs of the StoxAocusticData are not ordered, e.g. if there are mmultiple instruments from the same cruise, and these instruments have both identical and differing times:
    
    atSSUInStoxData <- match(sort(thisSSU_PSU$SSU), sort(StationTable$SSU))
    if(any(is.na(atSSUInStoxData))) {
        warning("StoX: The StoxData must be the same data that were used to generate the PSUProcessData.")
        atSSUInStoxData <- atSSUInStoxData[!is.na(atSSUInStoxData)]
    }
    
    # Split the matches by Cruise in order to get time sequences for each Cruise (includes platform for NMD data?????):
    atSSUInStoxDataByCruise <- split(atSSUInStoxData, StationTable$Cruise[atSSUInStoxData])
    
    # Get the table of start and stop times of all Cruises and combine to a table:
    PSUStartStopDateTime <- lapply(
        X = names(atSSUInStoxDataByCruise), 
        FUN = getPSUStartStopDateTimeOneCruise, 
        # Parameters of getPSUStartStopDateTimeOneCruise(): 
        atSSUInStoxDataByCruise = atSSUInStoxDataByCruise, 
        StationTable = StationTable
    )
    PSUStartStopDateTime <- data.table::rbindlist(PSUStartStopDateTime)

    # Add the PSU:
    PSUStartStopDateTime <- data.table::data.table(
        PSU = PSU, 
        PSUStartStopDateTime
    )
    
    return(PSUStartStopDateTime)
}

# Function to get the table of start and stop times of one Cruise:
getPSUStartStopDateTimeOneCruise <- function(Cruise, atSSUInStoxDataByCruise, StationTable) {
    
    # Get start and stop of unbroken sequences fo SSUs:
    thisSSU <- atSSUInStoxDataByCruise[[Cruise]]
    steps <- which(diff(thisSSU) > 1)
    startInd <- c(1, steps + 1)
    stopInd <- c(steps, length(thisSSU))
    # Get start and stop times of the unbroken sequences:
    startTimes <- StationTable$StartDateTime[thisSSU[startInd]]
    stopTimes <- StationTable$StopDateTime[thisSSU[stopInd]]
    
    # Create the output table:
    PSUStartStopDateTimeOneCruise <- data.table::data.table(
        Cruise = Cruise, 
        StartDateTime = startTimes, 
        StopDateTime = stopTimes
    )
    
    return(PSUStartStopDateTimeOneCruise)
}


##################################################
##################################################
#' Define Layers
#' 
#' This function defines the \code{\link{BioticLayer}} process data, which sets the range intervals of the swetp-area layers used in biotic estimation models in StoX.
#' 
#' @inheritParams general_arguments
#' @param StoxData Either \code{\link[RstoxData]{StoxBioticData}} or \code{\link[RstoxData]{StoxAcousticData}} data.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "WaterColumn", to define one single for the entire water column; "HighestResolution", to use the maximum possible vertical resolution without intersecting hauls; "Resolution", which can be used to set a fixed layer thickness; and "LayerTable" to provide the \code{LayerTable}.
#' @param Resolution  Numeric: A single numeric giving the thickness of the layers.
#' @param LayerTable A table of Layer name, MinLayerDepth in meters and MaxLayerDepth in meters, defining the Layers.
#' 
#' @return
#' An object of StoX data type \code{\link{BioticLayer}}.
#' 
#' @seealso \code{\link{DefineAcousticLayer}} and \code{\link{DefineBioticLayer}}.
#' 
#' @export
#' 
DefineLayer <- function(
    processData, UseProcessData = FALSE, 
    StoxData, 
    DefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    LayerType = c("Acoustic", "Biotic")
) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    # Get the DefinitionMethod:
    LayerType <- match.arg(LayerType)
    
    # If given as a list of data.tables, extract the table holding the vertical resolution:
    if(is.list(StoxData) && all(sapply(StoxData, data.table::is.data.table))) {
        # Merge the tables of the input data:
        data <- RstoxData::mergeDataTables(StoxData, output.only.last = TRUE, all = TRUE)
        #data <- StoxData[[VerticalResolutionLevel]]
    }
    else {
        data <- StoxData
    }
    
    # SSULevel
    if(LayerType == "Acoustic") {
        # Add channel depths:
        getChannelDepth(data)
        #VerticalResolutionLevel <- "NASC"
        VerticalResolutionMin <- "MinChannelDepth"
        VerticalResolutionMax <- "MaxChannelDepth"
    }
    else if(LayerType == "Biotic") {
        #VerticalResolutionLevel <- "Haul"
        VerticalResolutionMin <- "MinHaulDepth"
        VerticalResolutionMax <- "MaxHaulDepth"
    }
    else {
        stop("Unknown model type")
    }
    
    
    
    # Get the common intervals:
    possibleIntervals <- getCommonIntervals(
        data = unique(data[, c(..VerticalResolutionMin, ..VerticalResolutionMax)]), 
        varMin = VerticalResolutionMin, 
        varMax = VerticalResolutionMax, 
        lowerName = "MinLayerDepth", 
        upperName = "MaxLayerDepth"
    )
    
    # If "WaterColumn" is requested use the full range:
    if(grepl("WaterColumn", DefinitionMethod, ignore.case = TRUE)) {
        Layer <- data.table::data.table(
            Layer = "WaterColumn", 
            #MinLayerDepth = possibleIntervals[1, 1], 
            MinLayerDepth = 0,
            #MaxLayerDepth = possibleIntervals[nrow(possibleIntervals), 2]
            MaxLayerDepth = Inf
        )
    }
    
    # If "HighestResolution" is requested use all possible breaks:
    else if(grepl("HighestResolution", DefinitionMethod, ignore.case = TRUE)) {
        Layer <- createLayerTable(possibleIntervals)
    }
    
    # If "LayerTable" is requested match the Breaks against the possible breaks:
    else if(grepl("LayerTable", DefinitionMethod, ignore.case = TRUE)) {
        # Error if any of the specified breaks are invalid:
        if(any(! unlist(LayerTable[, c("MinLayerDepth", "MaxLayerDepth")]) %in% unlist(possibleIntervals))) {
            stop("Some of the specified breaks are not at common breaks of all Log(distance)s. Possible breaks are [", paste(unlist(possibleIntervals), collapse = ", "), "]")
        }
        else {
            Layer <- LayerTable
        }
    }
    
    else if(grepl("Resolution", DefinitionMethod, ignore.case = TRUE)) {
        stop("DefinitionMethod \"Resolution\" not yet implemented")
    }
    
    else {
        stop("Invalid DefinitionMethod")
    }
    
    
    return(Layer)
}

# Function to create a LayerTable from breaks:
createLayerTable <- function(x) {
    # Create a data.table if a vector of breaks is given:
    if(length(dim(x)) == 1) {
        x <- data.table::data.table(
            MinLayerDepth = x[-length(x)], 
            MaxLayerDepth = x[-1]
        )
    }
    else {
        names(x) <- c("MinLayerDepth", "MaxLayerDepth")
    }
    # Create the Layer names:
    LayerNames <- getDefaultLayerNames(x)
    x <- cbind(
        Layer = LayerNames, 
        x
    )
    x
}

# Function to get defaul Layer names:
getDefaultLayerNames <- function(x) {
    if(length(dim(x) == 2)) {
        nlayers <- nrow(x)
    }
    else {
        nlayers <- length(x)
    }
    paste0("Layer", formatC(seq_len(nlayers), width = nchar(nlayers), format = "d", flag = "0"))
}


##################################################
##################################################
#' Define Acoustic Layer
#' 
#' This function defines the \code{\link{AcousticLayer}} process data, which sets the range intervals of the acoustic layers used in acoustic-trawl estimation models in StoX. 
#' 
#' @inheritParams general_arguments
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams DefineLayer
#' 
#' @return
#' An object of StoX data type \code{\link{AcousticLayer}}.
#' 
#' @seealso Biotic Layers are generated using \code{\link{DefineBioticLayer}}. For the horizontal resolution (Stratum/PSU) see \code{\link{DefineBioticPSU}} and \code{\link{DefineAcousticPSU}}.
#' 
#' @export
#' 
DefineAcousticLayer <- function(
    processData, UseProcessData = FALSE, 
    StoxAcousticData, 
    DefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table()
) {
    
    DefineLayer(
        processData = processData, 
        StoxData = StoxAcousticData, 
        DefinitionMethod = DefinitionMethod, 
        Resolution = Resolution, 
        LayerTable = LayerTable, 
        UseProcessData = UseProcessData, 
        LayerType = "Acoustic"
    )
}


##################################################
##################################################
#' Define biotic Layer
#' 
#' This function defines the \code{\link{BioticLayer}} process data, which sets the range intervals of the swetp-area layers used in biotic estimation models in StoX.
#' 
#' @inheritParams general_arguments
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @inheritParams DefineLayer
#' 
#' @return
#' An object of StoX data type \code{\link{BioticLayer}}.
#' 
#' @seealso Acoustic Layers are generated using \code{\link{DefineAcousticLayer}}. For the horizontal resolution (Stratum/PSU) see \code{\link{DefineBioticPSU}} and \code{\link{DefineAcousticPSU}}.
#' 
#' @export
#' 
DefineBioticLayer <- function(
    processData, UseProcessData = FALSE, 
    StoxBioticData, 
    DefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table()
) {
    
    DefineLayer(
        processData = processData, 
        StoxData = StoxBioticData, 
        DefinitionMethod = DefinitionMethod, 
        Resolution = Resolution, 
        LayerTable = LayerTable, 
        UseProcessData = UseProcessData, 
        LayerType = "Biotic"
    )
}



##################################################
##################################################
#' Assignment of biotic hauls to acoustic PSUs by layer
#' 
#' This function defines the \code{\link{BioticAssignment}} process data, linking biotic Hauls with acoustic PSUs by Layer.
#' 
#' @inheritParams general_arguments
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Stratum", assign all stations of each stratum to all acoustic PSUs; "Radius", to assign all stations within the radius given in \code{Radius} to each acoustic PSU; and "EllipsoidalDistance" to provide \code{MinNumberOfHauls}, \code{Distance}, \code{TimeDifference}, \code{BottomDepthDifference}, \code{LongitudeDifference} and \code{LatitudeDifference}, specifying the axes of an ellipsoid inside which to assign stations to acoustic PSUs.
#' @inheritParams SumNASC
#' @inheritParams DefineAcousticLayer
#' @param Radius Numeric: The radius inside which to assign biotic stations to each acoustic PSU.
#' @param MinNumberOfHauls For DefinitionMethod "EllipsoidalDistance": Integer minimum number of hauls selected inside the ellipsoid. If the number of hauls inside the ellispoid is lower than the \code{MinNumberOfHauls}, the \code{MinNumberOfHauls} closest Hauls will be used (in ellipsoidal distance). 
#' @param Distance For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing distance in nautical miles.
#' @param TimeDifference For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing time difference in hours.
#' @param BottomDepthDifference For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing difference in bottom depth in meters.
#' @param LongitudeDifference For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing difference in longitude in degrees.
#' @param LatitudeDifference For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing difference in latitude in degrees.
#' 
#' @details
#' The \emph{DefineBioticAssignment} function creates a list of which biotic hauls are assigned to each acoustic primary sampling unit (PSU) and assignment layer. An \emph{assignment layer} is made up of one or more \emph{layers}. The assigned biotic hauls are given the default weighting factor of 1. The list of assigned biotic stations will in another function be used to make a total combined length frequency distribution from all the individual haul distributions that have been assigned.
#' 
#' In addition to the option of modifying assignments manually through the StoX graphically user interface, several automatic assignment methods are available. The automated methods are applied by assignment layer on the biotic stations that are associated with each assignment layer. By default, the function parameter \emph{UseProcessData} is set to true, ensuring that assignment from previous execution is availabe upon execution. UseProcessData may be set to false to redo or update assignemts.
#' 
#' Layer definitions needed for assignment can be done in two ways using the function parameter:
#' 
#' \strong{LayerDefinition} 
#' 
#' The available parameters are:
#' \emph{FunctionInput} which utelizes input \code{\link{AcousticLayer}} process data generated in a previous process. This object contains the layer definitions
#' 
#' alternatively,
#' 
#' \emph{FunctionParameter} which use function input data type \code{\link{AcousticData}} and function parameter \emph{LayerDefinitionMethod}. The available methods are:
#' 
#' \emph{Watercolomn} method which defines one layer for the entire watercolumn
#' \emph{HighestResolution} method which makes the highest possible number of layers based on the resolution in the input AcousticData.
#' \emph{Resolution} method is assosiated with function parameter \emph{Resolution} which gives the desired thickness (in meters) of the layers.
#'
# Atle to fix this: \emph{LayerTable} method is assosiated with function parameter \emph{LayerTable} which \strong{??? NEED TO BE COMPLETED ???}
#'   
#' The available automatic assignment methods are:
#' 
#'\strong{Stratum}
#'
#'All biotic hauls within each stratum  are assigned to all the acoustic PSUs of the stratum.
#'
#'\strong{Radius}
#'
#'All biotic stations within the given radius (function parameter \emph{Radius} (nautical miles)) of one or more of the elementary distance sampling units (EDSU) that makes up a PSU, are assigned to that PSU. The start position of both the biotic station and the EDSU is used for distance calculations. The method does not take into consideration whether the biotic station is in the same stratum as the PSU or even outside the boundaries of the strata system. The function parameter \emph{MinNumberOfHauls} set a minimum number of hauls for each assignment. This implies that the search for hauls may go beyond the given radius.
#'
#'\strong{EllipsoidalDistance}
#'
#'This assignment method uses the ellipsoidal distance \href{https://doi.org/10.1016/j.fishres.2007.07.013}{(Johnsen and Iilende, 2007, equation 8)}.  All biotic stations that fulfills the selection criteria (scalar product f <=1) on one or more EDSUs of a PSU, will be assigned to the PSU. The scalar product of the method is calculated as:
#'
#' \deqn{f(d,t,b,l,o)=\left(\frac{\Delta d}{r_d}\right)^2 + \left(\frac{\Delta t}{r_t}\right)^2 +
#' \left(\frac{\Delta b}{r_b}\right)^2 + \left(\frac{\Delta l}{r_l}\right)^2 +
#' \left(\frac{\Delta o}{r_0}\right)^2}
#' 
#' where:
#' 
#' \eqn{f} = scalar product
#' 
#' \eqn{\Delta d} = great circle distance between the acoustic EDSU position  and the biotic station (nautical miles). Start positions for the biotic station and the EDSU are used.
#' 
#' \eqn{\Delta t} = time difference between the acoustic EDSU and the biotic station recordings (hours). Start time for the biotic station and the EDSU are used.
#' 
#' \eqn{\Delta b} = difference in bottom depth at the locations of the acoustic EDSU and the biotic station (meters). For the EDSU, the bottom depth is calculated as the average depth from the minimum and maximum depth recorded over the EDSU distance. The biotic station depth is calculated as the average at the start and stop of the station.
#' 
#'\eqn{\Delta l} = difference in latitude between the acoustic EDSU and the biotic station (degrees)
#'
#'\eqn{\Delta o} = difference in longitude between the acoustic EDSU and the biotic station (degrees) 
#'
#'\eqn{r_d} = reference value for great circle distance difference (nautical miles). Defined by function parameter \emph{Distance}) 
#'
#'\eqn{r_t} = reference value for time difference (hours). Defined by function parameter \emph{TimeDifference}) 
#'
#'\eqn{r_b} = reference value for bottom depth difference (meters). Defined  by function parameter \emph{BottomDepthDifference})
#'
#'\eqn{r_l} = reference value for latitude difference (degrees). Defined by function parameter \emph{LatitudeDifference})
#'
#'\eqn{r_o} = reference value for longitude difference (degrees). Defined by function parameter \emph{LongitudeDifference})
#'
#'The function parameter \emph{MinNumberOfHauls} can override the requirement to fulfill the selection criteria (scalar product f <=1) if the number of assigned haulss are lower than the MinNumberOfHauls parameter value. Hauls with a scalar product value closest to the minimum selection criteria, will be included in the assignment list to ensure that a minimum number of stations are assigned.
#'
#'NOTE! The end user will get a warning if one or more acoustic PSUs have not been assigned any biotic hauls.
#'
#' @references
#'
#'Johnsen E., Iilende T., 2007, Factors affecting the diel variation in commercial CPUE of Namibian hake. Can new information improve standard survey estimates?, Fisheries Research 88 (2007) p70 to 79, \url{https://doi.org/10.1016/j.fishres.2007.07.013}
#' 
#' 
#' @return
#' An object of StoX data type \code{\link{BioticAssignment}}.
#' 
#' @seealso \code{\link{BioticAssignmentWeighting}} for weighting BioticAssignment.
#' 
#' @export
#'
DefineBioticAssignment <- function(
    processData, UseProcessData = FALSE, 
    DefinitionMethod = c("Stratum", "Radius", "EllipsoidalDistance", "DeleteAllAssignments"), 
    StoxBioticData, 
    # For DefinitionMethod "Stratum": 
    StratumPolygon, AcousticPSU, #AcousticLayer, 
    LayerDefinition = c("FunctionParameter", "FunctionInput"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    AcousticLayer = NULL, 
    # (Additional) for DefinitionMethod "Radius": 
    StoxAcousticData, 
    Radius = double(), 
    # (Additional) for DefinitionMethod "EllipsoidalDistance": 
    MinNumberOfHauls = double(),
    Distance = double(), 
    TimeDifference = double(), 
    BottomDepthDifference = double(), 
    LongitudeDifference = double(), 
    LatitudeDifference = double()
)
{
    
    # Get the DefinitionMethod:
    LayerDefinition <- match.arg(LayerDefinition)
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        # Special action since we have included Layer in the BioticAssignment but have not yet opened for the possibility to assign differently to different layers. Re-add the Layer column:
        BioticAssignment <- addLayerToBioticAssignmentAndFormat(
            BioticAssignment = processData$BioticAssignment, 
            LayerDefinition = LayerDefinition, 
            AcousticLayer = AcousticLayer, 
            StoxAcousticData = StoxAcousticData, 
            LayerDefinitionMethod = LayerDefinitionMethod, 
            Resolution = Resolution, 
            LayerTable = LayerTable
        )
        return(BioticAssignment)
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    # Merge the StoxBioticData:
    MergeStoxBioticData <- RstoxData::MergeStoxBiotic(StoxBioticData, "Haul")
    
    # If DefinitionMethod == "Stratum", assign all stations of each stratum to all PSUs of the stratum:
    if(grepl("Stratum", DefinitionMethod, ignore.case = TRUE)) {
        # Create a spatial points object of the positions of the hauls:
        SpatialHauls <- sp::SpatialPoints(MergeStoxBioticData[, c("Longitude", "Latitude")])
        sp::proj4string(SpatialHauls) <- getRstoxBaseDefinitions("proj4string")
        
        # Get the stratum for each haul:
        locatedStratum <- unname(unlist(sp::over(SpatialHauls, StratumPolygon)))
        BioticAssignment <- MergeStoxBioticData
        BioticAssignment[, Stratum := ..locatedStratum]
        
        # Add the PSUs to the BioticAssignment:
        BioticAssignment <- merge(BioticAssignment, AcousticPSU$Stratum_PSU, all = TRUE, by = "Stratum", allow.cartesian = TRUE)
        
        # Discard all rows with missing PSU:
        BioticAssignment <- subset(BioticAssignment, !is.na(PSU))
    }
    # Search for Hauls around all EDSUs of each PSU:
    else if(grepl("Radius|EllipsoidalDistance", DefinitionMethod, ignore.case = TRUE)) {
        # Merge the StoxBioticData:
        MergeStoxAcousticData <- RstoxData::MergeStoxAcoustic(StoxAcousticData, "Log")
        
        # Get a table of EDSUs and Hauls:
        EDSU_Haul <- data.table::CJ(
            EDSU = MergeStoxAcousticData$EDSU, 
            Haul = MergeStoxBioticData$Haul, 
            sorted = FALSE
        )
        
        # Merge PSUs and strata into the table:
        EDSU_PSU_Stratum <- RstoxData::mergeDataTables(AcousticPSU[names(AcousticPSU)!= "PSUByTime"], all = TRUE, output.only.last = TRUE)
        BioticAssignment <- merge(EDSU_PSU_Stratum, EDSU_Haul, by = "EDSU", all = TRUE)
        
        # Get the distance units:
        if(grepl("Radius", DefinitionMethod, ignore.case = TRUE)) {
            
            differenceTable = data.table::data.table(
                distance = getDistance(
                    MergeStoxAcousticData = MergeStoxAcousticData, 
                    MergeStoxBioticData = MergeStoxBioticData
                )
            )
            
            # Tag the Hauls that are inside the radius:
            differenceTable[, inside := distance <= Radius]
        }
        else if(grepl("EllipsoidalDistance", DefinitionMethod, ignore.case = TRUE)) {
            differenceTable = data.table::data.table(
                # Get the distance between the EDSUs and Hauls:
                Distance = if(length(Distance)) {
                    getSquaredRelativeDistance(
                        MergeStoxAcousticData = MergeStoxAcousticData, 
                        MergeStoxBioticData = MergeStoxBioticData, 
                        Distance = Distance
                    )
                }, 
                # Get the time difference between the EDSUs and Hauls:
                TimeDifference = if(length(TimeDifference)) {
                    getSquaredRelativeTimeDiff(
                        MergeStoxAcousticData = MergeStoxAcousticData, 
                        MergeStoxBioticData = MergeStoxBioticData, 
                        TimeDifference = TimeDifference
                    )
                }, 
                # Get the difference in bottom depth between the EDSUs and Hauls:
                BottomDepthDifference = if(length(BottomDepthDifference)) {
                    getSquaredRelativeDiff(
                        MergeStoxAcousticData = MergeStoxAcousticData, 
                        MergeStoxBioticData = MergeStoxBioticData, 
                        variableName = "BottomDepth", 
                        axisLength = BottomDepthDifference
                    )
                }, 
                # Get the longitude difference between the EDSUs and Hauls:
                LongitudeDifference = if(length(LongitudeDifference)) {
                    getSquaredRelativeDiff(
                        MergeStoxAcousticData = MergeStoxAcousticData, 
                        MergeStoxBioticData = MergeStoxBioticData, 
                        variableName = "Longitude", 
                        axisLength = LongitudeDifference
                    )
                }, 
                # Get the latitude differerence between the EDSUs and Hauls:
                LatitudeDifference = if(length(LatitudeDifference)) {
                    getSquaredRelativeDiff(
                        MergeStoxAcousticData = MergeStoxAcousticData, 
                        MergeStoxBioticData = MergeStoxBioticData, 
                        variableName = "Latitude", 
                        axisLength = LatitudeDifference
                    )
                }
            )
            
            # Check whether any of the columns are all NA, indicating error in the data:
            NACols <- unlist(differenceTable[, lapply(.SD, function(x) all(is.na(x)))])
            if(any(NACols)) {
                warning("StoX: In function DefineBioticAssignment using DefinitionMethod \"EllipsoidalDistance\", the following axes of the ellipsoid were all NA, indicating missing data: ", paste0(names(differenceTable)[NACols], collapse = ", "))
            }
            
            # Sum and take the square root to get the ellipsoidal distance:
            #differenceTable[, distance := sqrt(rowSums(.SD, na.rm = TRUE))] # No need to sqrt:
            differenceTable[, distance := rowSums(.SD, na.rm = TRUE)]
            
            # Tag Hauls inside the ellipsoid:
            differenceTable[, inside := distance <= 1]
        }
        
        # Discard all rows with missing PSU:
        validRows <- BioticAssignment[, !is.na(PSU)]
        
        # Join the differenceTable into the BioticAssignment:
        BioticAssignment <- data.table::data.table(
            subset(BioticAssignment, validRows), 
            subset(differenceTable, validRows)
        )
        
        # Apply the requirement on the number of hauls per PSU:
        if(length(MinNumberOfHauls)) {
            # Get the minimum distance between the EDSUs of a PSU and the Hauls (all hauls):
            BioticAssignment[, minDistance := min(distance), by = c("PSU", "Haul")]
            
            # Keep those with minimum distance lower than or equal to the MinNumberOfHauls'th unique minimum distance:
            BioticAssignment[, inside := inside | distance <= sort(unique(minDistance))[MinNumberOfHauls], by = c("PSU")]
        }
        
        # Keep only Hauls inside the radius:
        BioticAssignment <- subset(BioticAssignment, inside)
        
        # Extract the columns Stratum, PSU and Haul, and uniquify:
        BioticAssignment <- unique(BioticAssignment, by = c("Stratum", "PSU", "Haul"))
    }
    else if(grepl("DeleteAllAssignments", DefinitionMethod, ignore.case = TRUE)) {
        BioticAssignment <- data.table::data.table()
    }
    else if(isEmptyString(DefinitionMethod)){
        if(length(processData)) {
            return(processData)
        }
        else {
            BioticAssignment <- data.table::data.table()
        }
    }
    else {
        stop("Inavlid DefinitionMethod")
    }
    
    ## Add all Layers to each assigned haul:
    #LayerDefinition <- match.arg(LayerDefinition)
    ## Get the Layers:
    #if(identical(LayerDefinition, "FunctionParameter")) {
    #    AcousticLayer <- DefineLayer(
    #        StoxData = StoxAcousticData, 
    #        DefinitionMethod = LayerDefinitionMethod, 
    #        Resolution = Resolution, 
    #        LayerTable = LayerTable, 
    #        LayerType = "Acoustic"
    #    )
    #}
    ## Add the layers:
    #Layer_PSU <- data.table::CJ(Layer = AcousticLayer$Layer, PSU = unique(BioticAssignment$PSU))
    #BioticAssignment <- merge(BioticAssignment, Layer_PSU, all = TRUE, by = "PSU", allow.cartesian = TRUE)
    
    
    BioticAssignment <- addLayerToBioticAssignmentAndFormat(
        BioticAssignment = BioticAssignment, 
        LayerDefinition = LayerDefinition, 
        AcousticLayer = AcousticLayer, 
        StoxAcousticData = StoxAcousticData, 
        LayerDefinitionMethod = LayerDefinitionMethod, 
        Resolution = Resolution, 
        LayerTable = LayerTable
    )
    
    return(BioticAssignment)
}

# Function to add Layer to BioticAssignment:
addLayerToBioticAssignmentAndFormat <- function(
    BioticAssignment, 
    LayerDefinition, 
    AcousticLayer, 
    StoxAcousticData, 
    LayerDefinitionMethod, 
    Resolution, 
    LayerTable
) {
    
    # Add layers and WeightingFactor only if BioticAssignment is nont empty:
    if(nrow(BioticAssignment)) {
        # Get the Layers:
        if(identical(LayerDefinition, "FunctionParameter")) {
            AcousticLayer <- DefineLayer(
                StoxData = StoxAcousticData, 
                DefinitionMethod = LayerDefinitionMethod, 
                Resolution = Resolution, 
                LayerTable = LayerTable, 
                LayerType = "Acoustic"
            )
        }
        # Add the Layers:
        Layer_PSU <- data.table::CJ(Layer = AcousticLayer$Layer, PSU = unique(BioticAssignment$PSU))
        # .. but remove the existing Layers first:
        if("Layer" %in% names(BioticAssignment)) {
            BioticAssignment[, Layer := NULL]
        }
        BioticAssignment <- merge(BioticAssignment, Layer_PSU, all = TRUE, by = "PSU", allow.cartesian = TRUE)
        # Remove duplicates (which may be generated when removing the Layer column):
        BioticAssignment <- unique(BioticAssignment)
        
        # Add weighting  = 1:
        BioticAssignment[, WeightingFactor := 1]
    }
    
    # Format the output:
    formatOutput(BioticAssignment, dataType = "BioticAssignment", keep.all = FALSE)
    
    return(BioticAssignment)
}

# Function to get the great circle distance between EDSUs in the MergeStoxAcousticData and Hauls in the MergeStoxBioticData: 
getDistance <- function(MergeStoxAcousticData, MergeStoxBioticData) {
    # Extract the goegraphical positions:
    HaulPositions <- as.matrix(MergeStoxBioticData[, c("Longitude", "Latitude")])
    EDSUPositions <- as.matrix(MergeStoxAcousticData[, c("Longitude", "Latitude")])
    # Get the distances between EDUSs and Hauls (sp::spDists() returns km when longlat = TRUE):
    #EDSUToHaulDistance <- c(sp::spDists(EDSUPositions, HaulPositions, longlat = TRUE))
    EDSUToHaulDistance <- c(sp::spDists(HaulPositions, EDSUPositions, longlat = TRUE))
    # Convert to nautical miles:
    EDSUToHaulDistance <- EDSUToHaulDistance * 1000 / getRstoxBaseDefinitions("nauticalMileInMeters")
    return(EDSUToHaulDistance)
}

# Function to ge the squared distance in units of the Distance squared:
getSquaredRelativeDistance <- function(MergeStoxAcousticData, MergeStoxBioticData, Distance) {
    # Get the distances between EDUSs and Hauls:
    EDSUToHaulDistance <- getDistance(
        MergeStoxAcousticData = MergeStoxAcousticData, 
        MergeStoxBioticData = MergeStoxBioticData
    )
    # Square and return:
    SquaredRelativeDistance <- EDSUToHaulDistance^2 / Distance^2
    return(SquaredRelativeDistance)
}

# Function to get the squared time difference in units of the TimeDifference squared:
getSquaredRelativeTimeDiff <- function(MergeStoxAcousticData, MergeStoxBioticData, TimeDifference, variableName = "DateTime") {
    # Get the time difference between all EDSUs and all Hauls:
    out <- data.table::CJ(
        x = MergeStoxAcousticData[[variableName]], 
        y = MergeStoxBioticData[[variableName]], 
        sorted = FALSE
    )
    TimeDiff <- as.numeric(out[, difftime(x, y, units = "hours")])
    # Square and return:
    SquaredTimeDiff <- TimeDiff^2 / TimeDifference^2
    return(SquaredTimeDiff)
}

getSquaredRelativeDiff <- function(MergeStoxAcousticData, MergeStoxBioticData, variableName, axisLength) {
    # Get the absolute difference between all EDSUs and all Hauls:
    out <- data.table::CJ(
        x = MergeStoxAcousticData[[variableName]], 
        y = MergeStoxBioticData[[variableName]], 
        sorted = FALSE
    )
    # Square and return:
    SquaredRelativeDiff <- c(out[, x - y])^2 / axisLength^2
    return(SquaredRelativeDiff)
}






##################################################
##################################################
#' Weighting of biotic hauls in biotic assignment
#' 
#' This function puts weights to the hauls assigned to acoustic PSUs in \code{\link{BioticAssignment}} process data.
#' 
#' @inheritParams general_arguments
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' @param WeightingMethod  Character: A string naming the method to use, one of "Equal", giving weight 1 to all Hauls; "NumberOfLengthSamples", weighting hauls by the number of length samples; "NASC", weighting by the surrounding NASC converted by the haul length distribution to a density equivalent; "NormalizedTotalWeight", weighting hauls by the total weight of the catch, normalized by dividing by towed distance; "NormalizedTotalCount", the same as "NormalizedTotalWeight" but for total count, "SumWeightedCount", weighting by the summed WeightedCount of the input LengthDistributionData; and "InverseSumWeightedCount", weighting by the inverse of the summed WeightedCount.
#' @param MaxNumberOfLengthSamples For \code{WeightingMethod} = "NumberOfLengthSamples": Values of the number of length samples that exceed \code{MaxNumberOfLengthSamples} are set to \code{MaxNumberOfLengthSamples}. This avoids giving too high weight to e.g. experimental hauls with particularly large length samples.
#' @param Radius For \code{WeightingMethod} = "NASC": The radius inside which the average NASC is calculated. 
#' @param LengthExponent For \code{WeightingMethod} = "NASC": A table linking AcousticCategory with the LengthExponent used to convert from NASC to density.
#' 
#' @details
#' The \emph{BioStationWeighting} function is used to update the weighting variables of the biotic stations that are associated in \code{\link{BioticAssignment}}. The list of assigned biotic hauls and weighting variables of an assignment, will in another function be used to make a total combined length frequency distribution from all the individual haul distributions.
#' 
#' A set of automatic \emph{WeightingMethod}s are available to update the haul weighing variables:
#' 
#'\strong{Equal}
#' 
#' All assigned biotic hauls are given equal weight by assigning the value 1 to the weighting variables.
#' 
#'\strong{NumberOfLengthSamples}
#'The assigned biotic hauls are given a weighting value according to the number of individual length samples of the target species at the biotic station.  The parameter \emph{MaxNumberOfLengthSamples} is also associated with this method and is used to limit the weighting to a maximum number of length samplesof a haul. 
#' 
#' \strong{NormTotalWeight}
#' 
#' The assigned biotic hauls are given a weighting value according to the normalized catch weight of the target species at the station. The weighting value is calculated as catch weight divided by towing distance. This normalization makes the stations comparable regardless of catch effort.
#' 
#'\strong{NormTotalCount}
#'
#'The assigned biotic haulss are given a weighting value according to the normalized catch count (number of individuals) of the target species at the biotic station. The weighting value is calculated as catch count divided by towing distance. This normalization makes the stations comparable regardless of catch effort.
#'
#'\strong{SumWeightedCount}
#'
#'The assigned biotic haulss are given a weighting value according to the estimated normalized length distribution count (number of individuals in all length groups) of the target species at the biotic station. It is a requirement that the lengthdistribution data is of distribution type \emph{Normalized} (normalized to one nautical mile towing distance).
#'
#'\strong{InvSumWeightedCount}
#'
#'The assigned biotic haulss are given a weighting value as the inverse sum of the count of all length groups and all species. The weighting value \eqn{w_b} is calculated as:
#'
#'\deqn{w_b = \frac{1}{\sum_{s_b}^{n_b} \sum_{l=1}^{m_{s,b}} c_{l,s,b} }}
#'
#'where:
#'
#'\eqn{w_b} = weighting value of biotic haul \emph{b}
#'
#'\eqn{s_b} = species in the  biotic haul \emph{b}
#'
#'\eqn{n_b} = number of species in the input data off biotic haul \emph{b}
#'
#'\eqn{l} =  length group number
#'
#'\eqn{m_{s,b}}  =	number of length groups for species \emph{s} in biotic haul \emph{b}
#'
#'\eqn{c_{l,s,b}} = count in length group \emph{l} for species \emph{s} in biotic haul \emph{b}
#' 
#'The method is commonly used in split NASC (nautical area scattering coefficient) models to split an acoustic category of several species by using the length distributions of the these species. The sum of the splitted NASC values of all the species will be equal to the NASC of the original combined acoustic multispecies category. By multiplying the calculated weighting value from this method, by the original (input) numbers in each length group for all species, a relative station length distribution can later be made (sum of length groups for all species is 1) and used in the split NASC process.
#'
#'\strong{NASC}
#'
#'The assigned biotic haulss are given weighting variable values with the basis in the surrounding NASC values. By combining these NASC values with the length distribution of the biotic haul, a density as number of fish per square nautical mile is calculated and used as the weighting variable value for each biotic haul.
#'
#'A search for acoustic NASC values (at EDSU resolution) is performed within a given radius around a biotic station. A weighted (by integrator distance of the EDSUs) mean NASC is calculated from the surrounding NASC values and this is used in the further weighting value calculations. Using this combined NASC value, the length distribution of the biotic station and a target strength (TS) versus length empirical relationship, the weighting variable density of the biotic station is first calculated by length group. The sum of densities (number per square nautical mile) for all length groups of the target species at the given biotic haul, is than calculated and applied as the weighting variable for the biotic haul.Note that if an EDSU NASC value is used for assignment to several biotic stations, the NASC value is split and devided between these biotic stations.
#'
#'The NASC WeightingMethod is associated with the following user parameters:
#'
#'\emph{Radius}: Search radius (nautical miles) for NASC values (at EDSU resolution) around a biotic station
#'
#'\emph{LengthExponent}: LengthExponent in the target stregth versus length formula.
#'
#' \emph{TargetStrength0}:  TargetStrength0 in the target stregth versus length formula as:
#'
#'\deqn{TS = LengthExponent \log_{10}{l} + TargetStrength0}
#'
#'where \emph{l} is the fish "total length" in centimeters given as the lower value of the length group interval. 
#'  
#' 
#' @return
#' An object of StoX data type \code{\link{BioticAssignment}}.
#' 
#' @seealso \code{\link{DefineBioticAssignment}} for generating BioticAssignment.
#' 
#' @export
#'
BioticAssignmentWeighting <- function(
    BioticAssignment, 
    #WeightingMethod = c("Equal", "NumberOfLengthSamples", "NASC", "NormalizedTotalWeight", "NormalizedTotalCount", "SumWeightedCount", "InverseSumWeightedCount"), 
    WeightingMethod = c("Equal", "NumberOfLengthSamples", "NormalizedTotalWeight", "NormalizedTotalCount", "SumWeightedCount", "InverseSumWeightedCount"), 
    StoxBioticData, 
    LengthDistributionData, 
    MaxNumberOfLengthSamples = 100, 
    StoxAcousticData, Radius = double(), LengthExponent = double()
) {
    
    # NOTE: This function assumes that the data variable in LengthDistributionData is "WeightedCount". If this is changed the function will not work.
    
    # Get the DefinitionMethod:
    WeightingMethod <- match.arg(WeightingMethod)
    
    # Define the weighting variable:
    weightingVariable <- getDataTypeDefinition(dataType = "BioticAssignment", elements = "weighting", unlist = TRUE)
    #weightedCountVariable <- getDataTypeDefinition(dataType = "LengthDistributionData", elements = "data", unlist = TRUE)

    # Make a copy of the BioticAssignment to enable safe modification by reference:
    BioticAssignmentCopy <- data.table::copy(BioticAssignment)
    
    # Put equal weight (1) to each haul:
    #if(WeightingMethod == "Equal") {
    #    # Simply set WeightingFactor to 1
    #    #BioticAssignmentCopy[, eval(weightingVariable) := 1]
    #}
    ## Weight hauls by the number of length samples (count the length samples for which IndividualTotalLength is not NA):
    #else 
    if(WeightingMethod == "NumberOfLengthSamples") {
        # Merge Haul and Individual, and count individuals with length for each Haul:
        Haul_Individual <- merge(StoxBioticData$Haul, StoxBioticData$Individual)
        NumberOfLengthSamples <- Haul_Individual[, .(NumberOfLengthSamples = as.double(sum(!is.na(IndividualTotalLength)))), by = "Haul"]
        # Apply the MaxNumberOfLengthSamples:
        NumberOfLengthSamples[NumberOfLengthSamples > MaxNumberOfLengthSamples, NumberOfLengthSamples := MaxNumberOfLengthSamples]
        
        # Merge into the BioticAssignmentCopy and set the weightingVariable to the NumberOfLengthSamples
        #BioticAssignmentCopy <- merge(BioticAssignmentCopy, NumberOfLengthSamples, by = "Haul")
        #BioticAssignmentCopy[, eval(weightingVariable) := NumberOfLengthSamples]
        BioticAssignmentCopy <- mergeIntoBioticAssignment(
            BioticAssignment = BioticAssignmentCopy, 
            toMerge = NumberOfLengthSamples, 
            variable = "NumberOfLengthSamples", 
            weightingVariable = weightingVariable, 
            # Important: We need to preserve all Hauls of the BioticAssignmentCopy:
            all.x = TRUE
        )
    }
    # Search around each station for the NASC values inside the range 'Radius':
    else if(WeightingMethod == "NASC") {
        warning("Not implemented")
        # Merge the Station and Haul table:
        ###if(any(unlist(LengthDistributionData[, lapply(.SD, function(x) all(is.na(x))), .SDcols = c("Station", "Haul")]))) {
        ###    stop("LengthDistributionData must have horizontal/vertical resolution Station/Haul (the finest resolution)")
        ###}
        stationInfo <- unique(LengthDistributionData[, c("Station", "Haul", "DateTime", "Longitude", "Latitude")])
        # Get unique hauls in BioticAssignmentCopy:
        uniqueHauls <- BioticAssignmentCopy[, .(Haul = unique(Haul))]
        # Get the position and NASC from the StoxAcousticData:
        EDSUInfo <- RstoxData::MergeStoxAcoustic(StoxAcousticData)
        
        # Get the average NASC around each haul:
        NASCData <- uniqueHauls[, 
            NASC := getAverageNASCInsideRadius(
                thisHaul = Haul, 
                stationInfo = stationInfo, 
                EDSUInfo = EDSUInfo, 
                Radius = Radius
            ), by = "Haul"
        ]
        
        # Change the weights to the average NASC:
        BioticAssignmentCopy <- merge(
            BioticAssignmentCopy, 
            NASCData, 
            by = "Haul"
        )
        BioticAssignmentCopy[, WeightingFactor := NASC]
        
        stop("Unfinished method")
    }
    # Weight hauls by the summed CatchFractionWeight divided by the EffectiveTowDistance:
    else if(WeightingMethod == "NormalizedTotalWeight") {
        # Merge Haul and Sample, and sum the catch weight divided by towed distance:
        Haul_Sample <- merge(StoxBioticData$Haul, StoxBioticData$Sample)
        NormalizedTotalWeight <- Haul_Sample[, .(NormalizedTotalWeight = sum(CatchFractionWeight) / EffectiveTowDistance[1]), by = "Haul"]
        # Merge into the BioticAssignmentCopy and set the weightingVariable to the NumberOfLengthSamples
        BioticAssignmentCopy <- mergeIntoBioticAssignment(
            BioticAssignment = BioticAssignmentCopy, 
            toMerge = NormalizedTotalWeight, 
            variable = "NormalizedTotalWeight", 
            weightingVariable = weightingVariable
        )
        
        #BioticAssignmentCopy <- merge(BioticAssignmentCopy, NormalizedTotalWeight, by = "Haul")
        #BioticAssignmentCopy[, eval(weightingVariable) := NormalizedTotalWeight]        
    }
    # Weight hauls by the summed CatchFractionCount divided by the EffectiveTowDistance:
    else if(WeightingMethod == "NormalizedTotalCount") {
        # Merge Haul and Sample, and sum the catch count divided by towed distance:
        Haul_Sample <- merge(StoxBioticData$Haul, StoxBioticData$Sample)
        NormalizedTotalCount <- Haul_Sample[, .(NormalizedTotalCount = sum(CatchFractionCount) / EffectiveTowDistance[1]), by = "Haul"]
        # Merge into the BioticAssignmentCopy and set the weightingVariable to the NumberOfLengthSamples
        BioticAssignmentCopy <- mergeIntoBioticAssignment(
            BioticAssignment = BioticAssignmentCopy, 
            toMerge = NormalizedTotalCount, 
            variable = "NormalizedTotalCount", 
            weightingVariable = weightingVariable
        )
        #BioticAssignmentCopy <- merge(BioticAssignmentCopy, NormalizedTotalCount, by = "Haul")
        #BioticAssignmentCopy[, eval(weightingVariable) := NormalizedTotalCount]
    }
    # Weight hauls by the summed CatchFractionCount divided by the EffectiveTowDistance:
    else if(WeightingMethod == "SumWeightedCount") {
        BioticAssignmentCopy <- addSumWeightedCount(
            BioticAssignment = BioticAssignmentCopy, 
            LengthDistributionData = LengthDistributionData, 
            weightingVariable = weightingVariable, 
            inverse = FALSE
        )
    }
    else if(WeightingMethod == "InverseSumWeightedCount") {
        BioticAssignmentCopy <- addSumWeightedCount(
            BioticAssignment = BioticAssignmentCopy, 
            LengthDistributionData = LengthDistributionData, 
            weightingVariable = weightingVariable, 
            inverse = TRUE
        )
    }
    
    # Format the output:
    formatOutput(BioticAssignmentCopy, dataType = "BioticAssignment", keep.all = FALSE)
    
    return(BioticAssignmentCopy)
}


mergeIntoBioticAssignment <- function(BioticAssignment, toMerge, variable, weightingVariable, ...) {
    BioticAssignment <- merge(BioticAssignment, toMerge, by = "Haul", ...)
    BioticAssignment[, eval(weightingVariable) := as.double(get(variable)) * get(weightingVariable)]
    return(BioticAssignment)
}


isLengthDistributionType <- function(LengthDistributionData, LengthDistributionType) {
    LengthDistributionData$LengthDistributionType[1] == LengthDistributionType
}

# Function to get great circle distance between a Haul and the EDSUs:
getHaulToEDSUDistance <- function(thisHaul, stationInfo, EDSUInfo) {
    # Get the distance from the station of the Haul to the EDSUs:
    stationPosition <- stationInfo[Haul == thisHaul, c("Longitude", "Latitude")]
    # Get the distance using WGS84 ellipsoid:
    haulToEDSUDistance <- sp::spDistsN1(
        pts = as.matrix(EDSUInfo[, c("Longitude", "Latitude")]), 
        pt = as.matrix(stationPosition), 
        longlat = TRUE
    )
    # plot(NSAC$Longitude, NSAC$Latitude, cex = haulToEDSUDistance/max(haulToEDSUDistance) * 5)
    return(haulToEDSUDistance)
}
# Function to get great circle distance between a Haul and the EDSUs:
getEDSUsInsideRadius <- function(thisHaul, stationInfo, EDSUInfo, Radius) {
    # Get the distance from the station of the Haul to the EDSUs:
    haulToEDSUDistance <- getHaulToEDSUDistance(
        thisHaul = thisHaul, 
        stationInfo = stationInfo, 
        EDSUInfo = EDSUInfo
    )
    
    # Identify EDSUs within the specified radius:
    EDSUsInsideRadius <- haulToEDSUDistance <= Radius
    return(EDSUsInsideRadius)
}
# Function to get average NASC around one haul:
getAverageNASCInsideRadius <- function(thisHaul, stationInfo, EDSUInfo, Radius) {
    # Identify EDSUs within the specified radius:
    EDSUsInsideRadius <- getEDSUsInsideRadius(
        thisHaul = thisHaul, 
        stationInfo = stationInfo, 
        EDSUInfo = EDSUInfo, 
        Radius = Radius
    )
    
    # Get the average NASC inside of the radius (across Frequency and AcosuticCategory):
    averageNASC <- EDSUInfo[EDSUsInsideRadius, mean(NASC, na.rm = TRUE)]
    return(averageNASC)
}

# Function to sum up the WeightedCount
addSumWeightedCount <- function(BioticAssignment, LengthDistributionData, weightingVariable, inverse = FALSE) {
    
    # Make a copy of the LengthDistributionData to enable safe modification by reference:
    LengthDistributionDataCopy <- data.table::copy(LengthDistributionData)
    
    # Normalize the WeightedCount:
    if(isLengthDistributionType(LengthDistributionData, "Standard")) {
        LengthDistributionData[, WeightedCount := WeightedCount / EffectiveTowDistance]
    } 
    else if(!isLengthDistributionType(LengthDistributionData, "Normalized")) {
        stop("The LengthDistributionType must be \"Standard\" (in which case the WeightedCount will be divided by EffectiveTowDistance) or \"Normalized\"")
    }
    # Sum the normalized WeightedCount for each Haul:
    SumWeightedCount <- LengthDistributionData[, .(SumWeightedCount = sum(WeightedCount, na.rm = TRUE)), by = "Haul"]
    
    # Merge the NormalizedTotalWeight into the BioticAssignment by the Haul identifyer: 
    BioticAssignment <- merge(BioticAssignment, SumWeightedCount, by = "Haul")
    
    # Copy the NormalizedTotalWeight into the weightingVariable:
    if(inverse) {
        BioticAssignment[, eval(weightingVariable) := 1 / SumWeightedCount]
    }
    else {
        BioticAssignment[, eval(weightingVariable) := SumWeightedCount]
    }
    
    BioticAssignment[]
}


##################################################
#' Acoustic target strength definition
#' 
#' This function returns a table of parameters specifying the acoustic target strength as a function of length for different values of user selected variables in the NASC data.
#' 
#' @inheritParams general_arguments
#' @param TargetStrengthMethod  Character: The target strength methdo/function to use. Currently implemented are "LengthDependent", "LengthAndDepthDependent", "LengthExponent" and "TargetStrengthByLength". See Details.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "TargetStrengthTable", for providing the acoustic target strength parameters in the table \code{TargetStrengthTable}; and "ResourceFile" for reading the acoustic tfarget strength table from the text file \code{FileName}.
#' @param TargetStrengthTable A table holding the specification of the target strength function/table. The first two columns are AcocusticCategory and Frequenccy. See details for other columns.
#' @param FileName A file from which to read the \code{TargetStrengthTable}.
#' 
#' @details
#' The \code{TargetStrengthMethod} has the following possible values: 
#' \enumerate{
#'   \item LengthDependent, applying the logarithmic function TargetStrength = Targetstrength0 + LengthExponent * log10(Length). Required columns: Targetstrength0 and LengthExponent.
#'   \item LengthAndDepthDependent, applying the logarithmic function TargetStrength = Targetstrength0 + LengthExponent * log10(Length) + DepthExponent * log10(1 + Depth/10). Required columns: Targetstrength0, LengthExponent and DepthExponent.
#'   \item TargetStrengthByLength, applying a table of TargetStrength and TotalLength. Required columns: TargetStrength and TotalLength.
#'   \item LengthExponent, applying the logarithmic function TargetStrength = LengthExponent * log10(Length). Required columns: LengthExponent.
#' }
#' The parameters/values can be given by tables with the first columns being AcousticCategory and Frequency, or as a csv file.
#' 
#' @return
#' An \code{\link{AcousticTargetStrength}} object.
#' 
#' @seealso \code{\link{AcousticDensity}} for applying the AcousticTargetStrength.
#' 
#' @export
#' 
DefineAcousticTargetStrength <- function(
    processData, UseProcessData = FALSE, 
    # Note that "LengthExponent" is an option for TargetStrengthMethod (used by BioticAssignmentWeighting()), but this is not shown.
    DefinitionMethod = c("ResourceFile", "TargetStrengthTable"),
    TargetStrengthMethod = c("LengthDependent", "LengthAndDepthDependent", "TargetStrengthByLength"), 
    TargetStrengthTable = data.table::data.table(), 
    FileName
) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the methods:
    TargetStrengthMethod <- match.arg(TargetStrengthMethod)
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    # Get or read the TargetStrength and return in a list with the TargetStrengthMethod:
    AcousticTargetStrength <- getAcousticTargetStrength(
        TargetStrengthMethod = TargetStrengthMethod, 
        DefinitionMethod = DefinitionMethod, 
        TargetStrengthTable = TargetStrengthTable, 
        #TargetStrength = get(paste0(TargetStrengthMethod, "Table")), 
        FileName = FileName
    )
    
    return(AcousticTargetStrength)
}

getAcousticTargetStrength <- function(TargetStrengthMethod, DefinitionMethod, TargetStrengthTable, FileName) {
    
    # Read the table if requested, or issue an error if not given:
    if(DefinitionMethod == "Table") {
        if(length(TargetStrengthTable) == 0) {
            stop(TargetStrengthMethod, "TargetStrengthTable must be given if DefinitionMethod = \"TargetStrengthTable\"")
        }
    }
    else if(DefinitionMethod == "ResourceFile") {
        TargetStrengthTable <- data.table::fread(FileName)
    }
    
    # Check the columns of the table:
    checkTargetStrength(TargetStrengthTable, TargetStrengthMethod)
    
    # Define the output AcousticTargetStrength as a list of the method and the table:
    AcousticTargetStrength <- list(
        TargetStrengthMethod = data.table::data.table(TargetStrengthMethod = TargetStrengthMethod), 
        TargetStrengthTable = TargetStrengthTable
    )
    
    return(AcousticTargetStrength)
}


checkTargetStrength <- function(TargetStrengthTable, TargetStrengthMethod) {
    
    # Get and check the TargetStrengthMethod:
    targetStrengthParameters <- getRstoxBaseDefinitions("targetStrengthParameters")
    if(! TargetStrengthMethod %in% names(targetStrengthParameters)) {
        stop("Wrong TargetStrengthMethod. Must be one of ", paste(names(targetStrengthParameters), collapse = ", "))
    }
    
    # Check that the TargetStrengthTable contains the required columns:
    if(! all(targetStrengthParameters[[TargetStrengthMethod]] %in% names(TargetStrengthTable))) {
        stop("The ", TargetStrengthMethod, "Table must contain the required column; ", paste(targetStrengthParameters[[TargetStrengthMethod]], collapse = ", "))
    }
    
    # Check for duplicated keys:
    keys <- setdiff(
        names(TargetStrengthTable), 
        targetStrengthParameters[[TargetStrengthMethod]]
    )
    dup <- duplicated(TargetStrengthTable[, ..keys])
    if(any(dup)) {
        duprev <- duplicated(TargetStrengthTable[, ..keys], fromLast = TRUE)
        alldup <- sort(unique(c(which(dup), which(duprev))))
        stop("The output from DefineAcousticTargetStrength contains duplicated keys (", paste(keys, collapse = ", "), ")", " in rows ", paste(alldup, collapse = ", "), ".")
    }
}




##################################################
##################################################
#' Biotic Survey
#' 
#' This function defines the Strata associated to different surveys (in the sense that a separate estimate should be made for those strata). 
#' 
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @param DefinitionMethod Character: A string naming the method to use, one of "AllStrata", which defines all strata as the same survey named "Survey"; and "SurveyTable", which requires the \code{SurveyTable} to be given.
#' @param SurveyTable A table of the two columns Stratum and Survey.
#' 
#' @return
#' An object of StoX data type \code{\link{BioticPSU}}.
#' 
#' @seealso Acousic PSUs are generated using \code{\link{DefineAcousticPSU}}. For the vertical resolution (Layer) see \code{\link{DefineBioticLayer}} and \code{\link{DefineAcousticLayer}}.
#' 
#' @export
#' 
DefineSurvey <- function(
    processData, UseProcessData = FALSE, 
    DefinitionMethod = c("AllStrata", "SurveyTable"), 
    StratumPolygon, 
    SurveyTable = data.table::data.table()
) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    # Get the survey table using the stratum names:
    stratumNames <- getStratumNames(StratumPolygon)
    SurveyTable <- getSurveyTable(
        DefinitionMethod = DefinitionMethod, 
        stratumNames = stratumNames, 
        SurveyTable = SurveyTable
    )
        
    return(SurveyTable)
}


getSurveyTable <- function(
    DefinitionMethod, 
    stratumNames, 
    SurveyTable = data.table::data.table()
) {
    
    # Define one single survey:
    if(DefinitionMethod == "AllStrata") {
        SurveyTable <- data.table::data.table(
            Stratum = stratumNames, 
            Survey = "Survey"
        )
    }
    # Or accept/reject the input SurveyTable:
    else if(DefinitionMethod == "SurveyTable") {
        # Delete rows with missing Survey:
        if(any(is.na(SurveyTable$Survey))) {
            warning("Removing rows of missing Survey in SurveyTable")
            SurveyTable <- SurveyTable[!is.na(Survey)]
        }
        if(any(is.na(SurveyTable$Stratum))) {
            warning("Removing rows of missing Stratum in SurveyTable")
            SurveyTable <- SurveyTable[!is.na(Stratum)]
        }
        # Delete also rows with unrecognized Stratum:
        if(!all(SurveyTable$Stratum %in% stratumNames)) {
            warning("Removing rows of Stratum not present in the SurveyTable")
            SurveyTable <- SurveyTable[Stratum %in% stratumNames, ]
        }
        # If no rows in the SurveyTable, issue an error:
        if(!nrow(SurveyTable)) {
            stop("SurveyTable must be a table of at least one row, with Stratum and Survey as columns")
        }
    }
    
    return(SurveyTable)
}


