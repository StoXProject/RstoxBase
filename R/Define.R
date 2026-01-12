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
#' @param FileName The path to a resource file from which to read PSUs, in the case that \code{DefinitionMethod} is "ResourceFile". Currently, only a project.xml file from StoX 2.7 can be read. Must include file extension.
#' @param SavePSUByTime Logical: If TRUE save the start and end times of sequences of EDSUs or Stations for each PSU.
#' @param PSUProcessData Previously generated PSU process data, one of \code{\link{AcousticPSU}} or \code{\link{BioticPSU}}.
#' 
#' @return
#' A list of three objects, Stratum_PSU, SSU_PSU and PSUByTime in the case that SavePSUByTime is TRUE.
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
    DefinitionMethod = c("Manual", "Identity", "DeleteAllPSUs", "PreDefined", "ResourceFile"), 
    FileName = character(), 
    #IntervalVariable = character(),
    #Interval = double(), 
    PSUType = c("Acoustic", "Biotic"), 
    SavePSUByTime = FALSE, 
    PSUProcessData
) {
    
    # Get the DefinitionMethod and PSUType:
    DefinitionMethod <- RstoxData::match_arg_informative(DefinitionMethod)
    PSUType <- RstoxData::match_arg_informative(PSUType)
    
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
            stop("One of MergedStoxDataStationLevel and StoxData must be given in order to check the validity of the PSUs.")
        }
    }
    
    # Define the PSU prefix and the SSU label, which is the name of the SSU column:
    prefix <- getRstoxBaseDefinitions("getPSUPrefix")(PSUType)
    SSULabel <- getRstoxBaseDefinitions("getSSULabel")(PSUType)
    
    # Make sure that there is only one row per SSU in the MergedStoxDataStationLevel:
    notDuplicatedSSUs <- !duplicated(MergedStoxDataStationLevel[[SSULabel]])
    MergedStoxDataStationLevel <- MergedStoxDataStationLevel[notDuplicatedSSUs, ]
    
    # This is taken care of in formatOutput():
    ### # And order the SSUs by time:
    ### data.table::setorderv(MergedStoxDataStationLevel, "DateTime")
    
    # Get SSUs, as it is needed for more than one DefinitionMethod (Identity, DeleteAllPSUs, Manual):
    SSUs <- MergedStoxDataStationLevel[[SSULabel]]
    
    # If UseProcessData = TRUE, from "EDSU"/"Station" to "SSU": 
    if(UseProcessData) {
        if(!length(processData)) {
            # Create and return empty PSU process data:
            processData <- list(
                SSU_PSU = data.table::data.table(
                    SSU = SSUs, 
                    PSU = NA_character_
                ), 
                Stratum_PSU = data.table::data.table()
            )
            processData <- renameSSULabelInPSUProcessData(processData, PSUType = PSUType, reverse = FALSE)
            return(processData)
        }
        # Otherwise rename to SSU:
        else {
            processData <- renameSSULabelInPSUProcessData(processData, PSUType = PSUType, reverse = TRUE)
        }
    }
    else {
        # If DefinitionMethod is "ResourceFile", read from a project.xml file:
        if(grepl("ResourceFile", DefinitionMethod, ignore.case = TRUE)) {
            if(PSUType == "Acoustic") {
                ## Read from the project.xml:
                if(basename(FileName) == "project.json") {
                    AcousticPSU <- readOneProcessDataByFunctionNameFromProjectJSON(FileName, functionName = "DefineAcousticPSU")
                }
                else if(tolower(getResourceFileExt(FileName)) == "xml" && any(grepl("http://www.imr.no/formats/stox/v1", readLines(FileName, 5)))) {
                    AcousticPSU <- readAcousticPSUFrom2.7(FileName)
                }
                else {
                    # Read the file as a table:
                    PSUByTimeFromFile <- tryCatch(
                        data.table::fread(FileName), 
                        error = function(err) {
                            stop("StoX: Error in data.table::fread: ", err)
                        }
                    )
                    
                    # Check whether all required columns are present:
                    requiredPSUByTimeColumns <- getDataTypeDefinition(dataType = "AcousticPSU", subTable = "PSUByTime", unlist = TRUE)
                    if(all(requiredPSUByTimeColumns %in% names(PSUByTimeFromFile))) {
                        AcousticPSU <- getPSUProcessDataFromPSUByTime(
                            PSUByTimeFromFile, 
                            MergedStoxDataStationLevel = MergedStoxDataStationLevel, 
                            PSUType = "Acoustic"
                        )
                        # Rename form SSU to EDSU:
                        AcousticPSU <- renameSSULabelInPSUProcessData(AcousticPSU, PSUType = PSUType, reverse = FALSE)
                    }
                    else {
                        stop("StoX: Invalid file. Must be a StoX project description file with file name project.json or project.xml, or a file with a PSUByTime table containing the columns ", paste(requiredPSUByTimeColumns, collapse = ", "), ".")
                    }
               }
                
                # Add the PSUByTime:
                if(SavePSUByTime) {
                    AcousticPSU$PSUByTime <- getPSUByTime(
                        AcousticPSU, 
                        MergedStoxDataStationLevel = MergedStoxDataStationLevel, 
                        PSUType = PSUType
                    )
                }
                return(AcousticPSU)
            }
            else if(PSUType == "Biotic") {
                
                if(basename(FileName) == "project.json") {
                    BioticPSU <- readOneProcessDataByFunctionNameFromProjectJSON(FileName, functionName = "DefineBioticPSU")
                }
                else if(tolower(getResourceFileExt(FileName)) == "xml" && any(grepl("http://www.imr.no/formats/stox/v1", readLines(FileName, 5)))) {
                    # Read from the project.xml:
                    MergedStoxDataHaulLevel <- RstoxData::mergeDataTables(
                        StoxData, 
                        tableNames = c("Cruise", "Station", "Haul"), 
                        output.only.last = TRUE, 
                        all = TRUE
                    )
                    
                    BioticPSU <- readBioticPSUFrom2.7(FileName, MergedStoxDataHaulLevel = MergedStoxDataHaulLevel)
                }
                else {
                    stop("StoX: Invalid file. Must be a StoX project description file with file name project.json or project.xml")
                }
                
                return(BioticPSU)
            }
            else {
                stop("Invalid PSUType, must be one of \"Acoustic\" and \"Biotic\"")
            }
        }
        
        # If DefinitionMethod = "Identity", use each SSU as a PSU:
        if(grepl("Identity", DefinitionMethod, ignore.case = TRUE)) {
            
            # Define PSUIDs and PSUNames:
            PSUID <- seq_along(SSUs)
            PSUName <- getPSUName(PSUID, prefix)
            
            # Set each SSU as a PSU:
            SSU_PSU <- data.table::data.table(
                SSU = SSUs, 
                PSU = PSUName
            )
            
            # Find the stratum of each PSU (here s2 is used inside getStratumOfSSUs_SF() when locateInStratum(), so we do not apply s2 around this):
            #turn_off_s2(
                Stratum_PSU <- getStratumOfSSUs_SF(SSU_PSU, MergedStoxDataStationLevel, StratumPolygon, SSULabel, StationLevel)#, 
            #    msg = FALSE
            #)
                
            # Define the processData:
            processData <- list(
                Stratum_PSU = Stratum_PSU, 
                SSU_PSU = SSU_PSU
            )
        }
        # If DefinitionMethod = "PreDefined" use times to interpret the PSUs (only used for acoustic PSUs):
        else if(grepl("PreDefined", DefinitionMethod, ignore.case = TRUE)) {
            
            processData <- getPSUProcessDataFromPSUByTime(
                PSUProcessData$PSUByTime, 
                MergedStoxDataStationLevel = MergedStoxDataStationLevel, 
                PSUType = PSUType
            )
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
        #    Stratum_PSU <- getStratumOfSSUs_SP(SSU_PSU, MergedStoxDataStationLevel, StratumPolygon, SSULabel, Stat#ionLevel)
        #}
        
        
        # If DefinitionMethod = "DeleteAllPSUs" return empty Stratum_PSU and SSU_PSU with all SSUs and empty string as PSU:
        else if(grepl("DeleteAllPSUs", DefinitionMethod, ignore.case = TRUE)) {
            SSU_PSU <- data.table::data.table(
                SSU = SSUs, 
                PSU = NA_character_
            )
            Stratum_PSU <- data.table::data.table()
            
            # Define the processData:
            processData <- list(
                Stratum_PSU = Stratum_PSU, 
                SSU_PSU = SSU_PSU
            )
        }
        # Manual implies to use the existing process data, or create an empty set if not present:
        else if(grepl("Manual", DefinitionMethod, ignore.case = TRUE)) {
            if(length(processData)) {
                processData <- renameSSULabelInPSUProcessData(processData, PSUType = PSUType, reverse = TRUE)
                #return(processData)
                SSU_PSU <- processData$SSU_PSU
                Stratum_PSU <- processData$Stratum_PSU
            }
            else {
                SSU_PSU <- data.table::data.table(
                    SSU = SSUs, 
                    PSU = NA_character_
                )
                Stratum_PSU <- data.table::data.table()
            }
            
            # Define the processData:
            processData <- list(
                Stratum_PSU = Stratum_PSU, 
                SSU_PSU = SSU_PSU
            )
        }
        else {
            stop("Inavlid DefinitionMethod")
        }
    }
    
    
    #### Warning action 1: ####
    # Warning if the is no intersection between the SSUs of the processData and StoxData. This can sometimes happen if the user makes a copy of an old project and does not redefine the PSUs:
    allSSUsMissing <- !length(intersect(SSUs, processData$SSU_PSU$SSU))
    if(allSSUsMissing) {
        warning(
            "StoX: All ", 
            SSULabel, "s", 
            " that are present as tagged to one or more PSUs in the process data are missing in the ", 
            "Stox", PSUType, "Data. If you are using a copy of an old project with new data, please be aware that DefineAcousticPSU and DefineBioticAssignment processes contains the process data of the old project, which usually does not fit to the new data. Rerun these processes with UseProcessData turned off, and an appropriate DefinitionMethod."
        )
    }
    
    #### Warning action 2: ####
    # Remove any SSUs that are not tagged to a PSU and that are missing in the StoxData. This should be quite safe, and fixes an obvivous error in the tagging:
    unusedAndMissingInStoxData <- processData$SSU_PSU[, is.na(PSU) & ! SSU %in% SSUs]
    if(any(unusedAndMissingInStoxData, na.rm = TRUE)) {
        processData$SSU_PSU <- subset(processData$SSU_PSU, !unusedAndMissingInStoxData)
    }
    
    #### Warning action 3: ####
    # Add any SSUs present in the StoxData that are not present in the processData. This is important, as we require that all EDSUs are present in the PSU-data!!:
    missingSSUs <- ! SSUs %in% processData$SSU_PSU$SSU
    if(any(missingSSUs, na.rm = TRUE)) {
        SSU_PSU <- data.table::data.table(
            SSU = SSUs[missingSSUs], 
            PSU = NA_character_
        )
        processData$SSU_PSU <- rbind(
            processData$SSU_PSU, 
            SSU_PSU
        )
    }
    
    #### Warning action 4: ####
    # Remove any empty string PSUs, which were created with RstoxFramework::removeEDSU in RstoxFramework <= 3.3.5:
    atEmptyStringPSUs <- processData$SSU_PSU[, nchar(PSU) == 0]
    if(any(atEmptyStringPSUs, na.rm = TRUE)) {
        processData$SSU_PSU[atEmptyStringPSUs, PSU := NA_character_]
    }
    
    #### Warning action 5: ####
    # Add a warning if there are EDSUs that are tagged but not present in the StoxAcousticData:
    usedButMissingInStoxData <- processData$SSU_PSU[, !is.na(PSU) & ! SSU %in% SSUs]
    if(any(usedButMissingInStoxData, na.rm = TRUE) && !allSSUsMissing) {
        #SSULabel <- switch(PSUType, Acoustic = "EDSU", Biotic = "Station")
        
        # Get the SSUs that are missing in the StoxData:
        usedButMissingInStoxData_SSU <- processData$SSU_PSU[usedButMissingInStoxData, SSU]
        # Get a table of Stratum, PSU and SSU for these SSUs:
        Stratum_PSU_SSU <- merge(processData$Stratum_PSU, processData$SSU_PSU, by = "PSU", all = TRUE)
        Stratum_PSU_SSU <- subset(Stratum_PSU_SSU, SSU %in% usedButMissingInStoxData_SSU)
        data.table::setnames(Stratum_PSU_SSU, "SSU", SSULabel)
        # Reorder and paste to one line per SSU:
        data.table::setcolorder(Stratum_PSU_SSU, c("Stratum", "PSU", SSULabel))
        usedButMissingInStoxDataInfo <- do.call(paste, c(Stratum_PSU_SSU, list(sep = ",")))
        
        warning(
            "StoX: There are ", sum(usedButMissingInStoxData), " out of ", length(usedButMissingInStoxData), " ", 
            SSULabel, if(length(usedButMissingInStoxData) > 1) "s", 
            " that are present as tagged to one or more PSUs in the process data, but that are not present in the ", 
            "Stox", PSUType, "Data. This indicates that data used when defining the PSUs have been removed either in the input data or using a filter. StoX should ignore these ", 
            SSULabel, if(length(usedButMissingInStoxData) > 1) "s", " but for clarity it is advised to remove them from the PSUs. This can be done manually in the Stratum/PSU winidow of the StoX GUI, or by re-running the Define", PSUType, "PSU proecss with an automatic DefinitionMethod. The following Stratum,PSU,", SSULabel, " are not present in the Stox", PSUType, "Data: \n", RstoxData::printErrorIDs(usedButMissingInStoxDataInfo)
        )
    }
    
    # Remove PSUs that do not have a stratum:
    processData <- removePSUsWithMissingStratum(processData, SSULabel = SSULabel)
    
    # Remove empty PSUs:
    processData <- removeEmptyPSUs(processData)
    
    # Rename back from "SSU" to "EDSU"/"Station": 
    processData <- renameSSULabelInPSUProcessData(processData, PSUType = PSUType, reverse = FALSE)
    
    # Add the PSU time information:
    if(SavePSUByTime) {
        # If UseProcessData is TRUE check whether recalculated PSUByTime differs from the existing in processData. If differing, give a message to the user that the process can be re-run with DefinitionMethod "Manual" in order to update the PSUByTime:
        if(UseProcessData) {
            
            # If the PSUByTime is missing altogether, warn the user specifically:
            if(!NROW(processData$PSUByTime)) {
                message("StoX: The table PSUByTime is empty. If you plan to use this process as input to another PSU process using the DefinitionMethod \"PreDefined\", please re-run the process with DefinitionMethod \"Manual\" to produce the PSUByTime table.")
            }
            
            # Calculate the PSUByTime:
            newPSUByTime <- getPSUByTime(
                processData, 
                MergedStoxDataStationLevel = MergedStoxDataStationLevel, 
                PSUType = PSUType
            )
            # Compare to existing:
            differs <- !isTRUE(all.equal(processData$PSUByTime, newPSUByTime))
            if(differs) {
                message("StoX: The table PSUByTime does not match the PSUs. If you plan to use this process as input to another PSU process using the DefinitionMethod \"PreDefined\", please re-run the process with DefinitionMethod \"Manual\" to produce the PSUByTime table.")
            }
        }
        else {
            processData$PSUByTime <- getPSUByTime(
                processData, 
                MergedStoxDataStationLevel = MergedStoxDataStationLevel, 
                PSUType = PSUType
            )
        }
        
        
    }
    
    return(processData)
}


getPSUProcessDataFromPSUByTime <- function(PSUByTime, MergedStoxDataStationLevel, PSUType = c("Acoustic", "Biotic")) {
    # Interpret middle times:
    MergedStoxDataStationLevel <- StoxDataStartMiddleStopDateTime(MergedStoxDataStationLevel)
    
    # Rename the SSULabel to "SSU":
    MergedStoxDataStationLevel <- renameSSUToSSULabelInTable(MergedStoxDataStationLevel, PSUType = PSUType, reverse = TRUE)
    
    # Get the SSU indices for each PSU:
    Stratum_PSU_SSU <- PSUByTime[, data.table::data.table(
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
    by = seq_len(nrow(PSUByTime))]
    
    # Remove PSUs with no SSUs:
    Stratum_PSU_SSU <- Stratum_PSU_SSU[!is.na(SSUIndex), ]
    
    # Add the SSUs:
    Stratum_PSU_SSU[, SSU := MergedStoxDataStationLevel$SSU[SSUIndex]]
    
    # Split into Stratum_PSU and SSU_PSU:
    Stratum_PSU <- unique(Stratum_PSU_SSU[, c("Stratum", "PSU")])
    SSU_PSU <- unique(Stratum_PSU_SSU[, c("SSU", "PSU")])
    
    # Add all SSUs:
    SSU_PSU <- merge(MergedStoxDataStationLevel[, "SSU"], SSU_PSU, all = TRUE)
    
    return(
        list(
            Stratum_PSU = Stratum_PSU, 
            SSU_PSU = SSU_PSU 
        )
    )
}


# Function to get the file extension (used to get the ext of a project description file in particular):
getResourceFileExt <- function(FileName) {
    if(length(unlist(strsplit(FileName, "\\."))) < 2) {
        stop("StoX: FileName must include file extension.")
    }
    else {
        fileParts <- unlist(strsplit(FileName, "\\."))
        FileExt <- utils::tail(fileParts, 1)
    }
    
    return(FileExt)
}




# Function to get the stratum of each PSU, taken as the most frequent Stratum in which the PSU i loacted geographically:
getStratumOfSSUs_SF <- function(SSU_PSU, MergedStoxDataStationLevel, StratumPolygon, SSULabel, StationLevel) {
    # Error if StratumPolygon is missing:
    if(missing(StratumPolygon) || !length(StratumPolygon)) {
        stop("StratumPolygon must be given.")
    }
    
    # Get unique PSUs:
    allPSUs <- unique(SSU_PSU$PSU)
    allPSUs <- allPSUs[!is.na(allPSUs)]
    
    # Get the SSU positions and convert to spatialpoints:
    pos <- MergedStoxDataStationLevel[get(SSULabel) %in% SSU_PSU$SSU, c(SSULabel, "Longitude", "Latitude"), with = FALSE]
    
    # Changed on 2021-09-10 to use getStratumNames():
    #StratumNames <- getStratumNames(sp::over(SpatialPSUs, StratumPolygon), check.unique = FALSE)
    turn_off_s2(
        StratumNames <- locateInStratum(pos, StratumPolygon, SSULabel = SSULabel), 
        msg = FALSE
    )
    
    Stratum_PSU <- data.table::data.table(
        Stratum = unlist(StratumNames), 
        PSU = SSU_PSU$PSU
    )
    
    return(Stratum_PSU)
}

#getStratumOfPSU <- function(thisPSU, SSU_PSU, MergedStoxDataStationLevel, StratumPolygon, SSULabel, StationLevel) {
#    
#    # Get the MergedStoxDataStationLevel of the specified PSU:
#    SSUs <- SSU_PSU[PSU == thisPSU, SSU]
#    pos <- MergedStoxDataStationLevel[get(SSULabel) %in% SSUs, c("Longitude", "Latitude")]
#    SpatialPSUs <- sp::SpatialPoints(pos)
#    # Det the default projection to the points:
#    sp::proj4string(SpatialPSUs) <- getRstoxBaseDefinitions("proj4string")
#    
#    # Find the stratum of each PSU:
#    StratumNames <- sp::over(SpatialPSUs, StratumPolygon)
#    # Select the most frequent:
#    MostFrequentStratumName <- names(which.max(table(StratumNames)))
#    
#    # Create the Stratum_PSU data.table:
#    Stratum_PSU <- data.table::data.table(
#        #Stratum = NonEmptyStrata, 
#        Stratum = if(length(MostFrequentStratumName)) MostFrequentStratumName else NA, 
#        PSU = thisPSU
#    )
#    
#    return(Stratum_PSU)
#}

# Function to remove PSUs with missing Stratum:
removePSUsWithMissingStratum <- function(PSUProcessData, SSULabel) {
    
    validPSUs <- unique(PSUProcessData$Stratum_PSU$PSU[!is.na(PSUProcessData$Stratum_PSU$Stratum)])
    if(!length(validPSUs)) {
        warning("StoX: No PSUs with Stratum")
        return(PSUProcessData)
    }
    invalidPSUs <- setdiff(PSUProcessData$Stratum_PSU$PSU, validPSUs)
    if(length(invalidPSUs)) {
        invalidSSUs <- sapply(lapply(split(subset(PSUProcessData$SSU_PSU, PSU %in% invalidPSUs), by = "PSU"), subset, select = "SSU"), paste, collapse = ", ")
        warning("StoX: Removing the following PSUs with no Stratum:\n\t", paste(paste0(invalidPSUs, "(", SSULabel, ": ", invalidSSUs, ")"), collapse = "\n\t"))
        PSUProcessData$Stratum_PSU <- PSUProcessData$Stratum_PSU[ PSU %in% validPSUs ]
        PSUProcessData$SSU_PSU[! PSU %in% validPSUs, PSU := NA_character_]
    }
    
    return(PSUProcessData)
}

# Function to remove PSUs with missing Stratum:
removeEmptyPSUs <- function(PSUProcessData) {
    emptyPSUs <- setdiff(PSUProcessData$Stratum_PSU$PSU, PSUProcessData$SSU_PSU$PSU)
    if(length(emptyPSUs)) {
        warning("StoX: Removing the following empty PSUs:\n\t", paste(emptyPSUs, collapse = ", "))
        PSUProcessData$Stratum_PSU <- subset(PSUProcessData$Stratum_PSU, ! PSU %in% emptyPSUs)
    }
    
    return(PSUProcessData)
}



#' Rename SSU to either EDSU or Station
#' 
#' @inheritParams general_arguments
#' @param PSUProcessData A table to rename in.
#' @param reverse Logical: If TRUE rename to SSU.
#' 
#' @export
#' 
renameSSULabelInPSUProcessData <- function(PSUProcessData, PSUType = c("Acoustic", "Biotic"), reverse = FALSE) {
    PSUType <- RstoxData::match_arg_informative(PSUType)
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
    PSUType <- RstoxData::match_arg_informative(PSUType)
    SSULabel <- getRstoxBaseDefinitions("getSSULabel")(PSUType)
    
    # Make a copy, since we are using setnames:
    if(is.data.table(table)) {
        table <- data.table::copy(table)
        if(reverse) {
            data.table::setnames(table, SSULabel, "SSU")
        }
        else {
            data.table::setnames(table, "SSU", SSULabel)
        }
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
#' @inheritParams DefinePSU
#' @param DefinitionMethod Character: A string naming the method to use, one of "Manual" for manual tagging of stations to PSUs (not yet implemented); "StationToPSU", which sets each Station as a PSU; "DeleteAllPSUs" to delete all PSUs; and "ResourceFile" to read from a project.xml file from StoX <= 2.7. Note that in the latter case it is assumed that the contents of the project.xml file is actually used in the StoX <= 2.7 project, i.e. that Method = "UseProcessData" in DefineSweptAreaPSU().
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
    DefinitionMethod = c("Manual", "StationToPSU", "DeleteAllPSUs", "ResourceFile"), 
    FileName = character()
) {
    
    #if(!UseProcessData) {
    #    if(grepl("Manual", DefinitionMethod, ignore.case = TRUE)) {
    #        warning("StoX: Manual tagging of stations as biotic PSUs is not yet supported in the StoX GUI")
    #    }
    #}
    
    # Get the DefinitionMethod:
    #DefinitionMethod <- match.arg(DefinitionMethod)
    DefinitionMethod <- if(isEmptyString(DefinitionMethod)) "" else RstoxData::match_arg_informative(DefinitionMethod)
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
        FileName = FileName, 
        PSUType = "Biotic"
    )
    
    # Format the output:
    formatOutput(BioticPSU, dataType = "BioticPSU", keep.all = FALSE)
    
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
#' @param DefinitionMethod Character: A string naming the method to use, one of "Manual" for manual tagging of EDUSs to PSUs (passes the process data through the function unchanged); "EDSUToPSU", which sets each EDSU as a PSU; "DeleteAllPSUs" to delete all PSUs; "PreDefined" to read from a previous process; and "ResourceFile" to read from either a project.xml file from StoX <= 2.7, a project.json file from StoX >=3.0.0, or a file containing a PSUByTime table. 
#' 
#' @details
#'  Note that if the \code{FileName} is a project.xml file, it is assumed that the contents of the file is actually used in the StoX <= 2.7 project, i.e. that UseProcessData = TRUE in DefineAcousticPSU(). 
#'  
#'  If the the \code{FileName} comtains a table with PSUByTime info, the following columns are required: "Stratum", "PSU", "Cruise", "StartDateTime", "StopDateTime".
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
    DefinitionMethod = c("Manual", "EDSUToPSU", "DeleteAllPSUs", "PreDefined", "ResourceFile"), 
    FileName = character(), 
    #IntervalVariable = character(),
    #Interval = double(), 
    #SavePSUByTime = FALSE, 
    AcousticPSU
) {
    
    # Get the DefinitionMethod:
    #DefinitionMethod <- match.arg(DefinitionMethod)
    DefinitionMethod <- if(isEmptyString(DefinitionMethod)) "" else RstoxData::match_arg_informative(DefinitionMethod)
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
        FileName = FileName, 
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
    
    # If empty process data:
    if(!NROW(PSUProcessData$SSU_PSU)) {
        PSUStartStopDateTime <- data.table(1)[,`:=`(unlist(getRstoxBaseDefinitions("PSUByTime")), NA)][, V1 := NULL][.0]
        return(PSUStartStopDateTime)
    }
    
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
    if(!length(PSUStartStopDateTime)) {
        stop("The StoxData does not contain any EDSUs/Stations matching the PSUs. This can happen if the PSUs were created using one StoxData and then DefinePSU() is run with a different StoxData.")
    }
    
    # Add the Stratum:
    PSUStartStopDateTime <- RstoxData::mergeByIntersect(PSUProcessData$Stratum_PSU, PSUStartStopDateTime)
    
    return(PSUStartStopDateTime)
}

# Function to get the start and end times of one acoustic PSU:
getPSUStartStopDateTimeByPSU <- function(PSU, SSU_PSU_ByPSU, StationTable) {
    
    # For convenience extract the SSUs of the current PSU:
    thisSSU_PSU <- SSU_PSU_ByPSU[[PSU]]
    
    # Match the SSUs of the PSUProcessData with SSUs of the StationTable:
    # Order both since it may happen that the EDSUs of the StoxAcousticData are not ordered, e.g. if there are multiple instruments from the same cruise, and these instruments have both identical and differing times:
    atSSUInStoxData <- match(sort(thisSSU_PSU$SSU), sort(StationTable$SSU))
    if(any(is.na(atSSUInStoxData))) {
        warning("StoX: The StoxData must be the same data that were used to generate the PSUProcessData. (Number of EDSUs not found in the StoxData: ", sum(is.na(atSSUInStoxData)), ". These are the following: ", paste(sort(thisSSU_PSU$SSU)[is.na(atSSUInStoxData)], collapse = ", "), ".")
        
        if(all(is.na(atSSUInStoxData))) {
            
            # Return a one row table with NAs in all columns except the PSU:
            PSUStartStopDateTime <- data.table::data.table(
                PSU = PSU, 
                Cruise = NA_character_, 
                StartDateTime = NA, 
                StopDateTime = NA
            )
            
            return(PSUStartStopDateTime)
        }
        else {
            atSSUInStoxData <- atSSUInStoxData[!is.na(atSSUInStoxData)]
        }
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
#' This function defines the Layer process data, which sets the range intervals of layers used in estimation models in StoX.
#' 
#' @inheritParams general_arguments
#' @param StoxData Either \code{\link[RstoxData]{StoxBioticData}} or \code{\link[RstoxData]{StoxAcousticData}} data.
#' @param DefinitionMethod Character: A string naming the method to use, one of "WaterColumn", to define one single for the entire water column; "HighestResolution", to use the maximum possible vertical resolution without intersecting hauls; "Resolution", which can be used to set a fixed layer thickness; and "Table" to provide the \code{LayerTable}.
#' @param Resolution  Numeric: A single numeric giving the thickness of the layers.
#' @param LayerTable A table of Layer name, MinLayerDepth in meters and MaxLayerDepth in meters, defining the Layers.
#' 
#' @return
#' A table of layer intervals.
#' 
#' @seealso \code{\link{DefineAcousticLayer}} and \code{\link{DefineBioticLayer}}.
#' 
DefineLayer <- function(
    processData, UseProcessData = FALSE, 
    StoxData, 
    DefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    LayerType = c("Acoustic", "Biotic")
) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- RstoxData::match_arg_informative(DefinitionMethod)
    # Get the DefinitionMethod:
    LayerType <- RstoxData::match_arg_informative(LayerType)
    
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
        #if(all(is.na(data[[VerticalResolutionMin]]))) {
        #    MinLayerDepth <- 0
        #}
        #else {
        #    MinLayerDepth <- min(data[[VerticalResolutionMin]], na.rm = TRUE)
        #}
        #if(all(is.na(data[[VerticalResolutionMax]]))) {
        #    MaxLayerDepth <- Inf
        #}
        #else {
        #    MaxLayerDepth <- max(data[[VerticalResolutionMax]], na.rm = TRUE)
        #}
        
        #Layer <- data.table::data.table(
        #    Layer = "WaterColumn", 
        #    #MinLayerDepth = possibleIntervals[1, 1], 
        #    MinLayerDepth = MinLayerDepth, 
        #    #MaxLayerDepth = possibleIntervals[nrow(possibleIntervals), 2]
        #    MaxLayerDepth = MaxLayerDepth
        #)
        
        Layer <- data.table::data.table(
            Layer = "WaterColumn", 
            MinLayerDepth = 0, 
            MaxLayerDepth = NA
        )
    }
    
    # If "HighestResolution" is requested use all possible breaks:
    else if(grepl("HighestResolution", DefinitionMethod, ignore.case = TRUE)) {
        Layer <- createLayerTable(possibleIntervals)
    }
    
    # If "Table" is requested match the Breaks against the possible breaks:
    else if(grepl("Table", DefinitionMethod, ignore.case = TRUE)) {
        # Detect invalid breaks:
        allBreaks <- unlist(LayerTable[, c("MinLayerDepth", "MaxLayerDepth")])
        rangeOfPossibleIntervals <- range((possibleIntervals))
        # Accept values outside of the range of the possibleIntervals:
        validBreaks <- allBreaks %in% unlist(possibleIntervals) | allBreaks < rangeOfPossibleIntervals[1] | allBreaks > rangeOfPossibleIntervals[2]
        
        if(any(is.na(validBreaks))) {
            stop("The LayerTable contains missing values (NA).")
        }
        # Error if any of the specified breaks are invalid:
        if(any(!validBreaks)) {
            stop("Some of the specified breaks are not at common breaks of all Log(distance). Possible breaks are [", paste(unlist(possibleIntervals), collapse = ", "), "]")
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
    DefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
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
    DefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
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
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Stratum", to assign all stations of each stratum to all acoustic PSUs; "Radius", to assign all stations within the radius given in \code{Radius} to each acoustic PSU; and "EllipsoidalDistance" to provide \code{MinNumberOfHauls}, \code{Distance}, \code{TimeDifference}, \code{BottomDepthDifference}, \code{LongitudeDifference}, \code{LatitudeDifference}, specifying the axes of an ellipsoid inside which to assign stations to acoustic PSUs, and "ResourceFile" to read from a project.xml file from StoX <= 2.7. Note that in the latter case it is assumed that the contents of the project.xml file is actually used in the StoX <= 2.7 project, i.e. that UseProcessData = TRUE in BioStationAssignment().
#' @param FileName The path to the StoX 2.7 project.xml file to read "bioticassignment", "suassignment" and "psustratum" from, in the case that \code{DefinitionMethod} is "ResourceFile". Must include file extension.
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
#' \emph{FunctionParameter} which use function input data type \code{\link[RstoxData]{AcousticData}} and function parameter \emph{LayerDefinitionMethod}. The available methods are:
#' 
#' \emph{Watercolomn} method which defines one layer for the entire watercolumn
#' \emph{HighestResolution} method which makes the highest possible number of layers based on the resolution in the input AcousticData.
#' \emph{Resolution} method is assosiated with function parameter \emph{Resolution} which gives the desired thickness (in meters) of the layers.
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
#'This assignment method uses the ellipsoidal distance \doi{10.1016/j.fishres.2007.07.013}{(Johnsen and Iilende, 2007, equation 8)}. All biotic stations that fulfills the selection criteria (scalar product f <= 1) on one or more EDSUs of a PSU, will be assigned to the PSU. The scalar product of the method is calculated as:
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
#'The function parameter \emph{MinNumberOfHauls} can override the requirement to fulfill the selection criteria (scalar product f <=1) if the number of assigned hauls are lower than the MinNumberOfHauls parameter value. Hauls with a scalar product value closest to the minimum selection criteria, will be included in the assignment list to ensure that a minimum number of stations are assigned.
#'
#'NOTE! The end user will get a warning if one or more acoustic PSUs have not been assigned any biotic hauls.
#'
#' @references
#'
#'Johnsen E., Iilende T., 2007, Factors affecting the diel variation in commercial CPUE of Namibian hake. Can new information improve standard survey estimates?, Fisheries Research 88 (2007) p70 to 79, \doi{10.1016/j.fishres.2007.07.013}
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
    DefinitionMethod = c("Manual", "Stratum", "Radius", "EllipsoidalDistance", "DeleteAllAssignments", "ResourceFile"), 
    StoxBioticData, 
    FileName = character(), 
    # For DefinitionMethod "Stratum": 
    StratumPolygon, AcousticPSU, #AcousticLayer, 
    LayerDefinition = c("FunctionParameter", "FunctionInput"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
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
    LayerDefinition <- RstoxData::match_arg_informative(LayerDefinition)
    
    if(!length(AcousticPSU$Stratum_PSU)) {
        stop("The input AcousticPSU does not contain any PSUs, which is required to assign biotic hauls to acoustic PSUs.")
    }
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        
        # If emtpy BioticAssignment, return immediately:
        if(!NROW(processData$BioticAssignment)) {
            return(data.table::data.table())
        }
        
        # Check the BioticAssignment:
        processData$BioticAssignment <- DefineBioticAssignment_Warnings(processData$BioticAssignment, AcousticPSU) 
        
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
    DefinitionMethod <- RstoxData::match_arg_informative(DefinitionMethod)
    
    # Merge the StoxBioticData:
    HaulData <- RstoxData::MergeStoxBiotic(StoxBioticData, TargetTable = "Haul")
    
    # If DefinitionMethod == "Stratum", assign all stations of each stratum to all PSUs of the stratum:
    if(grepl("Manual", DefinitionMethod, ignore.case = TRUE)) {
        if(length(processData)) {
            return(processData)
        }
        else {
            BioticAssignment <- data.table::data.table()
        }
    }
    else if(grepl("Stratum", DefinitionMethod, ignore.case = TRUE)) {
        
        # Return empty table if AcousticPSU$Stratum_PSU is empty for some reason:
        if(!NROW(AcousticPSU$Stratum_PSU)) {
            return(data.table::data.table())
        }
        
        # :
        attr(HaulData, "pointLabel") <- "Station"
        attr(HaulData, "Station") <- HaulData$Station
        
        turn_off_s2(
            locatedStratum <- locateInStratum(HaulData, StratumPolygon, SSULabel = "Station"), 
            msg = FALSE
        )
        
        # Add to the BioticAssignment:
        BioticAssignment <- HaulData
        BioticAssignment[, Stratum := ..locatedStratum]
        
        # Add the PSUs to the BioticAssignment:
        BioticAssignment <- merge(BioticAssignment, AcousticPSU$Stratum_PSU, all = TRUE, by = "Stratum", allow.cartesian = TRUE)
        
        # Discard all rows with missing PSU:
        BioticAssignment <- subset(BioticAssignment, !is.na(PSU))
        
        # Give a warning if all Haul are NA, indicating no intersection between the Strata of the AcousticPSU$Stratum_PSU and BioticAssignment:
        if(BioticAssignment[, all(is.na(Haul))]) {
            warning("StoX: No Hauls are loacted in any of the strata of the Acoustic PSUs, resulting in empty BioticAssignment.")
        }
        
        # 2022-10-31: why was this included? It causes warnings when adding hauls to a PSU that does not have any assigned hauls:
        # Discard  all rows with missing Haul:
        #BioticAssignment <- subset(BioticAssignment, !is.na(Haul))
    }
    # Search for Hauls around all EDSUs of each PSU:
    else if(grepl("Radius|EllipsoidalDistance", DefinitionMethod, ignore.case = TRUE)) {
        # Merge the StoxBioticData:
        LogData <- RstoxData::MergeStoxAcoustic(StoxAcousticData, TargetTable = "Log")
        
        # Get a table of EDSUs and Hauls present in the StoxAcoustic and StoxBiotic data. Here all hauls are listed for each EDSU, i.e., hauls are in the fastest changing column:
        EDSU_Haul <- data.table::CJ(
            EDSU = LogData$EDSU, 
            Haul = HaulData$Haul, 
            sorted = FALSE
        )
        
        # Merge PSUs and strata into the table:
        EDSU_PSU_Stratum <- RstoxData::mergeDataTables(AcousticPSU[names(AcousticPSU)!= "PSUByTime"], all = TRUE, output.only.last = TRUE)
        # .. but make sure that we only keep EDSUs that are present in the StoxAcoustic data, as the AcousticPSU data type is free to contain any EDSUs tagged to or not tagged to PSUs (using all.y = TRUE):
        BioticAssignment <- merge(EDSU_PSU_Stratum, EDSU_Haul, by = "EDSU", all.y = TRUE)
        
        # Get the distance units:
        if(grepl("Radius", DefinitionMethod, ignore.case = TRUE)) {
            # Calculate distances in a matrix with hauls as rows and EDSUs as columns, so that hauls change the fastest after converting to a vector, consistent with EDSU_Haul:
            differenceTable = data.table::data.table(
                distance = c(
                    getEDSUToHaulDistance(
                        LogData = LogData, 
                        HaulData = HaulData
                    )
                )
            )
            
            # Tag the Hauls that are inside the radius:
            Radius <- units::set_units(Radius, "nautical_mile")
            differenceTable[, inside := distance <= Radius]
        }
        else if(grepl("EllipsoidalDistance", DefinitionMethod, ignore.case = TRUE)) {
            differenceTable = data.table::data.table(
                # Get the distance between the EDSUs and Hauls:
                Distance = if(length(Distance)) {
                    getSquaredRelativeDistance(
                        LogData = LogData, 
                        HaulData = HaulData, 
                        Distance = Distance
                    )
                }, 
                # Get the time difference between the EDSUs and Hauls:
                TimeDifference = if(length(TimeDifference)) {
                    getSquaredRelativeTimeDiff(
                        LogData = LogData, 
                        HaulData = HaulData, 
                        TimeDifference = TimeDifference
                    )
                }, 
                # Get the difference in bottom depth between the EDSUs and Hauls:
                BottomDepthDifference = if(length(BottomDepthDifference)) {
                    getSquaredRelativeDiff(
                        LogData = LogData, 
                        HaulData = HaulData, 
                        variableName = "BottomDepth", 
                        axisLength = BottomDepthDifference
                    )
                }, 
                # Get the longitude difference between the EDSUs and Hauls:
                LongitudeDifference = if(length(LongitudeDifference)) {
                    getSquaredRelativeDiff(
                        LogData = LogData, 
                        HaulData = HaulData, 
                        variableName = "Longitude", 
                        axisLength = LongitudeDifference
                    )
                }, 
                # Get the latitude differerence between the EDSUs and Hauls:
                LatitudeDifference = if(length(LatitudeDifference)) {
                    getSquaredRelativeDiff(
                        LogData = LogData, 
                        HaulData = HaulData, 
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
    else if(grepl("ResourceFile", DefinitionMethod, ignore.case = TRUE)) {
        
        if(basename(FileName) == "project.json") {
            BioticAssignment <- readOneProcessDataByFunctionNameFromProjectJSON(FileName, functionName = "DefineBioticAssignment")$BioticAssignment
        }
        else if(tolower(getResourceFileExt(FileName)) == "xml" && any(grepl("http://www.imr.no/formats/stox/v1", readLines(FileName, 5)))) {
            BioticAssignment <- readBioticAssignmentFrom2.7(FileName)
            
            # Translate to the Haul defined by StoX >=3:
            HaulsAsIn2.7 <- sub("/.*&*-", "/", HaulData$Haul)
            
            BioticAssignment$Haul <- HaulData$Haul[
                match(
                    BioticAssignment$Haul, 
                    HaulsAsIn2.7
                )
            ]
        }
        else {
            stop("StoX: Invalid file. Must be a StoX project description file with file name project.json or project.xml")
        }
    }
    # Is this for backward campatibility??????????????
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
    
    # Check the BioticAssignment:
    BioticAssignment <- DefineBioticAssignment_Warnings(BioticAssignment, AcousticPSU) 
    
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



DefineBioticAssignment_Warnings <- function(BioticAssignment, AcousticPSU) {
    
    # These warnings can only be given if there is non-empty BioticAssignment:
    if(NROW(BioticAssignment)) {
        # Check whether all PSUs are present in the processData, and issue a warning if there are new PSUs to be included:
        newPSUs <- setdiff(AcousticPSU$Stratum_PSU$PSU, subset(BioticAssignment, !is.na(Haul))$PSU)
        if(length(newPSUs)) {
            warning("StoX: The following acoustic PSUs are not present in the BioticAssignment processData or have no assigned biotic Hauls. This may not be a problem if there is no NASC in those PSUs. Otherwise, please add assignment to these acoustic PSUs, or if an automatic method was used in DefineBioticAssignment, rerun that process with UseProcecssData set to FALSE (unchecked):\n", paste("\t", newPSUs, collapse = "\n"))
        }
        
        # Also issue a warning for assignment to non-existing acoustic PSUs:
        nonExistingPSUs <- setdiff(BioticAssignment$PSU, AcousticPSU$Stratum_PSU$PSU)
        if(length(nonExistingPSUs)) {
            warning("StoX: There are assignments to the following non-existing acoustic PSUs. Please remove these assignments, or if an automatic method was used in DefineBioticAssignment, rerun that process with UseProcecssData set to FALSE (unchecked):\n", paste("\t", nonExistingPSUs, collapse = "\n"))
        }
        
        # Give a warning for and remove missing Haul:
        missingHaul <- BioticAssignment[, is.na(Haul)]
        if(any(missingHaul)) {
            warning("StoX: There are assignments to missing Hauls. These were removed.")
            BioticAssignment <- subset(BioticAssignment, !is.na(Haul))
        }
    }
    
    
    return(BioticAssignment)
}




notAllStationsInStratum_Warning <- function(BioticAssignment, StoxBioticData) {
    
    
    
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







getEDSUToHaulDistance <- function(LogData, HaulData, nautical_mile = FALSE) {
    
    # Extract the geographical positions:
    HaulPositions <- sf::st_as_sf(HaulData[, c("Longitude", "Latitude")], coords = c("Longitude", "Latitude"), crs = sf::st_crs("WGS84"))
    EDSUPositions <- sf::st_as_sf(LogData[, c("Longitude", "Latitude")], coords = c("Longitude", "Latitude"), crs = sf::st_crs("WGS84"))
    
    # Get the distances between EDUSs and Hauls in m:
    turn_off_s2(
        EDSUToHaulDistance <- sf::st_distance(HaulPositions, EDSUPositions), 
        msg = FALSE
    )
    
    # Convert to nautical miles:
    if(nautical_mile) {
        EDSUToHaulDistance <- units::set_units(EDSUToHaulDistance, "nautical_mile")
    }
    
    return(EDSUToHaulDistance)
}

# Function to ge the squared distance in units of the Distance squared:
getSquaredRelativeDistance <- function(LogData, HaulData, Distance) {
    # Get the distances between EDUSs and Hauls:
    EDSUToHaulDistance <- c(
        getEDSUToHaulDistance(
            LogData = LogData, 
            HaulData = HaulData
        )
    )
    # Square and return:
    SquaredRelativeDistance <- EDSUToHaulDistance^2 / Distance^2
    return(SquaredRelativeDistance)
}

# Function to get the squared time difference in units of the TimeDifference squared:
getSquaredRelativeTimeDiff <- function(LogData, HaulData, TimeDifference, variableName = "DateTime") {
    # Get the time difference between all EDSUs and all Hauls:
    out <- data.table::CJ(
        x = LogData[[variableName]], 
        y = HaulData[[variableName]], 
        sorted = FALSE
    )
    TimeDiff <- as.numeric(out[, difftime(x, y, units = "hours")])
    # Square and return:
    SquaredTimeDiff <- TimeDiff^2 / TimeDifference^2
    return(SquaredTimeDiff)
}

getSquaredRelativeDiff <- function(LogData, HaulData, variableName, axisLength) {
    # Get the absolute difference between all EDSUs and all Hauls:
    out <- data.table::CJ(
        x = LogData[[variableName]], 
        y = HaulData[[variableName]], 
        sorted = FALSE
    )
    # Square and return:
    SquaredRelativeDiff <- c(out[, x - y])^2 / axisLength^2
    if(all(is.na(SquaredRelativeDiff))) {
        warning("StoX: All ", variableName, " are NA. Using this as an axis in the ellipsoid will have no effect.")
    }
    
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
#' @inheritParams MeanNASC
#' @inheritParams AcousticDensity
#' @param WeightingMethod  Character: A string naming the method to use, one of "Equal", giving weight 1 to all Hauls; "NumberOfLengthSamples", weighting hauls by the number of length samples; "AcousticDensity", weighting by the surrounding NASC converted by the haul length distribution to an acoustic density equivalent; "NormalizedTotalWeight", weighting hauls by the total weight of the catch, normalized by dividing by towed distance; "NormalizedTotalNumber", the same as "NormalizedTotalWeight" but for total number, "SumWeightedNumber", weighting by the summed WeightedNumber of the input LengthDistributionData; and "InverseSumWeightedNumber", weighting by the inverse of the summed WeightedNumber.
#' @param LengthDistributionData The LengthDistributionData of LengthDistributionType "Standard" or "Normalized", used for WeightingMethod = "SumWeightedNumber" or "InverseSumWeightedNumber", in which case the column WeightedNumber is summed in each Haul (summed over all length groups, which implies that the resolution of length intervals does not matter).
#' @param MaxNumberOfLengthSamples For \code{WeightingMethod} = "NumberOfLengthSamples": Values of the number of length samples that exceed \code{MaxNumberOfLengthSamples} are set to \code{MaxNumberOfLengthSamples}. This avoids giving too high weight to e.g. experimental hauls with particularly large length samples.
#' @param Radius For \code{WeightingMethod} = "AcousticDensity": The radius (nautical miles) inside which the average AcousticDensity is calculated. 
#' @param MinNumberOfEDSUs For \code{WeightingMethod} = "AcousticDensity": The minimum number of EDSUs to link to each Haul, applied if there are less than \code{MinNumberOfEDSUs} EDSUs inside the \code{Radius}. This can be used to avoid the warning for "no positive NASC inside the specified radius", but only if the NASCData input does not contain EDSUs with no positive NASC for the target species. \code{\link[RstoxData]{FilterStoxAcoustic}} can be used to filter out EDSUs with no positive NASC. It can also happen that the LengthDistributionData contains Hauls with no length distribution for the target species, also resulting in the warning. This can also be solved by filtering. It is in fact recommended to only keep Hauls with the target species in acoustic-trawl models.
#' 
#' @details
#' The \emph{BioStationWeighting} function is used to update the weighting variables of the biotic stations that are associated in \code{\link{BioticAssignment}}. The list of assigned biotic hauls and weighting variables of an assignment, will in another function be used to make a total combined length frequency distribution from all the individual haul distributions.
#' 
#' A set of automatic \emph{WeightingMethod}s are available to update the haul weighing variables. Note that the weighting may change if an additional species is included for all WeightingMethods except "Equal":
#' 
#'\strong{Equal}
#' 
#' All assigned biotic hauls are given equal weight by assigning the value 1 to the weighting variables.
#' 
#'\strong{NumberOfLengthSamples}
#'The assigned biotic hauls are given a weighting value according to the number of individual length samples of the target species at the biotic station. The parameter \emph{MaxNumberOfLengthSamples} is also associated with this method and is used to limit the weighting to a maximum number of length samplesof a haul. Note that the weighting may change if an additional species is included.
#' 
#' \strong{NormalizedTotalWeight}
#' 
#' The assigned biotic hauls are given a weighting value according to the normalized catch weight of the target species at the station. The weighting value is calculated as catch weight divided by towing distance. This normalization makes the stations comparable regardless of catch effort.
#' 
#'\strong{NormalizedTotalNumber}
#'
#'The assigned biotic hauls are given a weighting value according to the normalized catch number (number of individuals) of the target species at the biotic station. The weighting value is calculated as catch number divided by towing distance. This normalization makes the stations comparable regardless of catch effort.
#'
#'\strong{SumWeightedNumber}
#'
#'The assigned biotic hauls are given a weighting value according to the sum of the WeightedNumber of the target species at the biotic station. It is a requirement that the lengthdistribution data is of distribution type \emph{Standard} or \emph{Normalized} (normalized to one nautical mile towing distance).
#'
#'\strong{InverseSumWeightedNumber}
#'
#'The assigned biotic hauls are given a weighting value as the inverse of the sum of the WeightedNumber of all length groups and all species. The weighting value \eqn{w_b} is calculated as:
#'
#'\deqn{w_b = \frac{1}{\sum_{s_b}^{n_b} \sum_{l=1}^{m_{s,b}} c_{l,s,b} }}
#'
#'where:
#'
#'\eqn{w_b} = weighting value of biotic haul \emph{b}
#'
#'\eqn{s_b} = species in the biotic haul \emph{b}
#'
#'\eqn{n_b} = number of species in the input data of biotic haul \emph{b}
#'
#'\eqn{l} =  length group number
#'
#'\eqn{m_{s,b}}  =	number of length groups for species \emph{s} in biotic haul \emph{b}
#'
#'\eqn{c_{l,s,b}} = number in length group \emph{l} for species \emph{s} in biotic haul \emph{b}
#' 
#'The method is commonly used in split NASC (nautical area scattering coefficient) models to split an acoustic category of several species by using the length distributions of the these species. The sum of the splitted NASC values of all the species will be equal to the NASC of the original combined acoustic multispecies category. By multiplying the calculated weighting value from this method, by the original (input) numbers in each length group for all species, a relative station length distribution can later be made (sum of length groups for all species is 1) and used in the split NASC process.
#'
#'It is a requirement that the LengthDistribution Data is of distribution type \emph{Standard} or \emph{Normalized} (normalized to one nautical mile towing distance).
#'
#'\strong{AcousticDensity}
#'
#'The assigned biotic hauls are given weighting variable values with the basis in the surrounding NASC values that are tagged to an acoustic PSU. By combining these NASC values with the length distribution of the biotic haul, an acoustic density as number of fish per square nautical mile is calculated and used as the weighting variable value for each biotic haul.
#'
#'A search for NASC values (at EDSU resolution) is performed within a given radius around a biotic station. For each EDSU inside the radius the length distribution of the biotic station and a target strength (TS) versus length empirical relationship is used to calculate acoustic density for each length group and beam. The sum of densities (number per square nautical mile) over all length groups of the target species at the given biotic haul is then calculated and applied as the weighting variable for the biotic haul.
#'
#'The AcousticDensity WeightingMethod is associated with the following function inputs and parameters:
#'
#'\emph{AcousticTargetStrength}: The acoustic target strength model and parameters, defined by \code{\link{DefineAcousticTargetStrength}}
#'
#'\emph{SpeciesLink}: The table linking the AcousticCategory and SpeciesCategory.
#'
#'\emph{Radius}: Search radius (nautical miles) for NASC values (at EDSU resolution) around a biotic station
#'
#'\emph{MinNumberOfEDSUs} The minimum number of EDSUs to use, effectively expanding the radius to cover this number of EDSUs if an insufficient number of EDSUs is found using the specified radius
#'  
#' Note that the AcousticDensity WeightingMethod is time consuming, as distances are calculated to all EDSUs and the acoustic density is calculated for each assigned Haul.
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
    WeightingMethod = c("Equal", "NumberOfLengthSamples", "NormalizedTotalWeight", "NormalizedTotalNumber", "SumWeightedNumber", "InverseSumWeightedNumber", "AcousticDensity"), 
    StoxBioticData, 
    LengthDistributionData, 
    MaxNumberOfLengthSamples = 100, 
    # Used in WeightingMethod = "AcousticDensity":
    NASCData, 
    LayerDefinition = c("FunctionParameter", "FunctionInput"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    AcousticLayer = NULL, 
    # Used in AcousticDensity, which is used in WeightingMethod = "AcousticDensity":
    AcousticPSU, AcousticTargetStrength, SpeciesLink = data.table::data.table(), Radius = double(), MinNumberOfEDSUs = integer()
) {
    
    if(!NROW(BioticAssignment)) {
        stop("BioticAssignment is empty. No assigned Hauls to calculate weights for.")
    }
    
    # Get the DefinitionMethod:
    WeightingMethod <- RstoxData::match_arg_informative(WeightingMethod)
    
    # Define the weighting variable:
    weightingVariable <- getDataTypeDefinition(dataType = "BioticAssignment", elements = "weighting", unlist = TRUE)
    
    # Make a copy of the BioticAssignment to enable safe modification by reference:
    BioticAssignmentCopy <- data.table::copy(BioticAssignment)
    
    
    WeightingMethodsRequiringLengthDistributionData <- c(
        "AcousticDensity", 
        "SumWeightedNumber", 
        "InverseSumWeightedNumber"
    )
    if(WeightingMethod %in% WeightingMethodsRequiringLengthDistributionData && !NROW(LengthDistributionData)) {
        stop("StoX: Non-empty LengthDistributionData is required when WeightingMethod is one of ", paste(WeightingMethodsRequiringLengthDistributionData, collapse = ", "), ".")
    }
    
    
    # Put equal weight (1) to each haul:
    #if(WeightingMethod == "Equal") {
    #    # Simply set WeightingFactor to 1
    #    #BioticAssignmentCopy[, eval(weightingVariable) := 1]
    #}
    ## Weight hauls by the number of length samples (count the length samples for which IndividualTotalLength is not NA):
    #else 
    if(WeightingMethod == "NumberOfLengthSamples") {
        
        # Allow only one species in StoX 3.1.0: 
        checkOneSpeciesInStoxBioticData(StoxBioticData, WeightingMethod = "NumberOfLengthSamples")
        
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
    else if(WeightingMethod == "AcousticDensity") {
        
        ### # SpeciesLink must contain only one link, and the SpeciesCategory and AcousticCategory must be present in the data:
        ### if(!(
        ###     nrow(SpeciesLink) == 1 && 
        ###     SpeciesLink$SpeciesCategory %in% LengthDistributionData$SpeciesCategory && 
        ###     SpeciesLink$AcousticCategory %in% NASCData$AcousticCategory
        ### )) {
        ###     stop("SpeciesLink must contain only one row, with SpeciesCategory and AcousticCategory present in the LengthDistributionData and NASCData, respectively.")
        ### }
        
        
        # SpeciesLink must contain only one link, and the SpeciesCategory and AcousticCategory must be present in the data. The requirement of only one species when weighting by AcousticDensity is a choice made to prevent nonsensical weighting. If there is a strong demand for multiple species when weighting by AcousticDensity, we might re-consider.:
        if(nrow(SpeciesLink) != 1) {
            stop("SpeciesLink must contain only one row, as weighting by AcousticDensity should focus only on the target species.")
        }
        if(!SpeciesLink$SpeciesCategory %in% LengthDistributionData$SpeciesCategory) {
            stop("SpeciesCategory of SpeciesLink must be present in the LengthDistributionData.")
        }
        if(!SpeciesLink$AcousticCategory %in% NASCData$AcousticCategory) {
            stop("AcousticCategory of SpeciesLink must be present in the NASCData")
        }
        
        ## Also the NASCData cannot contain more than one frequency:
        #if(NASCData[, length(unique(Beam))] > 1) {
        #    stop("Stox: The NASCData can only contain one Beam.")
        #}
        
        # Keep only the tagged EDSUs:
        taggedEDSUs <- AcousticPSU$EDSU_PSU[!is.na(PSU), EDSU]
        NASCData <- subset(NASCData, EDSU %in% taggedEDSUs)
        
        # Get the LayerDefinition
        LayerDefinition <- RstoxData::match_arg_informative(LayerDefinition)
        
        # Get the weights for each Haul:
        uniqueHauls <- unique(LengthDistributionData$Haul)
        weightsNASC <- sapply(
            uniqueHauls, 
            getMeanAcousticDensityAroundOneStation, 
            LengthDistributionData = LengthDistributionData, 
            NASCData = NASCData,
            LayerDefinition = LayerDefinition,
            LayerDefinitionMethod = LayerDefinitionMethod,
            Resolution = Resolution,
            LayerTable = LayerTable,
            AcousticLayer = AcousticLayer,
            Survey = Survey,
            Radius = Radius, 
            AcousticTargetStrength = AcousticTargetStrength,
            SpeciesLink = SpeciesLink, 
            MinNumberOfEDSUs = MinNumberOfEDSUs
        )
        
        # Give a warning if any of the new weights are NA:
        if(any(is.na(weightsNASC))) {
            warning("StoX: The following Hauls had no positive NASC inside the specified radius of ", Radius, " nautical miles for the species given by the SpeciesLink, resulting in 0 biotic assignment weight:\n\t", paste(names(weightsNASC)[is.na(weightsNASC)], collapse = "\n\t"))
            # Replace NA by 0:
            weightsNASC[is.na(weightsNASC)] <- 0
        }
        
        # Add the weights:
        #BioticAssignmentCopy <- merge(BioticAssignmentCopy, data.table::data.table(Haul = names(weightsNASC), weightsNASC = weightsNASC), by = "Haul")
        #BioticAssignmentCopy[, WeightingFactor := weightsNASC]
        weightsNASCTable <- data.table::data.table(Haul = names(weightsNASC), weightsNASC = weightsNASC)
        BioticAssignmentCopy <- mergeIntoBioticAssignment(
            BioticAssignment = BioticAssignmentCopy, 
            toMerge = weightsNASCTable, 
            variable = "weightsNASC", 
            weightingVariable = weightingVariable
        )
        
    }
    # Weight hauls by the summed CatchFractionWeight divided by the EffectiveTowDistance:
    else if(WeightingMethod == "NormalizedTotalWeight") {
        
        # Allow only one species in StoX 3.1.0: 
        checkOneSpeciesInStoxBioticData(StoxBioticData, WeightingMethod = "NormalizedTotalWeight")
        
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
    # Weight hauls by the summed CatchFractionNumber divided by the EffectiveTowDistance:
    else if(WeightingMethod == "NormalizedTotalNumber") {
        
        # Allow only one species in StoX 3.1.0: 
        checkOneSpeciesInStoxBioticData(StoxBioticData, WeightingMethod = "NormalizedTotalNumber")
        
        # Merge Haul and Sample, and sum the catch number divided by towed distance:
        Haul_Sample <- merge(StoxBioticData$Haul, StoxBioticData$Sample)
        NormalizedTotalNumber <- Haul_Sample[, .(NormalizedTotalNumber = sum(CatchFractionNumber) / EffectiveTowDistance[1]), by = "Haul"]
        # Merge into the BioticAssignmentCopy and set the weightingVariable to the NumberOfLengthSamples
        BioticAssignmentCopy <- mergeIntoBioticAssignment(
            BioticAssignment = BioticAssignmentCopy, 
            toMerge = NormalizedTotalNumber, 
            variable = "NormalizedTotalNumber", 
            weightingVariable = weightingVariable
        )
        #BioticAssignmentCopy <- merge(BioticAssignmentCopy, NormalizedTotalNumber, by = "Haul")
        #BioticAssignmentCopy[, eval(weightingVariable) := NormalizedTotalNumber]
    }
    # Weight hauls by the summed CatchFractionNumber divided by the EffectiveTowDistance:
    else if(WeightingMethod == "SumWeightedNumber") {
        # Allow only one species in StoX 3.1.0: 
        checkOneSpeciesInLengthDistributionData(LengthDistributionData, "SumWeightedNumber")
        
        BioticAssignmentCopy <- addSumWeightedNumber(
            BioticAssignment = BioticAssignmentCopy, 
            LengthDistributionData = LengthDistributionData, 
            weightingVariable = weightingVariable, 
            inverse = FALSE
        )
    }
    else if(WeightingMethod == "InverseSumWeightedNumber") {
        # This was a mistake, as InverseSumWeightedNumber is used in SplitNASC-projects, where several species should be supported:
        # Allow only one species in StoX 3.1.0: 
        #checkOneSpeciesInLengthDistributionData(LengthDistributionData, WeightingMethod = "NumberOfLengthSamples")
        
        BioticAssignmentCopy <- addSumWeightedNumber(
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


getMeanAcousticDensityAroundOneStation <- function(
    thisHaul, 
    LengthDistributionData, 
    NASCData,
    LayerDefinition,
    LayerDefinitionMethod,
    Resolution,
    LayerTable,
    AcousticLayer,
    Survey, 
    Radius, 
    AcousticTargetStrength,
    SpeciesLink, 
    MinNumberOfEDSUs = integer()
) {
    
    # Get the distance from the station to the EDSUs:
    stationPosition <- unique(LengthDistributionData[Haul == thisHaul, c("Longitude", "Latitude")])
    EDSUData <- subset(NASCData, !duplicated(EDSU))
    
    
    # Get the great circle distance:
    haulToEDSUDistance <- getEDSUToHaulDistance(LogData = EDSUData, HaulData = stationPosition, nautical_mile = TRUE)
    # Set the unit of Radius also to nautical_mile:
    Radius <- units::set_units(Radius, "nautical_mile")
    
    # Find hauls inside the radius:
    EDSUData[, insideRadius := haulToEDSUDistance <= Radius]
    
    # Apply the minimum number of EDSU requirement:
    if(length(MinNumberOfEDSUs)) {
        numInsideRadius <- EDSUData[, sum(insideRadius)]
        if(numInsideRadius < MinNumberOfEDSUs) {
            EDSUData[, insideRadius := insideRadius | haulToEDSUDistance <= sort(haulToEDSUDistance)[MinNumberOfEDSUs]]
        }
    }
    
    # Identify EDSUs within the specified radius:
    EDSUsInsideRadius <- subset(EDSUData, insideRadius == TRUE)
    if(!nrow(EDSUsInsideRadius)) {
        return(NA)
    }
    
    # Subset the NASCData:
    NASCDataInside <- subset(NASCData, EDSU %in% EDSUsInsideRadius$EDSU)
    
    # Get the SumNASCData and then the MeanNASCData:
    if(LayerDefinition != "PreDefined") {
        SumNASCDataInside <- SumNASC(
            NASCData = NASCDataInside, 
            LayerDefinition = LayerDefinition, 
            LayerDefinitionMethod = LayerDefinitionMethod, 
            Resolution = Resolution, 
            LayerTable = LayerTable, 
            AcousticLayer = AcousticLayer
        )
    }
    uniqueEDSUs <- unique(SumNASCDataInside$Data$EDSU)
    AcousticPSU <- list(
        Stratum_PSU = data.table::data.table(
            Stratum = "StratumForWeightingMethodAcousticDensity", 
            PSU = uniqueEDSUs
        ), 
        EDSU_PSU = data.table::data.table(
            EDSU = uniqueEDSUs, 
            PSU = uniqueEDSUs
        )
    )
    Survey <- data.table::data.table(
        Stratum = "StratumForWeightingMethodAcousticDensity",
        Survey = "SurveyForWeightingMethodAcousticDensity"
    )
    # The method of StoX 2.7, where MeanNASC is taken over all EDSUs for each Haul:
    #AcousticPSU <- list(
    #    Stratum_PSU = data.table::data.table(
    #        Stratum = "StratumForWeightingMethodAcousticDensity", 
    #        PSU = "PSUForWeightingMethodAcousticDensity"
    #    ), 
    #    EDSU_PSU = data.table::data.table(
    #        EDSU = uniqueEDSUs, 
    #        PSU = "PSUForWeightingMethodAcousticDensity"
    #    )
    #)
    suppressWarnings(MeanNASCDataInside <- MeanNASC(
        LayerDefinition = "PreDefined", 
        SumNASCData = SumNASCDataInside, 
        # Survey: 
        SurveyDefinition = "FunctionInput", 
        Survey = Survey, 
        # PSU: 
        PSUDefinition = "FunctionInput", 
        AcousticPSU = AcousticPSU
    ))
    
    # Contruct BioticAssignment, as the given Haul for all EDSUs inside the radius:
    BioticAssignment <- data.table::data.table(
        MeanNASCDataInside$Data[, c("Stratum", "PSU", "Layer")], 
        Haul = thisHaul, 
        WeightingFactor = 1
    )
    
    # Further, get the AssignmentLengthDistributionData:
    AssignmentLengthDistributionData <- AssignmentLengthDistribution(LengthDistributionData, BioticAssignment)
    
    # Convert the NASCData inside the radius to acoustic density: 
    # The suppressWarnings avoids warnings if only one Haul is assigned in a Stratum, etc.:
    suppressWarnings(AcousticDensityData <- AcousticDensity(
        MeanNASCData = MeanNASCDataInside,
        AssignmentLengthDistributionData = AssignmentLengthDistributionData,
        AcousticTargetStrength = AcousticTargetStrength,
        SpeciesLink = SpeciesLink
    ))
    
    # Sum the acoustic density over length groups and beams (and SpeciesCategory):
    AcousticDensityData$Data[, Density := sum(Density, na.rm = TRUE), by = "PSU"]
    AcousticDensityData$Data <- unique(AcousticDensityData$Data, by = "PSU")
    
    ## Average the acoustic density, but trick MeanDensity() to average across all strata in the survey:
    #AcousticDensityData$Data[, Stratum := Survey]
    MeanAcousticDensityData <- MeanDensity(AcousticDensityData)
    
    # Extract the row with non-missing Layer, Survey:
    MeanAcousticDensityData$Data <- subset(MeanAcousticDensityData$Data, !is.na(Layer) & !is.na(Survey))
    
    # In MeanDensity it may happen that if there are both PSUs (actually EDSUs) with and without the target species (both empty and positive EDSUs), resulting two rows, one with SpeciesCategory NA and one with the first SpeciesCategory per PSU. The SpeciesCategory is not interesting at this point, as we are producing a Haul weight, which is not species specific. So we simply sum over all rows of the MeanAcousticDensityData$Data with na.rm = TRUE, so as to get a value both if al EDSUs are empty and if there is a mix of empty and posistive EDSUs:
    averageAcousticDensity <- MeanAcousticDensityData$Data[, sum(Density, na.rm = TRUE)]
    
    return(averageAcousticDensity)
}


# Function to sum up the WeightedNumber
addSumWeightedNumber <- function(BioticAssignment, LengthDistributionData, weightingVariable, inverse = FALSE) {
    
    if(!NROW(LengthDistributionData)) {
        stop("StoX: Non-empty LengthDistributionData is required to add SumWeightedNumber.")
    }
    
    # Make a copy of the LengthDistributionData to enable safe modification by reference:
    LengthDistributionDataCopy <- data.table::copy(LengthDistributionData)
    
    ### # Normalize the WeightedNumber:
    ### if(isLengthDistributionType(LengthDistributionData, "Standard")) {
    ###     LengthDistributionData[, WeightedNumber := WeightedNumber / EffectiveTowDistance]
    ### } 
    ### else if(!isLengthDistributionType(LengthDistributionData, "Normalized")) {
    ###     stop("The LengthDistributionType must be \"Standard\" (in which case the WeightedNumber will be divided by EffectiveTowDistance) or###  \"Normalized\"")
    ### }
    #if(!isLengthDistributionType(LengthDistributionData, c("Standard", "Normalized"))) {
    if(!any(endsWith(firstNonNA(LengthDistributionData$LengthDistributionType), c("Standard", "Normalized")))) {
        stop("The LengthDistributionType must be \"Standard\" or \"Normalized\" (ending with \"Standard\" or \"Normalized\")")
    }
    # Sum the WeightedNumber for each Haul. Here it makes sense to use na.rm = TRUE, as we are only looking for a sum of the data that are present:
    SumWeightedNumber <- LengthDistributionData[, .(SumWeightedNumber = sum(WeightedNumber, na.rm = TRUE)), by = "Haul"]
    
    # Merge the SumWeightedNumber into the BioticAssignment by the Haul identifier: 
    BioticAssignment <- merge(BioticAssignment, SumWeightedNumber, by = "Haul")
    
    # Copy the SumWeightedNumber into the weightingVariable:
    if(inverse) {
        #BioticAssignment[, eval(weightingVariable) := 1 / SumWeightedNumber]
        BioticAssignment[, eval(weightingVariable) := get(weightingVariable) / SumWeightedNumber]
    }
    else {
        #BioticAssignment[, eval(weightingVariable) := SumWeightedNumber]
        BioticAssignment[, eval(weightingVariable) := get(weightingVariable) * SumWeightedNumber]
    }
    
    BioticAssignment[]
}




checkOneSpeciesInLengthDistributionData <- function(LengthDistributionData, WeightingMethod) {
    # Remove NA here, as this check for only one species should only count non-missing SpeciesCategory:
    numSpecies <- length(unique(stats::na.omit(LengthDistributionData$SpeciesCategory)))
    if(numSpecies > 1) {
        #stop("Only one species is allowed in BioticAssignmentWeighting when WeightingMethod is ", paste(WeightingMethod, collapse = ", "))
        stop("Only one species is allowed in BioticAssignmentWeighting when WeightingMethod is ", paste(WeightingMethod, collapse = ", "), ". ", "Please make sure that the column SpeciesCategory of LengthDistributionData contains only one unique value. If the column contains missing values (NA, shown as \"-\" in the StoX GUI) there are hauls with no individuals of the requested species in the StoxBioticData. For acoustic-trawl estimates such hauls should be filtered out using FilterUpwards  = TRUE in FilterStoxBiotic().")
    }
}


checkOneSpeciesInStoxBioticData <- function(StoxBioticData, WeightingMethod) {
    # Remove NA here, as this check for only one species should only count non-missing SpeciesCategory:
    numSpecies <- length(unique(stats::na.omit(StoxBioticData$SpeciesCategory$SpeciesCategory)))
    if(numSpecies > 1) {
        #stop("Only one species is allowed in BioticAssignmentWeighting when WeightingMethod is ", paste(WeightingMethod, collapse = ", "))
        stop("Only one species is allowed in BioticAssignmentWeighting when WeightingMethod is ", paste(WeightingMethod, collapse = ", "), ". ", "Please make sure that the column SpeciesCategory of the table SpeciesCategory of  StoxBioticData contains only one unique value. If the column contains missing values (NA, shown as \"-\" in the StoX GUI) there are hauls with no individuals of the requested species in the StoxBioticData used as input to LengthDistribution(). For acoustic-trawl estimates such hauls should be filtered out using FilterUpwards  = TRUE in FilterStoxBiotic().")
    }
}















##################################################
#' Define a parametric or numeric model
#' 
#' @inheritParams general_arguments
#' @param modelClass The model class, such as Regression.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Table" to define a table directly (in the GUI), and ResourceFile to read a file.
#' @param ModelName The model to use.
#' @param ParameterTable A table holding the parameter values.
#' @param FileName A file from which to read the \code{ParameterTable}.
#' 
#' @export
#' 
DefineModel <- function(
    modelClass, 
    processData, UseProcessData = FALSE, 
    DefinitionMethod = c("Table", "ResourceFile"),
    ModelName, # e.g. c("SimpleLinear", "Power")
    ParameterTable = data.table::data.table(), 
    FileName = character()
) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- RstoxData::match_arg_informative(DefinitionMethod)
    
    # Get or read the model parameters and return in a list with the model name:
    output <- getModel(
        modelClass = modelClass, 
        ModelName = ModelName, 
        DefinitionMethod = DefinitionMethod, 
        ParameterTable = ParameterTable, 
        FileName = FileName
    )
    
    return(output)
}


getModel <- function(modelClass, ModelName, DefinitionMethod, ParameterTable, FileName) {
    
    # Read the table if requested, or issue an error if not given:
    if(DefinitionMethod == "ResourceFile") {
        if(!length(FileName) || !file.exists(FileName)) {
            stop("FileName must be the path to an existing file.")
        }
        ParameterTable <- data.table::fread(FileName, encoding = "UTF-8")
    }
    
    # Check the columns of the table:
    checkModel(modelClass, ParameterTable, ModelName)
    
    # Define the output as a list of the method and the table:
    outputModelLabel <- paste0(modelClass, "Model")
    outputTableLabel <- paste0(modelClass, "Table")
    output <- list(
        data.table::setnames(data.table::data.table(ModelName), outputModelLabel), 
        ParameterTable
    )
    names(output) <- c(outputModelLabel, outputTableLabel)
    
    return(output)
}


checkModel <- function(modelClass, ParameterTable, ModelName) {
    
    # Get and check the ModelName:
    modelParameters <- getRstoxBaseDefinitions("modelParameters")[[modelClass]]
    if(! ModelName %in% names(modelParameters)) {
        stop("Wrong ModelName Must be one of ", paste(names(modelParameters), collapse = ", "))
    }
    
    # Check that the ParameterTable contains the required columns:
    if(! all(modelParameters[[ModelName]] %in% names(ParameterTable))) {
        stop("The parameter table for ", ModelName, " must contain the required parameter columns; ", paste(modelParameters[[ModelName]], collapse = ", "))
    }
    
    # Check for duplicated keys:
    keys <- setdiff(
        names(ParameterTable), 
        modelParameters[[ModelName]]
    )
    dup <- duplicated(ParameterTable[, ..keys])
    if(any(dup)) {
        duprev <- duplicated(ParameterTable[, ..keys], fromLast = TRUE)
        alldup <- sort(unique(c(which(dup), which(duprev))))
        stop("The output from Define", modelClass, "() contains duplicated keys (", paste(keys, collapse = ", "), ")", " in rows ", paste(alldup, collapse = ", "), ".")
    }
}








##################################################
#' Acoustic target strength definition
#' 
#' This function returns a table of parameters specifying the acoustic target strength as a function of length for different values of user selected variables in the NASC data.
#' 
#' @inheritParams general_arguments
#' @inheritParams DefineModel
#' @param AcousticTargetStrengthModel  Character: The target strength model/function to use. Currently implemented are "LengthDependent", "LengthAndDepthDependent", "LengthExponent" and "TargetStrengthByLength". See Details.
#' @param AcousticTargetStrengthTable A table holding the specification of the target strength function/table. The first two columns are AcousticCategory and Frequency. See details for other columns.
#' @param FileName A file from which to read the \code{AcousticTargetStrengthTable}.
#' 
#' @details
#' The \code{AcousticTargetStrengthModel} has the following possible values: 
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
    # Note that "LengthExponent" is an option for AcousticTargetStrengthModel (used by BioticAssignmentWeighting()), but this is not shown.
    DefinitionMethod = c("ResourceFile", "Table"),
    AcousticTargetStrengthModel = c("LengthDependent", "LengthAndDepthDependent", "TargetStrengthByLength"), 
    AcousticTargetStrengthTable = data.table::data.table(), 
    FileName = character()
) {
    
    # Get the methods:
    AcousticTargetStrengthModel <- RstoxData::match_arg_informative(AcousticTargetStrengthModel)
    
    # Define the model:
    DefineModel(
        modelClass = "AcousticTargetStrength", 
        processData = processData, UseProcessData = UseProcessData, 
        DefinitionMethod = DefinitionMethod,
        ModelName = AcousticTargetStrengthModel, 
        ParameterTable = AcousticTargetStrengthTable, 
        FileName = FileName
    )
}






##################################################
##################################################
#' Define a regression model and parameters
#' 
#' This function defines a regression model with parameters, where the model can be one of a set of pre-defined models (see the argument \code{RegressionModel}). The parameters can either be defined in a table or read from a resource file.
#' 
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @inheritParams DefineModel
#' @param GroupingVariables An optional vector of strings defining variables seving as grouping variables in the RegressionTable. Setting this adds the its elements as columns in the RegressionTable in the GUI.
#' @param RegressionModel Character: A string naming the model to use for the regression. See Details for options.
#' @param RegressionTable A table with one row defining the name of the dependent variable (column name \code{DependentVariable}), the name of the independent variable (column name \code{IndependentVariable}), and the \code{Intersect} and \code{Slope} if \code{RegressionModel} = "SimpleLinear" and \code{Factor} and \code{Exponent} if \code{RegressionModel} = "Power".
#' @param FileName The path to a CSV file containing the columns \code{DependentVariable}), \code{IndependentVariable} and the \code{RegressionTable}.
#' 
#' @details 
#' The currently implemented models are listed below:
#' * SimpleLinear \deqn{DependentVariable = Intercept + Slope * IndependentVariable}
#' * Power \deqn{DependentVariable = Factor * IndependentVariable^{Exponent}}
#' @md
#' 
#' @return
#' An object of StoX data type \code{\link{Regression}}.
#' 
#' @seealso \code{\link{EstimateBioticRegression}} for estimating regression parameters from a \code{\link[RstoxData]{StoxBioticData}}, \code{\link{IndividualsData}} or \code{\link{SuperIndividualsData}} object, and  \code{\link{ImputeSuperIndividuals}} for applying the regression to \code{\link{SuperIndividualsData}}.
#' 
#' @export
#' 
DefineRegression <- function(
    processData, UseProcessData = FALSE, 
    DefinitionMethod = c("ResourceFile", "Table"),
    GroupingVariables = character(), 
    RegressionModel = c("SimpleLinear", "Power"), 
    RegressionTable = data.table::data.table(), 
    FileName = character()
) {
    
    # Get the methods:
    RegressionModel <- RstoxData::match_arg_informative(RegressionModel)
    
    # Define the model:
    DefineModel(
        modelClass = "Regression", 
        processData = processData, UseProcessData = UseProcessData, 
        DefinitionMethod = DefinitionMethod,
        ModelName = RegressionModel, 
        ParameterTable = RegressionTable, 
        FileName = FileName
    )
}







##################################################
##################################################
#' Estimate a regression model and parameters for biotic data
#' 
#' This function estimates a regression model with parameters, where the model can be one of a set of pre-defined models.
#' 
#' @inheritParams general_arguments
#' @inheritParams ModelData
#' @inheritParams DefineModel
#' @inheritParams DefineRegression
#' @param InputDataType The type of biotic data to estimate the regression parameters based on, one of "IndividualsData" and "SuperIndividualsData". See Details.
#' @param DependentVariable The name of the dependent variable (response variable).
#' @param DependentResolutionVariable (Optional) The name of the variable that gives the resolution of the \code{DependentVariable}. If the \code{DependentResolutionVariable} is given, half of the resolution is added to the \code{DependentVariable}, and the regression model is fitted at those values. E.g., if \code{DependentVariable} = "IndividualTotalLength" and \code{DependentResolutionVariable} = "LengthResolution", the regression model is fitted at IndividualTotalLength + LengthResolution/2.
#' @param IndependentVariable The name of the independent variable (explanatory variable).
#' @param IndependentResolutionVariable (Optional) The name of the variable that gives the resolution of the \code{IndependentVariable}. See also \code{DependentResolutionVariable}.
#' 
#' @details The \code{RegressionModel} "Power" performs a log-log transformed simple linear regression of the model Y ~ a X^b exp(epsilon), where the error term epsilon is assumed to follow the normal distibution with mean 0 (see \href{http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf}{fishR}).
#' 
#' When using IndividualTotalLength as \code{DependentVariable} it can happen that IndividualTotalLength = 0 in \code{\link{SuperIndividualsData}} due to lower length resolution in the \code{\link{QuantityData}} than in the \code{\link{IndividualsData}} going in to the \code{\link{SuperIndividuals}} function. In such cases the only option is to use InputDataType = "IndividualsData".
#' 
#' @return
#' An object of StoX data type \code{\link{Regression}}.
#' 
#' @seealso \code{\link{DefineRegression}} for defining regression parameters directly of from a file.
#' 
#' @export
#' 
EstimateBioticRegression <- function(
    InputDataType = c("IndividualsData", "SuperIndividualsData"), 
    RegressionModel = c("SimpleLinear", "Power"), 
    DependentVariable = character(), 
    DependentResolutionVariable = character(), 
    IndependentVariable = character(), 
    IndependentResolutionVariable = character(), 
    GroupingVariables = character(), 
    IndividualsData, 
    SuperIndividualsData
) {
    
    # Get the methods:
    InputDataType <- RstoxData::match_arg_informative(InputDataType)
    RegressionModel <- RstoxData::match_arg_informative(RegressionModel)
    
    # Get the appropriate data:
    #if(InputDataType == "StoxBioticData") {
    #    data <- data.table::copy(StoxBioticData)
    #}
    if(InputDataType == "IndividualsData") {
        data <- data.table::copy(IndividualsData)
    }
    else if(InputDataType == "SuperIndividualsData") {
        data <- data.table::copy(SuperIndividualsData)
    }
    
    # Add support for accidental empty string for the GroupingVariables (must be entered maually in the GUI):
    GroupingVariables <- GroupingVariables[nchar(GroupingVariables) > 0]
    # Check that the GroupingVariables are present in the data:
    if(length(GroupingVariables) && nchar(GroupingVariables) && !all(GroupingVariables %in% names(data))) {
        stop("All of the GroupingVariables must be present in the data (", paste(setdiff(GroupingVariables, names(data)), collapse = ", "), " not present)")
    }
    
    if(!length(DependentVariable) == 1) {
        stop("DependentVariable must be given as the name of the dependent variable in the regression.")
    }
    if(!length(IndependentVariable) == 1) {
        stop("IndependentVariable must be given as the name of the independent variable in the regression.")
    }
    
    # Adjust for the resolution variables (adding half of the resolution to get mid interval values):
    addHalfResolution(data = data, variable = DependentVariable, resolutionVariable = DependentResolutionVariable)
    addHalfResolution(data = data, variable = IndependentVariable, resolutionVariable = IndependentResolutionVariable)
    
    # Run the estimation by the GroupingVariables:
    RegressionTable <- data[, 
        getRegressionTable(
            RegressionModel = RegressionModel, 
            DependentVariable = DependentVariable, 
            DependentResolutionVariable = DependentResolutionVariable, 
            IndependentVariable = IndependentVariable, 
            IndependentResolutionVariable = IndependentResolutionVariable, 
            GroupingVariables = GroupingVariables, 
            EstimationMethod = EstimationMethod,
            data = .SD
        ), 
        by = GroupingVariables, 
        .SDcols = names(data)] # Inlcude all columns, as the default is to skip the 'by' columns.
    
    ## Since this is such a flexible datatype, we define the column order here, and use it on the RegressionTable below:
    #columnOrder <- c(
    #    GroupingVariables, 
    #    "DependentVariable", 
    #    "IndependentVariable", 
    #    getRstoxBaseDefinitions("modelParameters")$Regression[[RegressionModel]], 
    #    "ResidualStandardError",
    #    "EstimationMethod"
    #)
    #data.table::setcolorder(RegressionTable, columnOrder)
    
    # Form the output:
    Regression <- list(
        RegressionModel = data.table::data.table(
            RegressionModel = RegressionModel
        ), 
        RegressionTable = RegressionTable
    )
    
    return(Regression)
}


getRegressionTable <- function(
    RegressionModel, 
    DependentVariable, 
    DependentResolutionVariable, 
    IndependentVariable, 
    IndependentResolutionVariable, 
    GroupingVariables, 
    EstimationMethod,
    data
) {
    
    # Apply the default EstimationMethod. Non-default (such as non-linear) may come later:
    EstimationMethod <- getRstoxBaseDefinitions("defaultEstimationMethod")$Regression[[RegressionModel]]
    
    # Estimate the regression model:
    estimationFunction <- getRstoxBaseDefinitions("estimationFunctions")$Regression[[RegressionModel]]
    
    regressionSummary <- tryCatch(
        summary(
            estimationFunction(
                dependentVariable = DependentVariable, 
                independentVariable = IndependentVariable, 
                data = data
            )
        ), 
        error = function(err) {
            if(length(GroupingVariables)) {
                warning("StoX: Unable to estimate regression for ", paste(GroupingVariables, sapply(GroupingVariables, function(x) unique(data[[x]])), sep = " = " ), " (details: ", err, ").")
            }
            else {
                warning("StoX: Unable to estimate regression (details: ", err, ").")
            }
            
            # Return NAs:
            list(
                # Dims here are (number of parameters, 1) to fit the coeffisients table from lm() which has one row per parameter:
                coefficients = array(NA_real_, dim = c(length(getRstoxBaseDefinitions("modelParameters")$Regression[[RegressionModel]]), 1)), 
                sigma = NA_real_
            )
        }
    )
    
    
    # Get the model parameter names:
    modelParameters <- getRstoxBaseDefinitions("modelParameters")$Regression[[RegressionModel]]
    
    # Create a table with the parameters and residual standard error:
    sumNonMissing <- sum(!is.na(data[[DependentVariable]]) & !is.na(data[[IndependentVariable]]))
    # This test of NROW is due to lm skipping rows in the coefficients when failing to estimate (even though a row with NaN is displayed when printing the output):
    if(NROW(regressionSummary$coefficients) < length(modelParameters)) {
        if(length(GroupingVariables)) {
            warning("StoX: Insufficient data to estimate regression for ", paste(GroupingVariables, sapply(GroupingVariables, function(x) unique(data[[x]])), sep = " = " ), " (Number of non-missing values: ", sumNonMissing, ").")
        }
        else {
            warning("StoX: Insufficient data to estimate regression. NAs returned which may propagate to reports. (Number of non-missing values: ", sumNonMissing, ").")
        }
        
        RegressionTable <- data.table::as.data.table(
            c(
                structure(as.list(rep(NA_real_, length(modelParameters))), names = modelParameters), 
                list(ResidualStandardError = NA_real_)
            )
        )
    }
    else {
        RegressionTable <- data.table::as.data.table(
            c(
                structure(as.list(regressionSummary$coefficients[, 1]), names = modelParameters), 
                list(ResidualStandardError = regressionSummary$sigma)
            )
        )
    }
    
    # Add also the DependentVariable, IndependentVariable and GroupingVariables at the start, and EstimationMethod at the end:
    RegressionTable <- data.table::data.table(
        DependentVariable = DependentVariable, 
        DependentResolutionVariable = if(length(DependentResolutionVariable)) DependentResolutionVariable else NA_character_, 
        IndependentVariable = IndependentVariable, 
        IndependentResolutionVariable = if(length(IndependentResolutionVariable)) IndependentResolutionVariable else NA_character_, 
        RegressionTable, 
        EstimationMethod = EstimationMethod
    )
    
    return(RegressionTable)
}



##################################################
##################################################
#' Define Survey
#' 
#' This function defines the Strata associated to different surveys (in the sense that a separate estimate should be made for those strata). 
#' 
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @param DefinitionMethod Character: A string naming the method to use, one of "AllStrata", which defines all strata as the same survey named "Survey"; "Table", which requires the \code{SurveyTable} to be given; and "ResourceFile" to read from a project.xml file from StoX 2.7.
#' @param SurveyTable A table of the two columns Stratum and Survey.
#' @param FileName The path to the StoX 2.7 project.xml file to read StratumPolygon from, in the case that \code{DefinitionMethod} is "ResourceFile".
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
    DefinitionMethod = c("AllStrata", "Table", "ResourceFile"), 
    StratumPolygon, 
    SurveyTable = data.table::data.table(), 
    FileName = character()
) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- RstoxData::match_arg_informative(DefinitionMethod)
    
    # Read from a stoX 2.7 project.xml:
    if(DefinitionMethod == "ResourceFile") {
        
        if(basename(FileName) == "project.json") {
            SurveyTable <- readOneProcessDataByFunctionNameFromProjectJSON(FileName, functionName = "DefineSurvey")
        }
        else if(tolower(getResourceFileExt(FileName)) == "xml" && any(grepl("http://www.imr.no/formats/stox/v1", readLines(FileName, 5)))) {
            
            # Read the StratumPolygon from the project.xml file:
            StratumPolygon <- readStratumPolygonFrom2.7(FileName, remove_includeintotal = FALSE)
            
            # Define all strata that have includeintotal = TRUE as a survey named "Survey", and all others as individual surveys:
            SurveyTable <- data.table::data.table(
                Stratum = StratumPolygon$StratumName, 
                Survey = StratumPolygon$StratumName
            )
            indcludedInTotal <- StratumPolygon$includeintotal %in% TRUE
            SurveyTable[indcludedInTotal, Survey := "Survey"]
        }
        #else {
        #    stop("StoX: Invalid file. Must be a StoX project description file with file name project.json or project.xml")
        #}
        else {
            # Read the file as a table:
            SurveyTable <- tryCatch(
                data.table::fread(FileName), 
                error = function(err) {
                    stop("StoX: Error in data.table::fread: ", err)
                }
            )
            
            # Check whether all required columns are present:
            requiredPSUByTimeColumns <- getDataTypeDefinition(dataType = "Survey", unlist = TRUE)
            if(!all(requiredPSUByTimeColumns %in% names(SurveyTable))) {
                stop("StoX: Invalid file. Must be a StoX project description file with file name project.json or project.xml, or a file with a Survey definition table containing the columns ", paste(requiredPSUByTimeColumns, collapse = ", "), ".")
            }
        }
    }
    else {
        # Get the survey table using the stratum names:
        stratumNames <- getStratumNames(StratumPolygon)
        SurveyTable <- getSurveyTable(
            DefinitionMethod = DefinitionMethod, 
            stratumNames = stratumNames, 
            SurveyTable = SurveyTable, 
            FileName = FileName
        )
    }
    
    
    return(SurveyTable)
}


getSurveyTable <- function(
    DefinitionMethod, 
    stratumNames, 
    SurveyTable = data.table::data.table(), 
    FileName = character()
) {
    
    # Define one single survey:
    if(DefinitionMethod == "AllStrata") {
        if(!length(stratumNames)) {
            stop("stratumNames is of length 0. Perhaps none of the stations are inside and stratum?")
        }
        SurveyTable <- data.table::data.table(
            Stratum = stratumNames, 
            Survey = "Survey"
        )
    }
    ### # Or accept/reject the input SurveyTable:
    ### else if(DefinitionMethod == "Table") {
    ###     # Delete rows with missing Survey:
    ###     if(any(is.na(SurveyTable$Survey))) {
    ###         warning("StoX: Removing rows of missing Survey in SurveyTable")
    ###         SurveyTable <- SurveyTable[!is.na(Survey)]
    ###     }
    ###     if(any(is.na(SurveyTable$Stratum))) {
    ###         warning("StoX: Removing rows of missing Stratum in SurveyTable")
    ###         SurveyTable <- SurveyTable[!is.na(Stratum)]
    ###     }
    ###     # Delete also rows with unrecognized Stratum:
    ###     if(!all(SurveyTable$Stratum %in% stratumNames)) {
    ###         warning("StoX: Removing rows of Stratum not present in the SurveyTable")
    ###         SurveyTable <- SurveyTable[Stratum %in% stratumNames, ]
    ###     }
    ###     # If no rows in the SurveyTable, issue an error:
    ###     if(!nrow(SurveyTable)) {
    ###         stop("SurveyTable must be a table of at least one row, with Stratum and Survey as columns")
    ###     }
    ### }
    # Read the table from a file:
    else if(DefinitionMethod == "ResourceFile") {
        SurveyTable <- data.table::fread(FileName)
        data.table::setnames(SurveyTable, names(SurveyTable), tools::toTitleCase(names(SurveyTable)))
        if(!all(c("Survey", "Stratum") %in% names(SurveyTable))) {
            stop("The file ", FileName, " does not have column names \"Survey\" and \"Stratum\" (present columns are ", paste(names(SurveyTable), collapse = ", "), ").")
        }
    }
    # Delete rows with missing Survey:
    if(any(is.na(SurveyTable$Survey))) {
        warning("StoX: Removing rows of missing Survey in SurveyTable")
        SurveyTable <- SurveyTable[!is.na(Survey)]
    }
    if(any(is.na(SurveyTable$Stratum))) {
        warning("StoX: Removing rows of missing Stratum in SurveyTable")
        SurveyTable <- SurveyTable[!is.na(Stratum)]
    }
    # Delete also rows with unrecognized Stratum:
    if(!all(SurveyTable$Stratum %in% stratumNames)) {
        warning("StoX: Removing the following (not used) Strata from in the SurveyTable: ", RstoxData::printErrorIDs(setdiff(SurveyTable$Stratum, stratumNames)))
        SurveyTable <- SurveyTable[Stratum %in% stratumNames, ]
    }
    # If no rows in the SurveyTable, issue an error:
    if(!nrow(SurveyTable)) {
        stop("SurveyTable must be a table of at least one row, with Stratum and Survey as columns")
    }
    
    return(SurveyTable)
}





#' Format process data
#' 
#' @param processData A StoX process data (list of sf or data.table)
#' @param columnClasses A list of lists of classes of the columns of process data tables, as read from process data schemas. Used only in RstoxFramework.
#' 
#' @export
#'
formatProcessData <-  function(processData, columnClasses = NULL) {
    if(!is.list(processData)) {
        stop("StoX: ProcessData must be a list consisting of objects of classes data.table or sf.")
    }
    
    if(length(processData)) {
        processData <- mapply(formatProcessDataOne, processDataName = names(processData), processDataOne = processData, MoreArgs = list(columnClasses = columnClasses), SIMPLIFY = FALSE)
    }
    
    return(processData)
}


formatProcessDataOne <-  function(processDataName, processDataOne, columnClasses = NULL) {
    
    if(!length(processDataOne)) {
        processDataOne <- data.table::data.table()
    }
    # Convert to sf:
    else if("features" %in% tolower(names(processDataOne))) {
        # Using geojsonsf instead of geojsonio to reduce the number of dependencies:
        #processDataOne <- geojsonio::geojson_sp(toJSON_Rstox(processDataOne, pretty = TRUE))
        # Check for empty multipolygon, which is not well treated by sf:
        StratumPolygon <- geojsonsf::geojson_sf(toJSON_Rstox(processDataOne, pretty = TRUE))
        
        if(length(StratumPolygon$geometry)) {
            
            # Set the assumed pojection:
            suppressWarnings(sf::st_crs(StratumPolygon) <- getRstoxBaseDefinitions("proj4string_longlat"))
            # Make sure that the StratumPolygon is a MULTIPOLYGON object:
            StratumPolygon <- sf::st_cast(StratumPolygon, "MULTIPOLYGON")
            
            # Add names:
            processDataOne <- addStratumNames(StratumPolygon, accept.wrong.name.if.only.one = TRUE)
        }
        else {
            processDataOne <- getRstoxBaseDefinitions("emptyStratumPolygon")
        }
    }
    # If a data.table:
    else if(length(processDataOne) && data.table::is.data.table(processDataOne)) {
        
        convertStringToNA(processDataOne)
        ## Set numeric NAs:
        #jsonNA <- getRstoxFrameworkDefinitions("jsonNA")
        #decodeNumericNAOneProcessData(processDataOne, na = jsonNA)
        
        if(length(columnClasses)) {
            convertClassOfDataTable(processDataOne, columnClasses = columnClasses[[processDataName]])
        }
        
        
        convertToPosixInDataTable(processDataOne)
    }
    # Otherwise try to convert to data.table:
    else if(length(processDataOne) && is.convertableToTable(processDataOne)) {
        # Why was this extremely slow method used, where converting to and then from JSON slows things down imensely?:
        # processDataOne <- simplifyListReadFromJSON(processDataOne)
        # processDataOne <- data.table::as.data.table(processDataOne)
        
        # Convert to data.table (suppress warnings like "Column 3 [''] of item 1 is length 0. This (and 0 others like it) has been filled with NA (NULL for list columns) to make each item uniform."):
        suppressWarnings(processDataOne <- data.table::rbindlist(processDataOne))
        
        convertStringToNA(processDataOne)
        ## Set numeric NAs:
        #jsonNA <- getRstoxFrameworkDefinitions("jsonNA")
        #decodeNumericNAOneProcessData(processDataOne, na = jsonNA)
        
        if(length(columnClasses)) {
            convertClassOfDataTable(processDataOne, columnClasses = columnClasses[[processDataName]])
        }
        
        convertToPosixInDataTable(processDataOne)
    }
    else {
        stop("StoX: ProcessData must be a list consisting of objects of classes data.table or sf.")
    }
    
    return(processDataOne)
}



convertStringToNA <- function(x) {
    chcols = names(x)[sapply(x, is.character)]
    #x[, (chcols) := lapply(.SD, replace, as.is=TRUE), .SDcols=chcols] # Changed to numeric when not intended
    x[, (chcols) := lapply(.SD, function(x) ifelse(x == "NA", NA, x)), .SDcols = chcols]
}

# Function to convert column classes of a data.table given a list of variablename-class pairs:
convertClassOfDataTable <- function(x, columnClasses) {
    for(col in intersect(names(x), names(columnClasses))) {
        fun <- paste("as", columnClasses[[col]], sep = ".")
        x[, eval(col) := do.call(fun, list(get(col)))]
    }
}


convertToPosixInDataTable <- function(x) {
    convertableToPOSIX <- unlist(x[, lapply(.SD, is.ConvertableToPOSIX)])
    if(any(convertableToPOSIX)) {
        DateTimeColumns <- names(x)[convertableToPOSIX]
        x[, (DateTimeColumns) := lapply(.SD, convertToPOSIX), .SDcols = DateTimeColumns]
    }
}


is.convertableToTable <- function(x, minLength = 1) {
    # If all elements of the list x are lists with equal length, x is convertable to data.table:
    length(x) && 
        is.list(x) && # The input must be a list
        all(sapply(x, is.list)) && # ... and a list of lists
        RstoxBase::allEqual(lengths(x)) && # ... and all must be of equal length
        all(lengths(x) >= minLength) && # ... and longer than 1
        !is.list(x[[1]][[1]]) # ... and finally, each list must not contain lists. We only check the first element here
}

#' Convert to JSON
#' 
#' This function takes care of the defaults preferred by the Rstox packages
#' 
#' @param x An object to convert to JSON.
#' @param ... Parameters overriding the defaults digits = NA, auto_unbox = TRUE, na = "null", null = "null".
#' 
#' @export
#' 
toJSON_Rstox <- function(x, ...) {
    # Define defaults:
    digits <- NA
    auto_unbox <- TRUE
    # Changed on 2021-04-21 to supports NA strings:
    #na <- "null"
    na <- "string"
    na <- "null"
    null <- "null"
    
    # Override by ...:
    lll <- list(...)
    
    if(!"digits" %in% names(lll)) {
        lll$digits <- digits
    }
    if(!"auto_unbox" %in% names(lll)) {
        lll$auto_unbox <- auto_unbox
    }
    if(!"na" %in% names(lll)) {
        lll$na <- na
    }
    if(!"null" %in% names(lll)) {
        lll$null <- null
    }
    
    #lll$x <- x
    lll <- c(list(x = x), lll
    )
    
    # Use ISO8601 for time:
    lll$POSIXt ="ISO8601"
    
    do.call(jsonlite::toJSON, lll)
}



is.ConvertableToPOSIX <- function(x) {
    if(is.character(x)) {
        # Convert to POSIX:
        POSIX <- convertToPOSIX(x)
        any(!is.na(POSIX))
    }
    else {
        FALSE
    }
}


convertToPOSIX <- function(x) {
    # Get the DateTime format used by StoX:
    StoxDateTimeFormat <- RstoxData::getRstoxDataDefinitions("StoxDateTimeFormat")
    StoxTimeZone <- RstoxData::getRstoxDataDefinitions("StoxTimeZone")
    
    # Convert to POSIX:
    POSIX <- as.POSIXct(x, format = StoxDateTimeFormat, tz = StoxTimeZone)
    
    return(POSIX)    
}

