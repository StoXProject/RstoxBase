# DefineAcousticPSU
# DefineAcousticLayer
# DefineSweptAreaPSU
# DefineStrata
# DefineSurvey


##################################################
##################################################
#' Definne PSU
#' 
#' Underlying function for \code{\link{DefineSweptAreaPSU}} and \code{\link{DefineAcousticPSU}}.
#' 
#' @inheritParams DefineStrata
#' @param StratumPolygon    The \code{\link{StratumPolygon}} process data.
#' @param StoxData          Either \code{\link[RstoxData]{StoxBioticData}} or \code{\link[RstoxData]{StoxAcousticData}} data.
#' @param DefinitionMethod  Character: A string naming the method to use, see \code{\link{DefineSweptAreaPSU}} and \code{\link{DefineAcousticPSU}}.
#' @param modelType         Character: A string naming the type of model, either "Acoustic" or "SweptArea".
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{AcousticPSU}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{AcousticPSU}}.
#' 
#' @export
#' @import data.table
#' 
DefinePSU <- function(processData, StratumPolygon, StoxData, DefinitionMethod = c("Identity", "None"), UseProcessData = FALSE, modelType = c("Acoustic", "SweptArea")) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod and modelType:
    DefinitionMethod <- match.arg(DefinitionMethod)
    modelType <- match.arg(modelType)
    
    # SSULevel
    if(modelType == "Acoustic") {
        SSULevel <- "Log"
        SSUName <- "EDSU"
        #prefix <- "T"
        prefix <- getRstoxBaseDefinitions("AcousticPSUPrefix")
    }
    else if(modelType == "SweptArea") {
        SSULevel <- "Station"
        SSUName <- "Station"
        #prefix <- "S"
        prefix <- getRstoxBaseDefinitions("SweptAreaPSUPrefix")
    }
    else {
        stop("Unknown model type")
    }
    
    # Get SSUs:
    SSU <- StoxData[[SSULevel]][[SSUName]]
    
    # Get the stratum names:
    StratumNames = getStratumNames(StratumPolygon)
    
    # Use each SSU as a PSU:
    if(grepl("Identity", DefinitionMethod, ignore.case = TRUE)) {
        
        # Define PSUIDs and PSUNames:
        PSUID <- seq_along(SSU)
        PSUName <- paste0(prefix, formatC(PSUID, width = max(nchar(PSUID)), format = "d", flag = "0"))
        
        # Set each SSU as a PSU:
        SSU_PSU <- data.table::data.table(
            SSU = SSU, 
            PSU = PSUName
        )
        
        # Find the stratum of each PSU:
        SpatialPSUs <- sp::SpatialPoints(StoxData[[SSULevel]][, c("Longitude", "Latitude")])
        
        StratumIndex <- sp::over(SpatialPSUs, StratumPolygon)
        # Converting from data frame to character vector 
        StratumIndex <- as.numeric(unlist(StratumIndex))
        NonEmptyStrata <- StratumNames[StratumIndex]
        
        # Create the Stratum_PSU data.table:
        Stratum_PSU <- data.table::data.table(
            Stratum = NonEmptyStrata, 
            PSU = PSUName
        )
        
        # Remove PSUs that do not have a stratum:
        validPSUs <- unique(Stratum_PSU$PSU[!is.na(Stratum_PSU$Stratum)])
        Stratum_PSU <- Stratum_PSU[ PSU %in% validPSUs ]
        #SSU_PSU <- SSU_PSU[ PSU %in% validPSUs ]
        #SSU_PSU$PSU[! SSU_PSU$PSU %in% validPSUs] <- NA
        
        SSU_PSU[! PSU %in% validPSUs, PSU := NA]
        
        #SSU_PSU[, PSU := ifelse(PSU %in% validPSUs, validPSUs, "")]
        
        
    }
    # Otherwise return empty Stratum_PSU and SSU_PSU with all SSUs and empty string as PSU:
    else {
        SSU_PSU <- data.table::data.table(
            SSU = SSU, 
            PSU = ""
        )
        Stratum_PSU <- data.table::data.table()
    }
    
    # Rename the data according to the model type:
    data.table::setnames(SSU_PSU, "SSU", SSUName)
    out <- structure(
        list(
            Stratum_PSU, 
            SSU_PSU
        ), 
        names = c("Stratum_PSU", paste(SSUName, "PSU", sep = "_"))
    )
    # Add a list of all strata:
    out$Stratum <- data.table::data.table(
        Stratum = getStratumNames(StratumPolygon)
    )
    
    return(out)
}


##################################################
##################################################
#' SweptArea PSU
#' 
#' This function defines the \code{\link{SweptAreaPSU}} process data, linking strata, swept-area PSUs and Stations 
#' 
#' @inheritParams DefineStrata
#' @inheritParams DefinePSU
#' @param StoxBioticData    The \code{\link[RstoxData]{StoxBioticData}} data.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "StationToPSU", which sets each Station as a PSU, and "None" for pure manual actions by the user.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{SweptAreaPSU}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{SweptAreaPSU}}.
#' 
#' @export
#' @import data.table
#' 
DefineSweptAreaPSU <- function(processData, StratumPolygon, StoxBioticData, DefinitionMethod = c("StationToPSU", "None"), UseProcessData = FALSE) {
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    if(grepl("StationToPSU", DefinitionMethod, ignore.case = TRUE)) {
        DefinitionMethod <- "Identity"
    }
    
    SweptAreaPSU <- DefinePSU(
        processData = processData, 
        StratumPolygon = StratumPolygon, 
        StoxData = StoxBioticData, 
        DefinitionMethod = DefinitionMethod, 
        UseProcessData = UseProcessData, 
        modelType = "SweptArea"
    )
    
    return(SweptAreaPSU)
}


##################################################
##################################################
#' Acoustic PSU
#' 
#' This function defines the \code{\link{AcousticPSU}} process data, linking strata, acoustic PSUs and EDSUs. 
#' 
#' @inheritParams DefineStrata
#' @inheritParams DefinePSU
#' @param StoxAcousticData  The \code{\link[RstoxData]{StoxAcousticData}} data.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "EDSUToPSU", which sets each EDSU as a PSU, and "None" for pure manual actions by the user.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{AcousticPSU}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{AcousticPSU}}.
#' 
#' @export
#' @import data.table
#' 
DefineAcousticPSU <- function(processData, StratumPolygon, StoxAcousticData, DefinitionMethod = c("EDSUToPSU", "None"), UseProcessData = FALSE) {
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    if(grepl("EDSUToPSU", DefinitionMethod, ignore.case = TRUE)) {
        DefinitionMethod <- "Identity"
    }
    
    AcousticPSU <- DefinePSU(
        processData = processData, 
        StratumPolygon = StratumPolygon, 
        StoxData = StoxAcousticData, 
        DefinitionMethod = DefinitionMethod, 
        UseProcessData = UseProcessData, 
        modelType = "Acoustic"
    )
    
    return(AcousticPSU)
}


##################################################
##################################################
#' Define Layers
#' 
#' This function defines the \code{\link{SweptAreaLayer}} process data, which sets the range intervals of the swetp-area layers used in swept-area estimation models in StoX.
#' 
#' @inheritParams DefineStrata
#' @param StoxData          Either \code{\link[RstoxData]{StoxBioticData}} or \code{\link[RstoxData]{StoxAcousticData}} data.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "WaterColumn", to define one single for the entire water column; "HighestResolution", to use the maximum possible vertical resolution without intersecting hauls; "Resolution", which can be used to set a fixed layer thickness; and "LayerTable" to provide the \code{LayerTable}.
#' @param Resolution  Numeric: A single numeric giving the thickness of the layers.
#' @param LayerTable A table of Layer name, MinLayerDepth in meters and MaxLayerDepth in meters, defining the Layers.
#' @param modelType Character: A string naming the type of model, either "Acoustic" or "SweptArea".
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{SweptAreaLayer}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{SweptAreaPSU}}.
#' 
#' @export
#' @import data.table
#' 
DefineLayer <- function(processData, StoxData, DefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), Resolution = double(), LayerTable = data.table::data.table(), UseProcessData = FALSE, modelType = c("Acoustic", "SweptArea")) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    # Get the DefinitionMethod:
    modelType <- match.arg(modelType)
    
    
    # SSULevel
    if(modelType == "Acoustic") {
        VerticalResolutionLevel <- "NASC"
        VerticalResolutionMin <- "MinChannelRange"
        VerticalResolutionMax <- "MaxChannelRange"
    }
    else if(modelType == "SweptArea") {
        VerticalResolutionLevel <- "Haul"
        VerticalResolutionMin <- "MinHaulDepth"
        VerticalResolutionMax <- "MaxHaulDepth"
    }
    else {
        stop("Unknown model type")
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
    
    # Get the common intervals:
    possibleIntervals <- getCommonIntervals(
        data = unique(StoxData[[VerticalResolutionLevel]][, c(..VerticalResolutionMin, ..VerticalResolutionMax)]), 
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


##################################################
##################################################
#' Define Acoustic Layer
#' 
#' This function defines the \code{\link{AcousticLayer}} process data, which sets the range intervals of the acoustic layers used in acoustic-trawl estimation models in StoX. 
#' 
#' @inheritParams DefineStrata
#' @inheritParams DefineLayer
#' @inheritParams DefineAcousticPSU
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{AcousticLayer}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{AcousticPSU}}.
#' 
#' @export
#' @import data.table
#' 
DefineAcousticLayer <- function(processData, StoxAcousticData, DefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), Resolution = double(), LayerTable = data.table::data.table(), UseProcessData = FALSE) {
    
    DefineLayer(
        processData = processData, 
        StoxData = StoxAcousticData, 
        DefinitionMethod = DefinitionMethod, 
        Resolution = Resolution, 
        LayerTable = LayerTable, 
        UseProcessData = UseProcessData, 
        modelType = "Acoustic"
    )
}


##################################################
##################################################
#' Define Swept-area Layer
#' 
#' This function defines the \code{\link{SweptAreaLayer}} process data, which sets the range intervals of the swetp-area layers used in swept-area estimation models in StoX.
#' 
#' @inheritParams DefineStrata
#' @inheritParams DefineLayer
#' @inheritParams DefineSweptAreaPSU
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{SweptAreaLayer}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{SweptAreaPSU}}.
#' 
#' @export
#' @import data.table
#' 
DefineSweptAreaLayer <- function(processData, StoxBioticData, DefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "LayerTable"), Resolution = double(), LayerTable = data.table::data.table(), UseProcessData = FALSE) {
    
    DefineLayer(
        processData = processData, 
        StoxData = StoxBioticData, 
        DefinitionMethod = DefinitionMethod, 
        Resolution = Resolution, 
        LayerTable = LayerTable, 
        UseProcessData = UseProcessData, 
        modelType = "SweptArea"
    )
}


##################################################
##################################################
#' Assignnment of biotic hauls to acoustic PSUs
#' 
#' This function defines the \code{\link{BioticAssignment}} process data, linking biotic Hauls with acoustic PSUs.
#' 
#' @inheritParams DefineStrata
#' @inheritParams DefineSweptAreaPSU
#' @inheritParams DefineAcousticPSU
#' @inheritParams SumNASC
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Stratum", assign all stations of each stratum to all acoustic PSUs; "Radius", to assign all stations within the radius given in \code{Radius} to each acoustic PSU; and "EllipsoidalDistance" to provide the \code{EllipsoidalDistanceTable} specifying the axes of an ellipsoid inside which to assign stations to acoustic PSUs.
#' @param AcousticPSU       The \code{\link{AcousticPSU}} process data.
#' @param AcousticLayer     The \code{\link{AcousticLayer}} process data.
#' @param Radius            Numeric: The radius inside which to assign biotic stations to each acoustic PSU.
#' @param EllipsoidalDistanceTable     Not yet implemented.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{BioticAssignment}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{BioticAssignment}}.
#' 
#' @export
#' @import data.table
#' 
DefineBioticAssignment <- function(
    processData, 
    NASCData, StoxBioticData, 
    DefinitionMethod = c("Stratum", "Radius", "EllipsoidalDistance"), 
    StoxAcousticData, 
    AcousticPSU, AcousticLayer, 
    StratumPolygon, 
    Radius = double(), EllipsoidalDistanceTable = data.table::data.table(), 
    #MinNumStations = integer(), RefGCDistance = double(), RefTime = "", RefBotDepth = double(), RefLatitude = double(), RefLongitude = double(), 
    UseProcessData = FALSE) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    # If DefinitionMethod == "Stratum", assign all stations of each stratum to all PSUs of the stratum:
    if(grepl("Stratum", DefinitionMethod, ignore.case = TRUE)) {
        
        # Create a spatial points object:
        SpatialStations <- sp::SpatialPoints(StoxBioticData$Station[, c("Longitude", "Latitude")])
        
        # Get the stratum for each point:
        StratumIndex <- sp::over(SpatialStations, StratumPolygon)
        StratumIndex <- as.numeric(unlist(StratumIndex))
        Stratum <- getStratumNames(StratumPolygon)[StratumIndex]
        
        # Create a list of the stations of each stratum:
        #stationIndex <- as.numeric(names(StratumIndex))
        #stationList <- split(StoxBioticData$Station$Station[stationIndex], Stratum)
        stationList <- split(StoxBioticData$Station$Station, Stratum)
        
        # Use all hauls of each station in the automatic assignment method "Stratum":
        Station_Haul <- merge(
            StoxBioticData$Station, 
            StoxBioticData$Haul, 
            by = intersect(names(StoxBioticData$Station), names(StoxBioticData$Haul))
        )
        
        # Small funciton to get the hauls of requested stations:
        getAllHaulsOfStations <- function(Station, Station_Haul) {
            requestedStations <- Station_Haul$Station %in% Station
            Station_Haul$Haul[requestedStations]
        }
        haulList <- lapply(stationList, getAllHaulsOfStations, Station_Haul = Station_Haul)
        
        # Issue an error if there are strata to be used which do not contain any hauls:
        uniqueStrata <- unique(NASCData$Stratum)
        nHaulsOfUniqueStrata <- lengths(haulList[uniqueStrata])
        hasNoHauls <- which(nHaulsOfUniqueStrata == 0)
        if(length(hasNoHauls)) {
            stop("The following strata containing acoustic PSUs have no biotic Hauls: ", paste(uniqueStrata[hasNoHauls], collapse = ", "))
        }
        
        # Link the stratum to PSU:
        dupRows <- duplicated(NASCData[, c("PSU", "Layer")])
        NASCData <- NASCData[!dupRows, ]
        Haul <- haulList[NASCData$Stratum]
        BioticAssignment <- cbind(
            NASCData[, c("Stratum", "PSU", "Layer")], 
            Haul = Haul, 
            WeightingFactor = lapply(Haul, function(x) rep(1, length(x)))
        )
    }
    else {
        stop("Only DefinitionMethod = Stratum currently implemented")
    }
    
    return(BioticAssignment)
}


##################################################
#' Acoustic target strength definition
#' 
#' This function returns a table of parameters specifying the acoustic target strength as a function of length for different values of user selected variables in the NASC data.
#' 
#' @inheritParams DefineStrata
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Table", for providing the acoustic target strength parameters in the table \code{ParameterTable}; and "ResourceFile" for reading the acoustic tfarget strength table from the text file \code{FileName}.
#' @param ParameterTable A table of the columns AcousticCategory, Frequency, m, a and d.
#' @param FileName A file from which to read the \code{ParameterTable}.
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
#' @export
#' 
DefineAcousticTargetStrength <- function(processData, DefinitionMethod = c("Table", "ResourceFile"), ParameterTable = data.table::data.table(), FileName, UseProcessData = FALSE) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    if(DefinitionMethod == "Table") {
        if(length(ParameterTable) == 0) {
            stop("ParameterTable must be given if DefinitionMethod = \"Table\"")
        }
    }
    else if(DefinitionMethod == "ResourceFile") {
        ParameterTable <- data.table::fread(FileName)
    }
    
    # Check that the ParameterTable contains only valid columns:
    #checkAcousticTargetStrengthEquationType(ParameterTable)
    #
    #checkAcousticTargetStrengthPresentColumns(ParameterTable)
    
    return(ParameterTable)
}


checkAcousticTargetStrengthPresentColumns <- function(ParameterTable) {
    
    # Get the valid columns of the NASCData:
    NASCDataDefinition <- getDataTypeDefinition("NASCData", unlist = TRUE)
    
    targetStrengthParameters <- getRstoxBaseDefinitions("targetStrengthParameters")
    
    validColumnNames <- c(
        NASCDataDefinition, 
        unique(unlist(targetStrengthParameters[ParameterTable$EquationType])), 
        "EquationType"
    )
    invalidColumns <- setdiff(
        names(ParameterTable), 
        validColumnNames
    )
    
    if(length(invalidColumns)) {
        stop("The acoustic target strength parameter table containes the following inavlid columns: ", paste(invalidColumns, collapse = ", "))
    }
}

checkAcousticTargetStrengthEquationType <- function(ParameterTable) {
    
    # Check that EquationType is given:
    #if(length(ParameterTable$EquationType) == 0) {
    #    stop("EquationType must be gievn")
    #}
    ## Check that all values are equal in the EquationType:
    #if(! all(ParameterTable$EquationType == ParameterTable$EquationType[1])) {
    #    stop("EquationType must be the same in all rows")
    #}
    
    # EquationType:
    targetStrengthParameters <- getRstoxBaseDefinitions("targetStrengthParameters")
    for(type in names(targetStrengthParameters)) {
        if(ParameterTable$EquationType[1] == type) {
            if(! all(targetStrengthParameters[[type]] %in% names(ParameterTable))) {
                stop("With EquationType = \"", type, "\" the columns ", paste(targetStrengthParameters[[type]], collapse = ", "), " must be given.")
            }
        }
    }
    if(! ParameterTable$EquationType[1] %in% names(targetStrengthParameters)) {
        stop("Invalid EquationType.")
    }
}


