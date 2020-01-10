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
#' @param StratumPolygon    A list of \code{\link{StratumPolygon}} process data.
#' @param StoxData  A list of \code{\link[roxygen2]{StoxAcousticData}} data.
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
#' @noRd
#' 
DefinePSU <- function(processData, StratumPolygon, StoxData, DefinitionMethod = c("Identity", "None"), UseProcessData = FALSE, modelType = c("Acoustic", "SweptArea")) {
    
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
        SSULevel <- "Log"
        SSUName <- "EDSU"
        prefix <- "T"
    }
    else if(modelType == "SweptArea") {
        SSULevel <- "Station"
        SSUName <- "Station"
        prefix <- "S"
    }
    else {
        stop("Unknown model type")
    }
    
    # Use each SSU as a PSU:
    if(grepl("Identity", DefinitionMethod, ignore.case = TRUE)) {
        
        # Define PSUIDs and PSUNames:
        SSU <- StoxData[[SSULevel]][[SSUName]]
        PSUID <- seq_along(SSU)
        PSUName <- paste0(prefix, formatC(PSUID, width = nchar(max(PSUID)), format = "d", flag = "0"))
        
        # Set each SSU as a PSU:
        PSU_SSU <- data.table::data.table(
            PSU = PSUName, 
            SSU = SSU
        )
        
        # Find the stratum of each PSU:
        SpatialPSUs <- sp::SpatialPoints(StoxData[[SSULevel]][, c("Longitude", "Latitude")])
        StratumIndex <- sp::over(SpatialPSUs, StratumPolygon)
        # Converting from data frame to character vector 
        StratumIndex <- as.numeric(unlist(StratumIndex))
        Stratum <- getStratumNames(StratumPolygon)[StratumIndex]
        
        # Create the Stratum_PSU data.table:
        Stratum_PSU <- data.table::data.table(
            Stratum = Stratum, 
            PSU = PSUName
        )
        
        # Remove PSUs that do not have a stratum:
        validPSUs <- Stratum_PSU$PSU[!is.na(Stratum_PSU$Stratum)]
        Stratum_PSU <- Stratum_PSU[ PSU %in% validPSUs ]
        PSU_SSU <- PSU_SSU[ PSU %in% validPSUs ]
        
        
    }
    # Otherwise return empty tables:
    else {
        PSU_SSU <- data.table::data.table()
        Stratum_PSU <- data.table::data.table()
    }
    
    # Rename the data according to the model type:
    data.table::setnames(PSU_SSU, "SSU", SSUName)
    out <- structure(list(Stratum_PSU, PSU_SSU), names = c("Stratum_PSU", paste("PSU", SSUName, sep = "_")))
    return(out)
}


##################################################
##################################################
#' SweptArea PSU
#' 
#' This function defines the \code{\link{SweptAreaPSU}} process data, linking strata, swept-area PSUs and Stations 
#' 
#' @inheritParams DefineStrata
#' @param StratumPolygon    A list of \code{\link{StratumPolygon}} process data.
#' @param StoxBioticData    A list of \code{\link[roxygen2]{StoxBioticData}} data.
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
    
    DefinePSU(
        processData = processData, 
        StratumPolygon = StratumPolygon, 
        StoxData = StoxBioticData, 
        DefinitionMethod = DefinitionMethod, 
        UseProcessData = UseProcessData, 
        modelType = "SweptArea"
    )
}


##################################################
##################################################
#' Acoustic PSU
#' 
#' This function defines the \code{\link{AcousticPSU}} process data, linking strata, acoustic PSUs and EDSUs. 
#' 
#' @inheritParams DefineStrata
#' @param StratumPolygon    A list of \code{\link{StratumPolygon}} process data.
#' @param StoxAcousticData  A list of \code{\link[roxygen2]{StoxAcousticData}} data.
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
    
    DefinePSU(
        processData = processData, 
        StratumPolygon = StratumPolygon, 
        StoxData = StoxAcousticData, 
        DefinitionMethod = DefinitionMethod, 
        UseProcessData = UseProcessData, 
        modelType = "Acoustic"
    )
}


##################################################
##################################################
#' Define Layers
#' 
#' This function defines the \code{\link{SweptAreaLayer}} process data, which sets the range intervals of the swetp-area layers used in swept-area estimation models in StoX.
#' 
#' @inheritParams DefineStrata
#' @inheritParams DefineAcousticLayer
#' @param StoxBioticData  A list of \code{\link[RstoxDatas]{StoxBioticData}} data.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "EDSUToPSU", which sets each EDSU as a PSU, and "None" for pure manual actions by the user.
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
DefineLayer <- function(processData, StoxData, DefinitionMethod = c("WaterColumn", "HighestResolution", "UserDefined"), Resolution = double(), LayerTable = data.table::data.table(), UseProcessData = FALSE, modelType = c("Acoustic", "SweptArea")) {
    
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
        VerticalResolutionMin <- "MinRange"
        VerticalResolutionMax <- "MaxRange"
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
                MinLayerRange = x[-length(x)], 
                MaxLayerRange = x[-1]
            )
        }
        else {
            names(x) <- c("MinLayerRange", "MaxLayerRange")
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
        lowerName = "MinLayerRange", 
        upperName = "MaxLayerRange"
    )
    
    # If "WaterColumn" is requested use the full range:
    if(grepl("WaterColumn", DefinitionMethod, ignore.case = TRUE)) {
        Layer <- data.table::data.table(
            Layer = "WaterColumn", 
            #MinLayerRange = possibleIntervals[1, 1], 
            MinLayerRange = 0,
            #MaxLayerRange = possibleIntervals[nrow(possibleIntervals), 2]
            MaxLayerRange = Inf
        )
    }
    
    # If "HighestResolution" is requested use all possible breaks:
    else if(grepl("HighestResolution", DefinitionMethod, ignore.case = TRUE)) {
        Layer <- createLayerTable(possibleIntervals)
    }
    
    # If "UserDefined" is requested match the Breaks against the possible breaks:
    else if(grepl("UserDefined", DefinitionMethod, ignore.case = TRUE)) {
        # Error if any of the specified breaks are invalid:
        if(any(! unlist(LayerTable[, c("MinLayerRange", "MaxLayerRange")]) %in% unlist(possibleIntervals))) {
            stop("Some of the specified breaks are not at common breaks of all Log(distance)s. Possible breaks are [", paste(unlist(possibleIntervals), collapse = ", "), "]")
        }
        else {
            Layer <- LayerTable
        }
    }
    
    else {
        stop("Invalid DefinitionMethod")
    }
    
    
    return(Layer)
}


##################################################
##################################################
#' Acoustic Layer
#' 
#' This function defines the \code{\link{AcousticLayer}} process data, which sets the range intervals of the acoustic layers used in acoustic-trawl estimation models in StoX. 
#' 
#' @inheritParams DefineStrata
#' @param StoxAcousticData  A list of \code{\link[RstoxDatas]{StoxAcousticData}} data.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "EDSUToPSU", which sets each EDSU as a PSU, and "None" for pure manual actions by the user.
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
DefineAcousticLayer <- function(processData, StoxAcousticData, DefinitionMethod = c("WaterColumn", "HighestResolution", "UserDefined"), Resolution = double(), LayerTable = data.table::data.table(), UseProcessData = FALSE) {
    
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
#' Swept-area Layer
#' 
#' This function defines the \code{\link{SweptAreaLayer}} process data, which sets the range intervals of the swetp-area layers used in swept-area estimation models in StoX.
#' 
#' @inheritParams DefineStrata
#' @inheritParams DefineAcousticLayer
#' @param StoxBioticData  A list of \code{\link[RstoxDatas]{StoxBioticData}} data.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "EDSUToPSU", which sets each EDSU as a PSU, and "None" for pure manual actions by the user.
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
DefineSweptAreaLayer <- function(processData, StoxBioticData, DefinitionMethod = c("WaterColumn", "HighestResolution", "UserDefined"), Resolution = double(), LayerTable = data.table::data.table(), UseProcessData = FALSE) {
    
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
#' @param AcousticPSU    A list of \code{\link{AcousticPSU}} process data.
#' @param AcousticLayer  A list of \code{\link{AcousticLayer}} process data.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Stratum", which assigns all Hauls to every PSU of each stratum; "Radius", which selects Hauls inside a radius around each PSU????; and "EllipsoidalDistance", which selects Hauls within an ellipsoid defined by axes in space, time, bottom depth *************.
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
DefineBioticAssignment_temp <- function(
    processData, 
    AcousticPSU, AcousticLayer, StoxBioticData, 
    DefinitionMethod = c("Stratum", "Radius", "EllipsoidalDistance"), 
    StratumPolygon, 
    AcousticData, Radius = double(), 
    MinNumStations = integer(), RefGCDistance = double(), RefTime = "", RefBotDepth = double(), RefLatitude = double(), RefLongitude = double(), 
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
        stationIndex <- as.numeric(names(StratumIndex))
        stationList <- split(StoxBioticData$Station$Station[stationIndex], Stratum)
        
        # Use all hauls of each station in the automatic assignment method "Stratum":
        Station_Haul <- merge(
            StoxBioticData$Station, 
            StoxBioticData$Haul, 
            by = intersect(names(StoxBioticData$Station), names(StoxBioticData$Haul))
        )
        #message("Remove the hack in BioticStationAssignment where Haul is generated")
        #Station_Haul$Haul <- paste(Station_Haul$CruiseKey, Station_Haul$StationKey, Station_Haul$HaulKey, sep = "/")
        
        # Small funciton to get the hauls of requested stations:
        getAllHaulsOfStations <- function(Station, Station_Haul) {
            requestedStations <- Station_Haul$Station %in% Station
            Station_Haul$Haul[requestedStations]
        }
        haulList <- lapply(stationList, getAllHaulsOfStations, Station_Haul = Station_Haul)
        
        # Issue an error if there are strata to be used which do not contain any hauls:
        uniqueStrata <- unique(AcousticPSU$Stratum_PSU$Stratum)
        nHaulsOfUniqueStrata <- lengths(haulList[uniqueStrata])
        hasNoHauls <- which(nHaulsOfUniqueStrata == 0)
        if(length(hasNoHauls)) {
            stop("The following strata containing acoustic PSUs have no biotic Hauls: ", paste(uniqueStrata[hasNoHauls], collapse = ", "))
        }
        
        # Link the stratum to PSU:
        # why was this included????? Stratum should not be NA in AcousticPSU
        #Stratum_PSU <- subset(AcousticPSU$Stratum_PSU, !is.na(AcousticPSU$Stratum_PSU$Stratum))
        Assignment <- cbind(
            #Stratum_PSU[, "PSU"], 
            AcousticPSU$Stratum_PSU[, "PSU"], 
            Layer = 1, 
            #Haul = haulList[Stratum_PSU$Stratum], 
            Haul = haulList[AcousticPSU$Stratum_PSU$Stratum], 
            WeightingFactor = 1
        )
    }
    
    return(Assignment)
}


##################################################
##################################################
#' Assignnment of biotic hauls to acoustic PSUs
#' 
#' This function defines the \code{\link{BioticAssignment}} process data, linking biotic Hauls with acoustic PSUs.
#' 
#' @inheritParams DefineStrata
#' @inheritParams DefineSweptAreaPSU
#' @param AcousticPSU    A list of \code{\link{AcousticPSU}} process data.
#' @param AcousticLayer  A list of \code{\link{AcousticLayer}} process data.
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Stratum", which assigns all Hauls to every PSU of each stratum; "Radius", which selects Hauls inside a radius around each PSU????; and "EllipsoidalDistance", which selects Hauls within an ellipsoid defined by axes in space, time, bottom depth *************.
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
DefineBioticAssignment <- function(
    processData, 
    NASCData, StoxBioticData, 
    DefinitionMethod = c("Stratum", "Radius", "EllipsoidalDistance"), 
    StratumPolygon, Radius = double(), 
    MinNumStations = integer(), RefGCDistance = double(), RefTime = "", RefBotDepth = double(), RefLatitude = double(), RefLongitude = double(), 
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
        stationIndex <- as.numeric(names(StratumIndex))
        stationList <- split(StoxBioticData$Station$Station[stationIndex], Stratum)
        
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
        Assignment <- cbind(
            NASCData[, c("PSU", "Layer")], 
            Haul = haulList[NASCData$Stratum], 
            WeightingFactor = 1
        )
    }
    
    return(Assignment)
}


