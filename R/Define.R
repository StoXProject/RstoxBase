# DefineAcousticPSU
# DefineAcousticLayer
# DefineSweptAreaPSU
# DefineStratum
# DefineSurvey

# BioticStationAssignment
# BioticStationWeigthing

# Possible structure of the fundamental process data:
# 
# Stratum:
#     StratumID    StratumName    Polygon 
# 
# AcousticPSU:
#     StratumID      PSUID          PSUName   EDSU
# 
# SweptAreaPSU:
#     StratumID      PSUID          PSUName   Station
# 
# AcousticLayer: 
#     LayerID      LayerName      MinRange  MaxRange
# 
# SweptAreaLayer: 
#     LayerID      LayerName      MinDepth  MaxDepth
# 
# Assignment: 
#     AcousticPSU  AcousticLayer  Station   StationWeight  AssignmentID
# 


# AcousticPSU:
#     Stratum_PSU [Stratum, PSUID (vector), PSUName (vector)]
#     PSU_EDSU [PSUID, EDSU (vector)]
# SweptAreaPSU:
#     Stratum_PSU [Stratum, PSUID (vector), PSUName (vector)]
#     PSU_Station [PSUID, Station (vector)]
# Assignment:
#     Assignment [PSUID, LayerID, Station (vector), StationWeight (vector)]
# AcousticLayer:
#     AcousticLayer [LayerID, LayerName, MinRange, MaxRange]
# SweptAreaLayer:
#     SweptAreaLayer [LayerID, LayerName, MinDepth, MaxDepth]







##################################################
##################################################
#' Acoustic PSU
#' 
#' This function defines the \code{\link{AcousticPSU}} process data, linking strata, acoustic PSUs and EDSUs. 
#' 
#' @inheritParams DefineStratum
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
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # For flexibility accept a list of the input data, named by the data type:
    if(is.list(StratumPolygon) && "StratumPolygon" %in% names(StratumPolygon)) {
        StratumPolygon <- StratumPolygon$StratumPolygon
    }
    if(is.list(StoxAcousticData) && "StoxAcousticData" %in% names(StoxAcousticData)) {
        StoxAcousticData <- StoxAcousticData$StoxAcousticData
    }
    
    # Use each EDSU as a PSU:
    if(DefinitionMethod[1] == "EDSUToPSU") {
        
        # Define PSUIDs and PSUNames:
        EDSU <- StoxAcousticData$Log$EDSU
        PSUID <- seq_along(EDSU)
        PSUName <- paste0("T", formatC(PSUID, width = nchar(max(PSUID)), format = "d", flag = "0"))
        
        # Set each EDSU as a PSU:
        PSU_EDSU <- data.table::data.table(
            PSU = PSUName, 
            EDSU = EDSU
        )
        
        # Find the stratum of each PSU:
        SpatialPSUs <- sp::SpatialPoints(StoxAcousticData$Log[, c("StartLongitude", "StartLatitude")])
        StratumIndex <- sp::over(SpatialPSUs, StratumPolygon)
        Stratum <- names(StratumPolygon)[StratumIndex]
        
        # Create the Stratum_PSU data.table:
        Stratum_PSU <- data.table::data.table(
            Stratum = Stratum, 
            PSU = PSUName
        )
    }
    # Otherwise return empty tables:
    else {
        PSU_EDSU <- data.table::data.table()
        Stratum_PSU <- data.table::data.table()
    }
    
    
    list(
        PSU_EDSU = PSU_EDSU, 
        Stratum_PSU = Stratum_PSU
    )
}


##################################################
##################################################
#' Acoustic Layer
#' 
#' This function defines the \code{\link{AcousticPSU}} process data, linking strata, acoustic PSUs and EDSUs. 
#' 
#' @inheritParams DefineStratum
#' @param StratumPolygon    A list of \code{\link{StratumPolygon}} process data.
#' @param StoxAcousticData  A list of \code{\link[RstoxDatas]{StoxAcousticData}} data.
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
DefineAcousticLayer <- function(processData, StoxAcousticData, DefinitionMethod = c("WaterColumn", "HighestResolution", "UserDefined"), Resolution = double(), LayerTable = data.table::data.table(), UseProcessData = FALSE) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Function to create a LayerTable from breaks:
    createLayerTable <- function(x) {
        # Create a data.table if a vector of breaks is given:
        if(length(dim(x)) == 1) {
            x <- data.table::data.table(
                MinRange = x[-length(x)], 
                MaxRange = x[-1]
            )
        }
        # Create the Layer names:
        LayerNames <- getDefaultLayerNames(x)
        x <- cbind(
            Layer = LayerNames, 
            x
        )
        x
    }
    
    
    getDefaultLayerNames <- function(x) {
        if(length(dim(x) == 2)) {
            nlayers <- nrow(x)
        }
        else {
            nlayers <- length(x)
        }
        paste0("Layer", formatC(seq_len(nlayers), width = nchar(nlayers), format = "d", flag = "0"))
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    # For flexibility accept a list of the input data, named by the data type:
    if(is.list(StoxAcousticData) && "StoxAcousticData" %in% names(StoxAcousticData)) {
        StoxAcousticData <- StoxAcousticData$StoxAcousticData
    }
    
    # Check if there is only 
    
    # Get the common intervals:
    possibleIntervals <- getCommonIntervals(
        data = unique(StoxAcousticData$NASC[, c("MinRange", "MaxRange")]), 
        varMin = "MinRange", 
        varMax = "MaxRange"
    )
    
    # If "WaterColumn" is requested use the full range:
    if(DefinitionMethod == "WaterColumn") {
        AcousticLayer <- data.table::data.table(
            Layer = "WaterColumn", 
            MinRange = possibleIntervals[1,1], 
            MaxRange = possibleIntervals[nrow(possibleIntervals), 2]
        )
    }
    
    # If "HighestResolution" is requested use all possible breaks:
    else if(DefinitionMethod == "HighestResolution") {
        AcousticLayer <- createLayerTable(possibleIntervals)
    }
    
    # If "UserDefined" is requested match the Breaks against the possible breaks:
    else if(DefinitionMethod == "UserDefined") {
        # Error if any of the specified breaks are invalid:
        if(any(! unlist(LayerTable[, c("MinRange", "MaxRange")]) %in% unlist(possibleIntervals))) {
            stop("Some of the specified breaks are not at common breaks of all Log(distance)s. Possible breaks are [", paste(unlist(possibleIntervals), collapse = ", "), "]")
        }
        else {
            AcousticLayer <- LayerTable
        }
    }
    
    else {
        stop("Invalid DefinitionMethod")
    }
    
    
    return(list(AcousticLayer = AcousticLayer))
}





##################################################
##################################################
#' 
#' @export
#' 
BioticStationAssignment <- function(
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
    
    # For flexibility accept a list of the input data, named by the data type:
    if(is.list(AcousticPSU) && "AcousticPSU" %in% names(AcousticPSU)) {
        AcousticPSU <- AcousticPSU$AcousticPSU
    }
    if(is.list(AcousticLayer) && "AcousticLayer" %in% names(AcousticLayer)) {
        AcousticLayer <- AcousticLayer$AcousticLayer
    }
    if(is.list(StoxBioticData) && "StoxBioticData" %in% names(StoxBioticData)) {
        StoxBioticData <- StoxBioticData$StoxBioticData
    }
    
    # If DefinitionMethod == "Stratum", assign all stations of each stratum to all PSUs of the stratum:
    if(DefinitionMethod == "Stratum") {
        
        # For flexibility accept a list of the input data, named by the data type:
        if(is.list(StratumPolygon) && "StratumPolygon" %in% names(StratumPolygon)) {
            StratumPolygon <- StratumPolygon$StratumPolygon
        }
        
        # Create a spatial points object:
        SpatialStations <- sp::SpatialPoints(StoxBioticData$Station[, c("StartLongitude", "StartLatitude")])
        # Get the stratum for each point:
        StratumIndex <- sp::over(SpatialStations, StratumPolygon)
        Stratum <- names(StratumPolygon)[StratumIndex]
        
        # Create a list of the stations of each stratum:
        stationIndex <- as.numeric(names(StratumIndex))
        stationList <- split(StoxBioticData$Station$Station[stationIndex], Stratum)
        
        # Use all hauls of each station in the automatic assignment method "Stratum":
        Station_Haul <- merge(
            StoxBioticData$Station, 
            StoxBioticData$Haul, 
            by = intersect(names(StoxBioticData$Station), names(StoxBioticData$Haul))
        )
        message("Remove the hack in BioticStationAssignment where Haul is generated")
        Station_Haul$Haul <- paste(Station_Haul$CruiseKey, Station_Haul$StationKey, Station_Haul$HaulKey, sep = "/")
        
        # Small funciton to get the hauls of requested stations:
        getAllHaulsOfStations <- function(Station, Station_Haul) {
            requestedStations <- Station_Haul$Station %in% Station
            Station_Haul$Haul[requestedStations]
        }
        haulList <- lapply(stationList, getAllHaulsOfStations, Station_Haul = Station_Haul)
        
        # Link the stratum to PSU:
        Stratum_PSU <- subset(AcousticPSU$Stratum_PSU, !is.na(AcousticPSU$Stratum_PSU$Stratum))
        Assignment <- cbind(
            Stratum_PSU[, "PSU"], 
            Layer = 1, 
            Haul = haulList[Stratum_PSU$Stratum]
        )
    }
    
    
    
    return(list(Assignment = Assignment))
}


