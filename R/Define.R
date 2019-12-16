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
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    # Get the DefinitionMethod:
    modelType <- match.arg(modelType)
    
    # SSULevel
    if(modelType == "Acoustic") {
        SSULevel <- "Log"
        SSUName <- "EDSU"
    }
    else if(modelType == "SweptArea") {
        SSULevel <- "Station"
        SSUName <- "Station"
    }
    else {
        stop("Unknown model type")
    }
    
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # For flexibility accept a list of the input data, named by the data type:
    if(is.list(StratumPolygon) && "StratumPolygon" %in% names(StratumPolygon)) {
        StratumPolygon <- StratumPolygon$StratumPolygon
    }
    if(is.list(StoxData) && "StoxData" %in% names(StoxData)) {
        StoxData <- StoxData$StoxData
    }
    
    # Use each SSU as a PSU:
    if(DefinitionMethod == "Identity") {
        
        # Define PSUIDs and PSUNames:
        SSU <- StoxData[[SSULevel]][[SSUName]]
        PSUID <- seq_along(SSU)
        PSUName <- paste0("T", formatC(PSUID, width = nchar(max(PSUID)), format = "d", flag = "0"))
        
        # Set each SSU as a PSU:
        PSU_SSU <- data.table::data.table(
            PSU = PSUName, 
            SSU = SSU
        )
        
        # Find the stratum of each PSU:
        SpatialPSUs <- sp::SpatialPoints(StoxData[[SSULevel]][, c("Longitude", "Latitude")])
        StratumIndex <- sp::over(SpatialPSUs, StratumPolygon)
        Stratum <- names(StratumPolygon)[StratumIndex]
        
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
    if(DefinitionMethod == "StationToPSU") {
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
    if(DefinitionMethod == "EDSUToPSU") {
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
#' Acoustic Layer
#' 
#' This function defines the \code{\link{AcousticPSU}} process data, linking strata, acoustic PSUs and EDSUs. 
#' 
#' @inheritParams DefineStrata
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
        else {
            names(x) <- c("MinRange", "MaxRange")
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
        SpatialStations <- sp::SpatialPoints(StoxBioticData$Station[, c("Longitude", "Latitude")])
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
            Haul = haulList[Stratum_PSU$Stratum], 
            WeightingFactor = 1
        )
    }
    
    
    
    return(list(Assignment = Assignment))
}


