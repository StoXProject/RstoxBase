##################################################
##################################################
#' Definne PSU
#' 
#' Underlying function for \code{\link{DefineBioticPSU}} and \code{\link{DefineAcousticPSU}}.
#' 
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @param StoxData Either \code{\link[RstoxData]{StoxBioticData}} or \code{\link[RstoxData]{StoxAcousticData}} data.
#' @param DefinitionMethod  Character: A string naming the method to use, see \code{\link{DefineBioticPSU}} and \code{\link{DefineAcousticPSU}}.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An list of two objects, Stratum_PSU and SSU_PSU.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{DefineAcousticPSU}} and \code{\link{DefineBioticPSU}}..
#' 
#' @export
#' 
DefinePSU <- function(
    processData, UseProcessData = FALSE, 
    StratumPolygon, 
    StoxData, 
    DefinitionMethod = c("Identity", "None"), 
    PSUType = c("Acoustic", "Biotic")
) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod and PSUType:
    DefinitionMethod <- match.arg(DefinitionMethod)
    PSUType <- match.arg(PSUType)
    
    # SSULevel
    if(PSUType == "Acoustic") {
        #SSULevel <- "Log"
        SSUName <- "EDSU"
        #prefix <- "T"
        prefix <- getRstoxBaseDefinitions("AcousticPSUPrefix")
    }
    else if(PSUType == "Biotic") {
        #SSULevel <- "Station"
        SSUName <- "Station"
        #prefix <- "S"
        prefix <- getRstoxBaseDefinitions("BioticPSUPrefix")
    }
    else {
        stop("Unknown model type")
    }
    
    # Make sure that there is only one row per SSU:
    notDuplicatedSSUs <- !duplicated(StoxData[[SSUName]])
    StoxData <- StoxData[notDuplicatedSSUs, ]
    
    # Get SSUs:
    #SSU <- StoxData[[SSULevel]][[SSUName]]
    SSU <- StoxData[[SSUName]]
    
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
        #SpatialPSUs <- sp::SpatialPoints(StoxData[[SSULevel]][, c("Longitude", "Latitude")])
        SpatialPSUs <- sp::SpatialPoints(StoxData[, c("Longitude", "Latitude")])
        
        StratumNames <- sp::over(SpatialPSUs, StratumPolygon)
        
        
        #StratumIndex <- sp::over(SpatialPSUs, StratumPolygon)
        ## Converting from data frame to character vector 
        #StratumIndex <- as.numeric(unlist(StratumIndex))
        #NonEmptyStrata <- StratumNames[StratumIndex]
        
        # Create the Stratum_PSU data.table:
        Stratum_PSU <- data.table::data.table(
            #Stratum = NonEmptyStrata, 
            Stratum = unlist(StratumNames), 
            PSU = PSUName
        )
        
        # Remove PSUs that do not have a stratum:
        validPSUs <- unique(Stratum_PSU$PSU[!is.na(Stratum_PSU$Stratum)])
        Stratum_PSU <- Stratum_PSU[ PSU %in% validPSUs ]
        SSU_PSU[! PSU %in% validPSUs, PSU := NA_character_]
    }
    # Otherwise return empty Stratum_PSU and SSU_PSU with all SSUs and empty string as PSU:
    else if(grepl("None", DefinitionMethod, ignore.case = TRUE)) {
        SSU_PSU <- data.table::data.table(
            SSU = SSU, 
            PSU = NA_character_
        )
        Stratum_PSU <- data.table::data.table()
    }
    else {
        stop("Inavlid DefinitionMethod")
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
    
    # No longer neede, as the GUI gets stratum names from DefineStratumPolygon instead:
    # Add a list of all strata:
    #out$Stratum <- data.table::data.table(
    #    Stratum = getStratumNames(StratumPolygon)
    #)
    
    return(out)
}


##################################################
##################################################
#' Biotic PSU
#' 
#' This function defines the \code{\link{BioticPSU}} process data, linking strata, biotic PSUs and Stations 
#' 
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @inheritParams ModelData
#' @param DefinitionMethod  Character: A string naming the method to use, one of "StationToPSU", which sets each Station as a PSU, and "None" for pure manual actions by the user.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{BioticPSU}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso Acousic PSUs are generated using \code{\link{DefineAcousticPSU}}. For the vertical resolution (Layer) see \code{\link{DefineBioticLayer}} and \code{\link{DefineAcousticLayer}}.
#' 
#' @export
#' 
DefineBioticPSU <- function(
    processData, UseProcessData = FALSE, 
    StratumPolygon, 
    StoxBioticData, 
    DefinitionMethod = c("StationToPSU", "None")
) {
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    if(grepl("StationToPSU", DefinitionMethod, ignore.case = TRUE)) {
        DefinitionMethod <- "Identity"
    }
    
    BioticPSU <- DefinePSU(
        processData = processData, 
        StratumPolygon = StratumPolygon, 
        StoxData = StoxBioticData$Station, 
        DefinitionMethod = DefinitionMethod, 
        UseProcessData = UseProcessData, 
        PSUType = "Biotic"
    )
    
    return(BioticPSU)
}


##################################################
##################################################
#' Acoustic PSU
#' 
#' This function defines the \code{\link{AcousticPSU}} process data, linking strata, acoustic PSUs and EDSUs. 
#' 
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @inheritParams ModelData
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
#' @seealso Biotic PSUs are generated using \code{\link{DefineBioticPSU}}. For the vertical resolution (Layer) see \code{\link{DefineBioticLayer}} and \code{\link{DefineAcousticLayer}}.
#' 
#' @export
#' 
DefineAcousticPSU <- function(
    processData, UseProcessData = FALSE, 
    StratumPolygon, 
    StoxAcousticData, 
    DefinitionMethod = c("EDSUToPSU", "None")
) {
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    if(grepl("EDSUToPSU", DefinitionMethod, ignore.case = TRUE)) {
        DefinitionMethod <- "Identity"
    }
    
    AcousticPSU <- DefinePSU(
        processData = processData, 
        StratumPolygon = StratumPolygon, 
        StoxData = StoxAcousticData$Log, 
        DefinitionMethod = DefinitionMethod, 
        UseProcessData = UseProcessData, 
        PSUType = "Acoustic"
    )
    
    return(AcousticPSU)
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
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{BioticLayer}}.
#' 
#' @examples
#' x <- 1
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
        data <- mergeDataTables(StoxData, output.only.last = TRUE, all = TRUE)
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
#' @inheritParams DefineLayer
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
#' @inheritParams DefineLayer
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' An object of StoX data type \code{\link{BioticLayer}}.
#' 
#' @examples
#' x <- 1
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
#' Assignnment of biotic hauls to acoustic PSUs
#' 
#' This function defines the \code{\link{BioticAssignment}} process data, linking biotic Hauls with acoustic PSUs.
#' 
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @inheritParams ModelData
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Stratum", assign all stations of each stratum to all acoustic PSUs; "Radius", to assign all stations within the radius given in \code{Radius} to each acoustic PSU; and "EllipsoidalDistance" to provide the \code{EllipsoidalDistanceTable} specifying the axes of an ellipsoid inside which to assign stations to acoustic PSUs.
#' @param Radius Numeric: The radius inside which to assign biotic stations to each acoustic PSU.
#' @param MinNumberOfHauls For DefinitionMethod "EllipsoidalDistance": Integer minimum number of hauls selected inside the ellipsoid. If the number of hauls inside the ellispoid is lower than the \code{MinNumberOfHauls}, the \code{MinNumberOfHauls} closest Hauls will be used (in ellipsoidal distance). 
#' @param DistanceNauticalMiles For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing distance in nautical miles.
#' @param TimeDifferenceHours For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing time difference in hours.
#' @param BottomDepthDifferenceMeters For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing difference in bottom depth in meters.
#' @param LongitudeDifferenceDegrees For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing difference in longitude in degrees.
#' @param LatitudeDifferenceDegrees For DefinitionMethod "EllipsoidalDistance": The semi axis of the ellipsoid representing difference in latitude in degrees.
#' 
#' @details
#' See Equation 8 in "Factors affecting the diel variation in commercial CPUE of Namibian hake - Can new information improve standard survey estimates?".
#' 
#' @return
#' An object of StoX data type \code{\link{BioticAssignment}}.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{BioticAssignmentWeighting}} for weighting BioticAssignment.
#' 
#' @export
#'
DefineBioticAssignment <- function(
    processData, UseProcessData = FALSE, 
    DefinitionMethod = c("Stratum", "Radius", "EllipsoidalDistance", "None"), 
    StoxBioticData, 
    # For DefinitionMethod "Stratum": 
    StratumPolygon, AcousticPSU, AcousticLayer, 
    # (Additional) for DefinitionMethod "Radius": 
    StoxAcousticData, 
    Radius = double(), 
    # (Additional) for DefinitionMethod "EllipsoidalDistance": 
    MinNumberOfHauls,
    DistanceNauticalMiles, 
    TimeDifferenceHours, 
    BottomDepthDifferenceMeters, 
    LongitudeDifferenceDegrees, 
    LatitudeDifferenceDegrees
)
{
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    # Merge the StoxBioticData:
    MergedStoxBioticData <- RstoxData::MergeStoxBiotic(StoxBioticData, "Haul")
    
    
    # If DefinitionMethod == "Stratum", assign all stations of each stratum to all PSUs of the stratum:
    if(grepl("Stratum", DefinitionMethod, ignore.case = TRUE)) {
        # Create a spatial points object of the positions of the hauls:
        SpatialHauls <- sp::SpatialPoints(MergedStoxBioticData[, c("Longitude", "Latitude")])
        # Get the stratum for each haul:
        Stratum <- unname(unlist(sp::over(SpatialHauls, StratumPolygon)))
        BioticAssignment <- MergedStoxBioticData
        BioticAssignment[, Stratum := ..Stratum]
        
        # Add the PSUs to the BioticAssignment:
        BioticAssignment <- merge(BioticAssignment, AcousticPSU$Stratum_PSU, all = TRUE, by = "Stratum", allow.cartesian = TRUE)
        
        # Discard all rows with missing PSU:
        BioticAssignment <- subset(BioticAssignment, !is.na(PSU))
    }
    # Search for Hauls around all EDSUs of each PSU:
    else if(grepl("Radius|EllipsoidalDistance", DefinitionMethod, 
                  ignore.case = TRUE)) {
        # Merge the StoxBioticData:
        MergedStoxAcousticData <- RstoxData::MergeStoxAcoustic(StoxAcousticData, "Log")
        
        # Get a table of EDSUs and Hauls:
        BioticAssignment <- data.table::CJ(
            EDSU = MergedStoxAcousticData$EDSU, 
            Haul = MergedStoxBioticData$Haul
        )
        
        # Merge PSUs and strata into the table:
        EDSU_PSU_Stratum <- RstoxData::mergeDataTables(AcousticPSU, all = TRUE, output.only.last = TRUE)
        BioticAssignment <- merge(EDSU_PSU_Stratum, BioticAssignment, by = "EDSU", all = TRUE)
        
        # Discard all rows with missing PSU:
        BioticAssignment <- subset(BioticAssignment, !is.na(PSU))
        
        # Get the distance units:
        if(grepl("Radius", DefinitionMethod, ignore.case = TRUE)) {
            
            differenceTable = data.table::data.table(
                distance = getDistanceNauticalMiles(
                    MergedStoxAcousticData = MergedStoxAcousticData, 
                    MergedStoxBioticData = MergedStoxBioticData
                )
            )
            
            # Tag the Hauls that are inside the radius:
            differenceTable[, inside := distance <= Radius]
        }
        else if(grepl("EllipsoidalDistance", DefinitionMethod, ignore.case = TRUE)) {
            
            differenceTable = data.table::data.table(
                # Get the distance between the EDSUs and Hauls:
                if(length(DistanceNauticalMiles)) {
                    DistanceNauticalMiles = getSquaredRelativeDistanceNauticalMiles(
                        MergedStoxAcousticData = MergedStoxAcousticData, 
                        MergedStoxBioticData = MergedStoxBioticData, 
                        DistanceNauticalMiles = DistanceNauticalMiles
                    )
                }, 
                # Get the time difference between the EDSUs and Hauls:
                if(length(TimeDifferenceHours)) {
                    TimeDifferenceHours = getSquaredRelativeTimeDiff(
                        MergedStoxAcousticData = MergedStoxAcousticData, 
                        MergedStoxBioticData = MergedStoxBioticData, 
                        TimeDifferenceHours = TimeDifferenceHours
                    )
                }, 
                # Get the difference in bottom depth between the EDSUs and Hauls:
                if(length(BottomDepthDifferenceMeters)) {
                    BottomDepthDifferenceMeters = getSquaredRelativeDiff(
                        MergedStoxAcousticData = MergedStoxAcousticData, 
                        MergedStoxBioticData = MergedStoxBioticData, 
                        variableName = "BottomDepth", 
                        axisLength = BottomDepthDifferenceMeters
                    )
                }, 
                # Get the longitude difference between the EDSUs and Hauls:
                if(length(LongitudeDifferenceDegrees)) {
                    LongitudeDifferenceDegrees = getSquaredRelativeDiff(
                        MergedStoxAcousticData = MergedStoxAcousticData, 
                        MergedStoxBioticData = MergedStoxBioticData, 
                        variableName = "Longitude", 
                        axisLength = LongitudeDifferenceDegrees
                    )
                }, 
                # Get the latitude differerence between the EDSUs and Hauls:
                if(length(LatitudeDifferenceDegrees)) {
                    LatitudeDifferenceDegrees = getSquaredRelativeDiff(
                        MergedStoxAcousticData = MergedStoxAcousticData, 
                        MergedStoxBioticData = MergedStoxBioticData, 
                        variableName = "Latitude", 
                        axisLength = LatitudeDifferenceDegrees
                    )
                }
            )
            
            # Check whether any of the columns are all NA, indicating error in the data:
            NACols <- unlist(differenceTable[, lapply(.SD, function(x) all(is.na(x)))])
            if(any(NACols)) {
                warning("StoX: The following axes of the ellipsoid were all NA, indicating missing data: ", paste0(names(differenceTable)[NACols], collapse = ", "))
            }
            
            # Sum and take the square root to get the ellipsoidal distance:
            #differenceTable[, distance := sqrt(rowSums(.SD, na.rm = TRUE))] # No need to sqrt:
            differenceTable[, distance := rowSums(.SD, na.rm = TRUE)]
            
            # Tag Hauls inside the ellipsoid:
            differenceTable[, inside := distance <= 1]
        }
        
        # Join the differenceTable into the BioticAssignment:
        BioticAssignment <- data.table::data.table(BioticAssignment, differenceTable)
        
        # Apply any requirement on the number of hauls per PSU:
        if(length(MinNumberOfHauls)) {
            BioticAssignment[, inside := inside | distance <= sort(distance)[MinNumberOfHauls], by = c("PSU")]
        }
        
        # Keep only Hauls inside the radius:
        BioticAssignment <- subset(BioticAssignment, inside)
    }
    else if(grepl("None", DefinitionMethod, ignore.case = TRUE)) {
        BioticAssignment <- data.table::data.table()
    }
    else {
        stop("Only DefinitionMethod = \"Stratum\" is currently implemented")
    }
    
    # Add all Layers to each assigned haul:
    Layer_PSU <- data.table::CJ(Layer = AcousticLayer$Layer, PSU = unique(BioticAssignment$PSU))
    BioticAssignment <- merge(BioticAssignment, Layer_PSU, all = TRUE, by = "PSU", allow.cartesian = TRUE)
    
    # Add weighting  = 1:
    BioticAssignment[, WeightingFactor := 1]
    
    # Extract only the relevant columns:
    formatOutput(BioticAssignment, dataType = "BioticAssignment", keep.all = FALSE)
    
    return(BioticAssignment)
}

# Function to get the great circle distance between EDSUs in the MergedStoxAcousticData and Hauls in the MergedStoxBioticData: 
getDistanceNauticalMiles <- function(MergedStoxAcousticData, MergedStoxBioticData) {
    # Extract the goegraphical positions:
    EDSUPositions <- as.matrix(MergedStoxAcousticData[, c("Longitude", "Latitude")])
    HaulPositions <- as.matrix(MergedStoxBioticData[, c("Longitude", "Latitude")])
    # Get the distances between EDUSs and Hauls:
    EDSUToHaulDistanceKilometers <- c(sp::spDists(EDSUPositions, HaulPositions, longlat = TRUE))
    # Convert to nautical miles:
    EDSUToHaulDistanceNauticalMiles <- EDSUToHaulDistanceKilometers * 1000 / getRstoxBaseDefinitions("nauticalMileInMeters")
    return(EDSUToHaulDistanceNauticalMiles)
}

# Function to ge the squared distance in units of the DistanceNauticalMiles squared:
getSquaredRelativeDistanceNauticalMiles <- function(MergedStoxAcousticData, MergedStoxBioticData, DistanceNauticalMiles) {
    # Get the distances between EDUSs and Hauls:
    EDSUToHaulDistanceNauticalMiles <- getDistanceNauticalMiles(MergedStoxAcousticData, MergedStoxBioticData)
    # Square and return:
    SquaredRelativeDistanceNauticalMiles <- EDSUToHaulDistanceNauticalMiles^2 / DistanceNauticalMiles^2
    return(SquaredRelativeDistanceNauticalMiles)
}

# Function to ge the squared time difference in units of the TimeDifferenceHours squared:
getSquaredRelativeTimeDiff <- function(MergedStoxAcousticData, MergedStoxBioticData, TimeDifferenceHours, variableName = "DateTime") {
    # Get the time difference between all EDSUs and all Hauls:
    out <- data.table::CJ(
        x = MergedStoxAcousticData[[variableName]], 
        y = MergedStoxBioticData[[variableName]]
    )
    TimeDiff <- as.numeric(out[, difftime(x, y, units = "hours")])
    # Square and return:
    SquaredTimeDiff <- TimeDiff^2 / TimeDifferenceHours^2
    return(SquaredTimeDiff)
}

getSquaredRelativeDiff <- function(MergedStoxAcousticData, MergedStoxBioticData, variableName, axisLength) {
    # Get the absolute difference between all EDSUs and all Hauls:
    out <- data.table::CJ(
        x = MergedStoxAcousticData[[variableName]], 
        y = MergedStoxBioticData[[variableName]]
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
#' @inheritParams ProcessData
#' @inheritParams ModelData
#' @param WeightingMethod  Character: A string naming the method to use, one of "Equal", giving weight 1 to all Hauls; "NumberOfLengthSamples", weighting hauls by the number of length samples; "NASC", weighting by the surrounding NASC converted by the haul length distribution to a density equivalent; "NormalizedTotalWeight", weighting hauls by the total weight of the catch, normalized by dividing by towed distance; "NormalizedTotalCount", the same as "NormalizedTotalWeight" but for total count, "SumWeightedCount", weighting by the summed WeightedCount of the input LengthDistributionData; and "InverseSumWeightedCount", weighting by the inverse of the summed WeightedCount.
#' @param MaxNumberOfLengthSamples For \code{WeightingMethod} = "NumberOfLengthSamples": Values of the number of length samples that exceed \code{MaxNumberOfLengthSamples} are set to \code{MaxNumberOfLengthSamples}. This avoids giving too high weight to e.g. experimental hauls with particularly large length samples.
#' @param Radius For \code{WeightingMethod} = "NASC": The radius inside which the average NASC is calculated. 
#' @param LengthExponentTable For \code{WeightingMethod} = "NASC": A table linking AcousticCategory with the LengthExponent used to convert from NASC to density.
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
#' @seealso \code{\link{DefineBioticAssignment}} for generating BioticAssignment.
#' 
#' @export
#'
BioticAssignmentWeighting <- function(
    BioticAssignment, 
    WeightingMethod = c("Equal", "NumberOfLengthSamples", "NASC", "NormalizedTotalWeight", "NormalizedTotalCount", "SumWeightedCount", "InverseSumWeightedCount"), 
    StoxBioticData, 
    LengthDistributionData, 
    MaxNumberOfLengthSamples = 100, 
    StoxAcousticData, Radius, LengthExponentTable
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
    if(WeightingMethod == "Equal") {
        # Simply set WeightingFactor to 1
        BioticAssignmentCopy[, eval(weightingVariable) := 1]
    }
    # Weight hauls by the number of length samples (count the length samples for which IndividualTotalLengthCentimeter is not NA):
    else if(WeightingMethod == "NumberOfLengthSamples") {
        # Merge Haul and Individual, and count individuals with length for each Haul:
        Haul_Individual <- merge(StoxBioticData$Haul, StoxBioticData$Individual)
        NumberOfLengthSamples <- Haul_Individual[, .(NumberOfLengthSamples = as.double(sum(!is.na(IndividualTotalLengthCentimeter)))), by = "Haul"]
        # Apply the MaxNumberOfLengthSamples:
        NumberOfLengthSamples[NumberOfLengthSamples > MaxNumberOfLengthSamples, NumberOfLengthSamples := MaxNumberOfLengthSamples]
        
        # Merge into the BioticAssignmentCopy and set the weightingVariable to the NumberOfLengthSamples
        #BioticAssignmentCopy <- merge(BioticAssignmentCopy, NumberOfLengthSamples, by = "Haul")
        #BioticAssignmentCopy[, eval(weightingVariable) := NumberOfLengthSamples]
        BioticAssignmentCopy <- mergeIntoBioticAssignment(
            BioticAssignment = BioticAssignmentCopy, 
            toMerge = NumberOfLengthSamples, 
            variable = "NumberOfLengthSamples", 
            weightingVariable = weightingVariable
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
    }
    # Weight hauls by the summed CatchFractionWeightKilogram divided by the EffectiveTowedDistance:
    else if(WeightingMethod == "NormalizedTotalWeight") {
        # Merge Haul and Sample, and sum the catch weight divided by towed distance:
        Haul_Sample <- merge(StoxBioticData$Haul, StoxBioticData$Sample)
        NormalizedTotalWeight <- Haul_Sample[, .(NormalizedTotalWeight = sum(CatchFractionWeightKilogram) / EffectiveTowedDistance[1]), by = "Haul"]
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
    # Weight hauls by the summed CatchFractionCount divided by the EffectiveTowedDistance:
    else if(WeightingMethod == "NormalizedTotalCount") {
        # Merge Haul and Sample, and sum the catch count divided by towed distance:
        Haul_Sample <- merge(StoxBioticData$Haul, StoxBioticData$Sample)
        NormalizedTotalCount <- Haul_Sample[, .(NormalizedTotalCount = sum(CatchFractionCount) / EffectiveTowedDistance[1]), by = "Haul"]
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
    # Weight hauls by the summed CatchFractionCount divided by the EffectiveTowedDistance:
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
    
    # Keep only relevant columns:
    formatOutput(BioticAssignmentCopy, dataType = "BioticAssignment", keep.all = FALSE)
    
    return(BioticAssignmentCopy)
}


mergeIntoBioticAssignment <- function(BioticAssignment, toMerge, variable, weightingVariable) {
    BioticAssignment <- merge(BioticAssignment, toMerge, by = "Haul")
    BioticAssignment[, eval(weightingVariable) := as.double(get(variable))]
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
        LengthDistributionData[, WeightedCount := WeightedCount / EffectiveTowedDistance]
    } 
    else if(!isLengthDistributionType(LengthDistributionData, "Normalized")) {
        stop("The LengthDistributionType must be \"Standard\" (in which case the WeightedCount will be divided by EffectiveTowedDistance) or \"Normalized\"")
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
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Table", for providing the acoustic target strength parameters in the table \code{ParameterTable}; and "ResourceFile" for reading the acoustic tfarget strength table from the text file \code{FileName}.
#' @param TargetStrengthDefinitionTable A table holding the specification of the target strength function/table. The first two columns are AcocusticCategory and Frequenccy. See details for other columns.
#' @param FileName A file from which to read the \code{ParameterTable}.
#' 
#' @details
#' The \code{TargetStrengthMethod} has the following possible values: 
#' \enumerate{
#'   \item LengthDependent, applying the logarithmic function TargetStrength = Targetstrength0 + LengthExponent * log10(Length). Required columns: Targetstrength0 and LengthExponent.
#'   \item LengthAndDepthDependent, applying the logarithmic function TargetStrength = Targetstrength0 + LengthExponent * log10(Length) + DepthExponent * log10(1 + DepthMeter/10). Required columns: Targetstrength0, LengthExponent and DepthExponent.
#'   \item TargetStrengthByLength, applying a table of TargetStrength and TotalLengthCentimeter. Required columns: TargetStrength and TotalLengthCentimeter.
#'   \item LengthExponent, applying the logarithmic function TargetStrength = LengthExponent * log10(Length). Required columns: LengthExponent.
#' }
#' The parameters/values can be given by tables with the first columns being AcousticCategory and Frequency, or as a csv file.
#' 
#' @return
#' An \code{\link{AcousticTargetStrength}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{AcousticDensity}} for applying the AcousticTargetStrength.
#' 
#' @export
#' 
DefineAcousticTargetStrength <- function(
    processData, UseProcessData = FALSE, 
    # Note that "LengthExponent" is an option for TargetStrengthMethod (used by BioticAssignmentWeighting()), but this is not shown.
    DefinitionMethod = c("Table", "ResourceFile"),
    TargetStrengthMethod = c("LengthDependent", "LengthAndDepthDependent", "TargetStrengthByLength"), 
    TargetStrengthDefinitionTable = data.table::data.table(), 
    FileName
) {
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the methods:
    TargetStrengthMethod <- match.arg(TargetStrengthMethod)
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    # Get or read the TargetStrengthTable and return in a list with the TargetStrengthMethod:
    AcousticTargetStrength <- getAcousticTargetStrength(
        TargetStrengthMethod = TargetStrengthMethod, 
        DefinitionMethod = DefinitionMethod, 
        TargetStrengthTable = TargetStrengthDefinitionTable, 
        #TargetStrengthTable = get(paste0(TargetStrengthMethod, "Table")), 
        FileName = FileName
    )
    
    return(AcousticTargetStrength)
}

getAcousticTargetStrength <- function(TargetStrengthMethod, DefinitionMethod, TargetStrengthTable, FileName) {
    
    # Read the table if requested, or issue an error if not given:
    if(DefinitionMethod == "Table") {
        if(length(TargetStrengthTable) == 0) {
            stop(TargetStrengthMethod, "Table must be given if DefinitionMethod = \"Table\"")
        }
    }
    else if(DefinitionMethod == "ResourceFile") {
        TargetStrengthTable <- data.table::fread(FileName)
    }
    
    # Check the columns of the table:
    checkTargetStrengthTable(TargetStrengthTable, TargetStrengthMethod)
    
    # Define the output AcousticTargetStrength as a list of the method and the table:
    AcousticTargetStrength <- list(
        TargetStrengthMethod = data.table::data.table(TargetStrengthMethod = TargetStrengthMethod), 
        TargetStrengthTable = TargetStrengthTable
    )
        
    return(AcousticTargetStrength)
}


checkTargetStrengthTable <- function(TargetStrengthTable, TargetStrengthMethod) {
    # Get and check the TargetStrengthMethod:
    targetStrengthParameters <- getRstoxBaseDefinitions("targetStrengthParameters")
    if(! TargetStrengthMethod %in% names(targetStrengthParameters)) {
        stop("Wrong TargetStrengthMethod. Must be one of ", paste(names(targetStrengthParameters), collapse = ", "))
    }
    # Check that the TargetStrengthTable contains the required columns:
    if(! all(targetStrengthParameters[[TargetStrengthMethod]] %in% names(TargetStrengthTable))) {
        stop("The ", TargetStrengthMethod, "Table must contain the required column; ", paste(targetStrengthParameters[[TargetStrengthMethod]], collapse = ", "))
    }
}


#checkAcousticTargetStrengthPresentColumns <- function(ParameterTable) {
#    
#    # Get the valid columns of the NASCData:
#    NASCDataDefinition <- getDataTypeDefinition("NASCData", unlist = TRUE)
#    
#    targetStrengthParameters <- getRstoxBaseDefinitions("targetStrengthParameters")
#    
#    validColumnNames <- c(
#        NASCDataDefinition, 
#        unique(unlist(targetStrengthParameters[ParameterTable$EquationType])), 
#        "EquationType"
#    )
#    invalidColumns <- setdiff(
#        names(ParameterTable), 
#        validColumnNames
#    )
#    
#    if(length(invalidColumns)) {
#        stop("The acoustic target strength parameter table containes the following inavlid columns: ", paste(invalidColumns, collapse = #", "))
#    }
#}

#checkAcousticTargetStrengthEquationType <- function(ParameterTable) {
#    
#    # Check that EquationType is given:
#    #if(length(ParameterTable$EquationType) == 0) {
#    #    stop("EquationType must be gievn")
#    #}
#    ## Check that all values are equal in the EquationType:
#    #if(! all(ParameterTable$EquationType == ParameterTable$EquationType[1])) {
#    #    stop("EquationType must be the same in all rows")
#    #}
#    
#    # EquationType:
#    targetStrengthParameters <- getRstoxBaseDefinitions("targetStrengthParameters")
#    for(type in names(targetStrengthParameters)) {
#        if(ParameterTable$EquationType[1] == type) {
#            if(! all(targetStrengthParameters[[type]] %in% names(ParameterTable))) {
#                stop("With EquationType = \"", type, "\" the columns ", paste(targetStrengthParameters[[type]], collapse = ", "), " #must be given.")
#            }
#        }
#    }
#    if(! ParameterTable$EquationType[1] %in% names(targetStrengthParameters)) {
#        stop("Invalid EquationType.")
#    }
#}


