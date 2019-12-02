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
        Stratum <- sp::over(SpatialPSUs, StratumPolygon)
        
        # Create the Stratum_PSU data.table:
        Stratum_PSU <- data.table::data.table(
            Stratum = Stratum, 
            PSU = PSUName
        )
    }
    # Otherwise return empry tables:
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








################ OLD STUFF: ##############

##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
DefineIndividualWeightGram <- function(BioticData, individualName = "individual", ...) {
    if(any(length(BioticData[[individualName]]) == 0)) {
        stop("'individual' is not present in the data.")
    }   

    temp <- BioticData[[individualName]]

    temp$IndividualWeightGram <- temp$individualweight * 1000

    BioticData[[individualName]] <- temp

    BioticData	
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
DefineLengthCentimeter <- function(BioticData, individualName = "individual", ...) {
    if(any(length(BioticData[[individualName]]) == 0)) {
        stop("'individual' is not present in the data.")
    }    

    temp <- BioticData[[individualName]]

    temp$LengthCentimeter <- temp$length * 100

    BioticData[[individualName]] <- temp

    BioticData
}


##################################################
##################################################
#' @param SpecCatMethod The method used for defining the SpecCat variable, which is the species variable used by StoX. The \code{SpecCatMethod} parameter has the following three possible values: (1) "SelectVar", which copies the column \code{SpecVarBiotic} of the "catchsample" table to the SpecCat column of the tables "catchsample", "individual" and all non-empty tables at lower levels. (2) "Expression", which defines the categories through a string. (3) "ResourceFile", which requires the parameters \code{FileName}, \code{SpecVarBiotic}, \code{SpecVarRef} and \code{SpecCatRef} to be set. See Details.
#' @param SpecCat       An expression indicating how to create the SpecCat given as SpecCat: species1, species2, ... (e.g., "Dentex:Dentex angolensis,Dentex congoensis").
#' @param FileName      The name of the file holding a table of at least two columns, (1) the species variable in a column named by \code{SpecVarRef}, corresponding to the field named by \code{SpecVarBiotic} in the biotic data, and (2) a column named by \code{SpecCatRef} defining the SpecCat variable.
#' @param SpecVarBiotic The name of the field on the biotic data to match with the column named by \code{SpecVarRef} in \code{FileName}.
#' @param SpecVarRef    The name of the column of \code{FileName} which should be matched with the field named by \code{SpecVarBiotic} in the biotic data.
#' @param SpecCatRef    The name of the column of \code{FileName} defining the SpecCat.
#' 
DefineSpecCat <- function(BioticData, 
	SpecCatMethod = c("SelectVar", "ResourceFile", "Expression"), 
	SpecVarBiotic = "commonname", ...) {

	if(!identical(SpecCatMethod[1], "SelectVar")){
		stop("Not a valid parameter")
	}
	
	BioticData[["catchsample"]]$SpecCat <- BioticData[["catchsample"]][[SpecVarBiotic]] 

	BioticData	
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
MergeAgeDeterminationToIndividual <- function(BioticData, 
    individualName = "individual",
	ageDeterminationName = "agedetermination",
    ...) {

    if(any(length(BioticData[[individualName]]) == 0, length(BioticData[[ageDeterminationName]]) == 0)) {
        stop("'individual' and/or 'agedetermination' are not present in the data.")
    }

    commonVar <- intersect(names(BioticData[[individualName]]), names(BioticData[[ageDeterminationName]]))
    
    # Merge individual and agedetermination:
    temp <- merge(BioticData[[individualName]], BioticData[[ageDeterminationName]], by = commonVar, all = TRUE)
    
    # Warning if there are more tmhan one age reading and preferredagereading is NA:
    temp$NumberOfAgeReadings <- table(apply(temp[, ..commonVar], 1, paste, collapse="_"))
    missing <- temp$NumberOfAgeReadings > 1 & is.na(temp$preferredagereading)
    if(any(missing)) {
        missingInfo <- paste(commonVar, BioticData[[individualName]][, commonVar][which(missing),], collapse=", ", sep=" = ")
        warning("The following individuals had several age readings but no preferred age reading. The first was chosen:\n", missingInfo)
    }
    
    # Insert 1 for missing preferredagereading:
    temp$PreferredAgeReadingTemp <- replace(temp$preferredagereading, is.na(temp$preferredagereading), values=1)
    
    # Pick out the preffered age readings:
    isPreferred <- temp$PreferredAgeReadingTemp == temp$agedeterminationid
    # Do not remove the individuals wich do not have age determination:
    isPreferred <- replace(isPreferred, is.na(isPreferred), values=TRUE)
    # Remove the non-preferred age readings:
    temp <- subset(temp, isPreferred)
    
    # Remove the temporary preferredagereading (since we inserted ones above):
    temp$NumberOfAgeReadings <- NULL
    temp$PreferredAgeReadingTemp <- NULL
    
    # Replace the individual table by the merged individual and agedetermination table, and return the biotic data:
    BioticData[[individualName]] <- temp
    BioticData
}

##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
DefineStation <- function(BioticData, StationKeys = c("missiontype", "startyear", "platform", "missionnumber", "serialnumber"), 
    ...) {

    commonVar <- intersect(names(BioticData[["mission"]]), names(BioticData[["fishstation"]]))

    temp <- merge(BioticData[["mission"]], BioticData[["fishstation"]], by = commonVar, all = TRUE)

    BioticData[["fishstation"]]$StationID <- apply(temp[, ..StationKeys], 1, paste, collapse="/")

    BioticData
}