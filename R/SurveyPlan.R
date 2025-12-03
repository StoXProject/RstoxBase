
#' Define transect design
#'
#' Defines transects through parameters that can be used to generate a transect design using the function \code{\link{TransectDesign}}.
#'
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @param DefinitionMethod  Character: A string naming the method to use, one of the three options "Parameter", for specifying common parameters for all strata given by \code{StratumNames}; "ParameterTable", for specifying individual parameters for each stratum in the table \code{ParameterTable}; and "ResourceFile", for reading the \code{ParameterTable} from a the file given by \code{FileName}.
#' @param FileName The path to a resource file from which to read the TransectParameter process data, in the case that \code{DefinitionMethod} is "ResourceFile".
#' @param StratumNames  Character: The names of the strata for which the parameters are valid. Defaults to all strata.
#' @param TransectType Character: The name of the type of survey design to create, one of "Parallel", "ZigZagEqualSpacing" and "ZigZagRectangularEnclosure" (see details).
#' @param Bearing  Character: A string indicating the survey bearing (direction) of each . See Details for options.
#' @param BearingAngle  Numeric: In the case that \code{Bearing = "Angle"}, \code{BearingAngle} gives the angle of the survey bearing (direction) counter clockwise from north in degrees.
#' @param Retour  Logical: If TRUE the transect design will be doubled by a retour.
#' @param SurveyTime  Numeric: The time to spend in the stratum including transit between segments, given in hours. Specifying the \code{SurveyTime} requires the \code{SurveySpeed} to be given as well. Note that the resulting accumulated time may not be exactly equal to \code{SurveyTime}.
#' @param SurveyDistance Numeric: The distance to travel in the stratum including transit between segments, given in nautical miles. The \code{SurveyDistance} has precedence over \code{SurveyTime}. Note that the resulting accumulated distance may not be exactly equal to \code{SurveyDistance}.
#' @param SurveySpeed Numeric: The speed of the vessel, needed if effort is specified by \code{SurveyTime}.
#' @param SurveyCoverage Numeric: The survey coverage, which when given and at least one of \code{SurveyTime} and \code{SurveySpeed} is not given will be used to calculate the \code{SurveyDistance} using the definition SurveyCoverage = SurveyDistance / sqrt(area of the stratum).
#' @param Seed Numeric: The seed to use when drawing the random starting point. 
#' @param ParameterTable A table specifying the parameters for each stratum to create transect design for. See the Details for a list of required columns.
#' 
#' @details
#' If the \code{ParameterTable} is given the table must contain the following columns:
#' \describe{
#'	\item{"StratumName"}{The name of the stratum, which will be matched against the names of the strata in the \code{StratumPolygon}}
#'	\item{"TransectType"}{See the argument \code{TransectType}}
#'	\item{"Bearing"}{See the argument \code{Bearing}}
#'	\item{"BearingAngle"}{If given, this overrides \code{Bearing}. See the argument \code{BearingAngle}}
#'	\item{"Retour"}{See the argument \code{Retour}}
#'	\item{"SurveyTime"}{See the argument \code{SurveyTime}}
#'	\item{"SurveyDistance"}{Overrides the \code{SurveyTime}. See the argument \code{SurveyDistance}}
#'	\item{"SurveySpeed"}{See the argument \code{SurveySpeed}}
#'	\item{"SurveyCoverage"}{See the argument \code{SurveyCoverage}}
#'	\item{"Seed"}{See the argument \code{Seed}}
#' }
#' 
#' The following types are implemented through \code{TransectType}:
#' \describe{
#'	\item{"Parallel"}{Parallel transects}
#'	\item{"ZigZagEqualSpacing"}{Equal space zigzag sampler, Strindberg and Buckland (2004). End transects are generated different from Strindberg and Buckland (2004), by mirroring the last transect around the line perpendicular to the survey direction passing through the last intersection point between the stratum border and the parallel lines used to generate the transects.}
#'	\item{"ZigZagRectangularEnclosure"}{Rectangular enclosure zigzag sampler, Harbitz (2019)}
#' }
#' 
#' @return
#' An object of StoX data type \code{\link{TransectParameter}}.
#'
#' @examples
#' 
#' stratumFile <- system.file(
#'   "testresources", 
#'   "strata_sandeel_2020_firstCoverage.wkt", package = "RstoxBase"
#'  )
#' stratumPolygon <- DefineStratumPolygon(
#'   DefinitionMethod = "ResourceFile", 
#'   FileName = stratumFile
#' )
#' 
#' parameterTable <- DefineTransectParameter(
#'  DefinitionMethod = "Parameter", 
#'  TransectType = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' print(parameterTable)
#'
#' @export
#' 
DefineTransectParameter <- function(
    processData, UseProcessData = FALSE, 
    DefinitionMethod = c("Parameter", "ParameterTable", "ResourceFile"), 
    
    StratumPolygon,
    
    # Arguments specific for DefinitionMethod "Parameter":
    StratumNames = character(), 
    TransectType = c("Parallel", "ZigZagRectangularEnclosure", "ZigZagEqualSpacing"), 
    Bearing = c("Along", "Across", "AlongReversed", "AcrossReversed"), 
    BearingAngle = numeric(), 
    Retour = FALSE, 
    SurveyTime = numeric(), 
    SurveyDistance = numeric(),
    SurveySpeed = numeric(), 
    SurveyCoverage = numeric(), 
    Seed = numeric(), 
    
    # Arguments specific for DefinitionMethod "ParameterTable":
    ParameterTable = data.table::data.table(),
    
    # Arguments specific for DefinitionMethod "ParameterFile" and "ResourceFile":
    FileName = character()
){
    
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the DefinitionMethod:
    DefinitionMethod <- RstoxData::match_arg_informative(DefinitionMethod)
    
    # Define the transec parameters as common parameters for the strata given by StratumNames:
    if(DefinitionMethod == "Parameter") {
        
        # Subset the strata if StratumNames is given:
        if(length(StratumNames)) {
            validStratumNames <- getStratumNames(StratumPolygon)
            if(!any(StratumNames %in% validStratumNames)) {
                stop("StratumNames can only contain valid stratum names. Possible names are: ", paste(validStratumNames, collapse = ", "))
            }
            
            StratumPolygon <- subsetStratumPolygon(StratumPolygon, StratumNames) 
        }
        numberOfStrata <- nrow(StratumPolygon)
        # Re-extract the StratumNames from the possibly subsetted StratumPolygon:
        StratumNames <- getStratumNames(StratumPolygon)
        
        # Get the TransectType and Bearing:
        Bearing <- RstoxData::match_arg_informative(Bearing)
        TransectType <- RstoxData::match_arg_informative(TransectType)
        
        # Currently StoX requires seed to be set. This may change in a future release, where empty seed could be accepted:
        if(!length(Seed) || is.na(Seed)) {
            stop("Seed must be given!")
        }
        
        
        
        # Get the number of strata and repeat all parameters if of length 1:
        if(numberOfStrata > 1) {
            
            errorMessage <- function(name) {
                stop(name, " cannot have length > 1. To specify ", name, " for each stratum use the ParameterTable.")
            }
            
            if(length(TransectType) == 1) TransectType <- rep(TransectType, numberOfStrata) else if(length(TransectType) > 1) errorMessage("TransectType")
            if(length(Bearing) == 1) Bearing <- rep(Bearing, numberOfStrata) else if(length(Bearing) > 1) errorMessage("Bearing")
            if(length(BearingAngle) == 1) BearingAngle <- rep(BearingAngle, numberOfStrata) else if(length(BearingAngle) > 1) errorMessage("BearingAngle")
            if(length(Retour) == 1) Retour <- rep(Retour, numberOfStrata) else if(length(Retour) > 1) errorMessage("Retour")
            if(length(SurveyTime) == 1) SurveyTime <- rep(SurveyTime / numberOfStrata, numberOfStrata) else if(length(SurveyTime) > 1) errorMessage("SurveyTime")
            if(length(SurveyDistance) == 1) SurveyDistance <- rep(SurveyDistance / numberOfStrata, numberOfStrata) else if(length(SurveyDistance) > 1) errorMessage("SurveyDistance")
            if(length(SurveySpeed) == 1) SurveySpeed <- rep(SurveySpeed, numberOfStrata) else if(length(SurveySpeed) > 1) errorMessage("SurveySpeed")
            if(length(SurveyCoverage) == 1) SurveyCoverage <- rep(SurveyCoverage, numberOfStrata) else if(length(SurveyCoverage) > 1) errorMessage("SurveyCoverage")
            if(length(Seed) == 1) Seed <- getSeedVector(Seed, numberOfStrata) else if(length(Seed) > 1) errorMessage("Seed")
            #if(length(Margin) == 1) Margin <- rep(Margin, numberOfStrata)
        }
        
        # Compose the parameter table:
        ParameterList <- list(
            StratumName = StratumNames, 
            TransectType = TransectType,
            Bearing = Bearing,
            BearingAngle = BearingAngle,
            Retour = Retour,
            SurveyTime = SurveyTime,
            SurveyDistance = SurveyDistance,
            SurveySpeed = SurveySpeed, 
            SurveyCoverage = SurveyCoverage, 
            Seed = Seed
            #Margin = Margin
        )
        ParameterTable <- data.table::as.data.table(ParameterList[lengths(ParameterList) > 0])
        
        
    }
    else if(DefinitionMethod == "ResourceFile") {
        
        # Read the parameter file:
        ParameterTable <- data.table::fread(FileName)
        
    }
    
    # Format the output:
    formatOutput(ParameterTable, dataType = "TransectParameter", keep.all = TRUE)
    
    return(ParameterTable[])
}




#' Define transect design
#'
#' @inheritParams general_arguments
#' @inheritParams ProcessData
#' @param TransectParameterDefinition Character: A string naming the method to use for defining the TransectParameter, one of \code{FunctionParameter} to define the TransectParameter on the fly in this function, or \code{FunctionInput} to import TransectParameter process data generated using the function \code{DefineTransectParameter}.
#' @param TransectParameterDefinitionMethod Character: A string naming the TransectParameter definition method to use, one of the two options "Parameter", for specifying common parameter for all strata given by \code{StratumNames}, or "ParameterTable", for specifying individual parameters for each stratum in the table \code{ParameterTable}. See \code{\link{DefineTransectParameter}} for details.
#' @param StratumNames  Character: The names of the strata to include in the transect design. Defaults to all strata.
#' @param TransectType Character: The name of the type of survey design to create, one of "Parallel", "ZigZagEqualSpacing" and "ZigZagRectangularEnclosure" (see details).
#' @param Bearing  Character: A string indicating the survey bearing (direction) of each . See Details for options.
#' @param BearingAngle  Numeric: In the case that \code{Bearing = "Angle"}, \code{BearingAngle} gives the angle of the survey bearing (direction) counter clockwise from north in degrees.
#' @param Retour  Logical: If TRUE the transect design will be doubled by a retour.
#' @param SurveyTime  Numeric: The time to spend in the stratum including transit between segments, given in hours. Specifying the \code{SurveyTime} requires the \code{SurveySpeed} to be given as well. Note that the resulting accumulated time may not be exactly equal to \code{SurveyTime}.
#' 
#' @param SurveyDistance Numeric: The distance to travel in the stratum including transit between segments, given in nautical miles. The \code{SurveyDistance} has precedence over \code{SurveyTime}. Note that the resulting accumulated distance may not be exactly equal to \code{SurveyDistance}.
#' @param SurveySpeed Numeric: The speed of the vessel, needed if effort is specified by \code{SurveyTime}.
#' @param SurveyCoverage Numeric: The survey coverage, which when given and at least one of \code{SurveyTime} and \code{SurveySpeed} is not given will be used to calculate the \code{SurveyDistance} using the definition SurveyCoverage = SurveyDistance / sqrt(area of the stratum).
#' @param Seed Numeric: The seed to use when drawing the random starting point. 
#' @param ParameterTable A table specifying the parameters for each stratum to create transect design for. See the Details for a list of required columns.
#' @param EqualEffort  Character: A string naming the method to use. See Details for options.
#' @param OrderAllToursFirst  Logical: If TRUE order all tours first and all retours last, which can be useful for multiple Strata in the same survey direction (e.g. a row of strata along a coast line).
#' @param Margin Numeric: The margin to use when iterating to fit the transect design to the desired survey distance (including transit between segments). The function iterates until the survey distance is within \code{Margin} of the desired survey distance, and jumps out of the iteration after 100 tries or if not converging.
#' 
#' @details 
#' The \code{TransectDesign} function generates a transect design in a Cartesian coordinate system, and transforms the positions to the geographical coordinate system (longitude, latitude) using the azimuthal equal distance projection, which ensures that distances are preserved in the transformation.
#' 
#' If the \code{ParameterTable} is given the table must contain the following columns:
#' \describe{
#'	\item{"StratumName"}{The name of the stratum, which will be matched against the names of the strata in the \code{StratumPolygon}}
#'	\item{"TransectType"}{See the argument \code{TransectType}}
#'	\item{"Bearing"}{See the argument \code{Bearing}}
#'	\item{"BearingAngle"}{If given, this overrides \code{Bearing}. See the argument \code{BearingAngle}}
#'	\item{"Retour"}{See the argument \code{Retour}}
#'	\item{"SurveyTime"}{See the argument \code{SurveyTime}}
#'	\item{"SurveyDistance"}{Overrides the \code{SurveyTime}. See the argument \code{SurveyDistance}}
#'	\item{"SurveySpeed"}{See the argument \code{SurveySpeed}}
#'	\item{"SurveyCoverage"}{See the argument \code{SurveyCoverage}}
#'	\item{"Seed"}{See the argument \code{Seed}}
#' }
#' 
#' The following types are implemented through \code{TransectType}:
#' \describe{
#'	\item{"Parallel"}{Parallel transects}
#'	\item{"ZigZagEqualSpacing"}{Equal space zigzag sampler, Strindberg and Buckland (2004). End transects are generated different from Strindberg and Buckland (2004), by mirroring the last transect around the line perpendicular to the survey direction passing through the last intersection point between the stratum border and the parallel lines used to generate the transects.}
#'	\item{"ZigZagRectangularEnclosure"}{Rectangular enclosure zigzag sampler, Harbitz (2019)}
#' }
#' 
#' @references 
#' Strindberg, S., & Buckland, S. T. (2004). Zigzag survey designs in line transect sampling. Journal of Agricultural, Biological, and Environmental Statistics, 9, 443-461.
#' 
#' Harbitz, A. (2019). A zigzag survey design for continuous transect sampling with guaranteed equal coverage probability. Fisheries Research, 213, 151-159.
#' 
#' @return
#' An object of StoX data type \code{\link{TransectDesignData}}.
#'
#' @examples
#' library(ggplot2)
#' 
#' stratumFile <- system.file(
#'   "testresources", 
#'   "strata_sandeel_2020_firstCoverage.wkt", package = "RstoxBase"
#'  )
#' stratumPolygon <- DefineStratumPolygon(
#'   DefinitionMethod = "ResourceFile", 
#'   FileName = stratumFile
#' )
#' 
#' # Harbitz zigzag survey design along each stratum:
#' transectDesignZZ_Along <- TransectDesign(
#'  TransectParameterDefinition = "FunctionParameter", 
#'  TransectParameterDefinitionMethod = "Parameter", 
#' 	TransectType = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' # Plot the stratumPolygon with the segments
#' ggplot() +
#'   geom_sf(data = stratumPolygon, aes(fill = StratumName), color = 'blue') +
#'   geom_segment(
#'     data = transectDesignZZ_Along, 
#'     aes(x = LongitudeStart, y = LatitudeStart, xend = LongitudeEnd, yend = LatitudeEnd)
#'   )
#' 
#' # Harbitz zigzag survey design across each stratum:
#' transectDesignZZ_Across <- TransectDesign(
#'  TransectParameterDefinition = "FunctionParameter", 
#'  TransectParameterDefinitionMethod = "Parameter", 
#' 	TransectType = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Across"
#' )
#' # Plot the stratumPolygon with the segments
#' ggplot() +
#'   geom_sf(data = stratumPolygon, aes(fill = StratumName), color = 'blue') +
#'   geom_segment(
#'     data = transectDesignZZ_Across, 
#'     aes(x = LongitudeStart, y = LatitudeStart, xend = LongitudeEnd, yend = LatitudeEnd)
#'   )
#' 
#' # Parallel survey design across each stratum:
#' transectDesignParallel_Across <- TransectDesign(
#'  TransectParameterDefinition = "FunctionParameter", 
#'  TransectParameterDefinitionMethod = "Parameter", 
#' 	TransectType = "Parallel", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' # Plot the stratumPolygon with the segments
#' ggplot() +
#'   geom_sf(data = stratumPolygon, aes(fill = StratumName), color = 'blue') +
#'   geom_segment(
#'     data = transectDesignParallel_Across, 
#'     aes(x = LongitudeStart, y = LatitudeStart, xend = LongitudeEnd, yend = LatitudeEnd)
#'   )
#'
#' @export
#' 
TransectDesign <- function(
    StratumPolygon, 
    
    # Use a Transect process data or define parameters on the fly:
    TransectParameterDefinition = c("FunctionParameter", "FunctionInput"), 
    
    # If defining parameters on the fly, specify the same parameters for all strata or a table of parameters for each stratum:
    TransectParameterDefinitionMethod = c("Parameter", "ParameterTable"), 
    
    # Arguments specific for DefinitionMethod "Parameter":
    StratumNames = character(), 
    TransectType = c("Parallel", "ZigZagRectangularEnclosure", "ZigZagEqualSpacing"), 
    Bearing = c("Along", "Across", "AlongReversed", "AcrossReversed"), 
    BearingAngle = numeric(), 
    Retour = FALSE, 
    SurveyTime = numeric(), 
    SurveyDistance = numeric(),
    SurveySpeed = numeric(), 
    SurveyCoverage = numeric(), 
    Seed = numeric(), 
    
    # Arguments specific for DefinitionMethod "ParameterTable":
    ParameterTable = data.table::data.table(),
    
    # The Transect process data:
    TransectParameter, 
    
    # General arguments irrespective of DefinitionMethod:
    EqualEffort = TRUE, 
    OrderAllToursFirst = FALSE, 
    Margin = 0.1
){
    
    # Get the TransectDefinition and TransectDefinitionMethod:
    TransectParameterDefinition <- RstoxData::match_arg_informative(TransectParameterDefinition)
    TransectParameterDefinitionMethod <- RstoxData::match_arg_informative(TransectParameterDefinitionMethod)
    
    # Use the input Transect if TransectDefinition == "FunctionInput":
    if(TransectParameterDefinition == "FunctionParameter") {
        TransectParameter <- DefineTransectParameter(
            
            UseProcessData = FALSE, 
            DefinitionMethod = TransectParameterDefinitionMethod, 
            
            StratumPolygon = StratumPolygon, 
            
            # Arguments specific for DefinitionMethod "Parameter":
            StratumNames = StratumNames, 
            TransectType = TransectType, 
            Bearing = Bearing, 
            BearingAngle = BearingAngle, 
            Retour = Retour, 
            SurveyTime = SurveyTime, 
            SurveyDistance = SurveyDistance,
            SurveySpeed = SurveySpeed, 
            SurveyCoverage = SurveyCoverage, 
            Seed = Seed, 
            # Arguments specific for DefinitionMethod "ParameterTable":
            ParameterTable = ParameterTable
            
        )
    }
    
    
    
    
    
    
    # Create the transect design from table of parameters:
    TransectDesign <- getTransectDesignFromTable(
        TransectParameter, 
        StratumPolygon = StratumPolygon, 
        EqualEffort = EqualEffort, 
        OrderAllToursFirst = OrderAllToursFirst, 
        Margin = Margin
    )
    
    
    
    formatOutput(TransectDesign, dataType = "TransectDesign", keep.all = FALSE)
    
    return(TransectDesign[])
}
































































# Function to create a transect design from a table of parameters:
getTransectDesignFromTable <- function(
        patameterTable, 
        StratumPolygon, 
        EqualEffort = FALSE, 
        OrderAllToursFirst = FALSE, 
        Margin = 0.1, 
        NumberOfPoints = 1e4
) {
    
    # Subset the StratumPolygon by the stratum names given by the patameterTable:
    stratumNames <- getStratumNames(StratumPolygon)
    if(!any(patameterTable$StratumName %in% stratumNames)) {
        stop("StoX: The column StratumName does not contain any of the stratum names of the StratumPolygon.")
    }
    if(!all(patameterTable$StratumName %in% stratumNames)){
        warning("StoX: The following stratum names given by the column StratumName do not exist in the StratumPolygon.: ", paste(setdiff(stratumNames, patameterTable$StratumName), collapse = ", "), ".")
        patameterTable <- subset(patameterTable, StratumName %in% stratumNames)
    }
    
    # Require seed:
    if(isEmpty(patameterTable$Seed)) {
        stop("StoX: Seed must be given as an integer.")
    }
    
    # Get the stratum areas:
    stratumArea <- StratumArea(StratumPolygon)
    # Merge the stratumArea into the patameterTable:
    patameterTable <- merge(patameterTable, stratumArea, by.x = "StratumName", by.y = "Stratum")
    
    # Get the survey distance from time:
    if(isEmpty(patameterTable$SurveyDistance)) {
        if(!isEmpty(patameterTable$SurveyTime) && !isEmpty(patameterTable$SurveySpeed)) {
            patameterTable$SurveyDistance <- patameterTable$SurveyTime * patameterTable$SurveySpeed
        }
        else if(!isEmpty(patameterTable$SurveyCoverage)) {
            patameterTable$SurveyDistance <- patameterTable$SurveyCoverage * sqrt(patameterTable$Area)
        }
        else {
            stop("StoX: If SurveyDistance is not given, both of SurveyTime and SurveySpeed must be given!")
        }
    }
    # Override the distances if EqualEffort is TRUE
    if(EqualEffort){
        thisStratumArea <- stratumArea[patameterTable$StratumName, Area, on = "Stratum"]
        patameterTable$SurveyDistance <- sum(patameterTable$SurveyDistance) * thisStratumArea / sum(thisStratumArea)
    }
    
    # Add the speed (as NA if not given):
    if(!length(patameterTable$SurveySpeed)) {
        patameterTable$SurveySpeed <- NA_real_
    }
    
    
    # Get the centroid used when transforming to Cartesian coordinates:
    turn_off_s2(
        commonCentroid <- getCentroid(StratumPolygon, iterativeCentroidCalculation = FALSE) , 
        msg = FALSE
    )
    # Transform to Cartesian coordinates:
    aeqd.CRS <- paste0(
        "+proj=aeqd +lon_0=", 
        commonCentroid[1], 
        " +lat_0=", 
        commonCentroid[2], 
        " +x_0=0 +y_0=0 +ellps=WGS84 +units=kmi +no_defs"
    )
    longlat.CRS <- paste0(
        "+proj=longlat +lon_0=0", 
        " +lat_0=0", 
        " +x_0=0 +y_0=0 +ellps=WGS84 +no_defs"
    )
    #longlat.CRS <- paste0(
    #  "+proj=longlat +lon_0=", 
    #  commonCentroid[1], 
    #  " +lat_0=", 
    #  commonCentroid[2], 
    #  " +x_0=0 +y_0=0 +ellps=WGS84 +no_defs"
    #)
    
    
    
    # Create the transect design for each stratum:
    numberOfStrata <- nrow(patameterTable)
    TransectDesignList <- vector("list", numberOfStrata)
    
    # Loop through the rows of the patameterTable:
    for(ind in seq_len(numberOfStrata)) {
        
        # Subset the StratumPolygon:
        atThisStratumName <- which(patameterTable$StratumName[ind] == stratumNames)
        thisStratumPolygon <- StratumPolygon[ind, ]
        
        # Define a temporary table of x and y:
        TransectDesignList[[ind]] <- transectsOneStratum(
            # Parameters from the StratumPolygon, which are references using the atThisStratumName:
            StratumPolygonOneStratum = StratumPolygon[atThisStratumName, ], 
            stratumName = stratumNames[atThisStratumName], 
            stratumArea = stratumArea[atThisStratumName], 
            # General parameters:
            aeqd.CRS = aeqd.CRS, 
            longlat.CRS = longlat.CRS, 
            # Parameters from the patameterTable:
            TransectType = patameterTable$TransectType[ind], 
            Bearing = patameterTable$Bearing[ind], 
            BearingAngle = patameterTable$BearingAngle[ind], 
            Retour = patameterTable$Retour[ind], 
            SurveyTime = patameterTable$SurveyTime[ind], 
            SurveyDistance = patameterTable$SurveyDistance[ind], 
            SurveySpeed = patameterTable$SurveySpeed[ind], 
            Seed = patameterTable$Seed[ind], 
            #Margin = patameterTable$Margin[ind], 
            Margin = Margin, 
            N = NumberOfPoints
        )
    }
    
    names(TransectDesignList) <- patameterTable$StratumName
    
    
    # Convert to start and end position (latitude and longitude):
    TransectDesign <- lapply(TransectDesignList, function(x) lapply(x, XY2startEnd))
    
    
    # Rbind the segments per stratum:
    TransectDesign <- lapply(TransectDesign, data.table::rbindlist, idcol = "Transect")
    # Add input survey speed:
    TransectDesign <- mapply(cbind, TransectDesign, lapply(patameterTable$SurveySpeed, function(x) data.table::setnames(data.table::data.table(x), "Speed")), SIMPLIFY = FALSE)
    # Rbind strata:
    TransectDesign <- data.table::rbindlist(TransectDesign, idcol = "Stratum")
    # Insert a sequence of segment indices:
    #TransectDesign[, Segment := paste("S", factor(paste(Stratum, sprintf("%06d", Segment))))]
    
    if(OrderAllToursFirst) {
        TransectDesign <- rbind(
            subset(TransectDesign, Direction == "Tour"), 
            subset(TransectDesign, Direction == "Retour")[rev(seq_len(nrow(TransectDesign))), ]
        )
    }
    
    return(TransectDesign)
}





isEmpty <- function(x) {
    length(x) == 0 || any(is.na(x))
}



XY2startEnd <- function(x) {
    # Assume that all other columns than X and Y are contant and can be added afterwards:
    XY <- subset(x, select = c("X", "Y"))
    otherCols <- x[1, ! c("X", "Y")]
    
    # Convert from 2 to 4 columns:
    nrowx <- nrow(XY)
    odd <- seq_len(nrowx) %% 2 == 1
    even <- seq_len(nrowx) %% 2 == 0
    out <- data.table::data.table(XY[odd, ], XY[even, ])
    data.table::setnames(out, c("LongitudeStart", "LatitudeStart", "LongitudeEnd", "LatitudeEnd"))
    
    # Add segment names:
    numberOfSegments <- nrow(out)
    out$Segment <- getElementID("Segment", numberOfSegments)
    #paste0("Segment_", sprintf(paste0("%0", nchar(numberOfSegments), "d"), seq_len(numberOfSegments)))
    
    # Add the otherCols
    out <- cbind(out, otherCols)
    
    # Remove entries with duplicated positions:
    dup <- out[, LongitudeStart == LongitudeEnd & LatitudeStart == LatitudeEnd]
    out <- subset(out, !dup)
    
    return(out)
}






#getBearing <- function(StratumPolygonXY, Bearing = c("Along", "Across", "AlongReversed", "AcrossReversed"), BearingAngle = NULL, N = 1e3){
#  
#  # Get the bearing:
#  numberOfStrata <- nrow(StratumPolygon)
#  BearingAngle <- logical(numberOfStrata)
#  for(ind in seq_len(numberOfStrata)) {
#    BearingAngle[ind] <- getBearingOneMultipolygon(StratumPolygonXYOne = StratumPolygonXY[ind, ], Bearing = Bearing[ind], N = N)
#  }
#  
#  return(BearingAngle)
#}


getBearingOneStratum <- function(StratumPolygonOneStratumXY, Bearing = c("Along", "Across", "AlongReversed", "AcrossReversed"), N = 1e3) {
    
    # Get the bearing as either "Along", "Across", "AlongReversed" or "AcrossReversed":
    Bearing <- RstoxData::match_arg_informative(Bearing)
    
    # Extract the coordinates:
    xy <- subset(data.table::as.data.table(sf::st_coordinates(StratumPolygonOneStratumXY)), select = c("X", "Y"))
    
    # Detect whether the bearing should be reversed or across instead of along:
    reversed <- grepl("Reversed", Bearing)
    across <- grepl("Across", Bearing)
    
    # Get the bearing along from the eigenvalues of the covariance matrix of the polygon borders populated by many points:
    xyPopulated <- populatePath(xy, N = N)
    eigenxy <- eigen(cov(xyPopulated))
    angle <- atan(eigenxy$vectors[2, 1] / eigenxy$vectors[1, 1]) * 180 / pi
    
    # Rotate by 90 degrees if across:
    if(across) {
        angle <- angle + 90
    }
    if(reversed) {
        angle <- angle - 180
    }
    
    return(angle)
}

# Function used for populating a path with points of constant distance 'dt':
populatePath <- function(xy, N = 100, dt = NULL){
    
    # Get traveled distance, which we interpret as time (for constant speed):
    diffXY <- xy[, lapply(.SD, diff)]
    difft <- diffXY[, sqrt(X^2 + Y^2)]
    # Get the cumulative time:
    cumtime <- c(0, cumsum(difft))
    # Get the increment in time:
    if(length(dt)==0){
        dt <- cumtime[length(cumtime)] / N
    }
    
    # Get the new time vector and the new x and y values:
    tnew <- unlist(lapply(seq_len(length(difft)), tseqOne, t = cumtime, dt = dt))
    #xynew <- apply(xy, 2, function(z) approx(x = cumtime, y = z, xout = tnew)$y)
    xynew <- xy[, lapply(.SD, function(z) approx(x = cumtime, y = z, xout = tnew)$y)]
    
    
    # Special care taken if only one point was populated (the last original point is added below):
    if(length(dim(xynew)) < 2){
        xynew <- t(xynew)
    }
    xynew <- data.table::as.data.table(xynew)
    
    # Add the last point to close the path:
    xynew <- rbind(xynew, xy[nrow(xy), ])
    
    # Include the projection in the output:
    return(xynew)
}

# Function for getting the time sequence in one stretch, as 
tseqOne <- function(i, t, dt){
    tnew <- seq(t[i], t[i+1], dt)
    tnew
}


subsetStratumPolygon <- function(StratumPolygon, subset = NA) {
    if(length(subset) == 1 && is.na(subset)) {
        return(StratumPolygon)
    }
    else {
        if(is.character(subset)) {
            subset <- match(subset, getStratumNames(StratumPolygon))
        }
        StratumPolygon <- StratumPolygon[subset, ]
    }
}



# Function for rotating 2-D points by an angle:
rotate2d <- function(StratumPolygonXY, BearingAngle) {
    
    for(ind in seq_len(nrow(StratumPolygonXY))) {
        StratumPolygonXY[ind, ] <- rotate2dOne(StratumPolygonXY[ind, ], BearingAngle[ind])
    }
    
    return(StratumPolygonXY)
}


rotate2dOne <- function(x, angleDegrees) {
    
    #angleRadians <- angleDegrees * pi / 180
    #
    ## Create a rotation matrix and apply it to the input data:
    #rotationMatrix = matrix(c(cos(angleRadians), sin(angleRadians), -sin(angleRadians), cos(angleRadians)), 2, 2, byrow = TRUE)
    #coords <- sf::st_coordinates(x)
    #coords[, c("X", "Y")] <- t(rotationMatrix %*% t(coords[, c("X", "Y")]))
    #sf::st_coordinates(x) <- coords
    
    angleRadians <- angleDegrees * pi / 180
    
    # Create a rotation matrix and apply it to the input data:
    sf::st_geometry(x) <- sf::st_geometry(x) * getRotationMatrix(angleRadians)
    
    return(x)
}


getRotationMatrix <- function(angleRadians) {
    matrix(c(cos(angleRadians), sin(angleRadians), -sin(angleRadians), cos(angleRadians)), 2, 2, byrow = TRUE)
}




############################################################
########## Function for generating the transects ###########
############ in one direction (tour or retour): ############
############################################################
getTransectsOneDirection <- function(
        xGrid, 
        corners, 
        StratumPolygonOneStratumXY, 
        Seed = 0, 
        TransectType = "Parallel", 
        Retour = FALSE
){
    # If we are on a retour, reverse the order of the grid points:
    # The downRandom is TRUE for even and FALSE for odd values of Seed but only for parallel:
    if(tolower(TransectType) != "parallel" ){
        downRandom <- FALSE
    }
    else {
        downRandom <- Seed %% 2 == 0
    }
    if(Retour){
        downRandom <- !downRandom 
    }
    
    # For parallel transects we need to shift by the halfTransectSpacing on the retour:
    if(tolower(TransectType) == "parallel" && Retour){
        halfTransectSpacing <- diff(xGrid[1:2]) / 2
        xGrid <- xGrid + halfTransectSpacing
    }
    
    ##### Create the grid to intersect with the stratum polygon: #####
    # Get the grid of lines which are used to find intersection points between transects and polygon borders:
    grid <- data.table::data.table(
        X = rep(xGrid, each = 2), 
        Y = rep(unlist(corners[c("ymin", "ymax")]), length(xGrid))
    )
    # Split into individual lines:
    gridList <- split(grid, rep(seq_along(xGrid), each = 2))
    # For converting to a multilinestring object, matrix is needed by sf:
    gridList <- lapply(gridList, as.matrix)
    
    # For Harbitz zigzag transects with equal coverage, convert the grid to zigzag lines, by selecting every other point:
    if(tolower(TransectType) == tolower("ZigZagRectangularEnclosure")){
        # Apply the ordering of the grid prior to linking consecutive high-high and low-low points:
        #gridList <- linkClosestByDistance(gridList, down = downRandom)
        gridList <- alternateTransectDirections(gridList, down = downRandom)
        
        # Select the first end point of each grid line, and generate zigzag grid by merging consecutive points:
        gridList <- parallel2zigzag(gridList)
    }
    
    
    # Get the intersections between the grid list and the polygon:
    intersectsCoordsList <- getSegmentsFromLinesAndPolygon(gridList, StratumPolygonOneStratumXY, downRandom = downRandom)
    
    # Then alternate the intersections. This is required for Parallel and for ZigZagRectangularEnclosure, which both are now finished and simply need to return the points in the correct order; and it is also required for the ZigZagEqualSpacing, as we will create a zig zag grid from the end point of each transect of intersection points:
    ###if(linkByDistance) {
    ###    intersectsCoordsList <- linkClosestByDistance(linesSplitList, down = downRandom)
    ###}
    ###else {
    ###    intersectsCoordsList <- alternateTransectDirections(linesSplitList, down = downRandom)
    ###}
    intersectsCoordsList <- alternateTransectDirections(intersectsCoordsList, down = downRandom)
    
    
    # For ZigZagEqualSpacing transects, set the end point of each transect to the start point of the next:
    if(tolower(TransectType) == tolower("ZigZagEqualSpacing")){
        
        if(length(intersectsCoordsList) == 1) {
            stop("Insufficient effort to produce one zig zag transect in the stratum ", StratumPolygonOneStratumXY$StratumName, ". Please increase effort or choose a different TransectType (e.g. \"Parallel\").")
        }
        
        # Convert to zigzag by selecting the last point in each transect, requiring that the transects are in alternate order, as obtained by alternateTransectDirections() above:
        intersectsCoordsListZigZag <- parallel2zigzag(intersectsCoordsList)
        
        # Set the first point as the same y value as the tail point of the first segment in intersectsCoordsList but on the previous xGrid (as the function parallel2zigzag uses the head point). See Strindberg, S., & Buckland, S. T. (2004), Figure 4. Do the same for the end point:
        
        startPoint <- utils::tail(intersectsCoordsList[[1]], 1)
        startPoint[, 1] <- startPoint[, 1] - diff(xGrid[1:2])
        startSegment <- rbind(startPoint, intersectsCoordsListZigZag[[1]][1, ])
        
        # The end point is likewise, but from the tail point if the there are an odd number of transects:
        endPoint <- utils::tail(intersectsCoordsList[[length(intersectsCoordsList)]], 1)
        endPoint[, 1] <- endPoint[, 1] + diff(xGrid[1:2])
        endSegment <- rbind(intersectsCoordsListZigZag[[length(intersectsCoordsListZigZag)]][2, ], endPoint)
        
        # Add the start and end point to the zigzag:
        intersectsCoordsList <- c(
            list(startSegment), 
            intersectsCoordsListZigZag, 
            list(endSegment)
        )
        
        # Re-intersect with the polygon:
        intersectsCoordsList <- getSegmentsFromLinesAndPolygon(intersectsCoordsList, StratumPolygonOneStratumXY, downRandom = downRandom)
        # Link the points by disance, as the direction of segments can be irregular at the start and end of a stratum:
        intersectsCoordsList <- linkClosestByDistance(intersectsCoordsList, down = downRandom)
    }
    
    
    # If on a retour, reverse order of the transects and within all transects:
    if(Retour){
        intersectsCoordsList <- rev(intersectsCoordsList)
        
        # 
        if(! TransectType %in% c("ZigZagEqualSpacing", "ZigZagRectangularEnclosure")){
            intersectsCoordsList <- lapply(intersectsCoordsList, function(x) x[seq(nrow(x), 1), , drop = FALSE])
        }
    }
    
    # Add a column denoting tour or retour. This must be numeric since we need a numeric matrix as input to sf::st_linestring later:
    intersectsCoordsList <- lapply(intersectsCoordsList, cbind, DirectionNumeric = as.numeric(Retour))
    
    
    return(intersectsCoordsList)
}
############################################################
############################################################



getSegmentsFromLinesAndPolygon <- function(list, pol, downRandom, digits = 5) {
    
    # Create a collection of linestrings:
    lines = lapply(list, sf::st_linestring)
    lines <- do.call(sf::st_sfc, lines)
    
    # Set the same CRS as the 
    sf::st_crs(lines) <- sf::st_crs(pol)
    
    # Split the lines by the multipolygons:
    #linesSplit <- lwgeom::st_split(lines, pol)
    #linesSplit <- sf::st_intersection(lines, pol)
    # Turn off s2 here. Introduced in R 4.5.2 on macOS latest (2025-11-01):
    #turn_off_s2(
    #    linesSplit <- sf::st_intersection(lines, pol), 
    #    msg = FALSE
    #)
    
    # To aviod potential problems with non-closed multipolygons, set precision to 12 digits:
    pol <- setPrecisionToSF(pol, digits = 12)
    linesSplit <- sf::st_intersection(lines, pol)
    
    
    # Get the coordinates:
    linesSplitList <- lapply(linesSplit, sf::st_coordinates)
    linesSplitList <- lapply(linesSplitList, "[", , 1:2)
    # Reduce precision to 5 digits (corresponding to 1852 * 1e-5 = 0.01852 m, as the positions are in nautical miles) to avoid duplicated points created in the intersection process. we may even use 3 or 4 digits here:
    
    linesSplitList <- lapply(linesSplitList, round, digits = digits)
    #linesSplitList <- lapply(linesSplitList, unique)
    
    # IT may happen that the linesSplitList is empty due to too low effort:
    if(!length(linesSplitList)) {
        stop("There were no intersections between the lines and polygons for Stratum ", pol$StratumName, ". Please increase effort.")
    }
    
    
    
    # ... and extract the actual intersection points as the first point of each segment, excluding the last segment, since the last point on one segment is identical to the first point of the next:
    #linesSplitList <- lapply(linesSplitList, function(x) unique(do.call(rbind, x)[, 1:2]))
    
    # The lwgeom::st_split keeps the original points 
    
    #linesSplitList <- lapply(linesSplitList, function(x) unique(do.call(rbind, lapply(y) if(NROW(y) >= 2) y[-c(1, nrow(y)), ] else y)[, 1:2]))
    
    
    
    # Remove elements with only 0 or 1 row:
    #intersectsCoordsList <- intersectsCoordsList[sapply(intersectsCoordsList, NROW) > 1]
    
    return(linesSplitList)
}





expand_transect_ends <- function(x, fact=1000){
    firstToLast <- apply(x, 2 , function(y) y[c(1, nrow(x))])
    mid <- matrix(colMeans(firstToLast), byrow=TRUE, nrow=2, ncol=2)
    out <- (firstToLast - mid) * fact + mid
    out
}



## Function for ordering points in each element of a list to be increasing by x and then y:
orderTransectsByXY <- function(x, down = FALSE){
    lapply(x, orderTransectsByXYOne, down = down)
}
orderTransectsByXYOne <- function(x, down = FALSE){
    if(NROW(x) > 1) {
        x <- x[order(x[, "X"], x[, "Y"], decreasing = down), ]
    }
    return(x)
}


# Function for linking consecutive transects stored in a list of data frames, in a way so that if the previous transect is uppwards in y, the next will be downwards, and vice versa:
alternateTransectDirections <- function(x, down = FALSE){
    
    x <- orderTransectsByXY(x, down = down)
    
    # Remove the empty elements, which are from grid lines outside of the polygons, which happens due to the flexibility related to the start position:
    x <- x[lengths(x) > 0]
    
    # Assume ordered data frames by x and then y, e.g., obtained by orderTransectsByXY():
    getUp <- function(x){
        #up <- unlist(x[c(1, nrow(x)), "Y"])
        #up <- diff(up) > 0
        #up
        x[nrow(x), "Y"] > x[1, "Y"]
    }
    if(length(x)==1){
        return(x)
    }
    
    
    
    # Get first up:
    up <- getUp(x[[1]])
    
    # Then move through the list and order according to the previous line segment:
    for(i in 1 + seq_along(x[-1])){
        npoints <- nrow(x[[i]])
        if(up == getUp(x[[i]])){
            x[[i]] <- x[[i]][rev(seq_len(npoints)), ]
        }
        up <- !up
    }
    return(x)
}



# Function for linking consecutive transects stored in a list of data frames, such that the distance between transects is minimized:
linkClosestByDistance <- function(x, down = FALSE){
    
    # Remove the empty elements, which are from grid lines outside of the polygons, which happens due to the flexibility related to the start position:
    x <- x[lengths(x) > 0]
    
    # Order the first point by x and then y:
    x[[1]] <- orderTransectsByXYOne(x[[1]], down = down)
    
    # Get distance between transect start and end points:
    for(ind in seq_len(length(x)- 1)) {
        
        # In the case that down it TRUE, implying Retour, compare the first point of each segment to the first and last point of the next:
        if(down) {
            distToHead <- distXY(
                utils::head(x[[ind]], 1), 
                utils::head(x[[ind + 1]], 1)
            )
            distToTail <- distXY(
                utils::head(x[[ind]], 1), 
                utils::tail(x[[ind + 1]], 1)
            )
        }
        # Otherwise, for Tour, compare the last point of each segment to the first and last point of the next:
        else {
            distToHead <- distXY(
                utils::tail(x[[ind]], 1), 
                utils::head(x[[ind + 1]], 1)
            )
            distToTail <- distXY(
                utils::tail(x[[ind]], 1), 
                utils::tail(x[[ind + 1]], 1)
            )
        }
        
        
        if(down) {
            if(distToTail >= distToHead) {
                x[[ind + 1]] <- x[[ind + 1]][rev(seq_len(nrow(x[[ind + 1]]))), ]
            }
        }
        else {
            if(distToTail < distToHead) {
                x[[ind + 1]] <- x[[ind + 1]][rev(seq_len(nrow(x[[ind + 1]]))), ]
            }
        }
        
    }

    return(x)
}


distXY <- function(xy1, xy2) {
    sqrt(sum((xy2 - xy1)^2))
}


# Function for selecting the first point of each list element, and generating zigzag grid by merging consecutive points:
parallel2zigzag <- function(x){
    
    # If only one segment, return the first and last position:
    if(length(x) == 1) {
        x[[1]] <- x[[1]][c(1, nrow(x[[1]])), ]
        return(x)
    }
    
    
    oldNames <- names(x[[1]])
    # Get the first element of each line, requiring that the data have been linked by alternate direction using alternateTransectDirections() or linkClosestByDistance() first:
    start <- do.call(rbind, lapply(x, utils::head, 1))
    # Generate the indices used to split the data into line segments:
    tempSeq <- seq_len(nrow(start) - 1)
    transecind <- rep(tempSeq, each = 2)
    # Generate the indices used to access the line segments in 'start':
    ind <- c(outer(0:1, tempSeq, "+"))
    # Select the line segments and split to one list per segment:
    start <- start[ind,]
    start <- split(start, transecind)
    start <- lapply(start, matrix, ncol = 2, nrow = 2, dimnames = list(NULL, c("X", "Y")))
    names(start) <- oldNames[-length(oldNames)]
    start
}




############################################################
#### Function for generating one set of transects given ####
#### the 'stratumArea', the tracklength minus the width of the ####
### stratum 'freeSurveyDistance', the seed factor 'startPositionFactor', the range ###
#### of x values in the rectangle that has been rotated ####
### to have x along the 'bearing' (the direction in which ##
#### to propagate through the stratum) ('xmin', 'xmax'), ###
############ and the x,y positions 'xy': ############
############################################################
getTransectsByArea <- function(
    freeSurveyDistance, 
    stratumArea, 
    startPositionFactor, 
    corners, 
    StratumPolygonOneStratumXY, 
    TransectType = "Parallel", 
    BearingAngle = 0, 
    Seed = 0, 
    Retour = FALSE
){
    
    # Get the transectSpacing. This calculation assumes rectangular strata, and may lead to too small transectSpacing for e.g. L-shaped strata. This can lead to long survey tracks and in extreme cases longer than twice the freeSurveyDistance, in which case freeSurveyDistance will be negative in the next iteration (as we are adding the 'rest' which is then larger negative than freeSurveyDistance is positive). In those cases we simply try with half the freeSurveyDistance.:
    transectSpacing <- stratumArea$Area / freeSurveyDistance
    
    # If the transect sould go tour-retour, use half spacing for parallel andtransects, and for zigzag simply go back with opposite order:
    #if(type == "Parallel" && retour){
    if(Retour){
        transectSpacing <- transectSpacing * 2
    }
    # Set the leftmost position of the grid lines:
    firstTransectPos <- 2 * transectSpacing * startPositionFactor
    # Get x positions of the grid:
    xGrid <- seq(corners$xmin - 2 * firstTransectPos, corners$xmax + 2 * transectSpacing, by = transectSpacing)
    
    
    # Generate the transects in one direction:
    intersectsCoordsList <- getTransectsOneDirection(
        xGrid = xGrid, 
        corners = corners, 
        StratumPolygonOneStratumXY = StratumPolygonOneStratumXY, 
        Seed = Seed, 
        TransectType = TransectType, 
        Retour = FALSE
    )
    if(Retour){
        intersectsCoordsList <- c(
            intersectsCoordsList, 
            getTransectsOneDirection(
                xGrid = xGrid, 
                corners = corners, 
                StratumPolygonOneStratumXY = StratumPolygonOneStratumXY, 
                Seed = Seed, 
                TransectType = TransectType, 
                Retour = TRUE
            )
        )
    }
    
    
    return(intersectsCoordsList)
}
############################################################
############################################################


############################################################
#### Function for generating transects for one stratum: ####
############################################################
transectsOneStratum <- function(
        StratumPolygonOneStratum, 
        stratumName, 
        stratumArea, 
        aeqd.CRS, 
        longlat.CRS, 
        TransectType = c("Parallel", "ZigZagRectangularEnclosure", "ZigZagEqualSpacing"), 
        Bearing = c("Along", "Across", "AlongReversed", "AcrossReversed"), 
        BearingAngle = numeric(), 
        Retour = FALSE, 
        SurveyTime = numeric(),
        SurveyDistance = numeric(),
        SurveySpeed = numeric(),
        Seed = numeric(), 
        Margin = 0, 
        N = 1e3
){
    
    # Repeat the angle if given:
    if(length(Bearing) && length(BearingAngle) && !is.na(BearingAngle)) {
        warning("StoX: When both Bearing and BearingAngle are given, the Bearing is ignored.")
    }
    
    # Transform to Cartesian coordinates:
    StratumPolygonOneStratumXY <- sf::st_transform(StratumPolygonOneStratum, aeqd.CRS)
    
    # 2. Get the bearing angle from Bearing, if given and BearingAngle is not already given:
    if(!length(BearingAngle) || is.na(BearingAngle)) {
        BearingAngle <- getBearingOneStratum(
            StratumPolygonOneStratumXY = StratumPolygonOneStratumXY, 
            Bearing = Bearing, 
            N = N
        )
    }
    
    
    # Rotate the individual features of the StratumPolygonOneStratumXY by the BearingAngle:
    StratumPolygonOneStratumXY <- rotate2d(StratumPolygonOneStratumXY, -BearingAngle)
    
    
    # Get the actual coordinates, and work with these from now on:
    xy <- sf::st_coordinates(StratumPolygonOneStratumXY)
    # For convenience:
    xy <- data.table::as.data.table(xy)
    
    
    # Get corners of the bounding box of the polygon (a slight value added to the y to ensure intersection with the polygon):
    dx <- xy[, diff(range(X))]
    mx <- xy[, mean(range(X))]
    dy <- xy[, diff(range(Y))]
    my <- xy[, mean(range(Y))]
    corners <- list(
        xmin = mx - dx / 2, 
        xmax = mx + dx / 2, 
        ymin = my - dy / 2 - dy * 1e-9, 
        ymax = my + dy / 2 + dy * 1e-9
    )
    
    # Get the length of the stratum along the bearing:
    lengthOfStratum <- corners$xmax - corners$xmin
    
    # Subtract the length of the stratum, and return NULL if the traveled distance is shorter than this:
    if(SurveyDistance < lengthOfStratum){
        stop("SurveyDistance (", SurveyDistance, ") is shorter than the length of the stratum (", lengthOfStratum, ") for stratum ", stratumName, ".")
    }
    
    # The is the distance that is available for the design, apart from the actual travel distance if crossing the stratum along the x-axis:
    freeSurveyDistance <- SurveyDistance - lengthOfStratum
    message("StoX: TransectDesign for Stratum ", stratumName, "...")
    
    # Get the random seed point for the transects:
    startPositionFactor <- sampleStartPositionFactor(Seed)
    
    # If margin is given, iterate to obtain transects with total track length deviating at most by 'margin' relative to the input track length (margin = 0.05 implies between 19  and 21 hours, say):
    intersectsCoordsList <- getTransectsByArea(
        freeSurveyDistance = freeSurveyDistance, 
        stratumArea = stratumArea, 
        startPositionFactor = startPositionFactor, 
        corners = corners, 
        StratumPolygonOneStratumXY = StratumPolygonOneStratumXY, 
        TransectType = TransectType, 
        BearingAngle = BearingAngle, 
        Seed = Seed, 
        Retour = Retour
    )
    
    # If we are iterating to get the best fit to the expected SurveyDistance:
    numIter <- 1
    if(length(Margin) && is.numeric(Margin) && Margin > 0){
        
        
        
        # Set the totalSailedDist, margin to use, and the last value for 'rest' and 'freeSurveyDistance':
        #totalSailedDist <- 0
        totalSailedDist <- getTotalSailedDist(intersectsCoordsList, includeTransit = TRUE)
        
        
        Margin_SurveyDistance <- SurveyDistance * Margin
        last_rest <- Inf
        last_nmi_rest <- Inf
        lastIntersectsCoordsList <- NULL
        # Iterate to get a calculated tracklength within the margins
        while(abs(SurveyDistance - totalSailedDist) > Margin_SurveyDistance){
            
            message("StoX: numIter: ", numIter)
            
            
            
            if(numIter > 100) {
                message("Reaching 100 iterations when searching for survey distance deviating less than the margin from the user specified distance.")
                break
            }
            
            intersectsCoordsList <- getTransectsByArea(
                freeSurveyDistance = freeSurveyDistance, 
                stratumArea = stratumArea, 
                startPositionFactor = startPositionFactor, 
                corners = corners, 
                StratumPolygonOneStratumXY = StratumPolygonOneStratumXY, 
                TransectType = TransectType, 
                BearingAngle = BearingAngle, 
                Seed = Seed, 
                Retour = Retour
            )
            
            
            
            # Here we need the totalSailedDist:
            
            
            totalSailedDist <- getTotalSailedDist(intersectsCoordsList, includeTransit = TRUE)
            rest <- SurveyDistance - totalSailedDist
            
            
            # If increasing in rest value, break the loop and rerun with the previous settings:
            if(abs(last_rest) < abs(rest)){
                intersectsCoordsList <- lastIntersectsCoordsList
                warning(paste0("Sailed distance in stratum ", stratumName, " did not converge to the desired sailed distance (",  SurveyDistance, " nmi). The closest (", totalSailedDist, ") used."))
                break
            }
            # Set the values of the last run:
            #freeSurveyDistance <- freeSurveyDistance + if(rest > 0) rest else 2 * rest
            last_nmi_rest <- freeSurveyDistance
            last_rest <- rest
            lastIntersectsCoordsList <- intersectsCoordsList
            # Set new freeSurveyDistance to use:
            
            # This can lead to long survey tracks and in extreme cases longer than twice the freeSurveyDistance, in which case freeSurveyDistance will be negative in the next iteration (as we are adding the 'rest' which is then larger negative than freeSurveyDistance is positive). In those cases we simply try with half the freeSurveyDistance.
            if(rest < -freeSurveyDistance) {
                freeSurveyDistance <- freeSurveyDistance / 2
            }
            else {
                freeSurveyDistance <- freeSurveyDistance + rest
            }
            
            # Increment numIter:
            numIter <- numIter + 1
        }
        #cat("Number of iterations to achieve total sailed distance within ", Margin, " of the requested nmi in stratum ", stratumName, " (", SurveyDistance, "): ", numIter, "\n", sep="")
    }
    
    
    # Get x,y coordinates of the transects:
    # Create a collection of linestrings:
    lines = lapply(intersectsCoordsList, sf::st_linestring)
    lines <- do.call(sf::st_sfc, lines)
    
    
    # Rotate back by the BearingAngle:
    lines <- lines * getRotationMatrix(BearingAngle * pi / 180)
    #lines <- rotate2d(lines, BearingAngle)
    
    
    # Transform back to geographical coordinates:
    sf::st_crs(lines) <- aeqd.CRS
    lines <- sf::st_transform(lines, longlat.CRS)
    
    
    
    
    # Convert to data.table:
    lines <- lapply(lines, sf::st_coordinates)
    lines <- lapply(lines, data.table::as.data.table)
    lines <- lapply(lines, subset, select = c("X", "Y"))
    
    # Add the Retour:
    lines <- mapply(cbind, lines, Direction = lapply(intersectsCoordsList, from01ToTourRetour), SIMPLIFY = FALSE)
    
    # Add transect IDs as names of the list of lines:
    numberOfTransects <- length(lines)
    names(lines) <- getElementID("Transect", numberOfTransects)
    #paste0(
    #    "Transect", 
    #    sprintf(paste0("%0", nchar(numberOfTransects), "d"), seq_len(numberOfTransects)), 
    #    sep = "_"
    #)
    
    
    
    ## Rotate back:
    #xy <- rotate2d(xy, -parameters$bearing)
    #coords[,c(xcols, ycols)] <- c(xy)
    
    
    return(lines)
}
############################################################
############################################################


# Small function to convert from 0/1 to "Tour"/"Retour":
# x is a numeric matrix:
from01ToTourRetour <- function(x) {
    ifelse(x[, "DirectionNumeric"] == 0, "Tour", "Retour")
}


getTotalSailedDist <- function(list, includeTransit = TRUE) {
    
    # We require even number of rows (accepting 1 row):
    nrows <- sapply(list, nrow)
    
    if(!all(nrows == 1 | nrows %% 2 == 0)) {
        stop("All elements of the list must have even number of rows.")
    }
    
    # Add all transit segments to the end of the list of segments, by extracting the last node of one segment and the first of the next (in total n - 1 transits, where n is the number of segments (elements in 'list')):
    if(includeTransit) {
        list <- addTransitToList(list)
    }
    
    
    # Create a table of start and end values for convenience:
    listStartEnd <- lapply(list, nodes2segments)
    
    # Calculate the distances:
    dists <- lapply(listStartEnd, function(x) sqrt( (x$X_end - x$X_start)^2 + (x$Y_end - x$Y_start)^2 ))
    
    sumDists <- sum(unlist(dists))
    
    return(sumDists)
}






nodes2segments <- function(x) {
    xStartEnd <- cbind(
        matrix(x[, c("X")], ncol = 2, byrow = TRUE), 
        matrix(x[, c("Y")], ncol = 2, byrow = TRUE)
    )
    xStartEndDT <- data.table::as.data.table(xStartEnd)
    names(xStartEndDT) <- c("X_start", "X_end", "Y_start", "Y_end")
    return(xStartEndDT)
}




addTransitToList <- function(list) {
    transitList <- lapply(seq_len(length(list) - 1), function(ind) rbind(utils::tail(list[[ind]], 1), utils::head(list[[ind + 1]], 1)))
    list <- c(list, transitList)
    return(list)
}



addTransitToSegments <- function(
    segments, 
    startNames = c("LongitudeStart", "LatitudeStart"), 
    endNames = c("LongitudeEnd", "LatitudeEnd"), 
    ordered = TRUE, 
    addLast0Transit = FALSE
) {
    
    # Store the column order:
    colnames_segments <- names(segments)
    otherNames <- setdiff(colnames_segments, c(startNames, endNames))
    
    
    # Create transits:
    transits <- lapply(seq_len(nrow(segments) - 1), function(ind) cbind(segments[ind, ..endNames], segments[ind + 1, ..startNames]))
    # Add the a final dummy transit from the last end point to itself, useful if we want to extract only start positions (used in WriteTransectDesign()):
    if(addLast0Transit) {
        lastEndPoint <- segments[nrow(segments), ..endNames]
        transits[[length(transits) + 1]] <- structure(cbind(lastEndPoint, lastEndPoint), names = c(endNames, startNames))
    }
    
    transits <- data.table::rbindlist(transits)
    data.table::setnames(transits, c(startNames, endNames))
    # Add the other column, where if different between segments (the Segment column) the two values are pasted:
    for(name in otherNames) {
        transits <- addTransitOtherColumn(transits, segments, name, addLast0Transit = addLast0Transit)
    }
    data.table::setcolorder(transits, colnames_segments)
    
    # Add the transits to a table of positions:
    output <- list(segments, transits)
    
    names(output) <- c("Segment", "Transit")
    output <- data.table::rbindlist(output, idcol = "SegmentType")
    data.table::setcolorder(output, c(setdiff(names(output), "SegmentType"), "SegmentType"))
    
    # Reorder the rows:
    if(ordered) {
        newOrder <- rep(seq_len(nrow(segments)), each = 2) + rep(c(0, nrow(segments)), nrow(segments))
        if(!addLast0Transit) {
            newOrder <- head(newOrder, -1)
        }
        output <- output[newOrder, ]
        #output[seq(1, nrow(output), 2), ] <- segments
        #output[seq(2, nrow(output), 2), ] <- transits
    }
    
    
    return(output)
}

addTransitOtherColumn <- function(transits, segments, name, sep = " - ", addLast0Transit = FALSE) {
    if(addLast0Transit) {
        first <- segments[[name]]
    }
    else {
        first <- utils::head(segments[[name]], -1)
    }
    
    #last <- utils::tail(segments[[name]], -1)
    #if(any(first == last)) {
    #    transits[[name]] <- first
    #}
    #else {
    #    transits[[name]] <-paste(
    #        first, 
    #        last, 
    #        sep = sep
    #    )
    #}
    transits[[name]] <- first
    
    return(transits)
}


sampleStartPositionFactor <- function(seed) {
    set.seed(seed)
    startPositionFactor <- runif(1)
    set.seed(NULL)
    return(startPositionFactor)
}

















# Function that converts points represented as geographical positions (LongitudeStart, LatitudeStart, LongitudeEnd, LatitudeEnd) in a table to a collection of individual line strings.
points2LineStringCollectionOne <- function(x) {
    sf::st_sfc(lapply(seq_len(nrow(x)), function(ind) points2LineStringOne(x[ind, ])), crs = 4326)
}
points2LineStringOne <- function(x) {
    sf::st_linestring(
        rbind(
            c(x$LongitudeStart, x$LatitudeStart), 
            c(x$LongitudeEnd, x$LatitudeEnd)
        )
    )
}










getElementID <- function(prefix, n, sep = "_") {
    paste(
        prefix, 
        sprintf(paste0("%0", nchar(n), "d"), seq_len(n)), 
        sep = sep
    )
}
















#' Report a transect design with additional information
#'
#' @inheritParams ModelData
#' @inheritParams ProcessData
#' 
#' @examples
#' 
#' # Read the stratum system:
#' stratumFile <- system.file(
#'   "testresources", 
#'   "strata_sandeel_2020_firstCoverage.wkt", package = "RstoxBase"
#'  )
#' stratumPolygon<- DefineStratumPolygon(
#'   DefinitionMethod = "ResourceFile", 
#'   FileName = stratumFile
#' )
#' # Get the area of the strata for use in ReportTransectDesign():
#' stratumAreaData <- StratumArea(stratumPolygon)
#' 
#' # Harbitz zigzag survey design along each stratum:
#' transectDesignZZ_Along <- TransectDesign(
#' 	TransectType = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' # Report the TransectDesign:
#' ReportTransectDesign(transectDesignZZ_Along, stratumAreaData)
#' 
#' @return
#' An object of StoX data type \code{\link{ReportTransectDesignData}}.
#' 
#' @export
#' 
ReportTransectDesign <- function(
    TransectDesignData, 
    StratumAreaData
){
    if(!length(TransectDesignData)) {
        stop("The input TransectDesignData must be given")
    }
    if(!length(StratumAreaData)) {
        stop("The input StratumAreaData must be given")
    }
    
    # Create a list of the tables Transect and Stratum:
    # First the Transect:
    Transect <- addTransitToSegments(TransectDesignData)
    
    # Add distances
    Transect[, Distance := distanceStartToEnd(.SD)]
    # Add durations
    Transect[, Duration := Distance / Speed]
    
    # Remove extremely short transits, 1e-3 nautical miles, 1.8 m:
    margin <- 1e-3
    Transect <- subset(Transect, ! (SegmentType == "Transit" & Distance <= margin))
    
    Stratum <- Transect[, .(
        DistanceTransect = sum(Distance * as.numeric(SegmentType == "Transect")), 
        DistanceTransit = sum(Distance * as.numeric(SegmentType == "Transit")), 
        Distance = sum(Distance), 
        Speed = Speed[1]), 
        by = "Stratum"
    ]
    Stratum <- merge(Stratum, StratumAreaData, all.x = TRUE)
    Stratum[, Coverage := DistanceTransect / sqrt(Area)]
    Stratum[, Duration := Distance / Speed]
    
    # Create an output with three tables, one for the transects WITHOUT transit, one for the transects WITH transit, and one for the stratum, which is based on the transects WITH transit:
    output <- list(
        Transect = Transect, 
        Stratum = Stratum
    )
    
    # Should we use formatOutput here???
    
    return(output)
}





distanceStartToEnd <- function(segment) {
    
    # Define start and end positions as sf objects:
    start <- data.frame(
        lon = segment$LongitudeStart, 
        lat = segment$LatitudeStart
    ) 
    end <- data.frame(
        lon = segment$LongitudeEnd, 
        lat = segment$LatitudeEnd
    )
    
    # With geographical projection:
    start <- sf::st_as_sf(start, coords = c("lon", "lat"), crs = 4326)
    end <- sf::st_as_sf(end, coords = c("lon", "lat"), crs = 4326)
    
    # Get the great circle distances of the segments in nautical miles:
    distance <- sf::st_distance(start, end, by_element = TRUE)
    distance <- units::set_units(distance, "nmile")
    distance <- units::drop_units(distance)
    
    
    return(distance)
}






#' Write a transect design to GPX file(s).
#'
#' @inheritParams ModelData
#' @inheritParams general_file_plot_arguments
#' @param Format The format of the output files. Currently the only option is "GPX", which prodices files that can be read by the Olex software.
#' @param TrackGroupingVariables Character: A vector of one or more variables to group the tracks by in the file(s). Possible values are "Stratum", "Direction" and "Transect".
#' @param FileGroupingVariables Character: A vector of one or more variables to group the files by. Possible values are "Stratum" and "Direction". One file is written for each combination of the \code{FileGroupingVariables}. 
#' 
#' @examples
#' 
#' library(sf)
#' 
#' stratumFile <- system.file(
#'   "testresources", 
#'   "strata_sandeel_2020_firstCoverage.wkt", package = "RstoxBase"
#'  )
#' stratumPolygon<- DefineStratumPolygon(
#'   DefinitionMethod = "ResourceFile", 
#'   FileName = stratumFile
#' )
#' 
#' # Harbitz zigzag survey design along each stratum:
#' transectDesignZZ_Along <- TransectDesign(
#' 	TransectType = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' # Convert the transect design to an sf object and write this as a gpx file 
#' # (this is done automatically by RstoxFramework in StoX):
#' gpxData <- WriteTransectDesign(transectDesignZZ_Along)
#' filePath <- tempfile(fileext = ".gpx")
#' layers <- c("geometry", "track_name", "track_fid", "track_seg_id", "track_seg_point_id")
#' st_write(
#'      subset(gpxData, select = layers),
#'      dsn = filePath,
#'      layer = "track_points",
#'      driver = "GPX"
#'  )
#'  
#'  # Read the data back in:
#'  gpxData_backin <- st_read(filePath)
#'  identical(gpxData, gpxData_backin)
#' 
#' @return
#' An object of StoX data type \code{\link{WriteTransectDesignData}}.
#' 
#' @export
#' 
WriteTransectDesign <- function(
    TransectDesignData, 
    Format = c("GPX"), 
    TrackGroupingVariables = character(), 
    FileGroupingVariables = character()
){
    
    if(!length(TransectDesignData)) {
        stop("The TransectDesignData is empty.")
    }
    
    
    # The FileGroupingVariables must be a subset of the TrackGroupingVariables:
    if(length(FileGroupingVariables) && !all(FileGroupingVariables %in% TrackGroupingVariables)) {
        stop("The FileGroupingVariables must be identical or a subset of the TrackGroupingVariables")
    }
    
    Format <- RstoxData::match_arg_informative(Format)
    
    # Create a list of the tables Segment and Stratum:
    SegmentWithTransit <- addTransitToSegments(TransectDesignData, addLast0Transit = TRUE)
    
    if(Format == "GPX") {
        output <- sf::st_as_sf(
            subset(SegmentWithTransit, select = c("Stratum", "Direction", "LongitudeStart", "LatitudeStart")), 
            coords = c("LongitudeStart", "LatitudeStart")
        )
        
        # Add tracks, segments and waypoints:
        #output$track_fid <- SegmentWithTransit$Stratum
        #output$track_seg_id <- SegmentWithTransit$Transect
        #output$track_seg_point_id <- SegmentWithTransit$Segment
        
        output$track_name <- SegmentWithTransit$Stratum
        
        
        # The driver translates to integers starting from 0, so we do this here explicitely to avoid confusion:
        output$track_fid <- as.integer(factor(SegmentWithTransit$Stratum)) - 1L
        
        # The track id is controlled by the GroupingVariables:
        if(length(TrackGroupingVariables)) {
            #trackID <- SegmentWithTransit[, do.call(paste, lapply(as.list(TrackGroupingVariables), function(x)  eval(get(x))), sep = "_")]
            
            trackID <- do.call(paste, c(SegmentWithTransit[, ..TrackGroupingVariables], list(sep = "_")))
        }
        # If the TrackGroupingVariables is not given, create one single route for the entire stratum:
        else {
            trackID <- 1L
        }
        
        
        #output$track_seg_id <- as.integer(sub(".*_", "", SegmentWithTransit$Transect)) - 1L
        output$track_seg_id <- match(trackID, unique(trackID)) - 1L
        #output$track_seg_point_id <- as.integer(sub(".*_", "", SegmentWithTransit$Segment)) - 1L
        output$track_seg_point_id <- unlist(
            by(
                output, 
                paste(output$track_fid, output$track_seg_id), 
                FUN = function(x) seq_len(nrow(x))
            )) - 1L
        
        
        # Split into a list of simple features, which will be written as individual files by RstoxFramework:
        if(length(FileGroupingVariables)) {
            # Define the file IDs as a concatenation of the columns specified by FileGroupingVariables:
            fileID <-  do.call(paste, c(SegmentWithTransit[, ..FileGroupingVariables], list(sep = "_")))
            # Split the sf object:
            output <- split(output, f = fileID)
            # Add names which will be used as file names:
            names(output) <- unique(fileID)
        }
    }
    else if(Format == "Ruter") {
        
        
        warning("StoX: Exporting files in the Ruter format is experimental. The file extension may need to be changed to use these files in Olex.")
        
        symbol <- "Brunsirkel"
        
        # Convert the positions to arc-minutes
        SegmentWithTransit[, LongitudeStart := LongitudeStart * 60]
        SegmentWithTransit[, LatitudeStart := LatitudeStart * 60]
        
        
        
        
        # The track id is controled by the GroupingVariables:
        if(length(TrackGroupingVariables)) {
            trackID <- do.call(paste, c(SegmentWithTransit[, ..TrackGroupingVariables], list(sep = "_")))
        }
        # If the TrackGroupingVariables is not given, create one single route for the entire stratum:
        else {
            trackID <- "Rute uten navn"
        }
        
        # Split into a list of simple features, which will be written as individual files by RstoxFramework:
        if(length(FileGroupingVariables)) {
            # Define the file IDs as a concatenation of the columns specified by FileGroupingVariables:
            fileID <-  do.call(paste, c(SegmentWithTransit[, ..FileGroupingVariables], list(sep = "_")))
        }
        else {
            # Set the class, which will be used in RstoxFramework when writing the output file:
            fileID <- "Ruter"
        }
        
        # Create a vector of lines in the Ruter format:
        output <- SegmentWithTransit[, paste(LatitudeStart, LongitudeStart, 0, symbol)]
        
        # Add track names with a line space before each name:
        atNonDup <- which(!duplicated(trackID))
        output[atNonDup] <- paste("", unique(trackID), output[atNonDup], sep = "\n")
        
        # Split by fileID and name the list elements by the file names:
        output <- split(output, f = fileID)
        
        # Split by line space to get one vector element per line:
        output <- lapply(output, function(x) unlist(strsplit(x, "\n")))
        
        # Remove the black line at the start of each file:
        output <- lapply(output, function(x) if(nchar(x[1]) == 0) x[-1] else x)
        
        # Set the class, which will be used in RstoxFramework when writing the output file:
        lapply(output, `class<-`, value = "Ruter")
    }
    
    return(output)
}



#walk  Simple feature collection with 2095 features and 3 fields Geometry 
#type: POINT Dimension: XY Bounding box: xmin: 5.951268 ymin: 47.29238 
#xmax: 5.974512 ymax: 47.3015 Geodetic CRS: WGS 84 First 10 features: 
#    track_fid ele geometry track_seg_id 1 1 243 POINT (5.95348 47.29709) 1 2 
#1 243 POINT (5.953438 47.29704) 1 3 1 244 POINT (5.953219 47.29702) 1 4 
#1 243 POINT (5.952884 47.29713) 1 5 1 243 POINT (5.952492 47.2972) 1 6 1 
#243 POINT (5.952335 47.29743) 1 7 1 242 POINT (5.951881 47.29732) 1 8 1 
#242 POINT (5.951495 47.29724) 1 9 1 293 POINT (5.951333 47.29719) 1 10 1 
#294 POINT (5.951388 47.29706) 1
#
#
#Then,
#
#st_write(walk,dsn="walk.gpx",layer="track_points",driver="GPX")


############################
#' Plot a transect design.
#'
#' @inheritParams ModelData
#' @inheritParams PlotAcousticTrawlSurvey
#' @inheritParams general_plot_arguments
#' @inheritParams general_file_plot_arguments
#' @inheritParams general_map_plot_arguments
#' @inheritParams general_track_plot_arguments
#' @inheritParams general_station_plot_arguments
#' @inheritParams general_stratum_plot_arguments
#' @inheritParams general_map_aspect_plot_arguments
#' 
#' @examples
#' 
#' stratumFile <- system.file(
#'   "testresources", 
#'   "strata_sandeel_2020_firstCoverage.wkt", package = "RstoxBase"
#'  )
#' stratumPolygon<- DefineStratumPolygon(
#'   DefinitionMethod = "ResourceFile", 
#'   FileName = stratumFile
#' )
#' 
#' # Harbitz zigzag survey design along each stratum:
#' transectDesignZZ_Along <- TransectDesign(
#' 	TransectType = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' PlotTransectDesign(
#'   transectDesignZZ_Along, 
#'   ShowStratumPolygon = TRUE, 
#'   StratumPolygon = stratumPolygon
#' )
#' 
#' @return
#' An object of StoX data type \code{\link{WriteTransectDesignData}}.
#' 
#' @export
#' 
PlotTransectDesign <- function(
    TransectDesignData,
    
    # Options for the track line and points:
    UseDefaultTrackSettings = TRUE, 
    TrackColor = character(), 
    TrackLineWidth = numeric(),  
    TrackPointColor = character(), 
    TrackPointSize = numeric(), 
    TrackPointShape = numeric(), 
    
    
    # Options for the stratum polygons:
    ShowStratumPolygon = FALSE, 
    StratumPolygon, 
    UseDefaultStratumPolygonSettings = TRUE, 
    StratumPolygonColor = character(), 
    StratumPolygonBorderColor = character(), 
    StratumPolygonBorderLineWidth = numeric(), 
    
    # Options for the map:
    ShowMap = TRUE, 
    UseDefaultMapSettings = TRUE, 
    LandColor = character(), 
    BorderColor = character(), 
    OceanColor = character(), 
    GridColor = character(),
    
    # Options for stations:
    ShowStations = FALSE, 
    UseDefaultStationSettings = TRUE, 
    StationsAlongTransectDesignData, 
    StationPointColor = character(), 
    StationPointSize = numeric(), 
    StationPointShape = numeric(), 
    
    # Options for the zoom and limits:
    UseDefaultAspectSettings = TRUE, 
    Zoom = numeric(), 
    LongitudeMin = numeric(), 
    LongitudeMax = numeric(), 
    LatitudeMin = numeric(), 
    LatitudeMax = numeric(), 
    LongitudeCenter = numeric(), 
    LatitudeCenter = numeric(), 
    
    # Options for the labels and other text:
    UseDefaultLabelSettings = TRUE, 
    Title = character(), 
    AxisTitleSize = numeric(), 
    AxisTickSize = numeric(), 
    LegendTitleSize = numeric(), 
    LegendTextSize = numeric(), 
    
    # Options for the output file:
    #Format = c("png", "tiff", "jpeg", "pdf"), 
    UseDefaultFileSettings = TRUE, 
    Format = character(), 
    Width = numeric(), 
    Height = numeric(), 
    DotsPerInch = numeric()
){
    
    # Get the formals:
    plotArguments <- allargs()
    
    # If the StratumPolygon is given, set ShowStratumPolygon to TRUE:
    if(!missing(StratumPolygon)) {
        ShowStratumPolygon <- TRUE
    }
    else {
        if(ShowStratumPolygon) {
            stop("StoX: StratumPolygon must be given.")
        }
    }
    
    # Set the default of the StoX function. These are defaults defined in the stoxFunctionAttributes.
    plotArguments <- setDefaultsInStoxFunction(plotArguments, StoxFunctionName = "PlotTransectDesign", stoxFunctionAttributes = stoxFunctionAttributes)

    
    # Split axis titles into x and y:
    plotArguments <- splitAxisTitleSize(plotArguments)
    
    # Set hard coded values used in plot_lon_lat():
    plotArguments <- c(
        plotArguments, 
        list(
            # Add Stratum as a grouping variable:
            trackLinesBy = "Stratum", 
            # Solid line for tour and dashed for retour:
            linetype.track = "Direction", 
            lon_name = "LongitudeStart", 
            lat_name = "LatitudeStart", 
            lon_name_end = "LongitudeEnd", 
            lat_name_end = "LatitudeEnd", 
            lon_name_station = "Longitude", 
            lat_name_station = "Latitude", 
            trackType = "sp"
        )
    )
    
    # Special care to add the positions, as we need Direction to be factor when plotting:
    TransectDesignData$Direction <- factor(TransectDesignData$Direction, levels = c("Tour", "Retour"))
    plotArguments$trackData <- TransectDesignData
    if(!missing(TransectDesignData)) {
        plotArguments$trackData <- TransectDesignData
    }
    if(!missing(StationsAlongTransectDesignData)) {
        plotArguments$stationData <- StationsAlongTransectDesignData
    }
    # Remove the inputs from the plotArguments in order to avoid corrumpting the ...:
    plotArguments$TransectDesignData <- NULL
    plotArguments$StationsAlongTransectDesignData <- NULL
    
    
    # Define the translation from the inputs to this function to the inputs to plot_lon_lat:
    translation <- c(
        #trackData = "TransectDesignData", 
        #stationData = "StationsAlongTransectDesignData", 
        
        polygon = "StratumPolygon",
        showMap = "ShowMap",
        
        linewidth.track = "TrackLineWidth", 
        linewidth.polygon.border = "StratumPolygonBorderLineWidth", 
        
        size = "TrackPointSize", 
        size.station.point = "StationPointSize", 
        
        color.track = "TrackColor", 
        color.station.point = "StationPointColor", 
        color.scale = "TrackPointColor",  
        color.land = "LandColor", 
        color.border = "BorderColor", 
        color.ocean = "OceanColor", 
        color.grid = "GridColor", 
        color.polygon = "StratumPolygonColor", 
        color.polygon.border = "StratumPolygonBorderColor", 
        
        shape = "TrackPointShape",
        shape.station.point = "StationPointShape", 
        # Do not set any alphas (transparency): alpha.point, alpha.track, alpha.polygon
        
        zoom = "Zoom", 
        # xlim, ylim and zoomCenter are set by set_xlim_ylim_zoomCenter()
        
        legend.text.size = "LegendTextSize", 
        legend.title.size = "LegendTitleSize", 
        main = "Title"
    )
    
    # Add xlim and ylim and center for zooming:
    plotArguments <- set_xlim_ylim_zoomCenter(plotArguments)
    
    
    # Set empty StratumPolygon if it should not be shown:
    if(!ShowStratumPolygon) {
        plotArguments$StratumPolygon <- NULL
    }
    
    # Rename to the argument names of the plot_lon_lat():
    plotArguments <- renameListByNames(
        plotArguments, 
        old = translation,  
        new = names(translation)
    )
    
    # Run the plot:
    output <- do.call(plot_lon_lat, plotArguments)
    
    
    
    output <- setPlotAttributes(
        plotObject = output, 
        plotArguments = plotArguments
    )
    
    return(output)
}

















#' Generate stations along a transect design
#'
#' @inheritParams general_arguments
#' @inheritParams ModelData
#' @param DefinitionMethod  Character: A string naming the method to use, one of "Parameter", for specifying common parameters for all strata given by \code{StratumNames}, or "ParameterTable", for specifying individual parameters for each stratum in the table \code{ParameterTable}.
#' @param StratumNames  Character: The names of the strata for which the parameters are valid. Defaults to all strata.
#' @param Distance Numeric: The distance in nautical miles between the stations along the transect design.
#' @param Seed Numeric: The seed to use when drawing the random starting point. 
#' @param ParameterTable A table specifying the parameters \code{Distance} and \code{Seed} for each stratum.
#' @param AddTransectEndPoints Logical: If TRUE add the start and end points of the transects given in \code{TransectDesignData}.
#' 
#' @details 
#' The \code{Seed} is used to generate one seed per Stratum which are then used to generate the random starting point of each Stratum. From the starting point the function \code{\link[sf]{st_line_sample}} is used to sample points along the transect design in cartesian coordinates in each Stratum separated by \code{Distance} nautical miles (ignoring transit between transects).
#' 
#' @return
#' An object of StoX data type \code{\link{StationsAlongTransectDesignData}}.
#'
#' @examples
#' library(ggplot2)
#' 
#' stratumFile <- system.file(
#'   "testresources", 
#'   "strata_sandeel_2020_firstCoverage.wkt", package = "RstoxBase"
#'  )
#' stratumPolygon <- DefineStratumPolygon(
#'   DefinitionMethod = "ResourceFile", 
#'   FileName = stratumFile
#' )
#' 
#' # Harbitz zigzag survey design along each stratum:
#' transectDesignZZ_Along <- TransectDesign(
#'  TransectParameterDefinition = "FunctionParameter", 
#'  TransectParameterDefinitionMethod = "Parameter", 
#' 	TransectType = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' stations <- StationsAlongTransectDesign(
#'   transectDesignZZ_Along, 
#'   Distance = 30, 
#'   Seed = 1
#' )
#' 
#' PlotTransectDesign(
#'   transectDesignZZ_Along, 
#'   ShowStratumPolygon = TRUE, 
#'   StratumPolygon = stratumPolygon, 
#'   StationsAlongTransectDesignData = stations, 
#'   StationPointColor = "red"
#' )
#'
#' @export
#' 
StationsAlongTransectDesign <- function(
    TransectDesignData, 
    DefinitionMethod = c("Parameter", "ParameterTable"), 
    StratumNames = character(), 
    Distance = numeric(), 
    Seed = numeric(), 
    ParameterTable = data.table::data.table(), 
    AddTransectEndPoints = FALSE
){
    # Get the 
    DefinitionMethod <- RstoxData::match_arg_informative(DefinitionMethod)
    
    
    # Get the unique stratum names:
    allStratumNames <- unique(TransectDesignData$Stratum)
    
    # Get the Seed and Distane form the ParameterTable if present:
    if(NROW(ParameterTable)) {
        Distance <- ParameterTable$Distance
        Seed <- ParameterTable$Seed
        StratumNames <- allStratumNames
    }
    else {
        if(!length(StratumNames)) {
            StratumNames <- allStratumNames
        }
        
        if(!all(StratumNames %in% StratumNames)) {
            stop("The following strata are specified in StratumNames but are not present in the TransectDesignData. Please only supply valid stratum names.")
        } else if(!any(StratumNames %in% StratumNames)) {
            stop("The StratumNames does not contain any of the strata in the TransectDesignData.")
        }
            
        Seed <- getSeedVector(size = length(StratumNames), seed = Seed)
    }
    
    # Generate the stations per Stratum:
    output <- mapply(
        getStationsAlongTransectDesignOneStratum, 
        stratumName = StratumNames, 
        Seed = Seed, 
        Distance = Distance, 
        MoreArgs = list(
            TransectDesignData = TransectDesignData, 
            AddTransectEndPoints = AddTransectEndPoints
        ), 
        SIMPLIFY = FALSE
    )
    
    # Rbind into one table:
    output <- data.table::rbindlist(output, idcol = "Stratum")
    
    formatOutput(output, dataType = "StationsAlongTransectDesign", keep.all = FALSE, orderRows = FALSE)
    
    return(output)
}




getStationsAlongTransectDesignOneStratum <- function(stratumName, TransectDesignData, Seed, Distance, AddTransectEndPoints) {
    
    # Select only the specified stratum:
    transectDesignDataOneStratum <- subset(TransectDesignData, Stratum == stratumName)
    
    
    # Create a list of collections of line strings, one line string for each transect:
    #transectNames <- unique(transectDesignDataOneStratum$Transect)
    linesStrings <- points2LineStringCollectionOne(transectDesignDataOneStratum)
    
    # Add distances
    transectDesignDataOneStratum[, Distance := distanceStartToEnd(.SD)]
    transectDesignDataOneStratum[, CumulativeDistance := cumsum(Distance)]
    
    # Set the seed and draw the starting point:
    set.seed(Seed)
    start <- Distance * runif(1)
    
    # Get the total distance and calculate cumulative distances at which to place stations:
    totalDistance <- utils::tail(transectDesignDataOneStratum$CumulativeDistance, 1)
    
    if(totalDistance < start) {
        stop("The Distance is too large for the stratum.")
    }
    cumulativeSamplingDistance <- start + seq(0, (totalDistance - start), by = Distance)
    
    # Find the transect of each station:
    atTransect <- findInterval(cumulativeSamplingDistance, c(0, transectDesignDataOneStratum$CumulativeDistance))
    # And convert the cumulativeSamplingDistance to normalized distances relative to the starting point of each transect:
    relativeCumulativeSamplingDistance <- cumulativeSamplingDistance - c(0, transectDesignDataOneStratum$CumulativeDistance)[atTransect]
    relativeCumulativeSamplingDistanceNormalized <- relativeCumulativeSamplingDistance / transectDesignDataOneStratum$Distance[atTransect]
    
    # Split into a list with one element per transec, named by the transect indices:
    #relativeCumulativeSamplingDistanceNormalizedList <- split(relativeCumulativeSamplingDistanceNormalized, atTransect)
    
    #### Get the stations: ####
    # Get first the centroids to use when transforming to cartesian coordinates. We found that the st_line_sample does not follow the great circle as do st_segmentize, but the latter cannot be controlled in terms of distance between samples. So we transform using the Azimuthal equidistant projection:
    #centroid <- getIndividualCentroids(linesStrings, iterativeCentroidCalculation = TRUE)
    
    # Get a common centroid of all transects (getting individual centroids for each transect is too slow):
    centroid <- getCentroid(linesStrings, iterativeCentroidCalculation = TRUE)
    
    
    output <- sampleLinesInXY(
        linesStrings, 
        centroid = centroid, 
        index = atTransect, 
        sample = relativeCumulativeSamplingDistanceNormalized, 
        iterativeCentroidCalculation = TRUE
    )
    #output <- mapply(sampleLineOne, linesStrings[atTransect], SIMPLIFY = FALSE)
    
    # Get the coordinates in a data table:
    output <- lapply(output, sf::st_coordinates)
    output <- lapply(output, data.table::as.data.table)
    
    # Rbind to one table:
    output <- data.table::rbindlist(output)
    
    output <- data.table::data.table(
        #Transect = transectNames[atTransect], 
        Station = getElementID("Station", nrow(output)), 
        Longitude = output$X, 
        Latitude = output$Y
    )
    
    uniqueTransectIDs <- unique(atTransect)
    numberOfTransects <- length(uniqueTransectIDs)
    
    # Add the transect start and end points for each transect:
    if(AddTransectEndPoints) {
        
        # Extract the start and end points of the transects:
        transectStart <- transectDesignDataOneStratum[, .(Longitude = utils::head(LongitudeStart, 1), Latitude = utils::head(LatitudeStart, 1)), by = Transect ]
        transectEnd <- transectDesignDataOneStratum[, .(Longitude = utils::tail(LongitudeEnd, 1), Latitude = utils::tail(LatitudeEnd, 1)), by = Transect ]
        
        # Rename the Transect column to Station (since we are about to represent the transect start and end points as stations in the output), and add "_start" and "_end":
        transectStart[, Station := paste(Transect, "Start", sep = "_")]
        transectEnd[, Station := paste(Transect, "End", sep = "_")]
        transectStart[, Transect := NULL]
        transectEnd[, Transect := NULL]
        
        # Create a list of stations, one element per transect, possibly empty:
        stations <- vector("list", numberOfTransects)
        stations[uniqueTransectIDs] <- split(output, atTransect)
        
        # Rbind the start and end of the transects:
        output <- mapply(
            rbind, 
            split(transectStart, seq_len(nrow(transectStart))), 
            stations, 
            split(transectEnd, seq_len(nrow(transectEnd))), 
            SIMPLIFY = FALSE
        )
        
        # Rbind to one table again:
        output <- data.table::rbindlist(output)
    }
    
    
    return(output)
}

sampleLinesInXY <- function(x, centroid, index, sample, iterativeCentroidCalculation = TRUE) {
    
    # Get proj4 strings:
    proj4_aeqd <- addCentroidToProjection(
        getRstoxBaseDefinitions("proj4string_aeqd"), 
        centroid = centroid, 
        additionalFields = " +x_0=0 +y_0=0 +units=kmi"
    )
    proj4_longlat <- getRstoxBaseDefinitions("proj4string_longlat")
    
    output <- vector("list", length(index))
    
    for(ind in seq_along(output)) {
        # Sample along the line in cartesian coordinates:
        output[[ind]] <- 
            sf::st_line_sample(
                sf::st_transform(x[index[ind]], proj4_aeqd), 
                sample = sample[ind]
            )
        # Transform back to geographic coordinates:
        output[[ind]] <- sf::st_transform(output[[ind]], proj4_longlat)
    }
    
    return(output)
}


#getIndividualCentroids <- function(x, iterativeCentroidCalculation = TRUE) {
#    output <- data.frame(
#        lon = rep(NA_real_, length(x)),
#        lat = rep(NA_real_, length(x))
#    )
#    
#    # We use a for loop and not an lapply here to keep the sfc object:
#    for(ind in seq_along(x)) {
#        output[ind, ] <- getCentroid(x[ind], iterativeCentroidCalculation = iterativeCentroidCalculation, msg = TRUE)
#    }
#    
#    return(output)
#}




#' Write a transect design to GPX file(s).
#'
#' @inheritParams ModelData
#' @inheritParams general_file_plot_arguments
#' @param Format The format of the output files. Currently the only option is "GPX", which prodices files that can be read by the Olex software.
#' 
#' @examples
#' 
#' library(ggplot2)
#' 
#' stratumFile <- system.file(
#'   "testresources", 
#'   "strata_sandeel_2020_firstCoverage.wkt", package = "RstoxBase"
#'  )
#' stratumPolygon <- DefineStratumPolygon(
#'   DefinitionMethod = "ResourceFile", 
#'   FileName = stratumFile
#' )
#' 
#' # Harbitz zigzag survey design along each stratum:
#' transectDesignZZ_Along <- TransectDesign(
#'  TransectParameterDefinition = "FunctionParameter", 
#'  TransectParameterDefinitionMethod = "Parameter", 
#' 	TransectType = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' stations <- StationsAlongTransectDesign(
#'   transectDesignZZ_Along, 
#'   Distance = 30, 
#'   Seed = 1
#' )
#' 
#' # Convert the transect design to an sf object and write this as a gpx file 
#' # (this is done automatically by RstoxFramework in StoX):
#' gpxData <- WriteStationsAlongTransectDesign(stations)
#' 
#' @return
#' An object of StoX data type \code{\link{WriteStationsAlongTransectDesignData}}.
#' 
#' @export
#' 
WriteStationsAlongTransectDesign <- function(
    StationsAlongTransectDesignData, 
    Format = c("GPX")
){
    
    if(!length(StationsAlongTransectDesignData)) {
        stop("The StationsAlongTransectDesignData is empty.")
    }
    
    Format <- RstoxData::match_arg_informative(Format)

    
    if(Format == "GPX") {
        output <- sf::st_as_sf(
            subset(StationsAlongTransectDesignData, select = c("Stratum", "Longitude", "Latitude")), 
            coords = c("Longitude", "Latitude")
        )
        # Add names of the points, as sequential numbers:
        #output$name <- getElementID("Station", nrow(output))
        output$name <- StationsAlongTransectDesignData$Station
        
    }
    else if(Format == "Ruter") {
        
        
        stop("Exporting files in the Ruter format is not implemented yet.")
        
        
    }
    
    return(output)
}






