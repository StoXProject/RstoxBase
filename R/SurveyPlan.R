#' Plan an acoustic-trawl survey.
#'
#' @inheritParams general_arguments
#' @inheritParams general_map_plot_arguments
#' @inheritParams general_track_plot_arguments
#' @inheritParams ProcessData
#' @param DefinitionMethod  Character: A string naming the method to use, either "ResourceFile" for reading the survey plan from a file, or a string naming the type of survey design to create (see details).
#' @param FileName The path to a resource file from which to read the SurveyPlan process data, in the case that \code{DefinitionMethod} is "ResourceFile".
#' @param StratumNames  Character: The names of the strata to include in the survey plan. Defaults to all strata.
#' @param OrderAllToursFirst  Logical: If TRUE order all tours first and all retours last, which can be useful for multiple Strata in the same survey direction (e.g. a row of strata along a coast line).
#' @param EqualEffort  Character: A string naming the method to use. See Details for options.
#' @param Bearing  Character: A string indicating the survey bearing (direction) of each . See Details for options.
#' @param BearingAngle  Numeric: In the case that \code{Bearing = "Angle"}, \code{BearingAngle} gives the angle of the survey bearing (direction) counter clockwise from north in degrees.
#' @param Retour  Logical: If TRUE the survey plan will be doubled by a retour.
#' @param SurveyTime,SurveyDistance  The time/traveled distance to spend in each stratum including transport between segments, given in hours/nautical miles, where \code{SurveyDistance} has precedence over \code{SurveyTime}. The vector is repeated to have length equal to the number of strata specified in \code{strata}, so that only one value is given, this is the hours/nmi in all strata. Optionally, if a single value is enclosed in a list, it is regarded as the total hours/nmi for the entire survey. In this case selecting only a subset of the strata using \code{strata} will increase the effort in the selected strata.
#' @param SurveySpeed  Numeric: The time to be used for each stratum. Note that the resulting accumulated time may not be exactly equal to \code{SurveyTime}.
#' @param Seed Numeric: The seed to use when drawing the random starting point. 
#' @param Margin Numeric: The margin to use when iterating to fit the survey plan to the desired survey distance (including transport between segments). The function iterates until the survey distance is within \code{Margin} of the desired survey distance, and jumps out of the iteration after 100 tries or if not converging.
#' 
#' @details 
#' The \code{DefineSurveyPlan} function generates the survey plan (transect lines) in a Cartesian coordinate system, and transforms the positions to the geographical coordinate system (longitude, latitude) using the azimuthal equal distance projection, which ensures that distances are preserved in the transformation. 
#' 
#' The following types are implemented throught \code{DefinitionMethod}:
#' \describe{
#'	\item{"Parallel"}{"Parallel transects"}
#'	\item{"ZigZagEqualSpacing"}{"Equal space zigzag sampler, Strindberg and Buckland (2004). End transects are generated different from Strindberg and Buckland (2004), by mirroring the last transect around the line perpendicular to the survey direction passing through the last intersection point between the stratum border and the parallel lines used to generate the transects."}
#'	\item{"ZigZagRectangularEnclosure"}{"Rectangular enclosure zigzag sampler, Harbitz (2019)"}
#' }
#' 
#' @references 
#' Strindberg, S., & Buckland, S. T. (2004). Zigzag survey designs in line transect sampling. Journal of Agricultural, Biological, and Environmental Statistics, 9, 443-461.
#' 
#' Harbitz, A. (2019). A zigzag survey design for continuous transect sampling with guaranteed equal coverage probability. Fisheries Research, 213, 151-159.
#' 
#' @return
#' An object of StoX data type \code{\link{SurveyPlan}}.
#'
#' @examples
#' library(ggplot2)
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
#' surveyPlanZZ_Along <- DefineSurveyPlan(
#' 	DefinitionMethod = "ZigZagRectangularEnclosure", 
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
#'     data = surveyPlanZZ_Along, 
#'     aes(x = LongitudeStart, y = LatitudeStart, xend = LongitudeEnd, yend = LatitudeEnd)
#'   )
#' 
#' # Harbitz zigzag survey design across each stratum:
#' surveyPlanZZ_Across<- DefineSurveyPlan(
#' 	DefinitionMethod = "ZigZagRectangularEnclosure", 
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
#'     data = surveyPlanZZ_Across, 
#'     aes(x = LongitudeStart, y = LatitudeStart, xend = LongitudeEnd, yend = LatitudeEnd)
#'   )
#' 
#' # Parallel survey design across each stratum:
#' surveyPlanParallel_Across<- DefineSurveyPlan(
#' 	DefinitionMethod = "Parallel", 
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
#'     data = surveyPlanParallel_Across, 
#'     aes(x = LongitudeStart, y = LatitudeStart, xend = LongitudeEnd, yend = LatitudeEnd)
#'   )
#'
#' @export
#' 
DefineSurveyPlan <- function(
    processData, UseProcessData = FALSE, 
    StratumPolygon, 
    
    DefinitionMethod = c("Parallel", "ZigZagRectangularEnclosure", "ZigZagEqualSpacing", "ResourceFile"), 
    FileName = character(), 
    
    StratumNames = character(), 
    
    Bearing = c("Along", "Across", "AlongReversed", "AcrossReversed"), 
    BearingAngle = numeric(), 
    Retour = FALSE, 
    SurveyTime = numeric(), 
    SurveyDistance = numeric(),
    SurveySpeed = numeric(), 
    EqualEffort = TRUE, 
    OrderAllToursFirst = FALSE, 
    # , "AlongAll", "AcrossAll", "AlongAllReversed", "AcrossAllReversed"
    Seed = numeric(), 
    Margin = 0.1
){
    # Hard code the nunber of points to use when finding the bearing:
    NumberOfPoints = 1e4
    
    # Return immediately if UseProcessData = TRUE:
    if(UseProcessData) {
        return(processData)
    }
    
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    if(DefinitionMethod == "ResourceFile") {
        if(!length(FileName) || !file.exists(FileName)) {
            stop("FileName must be the path to an existing file.")
        }
        
        SurveyPlan <- data.table::fread(FileName)
        
        data.table::setnames(SurveyPlan, names(SurveyPlan), tools::toTitleCase(names(SurveyPlan)))
        
        if(!all(c("Stratum", "LongitudeStart", "LatitudeStart", "LongitudeEnd", "LatitudeEnd") %in% names(SurveyPlan))) {
            stop("The file ", FileName, " does not have column names \"Stratum\", \"LongitudeStart\", \"LatitudeStart\", \"LongitudeEnd\" and \"LatitudeEnd\" (present columns are ", paste(names(SurveyPlan), collapse = ", "), ").")
        }
        
        return(SurveyPlan)
    }
    
    
    # Subset the strata:
    if(length(StratumNames)) {
        validStratumNames <- getStratumNames(StratumPolygon)
        if(!any(StratumNames %in% validStratumNames)) {
            stop("StratumNames can only contain valid stratum names. Possible names are: ", paste(validStratumNames, collapse = ", "))
        }
        
        StratumPolygon <- subsetStratumPolygon(StratumPolygon, StratumNames) 
    }
    
    # Get the number of strata and the names:
    numberOfStrata <- nrow(StratumPolygon)
    stratumNames <- getStratumNames(StratumPolygon)
    # Get the stratum areas:
    stratumArea <- StratumArea(StratumPolygon)
    
    # Currently StoX requires seed to be set. This may change in a future relese, where empty seed could be accepted:
    if(!length(Seed)) {
        stop("Seed must be given!")
    }
    
    Bearing <- match.arg(Bearing)
    
    # Get the number of strata and repeat all parameters if of length 1:
    if(numberOfStrata > 1) {
        if(length(DefinitionMethod) == 1) DefinitionMethod <- rep(DefinitionMethod, numberOfStrata)
        if(length(Bearing) == 1) Bearing <- rep(Bearing, numberOfStrata)
        if(length(BearingAngle) == 1) BearingAngle <- rep(BearingAngle, numberOfStrata)
        if(length(Retour) == 1) Retour <- rep(Retour, numberOfStrata)
        if(length(SurveyTime) == 1) SurveyTime <- rep(SurveyTime / numberOfStrata, numberOfStrata)
        if(length(SurveyDistance) == 1) SurveyDistance <- rep(SurveyDistance / numberOfStrata, numberOfStrata)
        if(length(SurveySpeed) == 1) SurveySpeed <- rep(SurveySpeed, numberOfStrata)
        if(length(Seed) == 1) Seed <- getSeedVector(Seed, numberOfStrata)
        if(length(Margin) == 1) Margin <- rep(Margin, numberOfStrata)
    }
    
    # Repeat the angle if given:
    if(length(Bearing) && length(BearingAngle) && all(!is.na(BearingAngle))) {
        warning("StoX: When both Bearing and BearingAngle are given, the Bearing is ignored.")
    }
    
    
    # Get the survey distance from time:
    if(!length(SurveyDistance)) {
        if(length(SurveyTime) && length(SurveySpeed)) {
            SurveyDistance <- SurveyTime * SurveySpeed
        }
        else {
            stop("If SurveyDistance is not given, both of SurveyTime and SurveySpeed must be given!")
        }
    }
    if(EqualEffort){
        thisStratumArea <- stratumArea[stratumNames, Area, on = "Stratum"]
        SurveyDistance <- sum(SurveyDistance) * thisStratumArea / sum(thisStratumArea)
    }
    
    
    
    # 1. StratumPolygon -> StratumPolygonXY (project to cartesian coordinates which are needed to plan the transects)
    #   - Get centroid and use that to get projection string
    #   - Project
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
    
    
    
    
    
    
    
    SurveyPlanList <- vector("list", numberOfStrata)
    for(ind in seq_len(numberOfStrata)) {
        # Define a temporary table of x and y:
        SurveyPlanList[[ind]] <- transectsOneStratum(
            StratumPolygonOneStratum = StratumPolygon[ind, ], 
            stratumName = stratumNames[ind], 
            stratumArea = stratumArea[ind], 
            aeqd.CRS = aeqd.CRS, 
            longlat.CRS = longlat.CRS, 
            DefinitionMethod = DefinitionMethod[ind], 
            Bearing = Bearing[ind], 
            BearingAngle = BearingAngle[ind], 
            Retour = Retour[ind], 
            SurveyDistance = SurveyDistance[ind], 
            Seed = Seed[ind], 
            Margin = Margin[ind], 
            N = NumberOfPoints
        )
    }
    names(SurveyPlanList) <- stratumNames
    
    
    # Convert to start and end position (latitude and longitude):
    SurveyPlan <- lapply(SurveyPlanList, function(x) lapply(x, XY2startEnd))
    
    
    
    # Rbind the segments per stratum:
    SurveyPlan <- lapply(SurveyPlan, data.table::rbindlist, idcol = "Transect")
    # Add input survey speed:
    SurveyPlan <- mapply(cbind, SurveyPlan, lapply(SurveySpeed, function(x) data.table::setnames(data.table::data.table(x), "Speed")), SIMPLIFY = FALSE)
    # Rbind strata:
    SurveyPlan <- data.table::rbindlist(SurveyPlan, idcol = "Stratum")
    # Insert a sequence of segment indices:
    #SurveyPlan[, Segment := paste("S", factor(paste(Stratum, sprintf("%06d", Segment))))]
    
    SurveyPlan <- formatOutput(SurveyPlan, dataType = "SurveyPlan", keep.all = FALSE)
    
    return(SurveyPlan[])
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
    out$Segment <- paste0("Segment_", sprintf(paste0("%0", nchar(numberOfSegments), "d"), seq_len(numberOfSegments)))
    
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
    Bearing <- match.arg(Bearing)
    
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
    # Get the cummulative time:
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
        DefinitionMethod = "Parallel", 
        Retour = FALSE
){
    
    # If we are on a retour, reverse the order of the grid points:
    # The downRandom is TRUE for even and FALSE for odd values of Seed but only for parallel:
    if(tolower(DefinitionMethod) != "parallel" ){
        downRandom <- FALSE
    }
    else {
        downRandom <- Seed %% 2 == 0
    }
    if(Retour){
        downRandom <- !downRandom 
    }
    
    # For parallel transects we need to shift by the halfTransectSpacing on the retour:
    if(tolower(DefinitionMethod) == "parallel" && Retour){
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
    if(tolower(DefinitionMethod) == tolower("ZigZagRectangularEnclosure")){
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
    if(tolower(DefinitionMethod) == tolower("ZigZagEqualSpacing")){
        
        if(length(intersectsCoordsList) == 1) {
            stop("Isufficient effort to produce one zig zag transect in the stratum ", StratumPolygonOneStratumXY$StratumName, ". Please increase effort or choose a different DefinitionMethod (e.g. \"Parallel\").")
        }
        
        ### # The ZigZagEqualSpacing method requires at least 2 grid lines to intersect with the polygon:
        ### if(length(intersectsCoordsList) < 2) {
        ###     stop("The DefinitionMethod \"ZigZagEqualSpacing\" requires at least two grid lines intersecting with the polygon. Increase the effort or choose an different DefinitionMethod.")
        ### }
        
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
        if(! DefinitionMethod %in% c("ZigZagEqualSpacing", "ZigZagRectangularEnclosure")){
            intersectsCoordsList <- lapply(intersectsCoordsList, function(x) x[seq(nrow(x), 1), , drop = FALSE])
        }
    }
    
    # Add a column denoting tour or retour:
    intersectsCoordsList <- lapply(intersectsCoordsList, cbind, Retour = Retour)
    
    
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
    DefinitionMethod = "Parallel", 
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
        DefinitionMethod = DefinitionMethod, 
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
                DefinitionMethod = DefinitionMethod, 
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
        DefinitionMethod = c("Parallel", "ZigZagRectangularEnclosure", "ZigZagEqualSpacing"), 
        Bearing = c("Along", "Across", "AlongReversed", "AcrossReversed"), 
        BearingAngle = numeric(), 
        Retour = FALSE, 
        SurveyDistance = numeric(),
        Seed = numeric(), 
        Margin = 0, 
        N = 1e3
){
    
    # Transform to Cartesian coordinates:
    StratumPolygonOneStratumXY <- sf::st_transform(StratumPolygonOneStratum, aeqd.CRS)
    
    # 2. Get the bearing angle:
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
    message("StoX: SurveyPlan for Stratum ", stratumName, "...")
    
    
    # Get the random seed point for the transects:
    startPositionFactor <- sampleStartPositionFactor(Seed)
    
    # If margin is given, iterate to obtain transects with total track length deviating at most by 'margin' relative to the input track length (margin = 0.05 implies between 19  and 21 hours, say):
    intersectsCoordsList <- getTransectsByArea(
        freeSurveyDistance = freeSurveyDistance, 
        stratumArea = stratumArea, 
        startPositionFactor = startPositionFactor, 
        corners = corners, 
        StratumPolygonOneStratumXY = StratumPolygonOneStratumXY, 
        DefinitionMethod = DefinitionMethod, 
        BearingAngle = BearingAngle, 
        Seed = Seed, 
        Retour = Retour
    )
    
    # If we are iterating to get the best fit to the expected SurveyDistance:
    numIter <- 1
    if(length(Margin) && is.numeric(Margin) && Margin > 0){
        
        
        
        # Set the totalSailedDist, margin to use, and the last value for 'rest' and 'freeSurveyDistance':
        #totalSailedDist <- 0
        totalSailedDist <- getTotalSailedDist(intersectsCoordsList, includeTransport = TRUE)
        
        
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
                DefinitionMethod = DefinitionMethod, 
                BearingAngle = BearingAngle, 
                Seed = Seed, 
                Retour = Retour
            )
            
            
            
            # Here we need the totalSailedDist:
            
            
            totalSailedDist <- getTotalSailedDist(intersectsCoordsList, includeTransport = TRUE)
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
    lines <- mapply(cbind, lines, Retour = lapply(intersectsCoordsList, "[", , "Retour"), SIMPLIFY = FALSE)
    
    # Remove duplicates. This may be risky, but duplicates points are confusing:
    #print(lines)
    #lines <- lapply(lines, unique)
    #print(lines)
    
    # Add transect IDs as names of the list of lines:
    numberOfTransects <- length(lines)
    names(lines) <- paste0("Transect_", sprintf(paste0("%0", nchar(numberOfTransects), "d"), seq_len(numberOfTransects)))
    
    
    
    ## Rotate back:
    #xy <- rotate2d(xy, -parameters$bearing)
    #coords[,c(xcols, ycols)] <- c(xy)
    
    
    return(lines)
}
############################################################
############################################################



getTotalSailedDist <- function(list, includeTransport = TRUE) {
    
    # We require even number of rows (accepting 1 row):
    nrows <- sapply(list, nrow)
    
    if(!all(nrows == 1 | nrows %% 2 == 0)) {
        stop("All elements of the list must have even number of rows.")
    }
    
    # Add all transport segments to the end of the list of segments, by extracting the last node of one segment and the first of the next (in total n - 1 transports, where n is the number of segments (elements in 'list')):
    if(includeTransport) {
        list <- addTransportToList(list)
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




addTransportToList <- function(list) {
    transportList <- lapply(seq_len(length(list) - 1), function(ind) rbind(utils::tail(list[[ind]], 1), utils::head(list[[ind + 1]], 1)))
    list <- c(list, transportList)
    return(list)
}



addTransportToSegments <- function(
    segments, 
    startNames = c("LongitudeStart", "LatitudeStart"), 
    endNames = c("LongitudeEnd", "LatitudeEnd"), 
    ordered = TRUE, 
    addLast0Transport = FALSE
) {
    
    # Store the column order:
    colnames_segments <- names(segments)
    otherNames <- setdiff(colnames_segments, c(startNames, endNames))
    
    
    # Create transports:
    transports <- lapply(seq_len(nrow(segments) - 1), function(ind) cbind(segments[ind, ..endNames], segments[ind + 1, ..startNames]))
    # Add the a final dummy transport from the last end point to itself, useful if we want to extract only start positions (used in WriteSurveyPlan()):
    if(addLast0Transport) {
        lastEndPoint <- segments[nrow(segments), ..endNames]
        transports[[length(transports) + 1]] <- structure(cbind(lastEndPoint, lastEndPoint), names = c(endNames, startNames))
    }
    
    transports <- data.table::rbindlist(transports)
    data.table::setnames(transports, c(startNames, endNames))
    # Add the other column, where if different between segments (the Segment column) the two values are pasted:
    for(name in otherNames) {
        transports <- addTransportOtherColumn(transports, segments, name, addLast0Transport = addLast0Transport)
    }
    data.table::setcolorder(transports, colnames_segments)
    
    # Add the transports to a table of positions
    output <- list(segments, transports)
    names(output) <- c("Segment", "Transport")
    output <- data.table::rbindlist(output, idcol = "SegmentType")
    data.table::setcolorder(output, c(setdiff(names(output), "SegmentType"), "SegmentType"))
    
    # Reorder the rows:
    if(ordered) {
        newOrder <- rep(seq_len(nrow(segments)), each = 2) + rep(c(0, nrow(segments)), nrow(segments))
        if(!addLast0Transport) {
            newOrder <- head(newOrder, -1)
        }
        output <- output[newOrder, ]
        #output[seq(1, nrow(output), 2), ] <- segments
        #output[seq(2, nrow(output), 2), ] <- transports
    }
    
    return(output)
}

addTransportOtherColumn <- function(transports, segments, name, sep = " - ", addLast0Transport = FALSE) {
    if(addLast0Transport) {
        first <- segments[[name]]
    }
    else {
        first <- utils::head(segments[[name]], -1)
    }
    
    #last <- utils::tail(segments[[name]], -1)
    #if(any(first == last)) {
    #    transports[[name]] <- first
    #}
    #else {
    #    transports[[name]] <-paste(
    #        first, 
    #        last, 
    #        sep = sep
    #    )
    #}
    transports[[name]] <- first
    
    return(transports)
}


sampleStartPositionFactor <- function(seed) {
    set.seed(seed)
    startPositionFactor <- runif(1)
    set.seed(NULL)
    return(startPositionFactor)
}



















#' Report a survey plan with additional information
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
#' # Get the area of the strata for use in ReportSurveyPlan():
#' stratumAreaData <- StratumArea(stratumPolygon)
#' 
#' # Harbitz zigzag survey design along each stratum:
#' surveyPlanZZ_Along <- DefineSurveyPlan(
#' 	DefinitionMethod = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' # Report the SurveyPlan:
#' ReportSurveyPlan(surveyPlanZZ_Along, stratumAreaData)
#' 
#' @return
#' An object of StoX data type \code{\link{ReportSurveyPlanData}}.
#' 
#' @export
#' 
ReportSurveyPlan <- function(
    SurveyPlan, 
    StratumAreaData
){
    if(!length(SurveyPlan)) {
        stop("The input SurveyPlan must be given")
    }
    if(!length(StratumAreaData)) {
        stop("The input StratumAreaData must be given")
    }
    
    # Create a list of the tables Segment and Stratum:
    Segment <- data.table::copy(SurveyPlan)
    SegmentWithTransport <- addTransportToSegments(Segment)
    
    # Add distances
    Segment[, Distance := distanceStartToEnd(.SD)]
    SegmentWithTransport[, Distance := distanceStartToEnd(.SD)]
    
    
    Stratum <- SegmentWithTransport[, .(
        DistanceSegment = sum(Distance * as.numeric(SegmentType == "Segment")), 
        DistanceTransport = sum(Distance * as.numeric(SegmentType == "Transport")), 
        Distance = sum(Distance), 
        Speed = Speed[1]), 
        by = "Stratum"
    ]
    Stratum <- merge(Stratum, StratumAreaData, all.x = TRUE)
    Stratum[, Coverage := DistanceSegment / sqrt(Area)]
    Stratum[, Time := Distance / Speed]
    
    output <- list(
        Segment = Segment, 
        SegmentWithTransport = SegmentWithTransport, 
        Stratum = Stratum
    )
    
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






#' Write a survey plan to a GPX file.
#'
#' @inheritParams ProcessData
#' @inheritParams general_file_plot_arguments
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
#' surveyPlanZZ_Along <- DefineSurveyPlan(
#' 	DefinitionMethod = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' # Convert the survey plan to an sf object and write this as a gpx file 
#' # (this is done automatically by RstoxFramework in StoX):
#' gpxData <- WriteSurveyPlan(surveyPlanZZ_Along)
#' filePath <- tempfile(fileext = ".gpx")
#' st_write(
#'      gpxData,
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
#' An object of StoX data type \code{\link{WriteSurveyPlanData}}.
#' 
#' @export
#' 
WriteSurveyPlan <- function(
    SurveyPlan, 
    Format = c("GPX", "Ruter")#, 
    #SplitByStratum or stratum and tourRetour??????
){
    
    Format <- RstoxData::match_arg_informative(Format)
    
    # Create a list of the tables Segment and Stratum:
    SegmentWithTransport <- addTransportToSegments(SurveyPlan, addLast0Transport = TRUE)
    
    if(Format == "GPX") {
        output <- sf::st_as_sf(
            subset(SegmentWithTransport, select = c("LongitudeStart", "LatitudeStart")), 
            coords = c("LongitudeStart", "LatitudeStart")
        )
        
        # Add tracks, segments and waypoints:
        #output$track_fid <- SegmentWithTransport$Stratum
        #output$track_seg_id <- SegmentWithTransport$Transect
        #output$track_seg_point_id <- SegmentWithTransport$Segment
        
        output$track_name <- SegmentWithTransport$Stratum
        
        # The driver translates to integers starting from 0, so we do this here explicitely to avoid confusion:
        output$track_fid <- as.integer(factor(SegmentWithTransport$Stratum)) - 1L
        output$track_seg_id <- as.integer(sub(".*_", "", SegmentWithTransport$Transect)) - 1L
        #output$track_seg_point_id <- as.integer(sub(".*_", "", SegmentWithTransport$Segment)) - 1L
        output$track_seg_point_id <- unlist(by(output, paste(output$track_fid, output$track_seg_id), FUN = function(x) seq_len(nrow(x)))) - 1L
    }
    else if(Format == "Ruter") {
        
        warning("StoX: Using Format = \"Ruter\" is experimental.")
        
        symbol <- "Brunsirkel"
        
        # Convert the positions to arc-minutes
        SegmentWithTransport[, LongitudeStart := LongitudeStart * 60]
        SegmentWithTransport[, LatitudeStart := LatitudeStart * 60]
        
        # Create individual points:
        output <- SegmentWithTransport[, .(Ruter = paste(LatitudeStart, LongitudeStart, 0, symbol, collapse = "\n")), by = "Stratum"]
        
        # Add one blank line between strata:
        output <- unlist(lapply(output$Ruter, function(x) c("Rute uten navn", x, "")))
        
        # Set the class, which will be used in RstoxFramework when writing the output file:
        class(output) <- "Ruter"
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
#' Plot a survey plan.
#'
#' @inheritParams ProcessData
#' @inheritParams PlotAcousticTrawlSurvey
#' @inheritParams general_plot_arguments
#' @inheritParams general_file_plot_arguments
#' @inheritParams general_map_plot_arguments
#' @inheritParams general_track_plot_arguments
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
#' surveyPlanZZ_Along <- DefineSurveyPlan(
#' 	DefinitionMethod = "ZigZagRectangularEnclosure", 
#' 	StratumPolygon = stratumPolygon, 
#' 	SurveyTime = 200, 
#' 	SurveySpeed = 10, 
#' 	Seed = 1, 
#' 	Bearing = "Along"
#' )
#' 
#' WriteSurveyPlan(surveyPlanZZ_Along)
#' 
#' @return
#' An object of StoX data type \code{\link{WriteSurveyPlanData}}.
#' 
#' @export
#' 
PlotSurveyPlan <- function(
    SurveyPlan,
    
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
    
    # Set the default of the StoX function. These are defaults defined in the stoxFunctionAttributes.
    plotArguments <- setDefaultsInStoxFunction(plotArguments, StoxFunctionName = "PlotSurveyPlan", stoxFunctionAttributes = stoxFunctionAttributes)

    
    # Split axis titles into x and y:
    plotArguments <- splitAxisTitleSize(plotArguments)
    
    # Set hard coded values used in plot_lon_lat():
    plotArguments <- c(
        plotArguments, 
        list(
            # Add Stratum as a grouping variable:
            trackLinesBy <- "Stratum", 
            # Solid line for tour and dashed for retour:
            linetype.track <- "Retour", 
            lon_name = "LongitudeStart", 
            lat_name = "LatitudeStart", 
            lon_name_end = "LongitudeEnd", 
            lat_name_end = "LatitudeEnd", 
            type = "sp"
        )
    )
    
    # Special care to add the positions, as we need Retour to be factor when plotting:
    SurveyPlan$Retour <- as.factor(SurveyPlan$Retour)
    plotArguments$x <- SurveyPlan
    
    
    # Define the translation from the inputs to this function to the inputs to plot_lon_lat:
    translation <- c(
        polygon = "StratumPolygon",
        showMap = "ShowMap",
        
        linewidth.track = "TrackLineWidth", 
        linewidth.polygon.border = "StratumPolygonBorderLineWidth", 
        
        size = "TrackPointSize", 
        
        color.track = "TrackColor", 
        color.scale = "TrackPointColor",  
        color.land = "LandColor", 
        color.border = "BorderColor", 
        color.ocean = "OceanColor", 
        color.grid = "GridColor", 
        color.polygon = "StratumPolygonColor", 
        color.polygon.border = "StratumPolygonBorderColor", 
        
        shape = "TrackPointShape",
        # Do not set any alphas (transparency): alpha.point, alpha.track, alpha.polygon
        
        zoom = "Zoom", 
        # xlim, ylim and zoomCenter are set by set_xlim_ylim_zoomCenter()
        
        legend.text.size = "LegendTextSize", 
        legend.title.size = "LegendTitleSize", 
        main = "Title"
    )
    
    # Add xlim and ylim and center for zooming:
    plotArguments <- set_xlim_ylim_zoomCenter(plotArguments)
    
    
    
    
    # Create a table with Longitude, Latitude and Stratum to plot:
    #plotArguments$x <- data.table::data.table(
    #    Longitude = c(t(subset(SurveyPlan, select = c("LongitudeStart", "LongitudeEnd")))), 
    #    Latitude = c(t(subset(SurveyPlan, select = c("LatitudeStart", "LatitudeEnd")))), 
    #    Stratum = c(t(subset(SurveyPlan, select = c("Stratum", "Stratum")))), 
    #    Type = rep(c("Transect", "Transport"), length.out = 2 * nrow(SurveyPlan)), 
    #    Retour = as.factor(rep(SurveyPlan$Retour, each = 2))
    #)
    
    
    # Add StratumPolygon:
    if(!ShowStratumPolygon) {
        plotArguments$StratumPolygon <- NULL
    }
    
    # Rename to the argument names of the plot_lon_lat():
    plotArguments <- renameListByNames(
        plotArguments, 
        old = translation,  
        new = names(translation)
    )
    
    
    
    
    
    output <- do.call(plot_lon_lat, plotArguments)
    
    
    
    ### # Plot the stratumPolygon with the segments
    ### output <- ggplot2::ggplot() +
    ###     ggplot2::geom_sf(data = StratumPolygon, ggplot2::aes(fill = StratumName), color = 'blue') +
    ###     ggplot2::geom_segment(
    ###         data = SurveyPlan, 
    ###         ggplot2::aes(x = LongitudeStart, y = LatitudeStart, xend = LongitudeEnd, yend = LatitudeEnd)
    ###     )
    ### 
    ### 
    ### lonlat <- SurveyPlan[, .(Longitude = c(LongitudeStart, LongitudeEnd), Latitude = c(LatitudeStart, LatitudeEnd))]
    ### 
    ### output <- zoom_lon_lat(
    ###     x = lonlat, 
    ###     lon = "Longitude", 
    ###     lat = "Latitude", 
    ###     xmin = LongitudeMin, 
    ###     xmax = LongitudeMax, 
    ###     ymin = LatitudeMin, 
    ###     ymax = LatitudeMax, 
    ###     zoom = Zoom, 
    ###     zoomCenter = c(LongitudeCenter, LatitudeCenter)
    ### ) + 
    ### ggplot2::xlab("Longitude") + 
    ### ggplot2::ylab("Latitude") + 
    ### ggplot2::ggtitle(Title)
    
    
    #output <- output + ggplot2::theme(
    #    axis.title.x = ggplot2::element_text(size = if(length(lll$axis.title.size.x)) lll$axis.title.size.x else 10), 
    #    axis.title.y = ggplot2::element_text(size = if(length(lll$axis.title.size.y)) lll$axis.title.size.y else 10), 
    #    axis.text.x = ggplot2::element_text(size = if(length(lll$axis.text.size.x)) lll$axis.text.size.x else 10), 
    #    axis.text.y = ggplot2::element_text(size = if(length(lll$axis.text.size.y)) lll$axis.text.size.y else 10), 
    #    legend.text = ggplot2::element_text(size = if(length(lll$legend.text.size)) lll$legend.text.size else 10), 
    #    legend.title = ggplot2::element_text(size = if(length(lll$legend.title.size)) lll$legend.title.size else 10),
    #    panel.background = ggplot2::element_rect(fill = color.ocean, color = "grey80"),
    #    panel.grid.major = ggplot2::element_line(color = color.grid), 
    #    panel.grid.minor = ggplot2::element_line(color = color.grid)
    #)
    
    output <- setPlotAttributes(
        plotObject = output, 
        plotArguments = plotArguments
    )
    
    return(output)
}
















