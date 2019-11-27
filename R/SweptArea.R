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
Catchability <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
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
LengthWeightRelationship <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
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
RegroupLengthDist <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
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
RelLengthDist <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


##################################################
##################################################
#' Station length distribution
#' 
#' This function calculates length frequency distribution per SpecCat per serialnumber, either given as percentages, or as counts, possibly normalized by towing distance.
#' 
#' @param BioticData			The BioticData input, which is a list of data.tables as returned from \code{\link{ReadBioticXML}}.
#' @param LengthDistType		The type of length distribution to use, one of "LengthDist", "NormLengthDist" and "PercentLengthDist" (see 'Details').
#' @param allowMissingWeight	Logical: If TRUE and \code{LengthDistType} == "PercentLengthDist" accept stations with missing pairs of lengthsampleweight and (total) weight or lengthsamplecatch and (total) catch.
#' @param fishstationName		The name of the fishstation table in the list BioticData.
#' @param catchsampleName		The name of the catchsample table in the list BioticData.
#' @param individualName		The name of the individual table in the list BioticData.
#'
#' @details The purpose of function StationLengthDist is to produce a length frequency distribution for each biotic station by species. Three different distributions (LengthDistType) can be generated:
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' library(Rstox)
#' # Read biotic data:
#' dat <- getBaseline("Test_Rstox", input=FALSE, proc="ReadBioticXML", endProcess="ReadBioticXML")
#' # Convert to data.table, since the current Rstox uses data.frames:
#' dat <- lapply(dat, data.table::as.data.table)
#' # Generate the station length distribution using the data.frame names of the current Rstox:
#' SLD <- StationLengthDist(dat, 
#'     fishstationName = "ReadBioticXML_BioticData_fishstation.txt", 
#'     catchsampleName = "ReadBioticXML_BioticData_catchsample.txt", 
#'     individualName = "ReadBioticXML_BioticData_individual.txt")
#' str(SLD)
#' 
#' @seealso \code{StationLengthDist} is called by \code{\link[Rstox]{getBaseline}}.
#' 
#' @export
#' @import data.table
#' 
StationLengthDist <- function(
    BioticData, 
    LengthDistType = "PercentLengthDist", 
    allowMissingWeight = TRUE, 
    fishstationName = "fishstation",
    catchsampleName = "catchsample",
    individualName = "individual",
    ...) {
    
    #### Functions: ####
    # Fast table function for integer valued data:
    tabulatePlusOne <- function(x, range) {
        # Allow for double precision, which may occur when multiplying by a raising factor such as weight/lengthsampleweight:
        out <- as.double(tabulate(x, range[2]))
        out[seq(range[1], range[2])]
    }
    
    # Take the sum of a number of length distributions, and return the input object subset to only the sum. This function is used in \code{getStationLengthDist} when summing over part samples, and is somewhat ad hoc:
    psum <- function(y) {
        y$WeightedCount <- rowSums(matrix(y$WeightedCount, nrow=numLengthIntervals), na.rm=TRUE)
        y[seq_len(numLengthIntervals), ]
    }
    
    # Function to get missing data and number of part samples
    checkCatchsample <- function(x) {
        has_SpecCat <- !is.na(x$SpecCat)
        has_weight <- !is.na(x$catchweight)
        has_lengthsampleweight <- !is.na(x$lengthsampleweight)
        has_weight <- has_weight & has_lengthsampleweight
        has_catchcount <- !is.na(x$catchcount)
        has_lengthsamplecount <- !is.na(x$lengthsamplecount)
        has_count <- has_catchcount & has_lengthsamplecount
        
        has_weightORcount <- has_weight | has_count
        has_NOTweightBUTcount <- !has_weight & has_count
        
        numPartSamples <- x$catchpartnumber
        has_onlyOnePartSample <- numPartSamples == 1
        
        out <- list(
            has_SpecCat = has_SpecCat, 
            has_weight = has_weight, 
            has_weightORcount = has_weightORcount, 
            has_NOTweightBUTcount = has_NOTweightBUTcount, 
            has_onlyOnePartSample = has_onlyOnePartSample
        )
        
        # Add the tests to the input data table and return:
        out <- as.data.table(out)
        cbind(x, out)
    }
    
    
    #### Names and definitions: ####
    # Check that the specified  'fishstationName', 'catchsampleName' and 'individualName' are present in the BioticData. This will be handeled by definitions using an initiateRstox function once the infrastructure has been decided upon:
    if(any(length(BioticData[[fishstationName]]) == 0, length(BioticData[[fishstationName]]) == 0, length(BioticData[[fishstationName]]) == 0)) {
        stop("'fishstationName', 'catchsampleName' and 'individualName' are not present in the data.")
    }
    
    # Define names of required variables:
    browser()
    var <- c("distance", "catchpartnumber", "catchweight", "lengthsampleweight", "catchcount", "lengthsamplecount", "length", "lengthresolution", "SpecCat")
    #keys <- c("cruise", "serialnumber", "SpecCat")
    bioticKeys <- getBioticKeys()
    # Add the SpecCat, since we wish to present the length distribution per species:
    #keys <- c(keys, )
    # Define also the key to use for aggregating the length distribution, i.e., one distribution per station:
    # aggregate_keys <- "samplenumber"
    # keys <- c(keys, aggregate_keys)
    
    
    #### First merge the fish station and catch sample data tables, to be sure to remove the entire station if none of the catch samples have e.g. weight and lengthsampleweight: ####
    keys <- unlist(bioticKeys[c("mission", "fishstation", "catchsample")])
    fishstation_catchsample <- merge2(BioticData[[fishstationName]], BioticData[[catchsampleName]], var=var, keys=keys)
    
    
    #### Subset datasets given 'LengthDistType': ####
    # If LengthDistType="NormLengthDist", accept only stations with 'distance':
    if(LengthDistType == "NormLengthDist") {
        fishstation_catchsample <- subset(fishstation_catchsample, !is.na(distance))
    }
    
    # Check validity of the data:
    fishstation_catchsample <- checkCatchsample(fishstation_catchsample)
    
    # If LengthDistType="PercentLengthDist" there is a possibility to accept stations with missing weight and count, as long as there is only one sample:
    if(LengthDistType == "PercentLengthDist" && allowMissingWeight) {
        fishstation_catchsample <- subset(fishstation_catchsample, has_SpecCat & (has_weightORcount | has_onlyOnePartSample))
    }
    else {
        fishstation_catchsample <- subset(fishstation_catchsample, has_SpecCat & has_weightORcount)
    }
    
    # Merge fishstation with catchsample, and then the result with individual:
    thisvar <- c(var, "has_weight", "has_NOTweightBUTcount", "has_onlyOnePartSample")
    keys <- unlist(bioticKeys[c("mission", "fishstation", "catchsample", "individual")])
    fishstation_catchsample_individual <- merge2(fishstation_catchsample, BioticData[[individualName]], var=thisvar, keys=keys)
    
    
    #### Get length intervals: ####
    # The 'lengthresolution' from biotic v1.4 is coded as given by getNMDinfo("lengthresolution"), which says code = 1:7, resolutionMeters = c(0.001, 0.005, 0.010, 0.030, 0.050, 0.0005, 0.0001):
    resolutionMeters = c(0.001, 0.005, 0.010, 0.030, 0.050, 0.0005, 0.0001)
    lengthRes <- max(resolutionMeters[fishstation_catchsample_individual$lengthresolution], na.rm=TRUE)
    lengthResCM <- lengthRes * 100
    
    # Get largest length resolution:
    # Round down all length measurements to the nearest length interval (add 1 to make the first interval start at 0, thus rounding down):
    fishstation_catchsample_individual$lengthInt <- floor(fishstation_catchsample_individual$length / lengthResCM) + 1
    rangeLengthInt <- range(fishstation_catchsample_individual$lengthInt, na.rm=TRUE)
    # Get the range of the lengths:
    rangeLengths <- (range(fishstation_catchsample_individual$lengthInt, na.rm=TRUE) - 1) * lengthResCM
    # Create a vector of all length intervals:
    lengthIntervals <- seq(rangeLengths[1], rangeLengths[2], by=lengthResCM)
    numLengthIntervals <- length(lengthIntervals)
    
    
    #### Generate length distributions per station and SpecCat: ####
    # Set the keys used by the data.table package:
    keys <- unlist(bioticKeys[c("mission", "fishstation", "catchsample")])
    setkeyv(fishstation_catchsample_individual, cols=keys)
    
    # Declare the variables used in the fishstation_catchsample_individual[] expression below (this is done to avoid warnings when building the package):
    . <- NULL
    distance <- NULL
    has_SpecCat <- NULL
    has_weightORcount <- NULL
    has_onlyOnePartSample <- NULL
    lengthInt <- NULL
    catchweight <- NULL
    lengthsampleweight <- NULL
    has_weight <- NULL
    has_NOTweightBUTcount <- NULL
    WeightedCount <- NULL
    count <- NULL
    lengthsamplecount <- NULL
    serialnumber <- NULL
    
    # Use the data.table package to generate the length distributions:
    out <- fishstation_catchsample_individual[,  .(
        "WeightedCount" = tabulatePlusOne(lengthInt, rangeLengthInt), 
        "LengthGroup (cm)" = lengthIntervals, 
        "LengthInterval (cm)" = rep(lengthResCM[1], numLengthIntervals), 
        "LengthDistType" = rep(LengthDistType[1], numLengthIntervals),
        "distance" = rep(distance[1], numLengthIntervals),
        "weight" = rep(catchweight[1], numLengthIntervals),
        "lengthsampleweight" = rep(lengthsampleweight[1], numLengthIntervals), 
        "has_weight" = rep(has_weight[1], numLengthIntervals),
        "has_NOTweightBUTcount" = rep(has_NOTweightBUTcount[1], numLengthIntervals), 
        "has_onlyOnePartSample" = rep(has_onlyOnePartSample[1], numLengthIntervals)
    ), by=keys]
    
    
    #### Sum over part samples: ####
    # Apply so called raising factors, which are total weight/count divided by sample weight/count:
    out[has_weight==TRUE, WeightedCount := WeightedCount * weight / lengthsampleweight]
    if(any(out$has_NOTweightBUTcount, na.rm=TRUE)) {
        message("StoX: Here there seems to be bug, where count is not present, and this should also be catchcount")
        out[has_NOTweightBUTcount==TRUE, WeightedCount := WeightedCount * count / lengthsamplecount]
    }
    
    # Set the keys to the StationID and SpecCat, which are introduced in ReadBioticXML():
    keys <- c("StationID", "SpecCat")
    setkeyv(out, cols=keys)
    
    # Sum over part samples only if there are stations with more than one part sample:
    if(!all(out$has_onlyOnePartSample, na.rm=TRUE)) {
        out <- out[has_onlyOnePartSample == TRUE, psum(.SD), by=keys]
    }
    
    
    ##### Output: #####
    # Convert to percent:
    if(LengthDistType == "PercentLengthDist") {
        out[, WeightedCount := WeightedCount/sum(WeightedCount, na.rm=TRUE) * 100, by=thiskeys]
    }
    # Normalize by trawled distance:
    else if(LengthDistType == "NormLengthDist") {
        out[, WeightedCount := WeightedCount/distance, by=thiskeys]
    }
    
    out
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
StationSpecCatDensity <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
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
SweptAreaDensity <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
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
TotalLengthDist <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


