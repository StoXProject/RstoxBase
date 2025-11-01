# We will support three different polygon files formats:
# 1. StoxWKT files, which are tables with two columns, one for the polygon names and one for the multipolygon WKT strings in a file with file extension "txt".
# 2. Shapefiles, which are folders with files with the following file extensions: "shp", shx" and "dbf".
# 3. GEOJSON files, which are text files with a GEOJSON string, with file extension "geojson".

# StoX will detect file extension and apply the appropriate reading function, returning an object of class SpatialPolygonsDataFrame. (readShapeFiles(), readGSOJSON(), readStoxMultipolygonWKT())

# We will also have a function converting from SpatialPolygonsDataFrame to GSOJSON string, and vice versa, for use when writing to the project.xml.





readStoxMultipolygonWKTFromFile <- function(FilePath) {
    # If the input is an existing file path instead of a data.table from the project.xml:
    if(!file.exists(FilePath) || (file.exists(FilePath) && isTRUE(file.info(FilePath)$isdir))) {
        stop("The StoX multipolygon WKT file ", FilePath, " does not exist or is a directory.")
    }
    tab <- data.table::fread(FilePath, sep = "\t", header = FALSE, colClasses = list(character=1:2), encoding = "UTF-8")
    names(tab) <- c("StratumName", "geometry")
    
    ### # Do a check for scandinavian letters given as hex code, and issue an error stating that the file needs to be converted to UTF-8:
    ### scandinavianLettersAsHex <- c("\xe6", "\xf8", "\xe5", "\xc6", "\xd8", "\xc5")
    invalidUTF8 <- detectInvalidUTF8(tab$StratumName)
    if(any(invalidUTF8)) {
        stop("The file ", FilePath, " contains characters indicating that the file was saved with an encoding different from the required UTF-8 (scandinavian letters, possibly saved as Latin 1). Please open the file in a text editor, verify that all characters appear as exptected, and save the file with encodnig UTF-8.")
    }
    
    # Stop also if stratum names are duplicated:
    if(any(duplicated(tab$StratumName))) {
        stop("The file ", FilePath, " contains duplicated polygon names (column 1). Please open the file in a text editor and rename or delete the duplicated strata: ", paste(paste0("\"", unique(tab$StratumName[duplicated(tab$StratumName)]), "\""), collapse = ", "), ".")
    }
    
    return(tab)
}




#dataTable2SpatialPolygonsDataFrame <- function(DataTable) {
#    
#    # Read the WKT strings to sf object:
#    polygons <- sf::st_as_sf(DataTable, wkt = 2)
#    # Set default projection:
#    sf::st_crs(polygons) <- getRstoxBaseDefinitions("proj4string_longlat")
#    
#    # Convert to spatialPolygonsDataFrame:
#    spatialPolygonsDataFrame <- sf::as_Spatial(polygons)
#    names(spatialPolygonsDataFrame) <- "StratumName"
#    
#    return(spatialPolygonsDataFrame)
#}
#dataTable2sf_MULTIPOLYGON <- function(x) {
#    
#    # Read the WKT strings to sf object:
#    polygons <- sf::st_as_sf(x, wkt = 2)
#    # Set default projection:
#    sf::st_crs(polygons) <- getRstoxBaseDefinitions("proj4string_longlat")
#    
#    return(polygons)
#}







#stoxMultipolygonWKT2SpatialPolygonsDataFrame <- function(FilePath) {
#    
#    # Read the file as data.table:
#    dataTable <- readStoxMultipolygonWKTFromFile(FilePath)
#    # Convert to SpatialPolygonsDataFrame:
#    dataTable2SpatialPolygonsDataFrame(dataTable)
#}

#stoxMultipolygonWKT2sf <- function(FilePath) {
#    
#    # Read the file as data.table:
#    dataTable <- readStoxMultipolygonWKTFromFile(FilePath)
#    ## Convert to SpatialPolygonsDataFrame:
#    #dataTable2SpatialPolygonsDataFrame(dataTable)
#    # Convert to sf:
#    dataTable2sf_MULTIPOLYGON(dataTable)
#}
#

##################################################
##################################################
#' Define stratum multipolygon
#' 
#' This function reads a \href{https://geojson.org/}{GeoJSON} file, \href{https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm}{shapefile} or a \code{\link{StoX_multipolygon_WKT}} file and returns an object of StoX data type \code{\link{StratumPolygon}} file.
#' 
#' @inheritParams general_arguments
#' @inheritParams getStratumNames
#' @param DefinitionMethod A string naming the method to use, one of "Manual" to start off with no strata and create the strata manually in the map of the StoX GUI. Strata can be added, modified and removed in the StoX GUI; or "ResourceFile", to read the file \code{FileName} holding the stratum multipolygon.
#' @param FileName The path to a \href{https://geojson.org/}{GeoJSON} file, \href{https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm}{shapefile, use the file with extension "shp"} (may be a folder containing the shapefiles); a \code{\link{StoX_multipolygon_WKT}} file; or a project.xml file from StoX <= 2.7. Note that in the latter case it is assumed that the contents of the project.xml file is actually used in the StoX <= 2.7 project, i.e. that UseProcessData = TRUE in DefineStrata(). Must include file extension.
#' @param SimplifyStratumPolygon Logical: If TRUE a simplification algorithm is applied to the stratum polygons (sf::st_simplify(), which "uses the GEOS implementation of the Douglas-Peucker algorithm to reduce the vertex count" as per quote from \href{https://r.geocompx.org/geometric-operations.html#simplification}{Geocomputation with R}). This option is useful for very complex stratum polygon files to reduce time for opening, running and saving a project.
#' @param SimplificationFactor A value between 0 and 1 specifying the desired object size of the stratum polygon after simplification. An iterative method is used to derive the to the desired object size. Strata that end up as empty strata after simplification are left un-simplified, and will contribute to a larger final object size than the desired object size. Note that larger values require more  time than smaller values. It is advisable to start with a low value (e.g. 0.01) and check the resolution of the strata borders in the map, and gradually increase \code{SimplificationFactor} until an acceptable resolution is obtained. Note that simplification of stratum polygons can lead to inaccuracies such as gaps between strata, which can lead to hauls falling outside of any stratum, causing under-estimation of survey indices, or failure to complete the model in the case of SplitNASC models which require all hauls to be located in a stratum. If such problems occur, the strata can be modified manually after simplification to include all hauls. Another problem is the change in stratum areas, which is reported to the console when running the simplification. A very rough rule of thumb is that \code{SimplificationFactor} = 0.1 seems to be a compromise between increased speed and reduction of stratum border resolution.
#' 
#' @details
#' The parameter \code{UseProcessData} is always set to TRUE when running a process, and needs to be explicitely set to FALSE to enable reading a file (It's set to FALSE at the moment).
#' 
#' Polygons are expected to have the the following projection: getRstoxBaseDefinitions("proj4string_longlat")
#' 
#' @return
#' An object of StoX data type \code{\link{StratumPolygon}}.
#' 
#' @references
#' Additonal information on GeoJSON and shapefiles specification can be found here:
#' \url{https://geojson.org/}
#' 
#' \url{https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm}
#' 
#' @seealso \code{\link{StratumArea}} for calculating the area of the strata.
#' 
#' @export
#'
DefineStratumPolygon <- function(
    processData, UseProcessData = FALSE, 
    DefinitionMethod = c("Manual", "DeleteAllStrata", "ResourceFile"), 
    FileName = character(), 
    StratumNameLabel = character(), 
    SimplifyStratumPolygon = FALSE, 
    SimplificationFactor = 1
) {
    
    #if(!is.null(processData) & UseProcessData) {
    if(UseProcessData) {
        return(processData)
    }
    
    # Get the definition method:
    DefinitionMethod <- RstoxData::match_arg_informative(DefinitionMethod)
    
    # Read the file:
    if(grepl("ResourceFile", DefinitionMethod, ignore.case = TRUE)) {
        StratumPolygon <- readStratumPolygonFromFile(FileName, StratumNameLabel = StratumNameLabel)
    }
    else if(grepl("DeleteAllStrata", DefinitionMethod, ignore.case = TRUE)) {
        StratumPolygon <- getRstoxBaseDefinitions("emptyStratumPolygon")
    }
    # Manual implies to use the existing process data, or create an empty set if not present:
    else if(grepl("Manual", DefinitionMethod, ignore.case = TRUE)) {
        if(length(processData)) {
            StratumPolygon <- processData
        }
        else {
            StratumPolygon <- getRstoxBaseDefinitions("emptyStratumPolygon")
        }
    }
    else {
        stop("Inavlid DefinitionMethod")
    }
    
    if(isTRUE(SimplifyStratumPolygon)) {
        # Use the sf::st_simplify, but turn off s2, as it does not perform well on longitude-latitude data due to the us off infinite lines instead of lines along the great circle (https://r-spatial.github.io/sf/articles/sf7.html, and https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data):
        turn_off_s2(
            StratumPolygon <- simplifyStratumPolygon(
                StratumPolygon = StratumPolygon, 
                SimplificationFactor = SimplificationFactor, 
                preserveTopology = FALSE
            ), 
            msg = FALSE
        )
    }
    
    return(StratumPolygon)
}




readStratumPolygonFromFile <- function(FileName, StratumNameLabel = character()) {
    
    if(length(unlist(strsplit(FileName, "\\."))) < 2) {
        stop("FileName must include file extension.")
    }
    else {
        fileParts <- unlist(strsplit(FileName, "\\."))
        FileExt <- utils::tail(fileParts, 1)
    }
    
    if(tolower(FileExt) %in% c("wkt", "txt")) {
        # Read the table of wkt strings:
        dataTable <- readStoxMultipolygonWKTFromFile(FileName)
        # Convert to sf (POLYGON or MULTIPOLYGON):
        StratumPolygon <- sf::st_as_sf(dataTable, wkt = 2)
        
        # Hard coded to "StratumName", as column names are not in the StoX wkt files:
        StratumPolygon <- addStratumNames(
            StratumPolygon, 
            StratumNameLabel = "StratumName"
        )
    }
    # If the FileName is a shapefile, or a directory with a shapefile:
    else if(tolower(FileExt) == "shp" || (isTRUE(file.info(FileName)$isdir) && any(tools::file_ext(list.files(FileName)) == "shp"))) {
        # Read the shape files:
        StratumPolygon <- sf::st_read(FileName, quiet = TRUE)
        
        # Set stratum names:
        StratumPolygon <- addStratumNames(
            StratumPolygon, 
            StratumNameLabel = StratumNameLabel
        )
    }
    else if(tolower(FileExt) %in% c("json", "geojson")) {
        # Read the geojson:
        if(basename(FileName) == "project.json") {
            StratumPolygon <- readOneProcessDataByFunctionNameFromProjectJSON(FileName, functionName = "DefineStratumPolygon")
        }
        else {
            # Read the geojson:
            StratumPolygon <-  geojsonsf::geojson_sf(FileName)
            
            # Set stratum names:
            StratumPolygon <- addStratumNames(
                StratumPolygon, 
                StratumNameLabel = StratumNameLabel
            )
        }
    }
    else if(tolower(FileExt) == "xml" && any(grepl("http://www.imr.no/formats/stox/v1", readLines(FileName, 5)))) {
        # Read the table of wkt strings (stratum name column is set to "StratumName" in this function):
        StratumPolygon <- readStratumPolygonFrom2.7(FileName, remove_includeintotal = TRUE)
        # Convert to sf (POLYGON or MULTIPOLYGON):
        StratumPolygon <- sf::st_as_sf(StratumPolygon, wkt = 2)
    }
    else {
        stop(paste("File extension", FileExt, "not supported yet. Contact the StoX developers."))
    }
    
    # Set the assumed projection:
    suppressWarnings(sf::st_crs(StratumPolygon) <- getRstoxBaseDefinitions("proj4string_longlat"))
    # Make sure that the StratumPolygon is a MULTIPOLYGON object:
    StratumPolygon <- sf::st_cast(StratumPolygon, "MULTIPOLYGON")
    
    return(StratumPolygon)
}


# Function to get the process data from a project.json:
readOneProcessDataByFunctionNameFromProjectJSON <- function(FileName, functionName) {
    projectDescriptionList <- tryCatch(
        jsonlite::read_json(FileName, simplifyVector = FALSE), 
        error = function(err) {
            stop("Unable to parse the StoX description file ", FileName, ", as jsonlite::read_json reported the error \n\n", err)
        }
    )
    # Find the requested processes:
    functionNames <- sapply(projectDescriptionList$project$models$baseline, "[[", "functionName")
    atFunction <- which(grepl(functionName, functionNames))
    nFunction <- length(atFunction)
    if(nFunction != 1) {
        stop("The StoX project description file ", FileName, " must contain exactly 1 process in Baseline using the function ", functionName, " (", nFunction, " such processes found).")
    }
    
    # Format the process data:
    output <- projectDescriptionList$project$models$baseline[[atFunction]]$processData
    if(!length(output)) {
        warning("PSU process data is empty for the process ", projectDescriptionList$project$models$baseline[[atFunction]]$processName)
    }
    output <- formatProcessData(output)
    
    return(output)
}

#setDefaultProjectionAndMULTIPOLYGON <- function(StratumPolygon) {
#    # Set the assumed pojection:
#    suppressWarnings(sf::st_crs(StratumPolygon) <- getRstoxBaseDefinitions("proj4string_longlat"))
#    # Make sure that the StratumPolygon is a MULTIPOLYGON object:
#    StratumPolygon <- sf::st_cast(StratumPolygon, "MULTIPOLYGON")
#    
#    return(StratumPolygon)
#}


simplifyStratumPolygon <- function(
    StratumPolygon = StratumPolygon, 
    SimplificationFactor = SimplificationFactor, 
    preserveTopology = FALSE
) {
    
    # Do nothing to empty:
    if(!NROW(StratumPolygon)) {
        return(StratumPolygon)
    }
    
    if(SimplificationFactor <= 0 || SimplificationFactor >= 1) {
        stop("SimplificationFactor must be between 0 and 1.")
    }
    
    # Not needed as we no longer use sp:
    ## Transform to sf:
    #sfStratumPolygon <- sf::st_as_sf(StratumPolygon)
    
    
    # Iterate to find the object size SimplificationFactor as a fraction of the original utils::object.size:
    originalSize <- utils::object.size(StratumPolygon)
    size <- originalSize
    desiredSize <- originalSize * SimplificationFactor
    margins <- seq(0.001, 1, length.out = 10) * originalSize
    marginInd <- 1
    
    # Define objects to update in the loop:
    down <- TRUE
    dTolerance <- 1e3
    #dTolerance_preivous <- dTolerance
    scalingFactor <- 0.1
    iteration <- 0
    StratumPolygon_out <- NULL
    lastDiffInTolerance <- dTolerance * scalingFactor
    root <- 1
    
    diffs <- Inf
    
    
    # Run the loop and stop when the object size is close enough to the desired size:
    while(utils::tail(diffs, 1) > margins[marginInd]) {
        
        if(iteration > 100) {
            stop("Simplification did not converge. Try a different SimplificationFactor.")
        }
        
        iteration <- iteration + 1
        dTolerance_preivous <- dTolerance
        
        # Reduce or increase the tolerance value:
        if(!down) {
            root <- root / 2
        }
        else {
            root <- root * 2
        }
        dTolerance <- dTolerance * scalingFactor^root
        
        # Simplify.
        suppressWarnings(StratumPolygon_out <- sf::st_simplify(
            StratumPolygon, 
            dTolerance = dTolerance, 
            preserveTopology = preserveTopology
        ))
        
        # Update size and check if we should go down or up in tolerance:
        size <- utils::object.size(StratumPolygon_out)
        down <- size < desiredSize
        sizeInPercentOfOriginal <- signif(100  *  as.numeric(size) / as.numeric(originalSize), digits = 2)
        
        # Stop if even the first iteration gives a too large object, which implies that either the desired size is too small or the complexity of the polygons to large in the case that preserveTopology is TRUE:
        if(iteration == 1 && !down) {
            stop("Either SimplificationFactor is too small, or in the case that preserveTopology = TRUE, the StratumPolygon is too complex to be reduced to the desired object size (object size ",  size, " obtained, which is ", sizeInPercentOfOriginal, " % of original size. Select SimplificationFactor > ", sizeInPercentOfOriginal, ").")
        }
        
        message("Finished iteration ", iteration, ", size ", sizeInPercentOfOriginal, " % of initial size, with dTolerance ", dTolerance, ".")
        
        # Revert the tolerance if we need to step back:
        if(!down) {
            dTolerance <- dTolerance_preivous
        }
        
        diffs <- append(diffs, abs(size - desiredSize))
        marginInd <- sum(utils::tail(diffs, 1) == diffs, na.rm = TRUE)
        
    }
    
    # Get the area of the strata, and replace empty strata by the original:
    area0 <- sf::st_area(StratumPolygon)
    area <- sf::st_area(StratumPolygon_out)
    StratumAreaTable <- data.table::data.table(
        Stratum = StratumPolygon_out$StratumName, 
        OriginalArea = area0, 
        SimplifiedArea = area
    )
    
    # Issue a warning if any strata are smaller than 1 square meter:
    empty <- area < units::set_units(1,  "m^2")
    if(any(empty)) {
        warning("StoX: The following strata was empty after simplification (less than 1 square meter), and were replaced by the original strata: \n", paste0(utils::capture.output(StratumAreaTable[empty]), collapse = "\n"))
    }
    
    # Replace empty strata by the original:
    StratumPolygon_out$geometry[empty] <- StratumPolygon$geometry[empty]
    
    # The StratumPolygon is an sf object as of StoX 4.0.0, so we skip this line:
    #StratumPolygon <- methods::as(sfStratumPolygon_out, "Spatial")

    
    message("Used ", iteration, " iterations to obtain desired object size  (SimplificationFactor = ", SimplificationFactor, " of the original size).")
    
    
    return(StratumPolygon_out)
}




#' Extract or add stratum names from a SpatialPolygonsDataFrame
#' 
#' The stratum names must be stored as the column StratumName of the data of the \code{\link[sp]{SpatialPolygonsDataFrame}} \code{stratum}.
#' 
#' @param stratum A \code{\link[sp]{SpatialPolygonsDataFrame}} with a column StratumName of the data of the \code{\link[sp]{SpatialPolygonsDataFrame}} \code{stratum}.
#' @param StratumNameLabel The name of the attribute representing the stratum names in the GeoJSON file or shapefile. When reading WKT or txt files the StratumNameLabel is ignored, as those formats does not support column names.
#' @param check.unique Logical: If TRUE, an error is given if stratum names are not unique.
#' @param accept.wrong.name.if.only.one Logical: If TRUE, interpret stratum names if only one column in the SpatialPolygonsDataFrame.
#' 
#' @export
#' 
getStratumNames <- function(stratum, StratumNameLabel = c("StratumName", "polygonName"), check.unique = TRUE, accept.wrong.name.if.only.one = FALSE) {
    
    
    #if("SpatialPolygonsDataFrame" %in% class(stratum) || is.data.frame(stratum)) {
        
        # No names for empty polygons:
        if(!length(stratum) || !nrow(stratum)) {
            return(character())
        }
        
        # Look for the strings given by StratumNameLabel in the names of the SpatialPolygonsDataFrame stratum:
        StratumNameLabel <- intersect(StratumNameLabel, names(stratum))
        if(!length(StratumNameLabel)) {
            # If there is only one column, accept this as stratum names if accept.wrong.name.if.only.one, regardless of StratumNameLabel:
            if(accept.wrong.name.if.only.one && ncol(stratum) == 1) {
                StratumNameLabel <- names(stratum)
            }
            else {
                stop("StratumNameLabel must be the name of one of the attributes of the GeoJSON/shapefile. Use one of the following attributes: ", paste(names(stratum), collapse = ", "), ".")
            }
        }
        else {
            StratumNameLabel <- StratumNameLabel[1]
        }
        
        # Get the stratum names:
        stratumNames <- as.character(stratum[[StratumNameLabel]])
        
        # Test uniqueness:
        if(check.unique  && !identical(stratumNames, unique(stratumNames))) {
            stop("Stratum names must be unique.")
        }
        
        #sapply(methods::slot(stratum, "polygons"), function(x) methods::slot(x, "ID"))
    #}
    #else {
    #    stop("Stratum polygon must be of class SpatialPolygonsDataFrame")
    #}
    
    return(stratumNames)
}
#' 
#' @rdname getStratumNames
#' @export
#' 
addStratumNames <- function(stratum, StratumNameLabel = c("StratumName", "polygonName"), check.unique = TRUE, accept.wrong.name.if.only.one = FALSE) {
    
    if(!NROW(stratum)) {
        return(stratum)
    }
    
    stratumNames <- getStratumNames(
        stratum, 
        StratumNameLabel = StratumNameLabel, 
        check.unique = check.unique, 
        accept.wrong.name.if.only.one = accept.wrong.name.if.only.one
    )
    
    row.names(stratum) <- stratumNames
    # This ensures that stratum polygons stored in the project.json with different StratumNameLabel than those definend in the parameter declaration will be stored properly on save:
    if(accept.wrong.name.if.only.one && NCOL(stratum) == 1) {
        names(stratum) <- "StratumName"
    }
    
    # Delete the columns (except the geometry column which sticks)  and then add the "StratumName" column holding the stratum names:
    stratum <- stratum[, NULL]
    stratum$StratumName <- stratumNames
    # Place geometry last, as this seems to be the default in sf, e.g. in st_cast:
    newOrder <- c(setdiff(names(stratum), "geometry"), "geometry")
    stratum <- stratum[, newOrder]
    
    
    return(stratum)
}


##################################################
##################################################
#' Convert StratumPolygon to list of polygon tables
#' 
#' This function extracts the polygons as a named list of tables of class \code{\link[data.table]{data.table}}
#' 
#' @inheritParams StratumArea
#' 
#' @return
#' A table of stratum name and area.
#' 
#' @seealso \code{\link{DefineStratumPolygon}} for the \code{StratumPolygon} input to the function.
#' 
#' @export
#' 
getStratumPolygonList <- function(StratumPolygon) {
    # Accept StratumPolygon contained in a list:
    if(is.list(StratumPolygon) && "StratumPolygon" %in% names(StratumPolygon)) {
        StratumPolygon <- StratumPolygon$StratumPolygon
    }
    # Get the names of the polygons:
    StratumNames <- names(StratumPolygon)
    # Get a list of the coordinates:
    stratumPolygonList <- lapply(StratumPolygon@polygons, function(x) data.table::as.data.table(x@Polygons[[1]]@coords))
    # Set the names of the list:
    names(stratumPolygonList) <- StratumNames
    
    stratumPolygonList
}


##################################################
##################################################
#' Calculate area of each stratum
#' 
#' This function calculated the area of each stratum.
#' 
#' @inheritParams ProcessData
#' @param AreaMethod The method to use for the area calculation, defaulted to "Accurate", which applied a Lambert azimuthal equal area projection. 
#' 
#' @details
#' The area output is given in international square nautical miles. 
#' 
#' The \code{AreaMethod} "Accurate" calculates each stratum are using the function \code{\link[sf]{st_area}} with the stratum transformed to Cartesian coordinates using the WGS84 Lambert Azimuthal Equal Area projection with origin at the centroid of the stratum as calculated from the geographical coordinates (longitude, latitude).
#' 
#' The \code{AreaMethod} "Simple" is used in StoX 2.7 and earlier versions and kept for backwards compatibility.
#'  
#' @return
#' An object of StoX data type \code{\link{StratumAreaData}}.
#' 
#' @seealso \code{\link{DefineStratumPolygon}} for the \code{StratumPolygon} input to the function.
#' 
#' @export
#'
StratumArea <- function(
    StratumPolygon, 
    AreaMethod = c("Accurate", "Simple")
) {
    
    AreaMethod <- RstoxData::match_arg_informative(AreaMethod)
    
    # Accept StratumPolygon contained in a list:
    if(is.list(StratumPolygon) && "StratumPolygon" %in% names(StratumPolygon)) {
        StratumPolygon <- StratumPolygon$StratumPolygon
    }
    
    # Get the polygon areas:
    if(AreaMethod == "Accurate") {
        # StoX 2.7 calculated the area for each stratum separately, thus using an origin from each stratum. This requires subsetting the StratumPolygon and calculating the areas in a loop:
        turn_off_s2(
            areaDT <- data.table::rbindlist(
                lapply(
                    seq_len(nrow(StratumPolygon)), 
                    function(ind) polygonArea_accurate(StratumPolygon[ind, ], useLonLatCentroid = TRUE)
                    )
            ), 
            msg = FALSE
        )
    }
    else if(AreaMethod == "Simple") {
        areaDT <- polygonArea_simple(StratumPolygon)
    }
    else {
        stop("Invalid AreaMethod")
    }
    
    return(areaDT)
}

##################################################
StratumArea_supportingIterativeCentroidCalculation <- function(
        StratumPolygon, 
        AreaMethod = c("Accurate", "AccurateProjection", "Simple")
) {
    
    # This function was to see whether the inaccurate calculation of centriod for use as origin when transforming from geographical to Cartesian coordinates using the LEAE projection had any effect on the stratum areas. The effect was small, at maximum 1e-4 for elongated strata in the capelin BESS 2017 stratum system in the Barents Sea. Keeping the function for reference. Can be deleted with a note on which tag it exists in in git.
    AreaMethod <- RstoxData::match_arg_informative(AreaMethod)
    
    # Accept StratumPolygon contained in a list:
    if(is.list(StratumPolygon) && "StratumPolygon" %in% names(StratumPolygon)) {
        StratumPolygon <- StratumPolygon$StratumPolygon
    }
    
    # Get the polygon areas:
    if(AreaMethod %in% c("Accurate", "AccurateProjection")) {
        # StoX 2.7 calculated the area for each stratum separately, thus using an origin from each stratum. This requires subsetting the StratumPolygon and calculating the areas in a loop:
        turn_off_s2(
            areaDT <- data.table::rbindlist(lapply(seq_along(StratumPolygon), function(ind) polygonArea_accurate(StratumPolygon[ind, ], useLonLatCentroid = AreaMethod == "Accurate"))), 
            msg = FALSE
        )
    }
    else if(AreaMethod == "Simple") {
        areaDT <- polygonAreaSP_simple(StratumPolygon)
    }
    else {
        stop("Invalid AreaMethod")
    }
    
    return(areaDT)
}


turn_off_s2 <- function(expr, msg = TRUE) {
    if(!msg) {
        suppressMessages(
            turn_off_s2_keep_msg(expr)
        )
    }
    else {
        turn_off_s2_keep_msg(expr)
    }
}

turn_off_s2_keep_msg <- function(expr) {
    current_use_s2 <- getOption("sf_use_s2", default = TRUE)
    if(isTRUE(current_use_s2)) {
        options(sf_use_s2 = FALSE)
        expr
        options(sf_use_s2 = TRUE)
    }
    else {
        expr
    }
}



#DataTable <- readStoxMultipolygonWKTFromFile("~/../workspace/stox/reference/stratum/kolmule.txt")
#
#s <- DT2SpatialPolygons(DataTable)
#
#ss <- stoxMultipolygonWKT2SpatialPolygons("~/../workspace/stox/reference/stratum/kolmule.txt")
#
#identical(s, ss)




# This shows that the sp package subtracts holes from holes:
# Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
# Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
# Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
# Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
# Sr5 = Polygon(cbind(c(5.3,6,6,5.3,5.3),c(4,4,3,3,4)), hole = TRUE)
# Srs1 = Polygons(list(Sr1), "s1")

# Srs2 = Polygons(list(Sr2), "s2")
# Srs3 = Polygons(list(Sr3, Sr4, Sr5), "s3/4/5")
# SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
# plot(SpP, col = 1:3, pbg="white")




#lat <- c(60, 65, 0.7267265, 0.7233676, 0.7232196, 0.7225059)
#lon <- c(-1.512977, -1.504216, -1.499622, -1.487970, -1.443160, -1.434848)
#xym <- cbind(lon, lat)

# deg to rad function converts deg to rad
deg2rad <- function(deg) {
    deg * pi / 180
}

# polygonAreaPolygonXY wraps x and y to dataframe and calls Polygon
# this is a helper function
polygonAreaPolygonXY <- function(x, y = NULL) {
    polygonAreaPolygonDT(
        as.data.table(
            grDevices::xy.coords(x, y)[c("x", "y")]
        )
    )
}

# polygonAreaPolygonDT calculates simple GCD (great circle distance) polygon area 
# based on dataframe with 2 columns x and y
polygonAreaPolygonDT <- function(df) {
    area <- 0
    len <- nrow(df)
    for (i in seq_len(len - 1)) {
        p1 <- df[i,]
        p2 <- df[i + 1,]
        area <- area + deg2rad(p1$x - p2$x) * (2 + sin(deg2rad(p1$y)) + sin(deg2rad(p2$y)))
    }
    r <- 6371000.0 * 0.000539956803;
    abs(area * r ^ 2 / 2.0);
}


# # polygonAreaSP_simple calculates simple GCD polygon area taking SpatialPolygons or SpatialPolygonsDataFrame:
# polygonAreaSP_simple <- function(stratumPolygon) {
#     
#     # Function to create a polygon area data table of polygon area, whether the polygon is a hole, and the ID:
#     polygon2AreaDT <- function(polygon, ID) {
#         if(all(c("x", "y") %in% colnames(polygon@coords))) {
#             data.table::data.table(
#                 Area = polygonAreaPolygonXY(polygon@coords[, "x"], polygon@coords[, "y"]),
#                 IsHole = polygon@hole,
#                 ID = ID
#             )
#         }
#         else{
#             data.table::data.table(
#                 Area = polygonAreaPolygonXY(polygon@coords[, 1], polygon@coords[, 2]),
#                 IsHole = polygon@hole,
#                 ID = ID
#             )
#         }
#     }
#     
#     # Function to rbind the polygon area data tables:
#     stratumAreaDT <- function(stratum) {
#         data.table::rbindlist(lapply(stratum@Polygons, polygon2AreaDT, ID = stratum@ID))
#     }
#     # Function to calculate the area of the multipolygon, and output a data table of stratun name and area:
#     stratumArea <- function(DT) {
#         areas <- as.list(by(DT$Area, DT$IsHole, sum))
#         data.table(
#             Stratum = unique(DT$ID),
#             Area = if (length(areas) == 2) areas$"FALSE" - areas$"TRUE" else unlist(areas)
#         )
#     }
#     
#     # Get the tables of areas of individual polygons in each multipolygon:
#     d <- lapply(stratumPolygon@polygons, stratumAreaDT)
#     # Extract the multipolygon area and rbind to a data table:
#     stratumAreas <- data.table::rbindlist(lapply(d, stratumArea))
#     stratumAreas
# }



polygonAreaSimpleOnePolygon <- function(DT) {
    x1 <- utils::head(DT[[1]], -1)
    x2 <- utils::tail(DT[[1]], -1)
    y1 <- utils::head(DT[[2]], -1)
    y2 <- utils::tail(DT[[2]], -1)
    
    area <- sum(deg2rad(x1 - x2) * (2 + sin(deg2rad(y1)) + sin(deg2rad(y2))))
    
    r <- 6371000.0 * 0.000539956803
    area <- abs(area * r ^ 2 / 2.0)
    
    return(area)
}

# polygonArea_simple calculates simple GCD polygon area taking SpatialPolygons or SpatialPolygonsDataFrame:
polygonArea_simple <- function(stratumPolygon) {
    
    # Get the coordinates. The columns L1, L2 and L3 are IDs of holes, polygons and strata, respecively:
    coordinates <- data.table::as.data.table(sf::st_coordinates(stratumPolygon))
    
    # Calculate the area of each individual polygon:
    areas  <- coordinates[, .(area = polygonAreaSimpleOnePolygon(.SD)), by = c("L1", "L2", "L3")]
    
    # Sum the areas for each stratum:
    areasPerStratum <- areas[, .(sumArea = sum(area)), by  = "L3"]
    
    # Calculate the areas of the holes:
    areasOfHoles <- areas[L1 > 1, .(area_hole = sum(area)), by  = "L3"]
    
    # Merge in the areas of the holes and subtract these (twice, since the holes are included in the sumArea):
    stratumAreas <- merge(areasPerStratum, areasOfHoles, all = TRUE)
    data.table::setnafill(stratumAreas, fill = 0)
    stratumAreas <- stratumAreas[, sumArea - 2 * area_hole]
    
    output <- data.table::data.table(
        Stratum = stratumPolygon$StratumName, 
        Area = stratumAreas
    )
    
    return(output)
}

polygonArea_accurate <- function(stratumPolygon, useLonLatCentroid = TRUE) {
    
    # No need for this anymore, as we have stoped using sp:
    # Get the sf object, as we strive to use sf instead of sp:
    #stratumPolygonSF <- sf::st_as_sf(stratumPolygon)
    #sf::st_crs(stratumPolygonSF) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
    
    # Set default projection:
    sf::st_crs(stratumPolygon) <- getRstoxBaseDefinitions("proj4string_longlat")
    
    # Removed this on 2021.02.01, as we rather should ensure that the projection is added in the stratumPolygon:
    #stratumPolygon1 <- sf::st_transform(ss, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    
    
    # Define projection:
            #sp::proj4string(stratumPolygon) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    
    # Define the proj4 definition of Lambert Azimuthal Equal Area (laea) CRS with origo in wkt center:
    # Units: international nautical miles:
            #laea.CRS <- sp::CRS(
            #    paste0(
            #        "+proj=laea +lat_0=", 
            #        stratumPolygon@polygons[[1]]@labpt[2], 
            #        " +lon_0=", 
            #        stratumPolygon@polygons[[1]]@labpt[1], 
            #        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=kmi +no_defs"
            #    )
            #)
    
    # New as of 2021-02-04 (RstoxBase 1.2.48): Calculate the polygon centroids using rgeos::gCentroid, as the labpt is pre-calculated and can in principle be modified:
    #laea.CRS <- paste0(
    #    "+proj=laea +lat_0=", 
    #    stratumPolygon@polygons[[1]]@labpt[2], 
    #    " +lon_0=", 
    #    stratumPolygon@polygons[[1]]@labpt[1], 
    #    " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=kmi +no_defs"
    #)
    
    # Re-project for equal area projection, using the centroid of the multipolygon:
    
    # rgeos is retiring, so we use sf instead to get the centroid:
    centroid <- getCentroid(stratumPolygon, iterativeCentroidCalculation = !useLonLatCentroid)
    
    #centroid <- rgeos::gCentroid(stratumPolygon)@coords
    laea.CRS <- addCentroidToProjection(
        getRstoxBaseDefinitions("proj4string_laea"), 
        centroid = centroid, 
        additionalFields = " +x_0=0 +y_0=0 +units=kmi"
    )
    
    stratumPolygon <- sf::st_transform(stratumPolygon, laea.CRS)
    
    ## Convert from sf to spatial and calculate area:
    #stratumPolygon <- sf::as_Spatial(stratumPolygonSF)
    #area <- rgeos::gArea(stratumPolygon, byid = TRUE)
    
    # Use sf to calculate the area, temporarily switching off s2:
    area <- sf::st_area(stratumPolygon)
    
    # No longer needed, as we only use sf:
    # Convert back from sf to sp:
    #stratumPolygon <- sf::as_Spatial(stratumPolygonSF)
    
    # Output the stratum names and areas:
    output <- data.table::data.table(
        Stratum = getStratumNames(stratumPolygon),
        Area = units::drop_units(area)
    )
    return(output)
}


#' Add centroid and possibly other fields to an existing proj4String:
#' 
#' @param proj4String A proj4 string.
#' @param centroid Either a vector of longitude and latitude value, or a named list of this information. 
#' @param lon_name,lat_name Character: The names of the longitude and latitude fields in \code{centroid} if given as a list.
#' @param additionalFields Character: Optional extra proj4 fields in a single string.
#' 
addCentroidToProjection <- function(proj4String, centroid, lon_name = "lon", lat_name = "lat", additionalFields = NULL) {
    if(!is.list(centroid)) {
        centroid <- structure(as.list(centroid), names = c(lon_name, lat_name))
    }
    paste0(
        proj4String, 
        " +lat_0=", 
        centroid[[lat_name]], 
        " +lon_0=", 
        centroid[[lon_name]], 
        additionalFields
    )
}



# Get the true centroid of a polygon, as calculated in x,y after projecting with aeqd (as default) to using the centroid in longitude,latitude as origin, transfoming the x,y centroid to longitude,latitude, and repeating the procecss until the centroid in longitude,latitude no longer change.
getCentroid <- function(stratumPolygon, par = list(), tol = 1e-9, msg = FALSE, iterativeCentroidCalculation = FALSE) {
    
    # Calculate the centroid iteratively in Cartesian coordinates using the continuously updated geographical centroid as projection origin:
    if(iterativeCentroidCalculation) {
        # Set default projection. Not great, but we suppress the warning "st_crs<- : replacing crs does not reproject data; use st_transform for that ":
        suppressWarnings(sf::st_crs(stratumPolygon) <- getRstoxBaseDefinitions("proj4string_longlat"))
        
        # Get the centroid for longitude/latitude, which is not correct, but may not be too far away:
        suppressWarnings(centroidLongLat <- sf::st_coordinates(sf::st_centroid(sf::st_combine(stratumPolygon))))
        
        # Iterate to find the true centroid:
        diff <- 1
        iter <- 0
        while(diff > tol) {
            centroidLongLatNew <- getCentroidOne(stratumPolygon, centroidLongLat, par = par)
            
            diff <- max((centroidLongLatNew - centroidLongLat) / centroidLongLat)
            centroidLongLat <- centroidLongLatNew
            iter <- iter + 1
        }
        if(msg) {
            message("Using ", iter, " iterations to calculate the centroid.")
        }
    }
    # Centroid in geographica coordinates, suppressing warning of inaccurate calculation:
    else {
        suppressWarnings(centroidLongLat <- sf::st_coordinates(sf::st_centroid(sf::st_combine(stratumPolygon))))
    }
    
    
    
    
    return(centroidLongLat)
}

# Function to get the centroid of a polygon in sf by projecting onto x,y using Azimuthal Equidistant projection (aeqd) projection at the given origin, calculating the centroid in 2-D, and projecting back to longitude,latitude. So the true centroid as seen by the aeqd projection at the given origin. The function considers the entire polygon by applying sf::st_combine() before getting the centroid:
getCentroidOne <- function(polygon, centroidLongLat, par = list()) {
    
    # Transform to x/y using Azimuthal Equidistant projection (aeqd) and re-calculate the centroid:
    aeqd.CRS <- paste0(
        "+proj=aeqd +lat_0=", 
        centroidLongLat[2], 
        " +lon_0=", 
        centroidLongLat[1], 
        " +ellps=WGS84 +units=kmi +no_defs"
    )
    aeqd.CRS <- modifyProj4String(aeqd.CRS, par)
    polygon_XY <- sf::st_transform(polygon, aeqd.CRS)
    # Recalculate centroid in x/y:
    centroidXY <- sf::st_centroid(sf::st_combine(polygon_XY))
    
    # Convert back to longitude/latitude:
    longlat.CRS <- modifyProj4String(aeqd.CRS, list(proj = "longlat", lon_0 = 0, lat_0 = 0))
    
    centroidLongLat_updated <- sf::st_coordinates(sf::st_transform(centroidXY, longlat.CRS))
    
    return(centroidLongLat_updated)
}



# Function to get the stratum of each PSU, taken as the most frequent Stratum in which the PSU is loacted geographically:
locateInStratum <- function(points, stratumPolygon, SSULabel, locationProjection = c("lonlat", "leae"), iterativeCentroidCalculation = FALSE, coords = c("Longitude", "Latitude")) {
    
    # We use sp::over(), as it is reported to be faster than sp::point.in.polygon() for complicated polygons, and also handles multipolygons better (https://stackoverflow.com/questions/32644301/r-point-in-polygon-speed-slow). Also, there does not seem to be stong enough reasons for using sf::st_intersects() (https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package).
    
    # Get the label and names of the points for use in the warnings:
    pointNames <- points[[SSULabel]]
    
    # Create an sf POINTS objects:
    points <- sf::st_as_sf(
        subset(points, select = coords), 
        coords = coords, 
        crs = getRstoxBaseDefinitions("proj4string_longlat")
    )
    
    # No longer needed, as all stratumPolygon are sf:
    #stratumPolygon <- sf::st_as_sf(stratumPolygon)
    
    # Make sure the stratumPolygon has the same projection. We do this since stratumPolygon is an input and could potentially have a different projection, althoug the should not be the case:
    sf::st_crs(stratumPolygon) <- getRstoxBaseDefinitions("proj4string_longlat")
    
    # Support using Lambert azimuthal equal area for potential future use, and for comparison:
    locationProjection <- RstoxData::match_arg_informative(locationProjection)
    if (locationProjection == "leae") {
        # Get the true centroid of the entire StratumPolygon:
        centroid <- getCentroid(StratumPolygon, iterativeCentroidCalculation = iterativeCentroidCalculation)
        
        # Define the projection to use when transforming to Cartesian coordinates:
        proj4String <- getRstoxBaseDefinitions("proj4string_laea")
        proj4String <- modifyProj4String(proj4String, par = list(lon_0 = centroid[1], lat_0 = centroid[2]))
        
        points <- sf::st_transform(points, proj4String)
        stratumPolygon <- sf::st_transform(stratumPolygon, proj4String)
    }
    
    # Locate in the strata:
    #stratumNames <- getStratumNames(sp::over(points, stratumPolygon), check.unique = FALSE)
    
    hits <- sf::st_intersects(points, stratumPolygon)
    
    
    # Detect points located in more than one stratum:
    numberOfStrata <- lengths(lapply(split(hits, seq_along(hits)), unlist))
    atMultipleStrata <- numberOfStrata > 1
    at0Strata <- numberOfStrata == 0
    if(any(atMultipleStrata)) {
        bad <- pointNames[atMultipleStrata]
        warning("The following ", SSULabel, " was detected in more than one stratum. The first stratum was selected:", RstoxData::printErrorIDs(bad))
    }
    if(any(at0Strata)) {
        bad <- pointNames[at0Strata]
        warning("StoX: The following ", SSULabel, if(length(bad) > 1) "s", " were not detected in any stratum:", RstoxData::printErrorIDs(bad))
    }
    
    ind <- lapply(hits, utils::head, 1)
    ind[at0Strata] <- NA
    ind <- unlist(ind)
    
    stratumNames <- getStratumNames(stratumPolygon)[ind]
    
    return(stratumNames)
}










#' Define a proj4 string
#' 
#' @param x				    A proj4 string or a list of proj4 parameters to modify.
#' @param par				A proj4 string or a list of proj4 parameters to modify by.
#' @param list.out			Logical: If TRUE the projection info is returned as a list instead of a concatenate string.
#'
modifyProj4String <- function(x, par, list.out = FALSE) {
    
    # Interpret the inputs as proj4 string:
    if (is.character(x)) {
        x <- proj4string2proj4list(x)
    }
    if (is.character(par)) {
        par <- proj4string2proj4list(par)
    }
    
    # Save the names of the original list:
    xNames <- names(x)
    # Include the freely specified args, set to override existing definitions:
    par <- c(par, x)
    # Get the CRS, using only the first of repeated parameters:
    par <- par[!duplicated(names(par))]
    
    # Order so that the original parameters are first:
    namesInOriginal <- names(par) %in% xNames
    par <- c(par[xNames], par[!namesInOriginal])
    
    # Convert the args to a string:
    if (!list.out) {
        # Add the +:
        parNamesPlus <- paste0("+", names(par))
        parString <- paste0(parNamesPlus, "=", unlist(par, recursive = FALSE))
        
        # Identify non-set parameters:
        nonSet <- is.na(par)
        if(any(nonSet)) {
            parString[nonSet] <- parNamesPlus[nonSet]
        }
        
        par <- paste(parString, collapse = " ")
    }
    
    par
}


proj4string2proj4list <- function(proj4string) {
    if(!nchar(proj4string)) {
        warning("proj4string does not contain a proj4 string.")
        return(proj4string)
    }
    parSplit <- strsplit(proj4string, " ", fixed = TRUE)[[1]]
    parSplit <- mapply(gsub, MoreArgs = list(pattern = "[[:blank:]]", replacement = ""), x = parSplit, SIMPLIFY = FALSE)
    parSplit <- lapply(parSplit, function(x) if(grepl("=", x)) strsplit(x, "=")[[1]] else x)
    parSplitNames <- sapply(parSplit, utils::head, 1)
    parSplitNames <- sub(".*?\\+", "", parSplitNames)
    parSplit <- lapply(parSplit, "[", 2)
    names(parSplit) <- parSplitNames
    return(parSplit)
}


#' Convert a data table to individual LINESTRINGs
#' 
#' @param x A a data.table or data.frame.
#' @param x1y1x2y2 Character: A vector of the names of start x, and end x, start y and end y coordinates.
#' @param idCol Character: A vector of the names of columns to use as ID columns in the returned sf object.
#' @inheritParams sf::st_sfc
#' 
#' @export
#'
dataTable2sf_LINESTRING <- function(
    x, 
    x1y1x2y2 = c("startLongitude", "startLatitude", "endLongitude", "endLatitude"), 
    idCol = NULL, 
    crs = NULL
) {
    
    # Create a geometry column:
    linestrings  <- data.frame(geometry = sf::st_sfc(sapply(seq_len(nrow(x)), function(i) create_segment(x[i, ..x1y1x2y2]), simplify = FALSE)))
    
    # Add info:
    if(length(idCol) && idCol %in% names(x)) {
        linestrings[[idCol]] <- x[, ..idCol]
    } 
    
    # Convert to proper sf data frame:
    linestrings <-  sf::st_sf(linestrings)
    
    # Set projection:
    sf::st_crs(linestrings) <- if(length(crs)) crs else RstoxBase::getRstoxBaseDefinitions("proj4string_longlat")
    
    return(linestrings)                   
}

# Simple function to create a segment:
create_segment <- function(r) {
    sf::st_linestring(t(matrix(unlist(r), 2, 2)))
}


#' Convert a data table to individual POINTs
#' 
#' @param x A a data.table or data.frame.
#' @param coords Character: A vector of the names of x and y coordinates.
#' @param idCol Character: A vector of the names of columns to use as ID columns in the returned sf object.
#' @inheritParams sf::st_sfc
#' 
#' @export
#'
dataTable2sf_POINT <- function(x, coords = c("Longitude", "Latitude"), idCol = NULL, crs = NULL) {
    
    # Keep only the idCol and the coords:
    pos <- subset(x, select = c(idCol, coords))
    
    # Convert to POINT sf object:
    points <- sf::st_as_sf(pos, coords = coords)
    # Set projection:
    sf::st_crs(points) <- if(length(crs)) crs else RstoxBase::getRstoxBaseDefinitions("proj4string_longlat")
    
    return(points)
}




#' Change precision of an sf object
#' 
#' @param x An sf object.
#' @param digits The number of digits of the output.
#' 
#' @export
#'
setPrecisionToSF <- function(x, digits) {
    
    precision <- 10^digits
    
    # Write the data to a file to utilize the function sf::st_set_precision:
    outdata <- sf::st_set_precision(x, precision = precision)
    tmpDir <- file.path(tempdir(), "shapefile")
    dir.create(tmpDir)
    tmpFile <- file.path(tmpDir, "nc.shp")
    
    # Keep the stratum names and crs: 
    stratumNames <- outdata$StratumName
    crs <- sf::st_crs(outdata)
    
    # Write the multipolygons to a temporary file (suppress the name abbreviation warning "Field names abbreviated for ESRI Shapefile driver"):
    suppressWarnings(sf::st_write(outdata, tmpFile, quiet = TRUE))
    x <- sf::st_read(tmpFile, quiet = TRUE)
    
    # Add the stratum names and crs again: 
    x <- x[, NULL]
    x$StratumName <- stratumNames
    # Suppress "replacing crs does not reproject data; use st_transform for that":
    suppressWarnings(sf::st_crs(x) <- crs)
    # sf::st_read may read as POLYGON. We want MULTIPOLYGON always:
    x <- sf::st_cast(x, "MULTIPOLYGON")
    
    # Place geometry last, as this seems to be the default in sf, e.g. in st_cast:
    newOrder <- c(setdiff(names(x), "geometry"), "geometry")
    x <- x[, newOrder]
    
    # delete the shape files:
    unlink(tmpDir, recursive = TRUE)
    
    return(x)
}



