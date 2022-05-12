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
    names(tab) <- c("Stratum", "Polygon")
    
    ### # Do a check for scandinavian letters given as hex code, and issue an error stating that the file needs to be converted to UTF-8:
    ### scandinavianLettersAsHex <- c("\xe6", "\xf8", "\xe5", "\xc6", "\xd8", "\xc5")
    invalidUTF8 <- detectInvalidUTF8(tab$Stratum)
    if(any(invalidUTF8)) {
        stop("The file ", FilePath, " contains characters indicating that the file was saved with an encoding different from the required UTF-8 (scandinavian letters, possibly saved as Latin 1). Please open the file in a text editor, verify that all characters appear as exptected, and save the file with encodnig UTF-8.")
    }
    
    # Stop also if stratum names are duplicated:
    if(any(duplicated(tab$Stratum))) {
        stop("The file ", FilePath, " contains duplicated polygon names (column 1). Please open the file in a text editor and rename or delete the duplicated strata: ", paste(paste0("\"", unique(tab$Stratum[duplicated(tab$Stratum)]), "\""), collapse = ", "), ".")
    }
    
    return(tab)
}




dataTable2SpatialPolygonsDataFrame <- function(DataTable) {
    # 
    StratumName <- as.character(DataTable$Stratum)
    multipolygon <- DataTable$Polygon
    
    # Convert each WKT strings to SpatialPolygonsDataFrame:
    spatialPolygonsList <- lapply(multipolygon, rgeos::readWKT)
    # Extract the Polygons objects to modify the IDs and merge to a SpatialPolygonsDataFrame:
    polygonsList <- lapply(spatialPolygonsList, function(x) methods::slot(x, "polygons")[[1]])
    # Add the polygon names as IDs:
    for (ind in seq_along(polygonsList)) {
        polygonsList[[ind]]@ID <- StratumName[ind]
    }
    # Merge to a SpatialPolygonsDataFrame object:
    data <- data.frame(
        StratumName = StratumName, 
        stringsAsFactors = FALSE
    )
    rownames(data) <- StratumName
    # Create a SpatialPolygons object, adding default longitude-latitude projection:
    # We decided on 2020-01-21 to skip proj4string, and always assume WGS84:
    #spatialPolygons = sp::SpatialPolygons(polygonsList, proj4string = getRstoxBaseDefinitions("proj4string"))
    spatialPolygons = sp::SpatialPolygons(polygonsList)
    # ... and convert to SpatialPolygonsDataFrame:
    spatialPolygonsDataFrame = sp::SpatialPolygonsDataFrame(spatialPolygons, data = data, match.ID = FALSE)
    #plot(SpP, col = 1:5, pbg="white")
    
    # Define default projection:
    sp::proj4string(spatialPolygonsDataFrame) <- getRstoxBaseDefinitions("proj4string")
    
    return(spatialPolygonsDataFrame)
}



stoxMultipolygonWKT2SpatialPolygonsDataFrame <- function(FilePath) {
    
    # Read the file as data.table:
    dataTable <- readStoxMultipolygonWKTFromFile(FilePath)
    # Convert to SpatialPolygonsDataFrame:
    dataTable2SpatialPolygonsDataFrame(dataTable)
}


##################################################
##################################################
#' Define stratum multipolygon
#' 
#' This function reads a \href{https://geojson.org/}{GeoJSON} file, \href{https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm}{shapefile} or a \code{\link{StoX_multipolygon_WKT}} file and returns an object of StoX data type \code{\link{StratumPolygon}} file.
#' 
#' @inheritParams general_arguments
#' @inheritParams getStratumNames
#' @param DefinitionMethod A string naming the method to use, one of "ResourceFile", to read the file \code{FileName} holding the stratum multipolygon, or "Manual" to start off with no strata and create the strata manually in the map of the StoX GUI. Strata can be added, modified and removed in the StoX GUI.
#' @param FileName The path to a \href{https://geojson.org/}{GeoJSON} file, \href{https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm}{shapefile} (may be a folder) or a \code{\link{StoX_multipolygon_WKT}} file. Must include file extension.
#' @param SimplifyStratumPolygon Logical: If TRUE a simplification algorithm is applied to the stratum polygons (sf::st_simplify(), which "uses the GEOS implementation of the Douglas-Peucker algorithm to reduce the vertex count" as per quote from \href{https://geocompr.robinlovelace.net/geometric-operations.html#simplification}{Geocomputation with R}). This option is useful for very complex stratum polygon files to reduce time for opening, running and saving a project.
#' @param SimplificationFactor A value between 0 and 1 specifying the desired object size of the stratum polygon after simplification. An iterative method is used to derive the to the desired object size. Strata that end up as empty strata after simplification are left un-simplified, and will contribute to a larger final object size than the desired object size. Note that larger values require more  time than smaller values. It is advisable to start with a low value (e.g. 0.01) and check the resolution of the strata borders in the map, and gradually increase \code{SimplificationFactor} until an acceptable resolution is obtained. Note that simplification of stratum polygons can lead to inaccuracies such as gaps between strata, which can lead to hauls falling outside of any stratum, causing under-estimation of survey indices, or failure to complete the model in the case of SplitNASC models which require all hauls to be located in a stratum. If such problems occur, the strata can be modified manually after simplification to include all hauls. Another problem is the change in stratum areas, which is reported to the console when running the simplification. A very rough rule of thumb is that \code{SimplificationFactor} = 0.1 seems to be a compromise between increased speed and reduction of stratum border resolution.
#' 
#' @details
#' The parameter \code{UseProcessData} is always set to TRUE when running a process, and needs to be explicitely set to FALSE to enable reading a file (It's set to FALSE at the moment).
#' 
#' Polygons are expected to have the the following projection: getRstoxBaseDefinitions("proj4string")
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
    DefinitionMethod = c("ResourceFile", "Manual"), 
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
    DefinitionMethod <- match.arg(DefinitionMethod)
    
    # Read the file:
    if(grepl("ResourceFile", DefinitionMethod, ignore.case = TRUE)) {
        
        if(length(unlist(strsplit(FileName, "\\."))) < 2) {
            stop("FileName must include file extension.")
        }
        else {
            fileParts <- unlist(strsplit(FileName, "\\."))
            FileExt <- utils::tail(fileParts, 1)
        }
        
        if(tolower(FileExt) %in% c("wkt", "txt")) {
            StratumNameLabel <- "StratumName"
            StratumPolygon <- stoxMultipolygonWKT2SpatialPolygonsDataFrame(FileName)
        }
        # If the FileName is a shapefile, or a directory with a shapefile:
        else if(tolower(FileExt) == "shp" || (isTRUE(file.info(FileName)$isdir) && any(tools::file_ext(list.files(FileName)) == "shp"))) {
            # On 2020-12-19 we got rid of rgdal, which is slower for reading shapefiles than sf:
            #StratumPolygon <- rgdal::readOGR(FileName, verbose = FALSE)
            
            # FileName can the the path to one of the shapefile or to the directory holding the shapefiles:
            StratumPolygon <- sf::as_Spatial(sf::read_sf(FileName))
        }
        else if(tolower(FileExt) %in% c("json", "geojson")) {
            # On 2020-12-19 we got rid of rgdal, which is slower for reading shapefiles than sf:
            #if(!"GeoJSON" %in% rgdal::ogrDrivers()$name) {
            #    stop("rgdal::ogrDrivers does not contain GeoJSON format. Cannot read these types of files. Install the driver or change fi#le format.")
            #}
            #
            #StratumPolygon <- rgdal::readOGR(FileName, "OGRGeoJSON")
            
            #StratumPolygon <- geojsonio::geojson_sp(
            #    toJSON_Rstox(
            #        geojsonio::geojson_read(
            #            FileName
            #        ), 
            #        pretty = TRUE
            #    )
            #)
            
            # StratumPolygon <-  sf::as_Spatial(geojsonsf::geojson_sf(FileName))
            
            StratumPolygon <- readGeoJSON(FileName)
        }
        else if(tolower(FileExt) == "xml" && any(grepl("http://www.imr.no/formats/stox/v1", readLines(FileName, 5)))) {
            # Read the StratumPolygon from the project.xml file:
            StratumPolygon <- readStratumPolygonFrom2.7(FileName, remove_includeintotal = TRUE)
            
            # Convert to SpatialPolygonsDataFrame:
            StratumPolygon <- dataTable2SpatialPolygonsDataFrame(StratumPolygon)
        }
        else {
            stop(paste("File extension", FileExt, "not supported yet. Contact the StoX developers."))
        }
        
        # Assume the default projection:
        suppressWarnings(sp::proj4string(StratumPolygon) <- RstoxBase::getRstoxBaseDefinitions("proj4string"))
    }
    else if(grepl("Manual", DefinitionMethod, ignore.case = TRUE)) {
        StratumPolygon <- getRstoxBaseDefinitions("emptyStratumPolygon")
    }
    else {
        stop("Inavlid DefinitionMethod")
    }
    
    # Add an attribute named StratumName:
    if(NROW(StratumPolygon)) {
        StratumPolygon <- addStratumNames(
            StratumPolygon, 
            StratumNameLabel = StratumNameLabel
        )
    }
    
    if(isTRUE(SimplifyStratumPolygon)) {
        StratumPolygon <- simplifyStratumPolygon(
            StratumPolygon = StratumPolygon, 
            SimplificationFactor = SimplificationFactor, 
            preserveTopology = FALSE
        )
    }
    
    return(StratumPolygon)
}


simplifyStratumPolygon <- function(
    StratumPolygon = StratumPolygon, 
    SimplificationFactor = SimplificationFactor, 
    preserveTopology = FALSE
) {
    
    if(SimplificationFactor <= 0 || SimplificationFactor >= 1) {
        stop("SimplificationFactor must be between 0 and 1.")
    }
    
    # Use the sf::st_simplify, but turn off s2, as it does not perform well on longitude-latitude data due to the us off infinite lines instead of lines along the great circle (https://r-spatial.github.io/sf/articles/sf7.html, and https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data):
    sf::sf_use_s2(FALSE)
    # Transform to sf:
    sfStratumPolygon <- sf::st_as_sf(StratumPolygon)
    
    
    # Iterate to find the object size SimplificationFactor as a fraction of the original utils::object.size:
    originalSize <- utils::object.size(sfStratumPolygon)
    size <- originalSize
    desiredSize <- originalSize * SimplificationFactor
    margin <- 0.001 * originalSize
    
    # Define objects to update in the loop:
    down <- TRUE
    dTolerance <- 1e3
    #dTolerance_preivous <- dTolerance
    scalingFactor <- 0.1
    iteration <- 0
    sfStratumPolygon_temp <- NULL
    lastDiffInTolerance <- dTolerance * scalingFactor
    root <- 1
    
    # Run the loop and stop when the object size is close enough to the desired size:
    while(abs(size - desiredSize) > margin) {
        
        iteration <- iteration + 1
        dTolerance_preivous <- dTolerance
        
        # Reduce or increase the tolerance value:
        if(!down) {
            root <- root / 2
        }
        dTolerance <- dTolerance * scalingFactor^root
        
        # Simplify.
        suppressWarnings(sfStratumPolygon_temp <- sf::st_simplify(
            sfStratumPolygon, 
            dTolerance = dTolerance, 
            preserveTopology = preserveTopology
        ))
        
        # Update size and check if we should go down or up in tolerance:
        size <- utils::object.size(sfStratumPolygon_temp)
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
    }
    
    # Get the area of the strata, and replace empty strata by the original:
    area0 <- sf::st_area(sfStratumPolygon)
    area <- sf::st_area(sfStratumPolygon_temp)
    StratumAreaTable <- data.table::data.table(
        Stratum = sfStratumPolygon_temp$StratumName, 
        OriginalArea = area0, 
        SimplifiedArea = area
    )
    
    # Issue a warning iff any strata are smaller than 1 square meter:
    empty <- area < units::set_units(1,  "m^2")
    if(any(empty)) {
        warning("StoX: The following strata was empty after simplification (less than 1 square meter), and were replaced by the original strata: \n", paste0(utils::capture.output(StratumAreaTable[empty]), collapse = "\n"))
    }
    
    # Replace empty strata by the original:
    sfStratumPolygon_temp$geometry[empty] <- sfStratumPolygon$geometry[empty]
    
    StratumPolygon <- methods::as(sfStratumPolygon_temp, "Spatial")

    
    message("Used ", iteration, " iterations to obtain desired object size  (SimplificationFactor = ", SimplificationFactor, " of the original size).")
    
    
    return(StratumPolygon)
}




##################################################
##################################################
#' Read geojson file
#' 
#' @param FileName The path to a \href{https://geojson.org/}{GeoJSON} file.
#' 
#' @export
#'
readGeoJSON <- function(FileName) {
    sf::as_Spatial(geojsonsf::geojson_sf(FileName))
}



#' Extract or add stratum names from a SpatialPolygonsDataFrame
#' 
#'  The stratum names must be stored as the column StratumName of the data of the \code{\link[sp]{SpatialPolygonsDataFrame}} \code{stratum}.
#' 
#' @param stratum A \code{\link[sp]{SpatialPolygonsDataFrame}} with a column StratumName of the data of the \code{\link[sp]{SpatialPolygonsDataFrame}} \code{stratum}.
#' @param StratumNameLabel The name of the attribute representing the stratum names in the GeoJSON file or shapefile.
#' @param check.unique Logical: If TRUE, an error is given if stratum names are not unique.
#' @param accept.wrong.name.if.only.one Logical: If TRUE, interpret stratum names if only one column in the SpatialPolygonsDataFrame.
#' 
#' @export
#' 
getStratumNames <- function(stratum, StratumNameLabel = c("StratumName", "polygonName"), check.unique = TRUE, accept.wrong.name.if.only.one = FALSE) {
    
    if("SpatialPolygonsDataFrame" %in% class(stratum) || is.data.frame(stratum)) {
        
        # No names for empty polygons:
        if(!length(stratum)) {
            return(NA_character_)
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
    }
    else {
        stop("Stratum polygon must be of class SpatialPolygonsDataFrame")
    }
    
    return(stratumNames)
}
#' 
#' @rdname getStratumNames
#' @export
#' 
addStratumNames <- function(stratum, StratumNameLabel = c("StratumName", "polygonName"), check.unique = TRUE, accept.wrong.name.if.only.one = FALSE) {
    
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
    
    # Create a new SpatialPolygonsDataFrame with only one column being the "StratumName" column holding the stratum names:
    stratum <- stratum[, NULL]
    stratum$StratumName <- stratumNames
    
    
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
#' @param AreaMethod The method to use for the area calculation, defaulted to "Accurate", which applied a lambert azimuthal equal area projection. 
#' 
#' @details
#' The area output is given in international square nautical miles. 
#' 
#' The \code{AreaMethod} "Accurate" projects the latitude and longitude startum polygons to Lambert Azimuthal Equal Area with origo in wkt center and coordinate reference system (CRS="+proj=longlat +ellps=WGS84") The areas are calculated using \code{\link[rgeos]{gArea}}.
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
    
    AreaMethod <- match.arg(AreaMethod)
    
    # Accept StratumPolygon contained in a list:
    if(is.list(StratumPolygon) && "StratumPolygon" %in% names(StratumPolygon)) {
        StratumPolygon <- StratumPolygon$StratumPolygon
    }
    
    # Get the polygon areas:
    if(AreaMethod == "Accurate") {
        # StoX 2.7 calculated the area for each stratum separately, thus using an origin from each stratum. This requires subsetting the StratumPolygon and calculating the areas in a loop:
        areaDT <- data.table::rbindlist(lapply(seq_along(StratumPolygon), function(ind) polygonAreaSP_accurate(StratumPolygon[ind, ])))
    }
    else if(AreaMethod == "Simple") {
        areaDT <- polygonAreaSP_simple(StratumPolygon)
    }
    else {
        stop("Invalid AreaMethod")
    }
    
    # Ensure that the numeric values are rounded to the defined number of digits:
    #RstoxData::setRstoxPrecisionLevel(areaDT)
    
    return(areaDT)
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


# polygonAreaSP_simple calculates simple GCD polygon area taking SpatialPolygons or SpatialPolygonsDataFrame:
polygonAreaSP_simple <- function(stratumPolygon) {
    
    # Function to create a polygon area data table of polygon area, whether the polygon is a hole, and the ID:
    polygon2AreaDT <- function(polygon, ID) {
        data.table::data.table(
            Area = polygonAreaPolygonXY(polygon@coords[, "x"], polygon@coords[, "y"]),
            IsHole = polygon@hole,
            ID = ID
        )
    }
    
    # Function to rbind the polygon area data tables:
    stratumAreaDT <- function(stratum) {
        data.table::rbindlist(lapply(stratum@Polygons, polygon2AreaDT, ID = stratum@ID))
    }
    # Function to calculate the area of the multipolygon, and output a data table of stratun name and area:
    stratumArea <- function(DT) {
        areas <- as.list(by(DT$Area, DT$IsHole, sum))
        data.table(
            Stratum = DT$ID,
            Area = if (length(areas) == 2) areas$"FALSE" - areas$"TRUE" else unlist(areas)
        )
    }
    
    # Get the tables of areas of individual polygons in each multipolygon:
    d <- lapply(stratumPolygon@polygons, stratumAreaDT)
    # Extract the multipolygon area and rbind to a data table:
    stratumAreas <- data.table::rbindlist(lapply(d, stratumArea))
    stratumAreas
}

polygonAreaSP_accurate <- function(stratumPolygon) {
    
    
    stratumPolygonSF <- sf::st_as_sf(stratumPolygon)
    #sf::st_crs(stratumPolygonSF) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
    
    # Removed this on 2021.02.01, as we rather should ensure that the projection is added in the stratumPolygon:
    sf::st_crs(stratumPolygonSF) <- getRstoxBaseDefinitions("proj4string")
    
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
    centroid <- rgeos::gCentroid(stratumPolygon)@coords
    laea.CRS <- paste0(
        "+proj=laea +lat_0=", 
        centroid[2], 
        " +lon_0=", 
        centroid[1], 
        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=kmi +no_defs"
    )
    
    # Re-project:
    stratumPolygonSF <- sf::st_transform(stratumPolygonSF, laea.CRS)
    
    
    # Project data points from longlat to given laea
            #stratumPolygon <- sp::spTransform(stratumPolygon, laea.CRS)
            #area <- rgeos::gArea(stratumPolygon, byid = TRUE)
    stratumPolygon <- sf::as_Spatial(stratumPolygonSF)
    area <- rgeos::gArea(stratumPolygon, byid = TRUE)
    
    
    
    output <- data.table::data.table(
        Stratum = getStratumNames(stratumPolygon),
        Area = area
    )
    return(output)
}

