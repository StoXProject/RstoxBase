# We will support three different polygon files formats:
# 1. StoxWKT files, which are tables with two columns, one for the polygon names and one for the multipolygon WKT strings in a file with file extension "txt".
# 2. Shape files, which are folders with files with the following file extensions: "shp", shx" and "dbf".
# 3. GEOJSON files, which are text files with a GEOJSON string, with file extension "geojson".

# StoX will detect file extension and apply the appropriate reading function, returning an object of class SpatialPolygonsDataFrame. (readShapeFiles(), readGSOJSON(), readStoxMultipolygonWKT())

# We will also have a function converting from SpatialPolygonsDataFrame to GSOJSON string, and vice versa, for use when writing to the project.xml.





readStoxMultipolygonWKTFromFile <- function(FilePath) {
    # If the input is an existing file path instead of a data.table from the project.xml:
    if(!file.exists(FilePath) || (file.exists(FilePath) && isTRUE(file.info(FilePath)$isdir))) {
        stop("The StoX multipolygon WKT file ", FilePath, " does not exist or is a directory.")
    }
    tab <- data.table::fread(FilePath, sep = "\t", header = FALSE, stringsAsFactors = FALSE, colClasses = list(character=1:2))
    names(tab) <- c("Stratum", "Polygon")
    tab
}




dataTable2SpatialPolygonsDataFrame <- function(DataTable) {
    
    # 
    polygonName <- as.character(DataTable$Stratum)
    multipolygon <- DataTable$Polygon
    
    # Convert each WKT strings to SpatialPolygonsDataFrame:
    spatialPolygonsList <- lapply(multipolygon, rgeos::readWKT)
    # Extract the Polygons objects to modify the IDs and merge to a SpatialPolygonsDataFrame:
    polygonsList <- lapply(spatialPolygonsList, function(x) methods::slot(x, "polygons")[[1]])
    # Add the polygon names as IDs:
    for (ind in seq_along(polygonsList)) {
        polygonsList[[ind]]@ID <- polygonName[ind]
    }
    # Merge to a SpatialPolygonsDataFrame object:
    data <- data.frame(
        polygonName = polygonName, 
        stringsAsFactors = FALSE
    )
    rownames(data) <- polygonName
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
#' @param DefinitionMethod A string naming the method to use, one of "ResourceFile", to read the file \code{FileName} holding the stratum multipolygon, or "Manual" to start off with no strata and create the strata manually in the map of the StoX GUI. Strata can be added, modified and removed in the StoX GUI.
#' @param FileName The path to a \href{https://geojson.org/}{GeoJSON} file, \href{https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm}{shapefile} (may be a folder) or a \code{\link{StoX_multipolygon_WKT}} file. Must include file extension.
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
    FileName
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
            StratumPolygon <- stoxMultipolygonWKT2SpatialPolygonsDataFrame(FileName)
        }
        else if(tolower(FileExt) == "shp") {
            # On 2020-12-19 we got rid of rgdal, which is slower for reading shapefiles than sf:
            #StratumPolygon <- rgdal::readOGR(FileName, verbose = FALSE)
            
            # FileName can the the path to one of the shapefile or to the directory holding the shapefiles:
            StratumPolygon <- sf::as_Spatial(sf::read_sf(FileName))
        }
        else if(tolower(FileExt) == "json") {
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
            
            StratumPolygon <-  sf::as_Spatial(geojsonsf::geojson_sf(FileName))
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

    return(StratumPolygon)
}


#' Extract stratum names from a SpatialPolygonsDataFrame
#' 
#'  The stratum names must be stored as the column polygonName of the data of the \code{\link[sp]{SpatialPolygonsDataFrame}} \code{stratum}.
#' 
#' @param stratum A \code{\link[sp]{SpatialPolygonsDataFrame}} with a column polygonName of the data of the \code{\link[sp]{SpatialPolygonsDataFrame}} \code{stratum}.
#' 
#' @export
#' 
getStratumNames <- function(stratum) {
    if("SpatialPolygonsDataFrame" %in% class(stratum)) {
        as.character(stratum$polygonName)
        #sapply(methods::slot(stratum, "polygons"), function(x) methods::slot(x, "ID"))
    }
    else {
        stop("Stratum polygon must be of class SpatialPolygonsDataFrame")
    }
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
#' @examples
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
    polygonNames <- names(StratumPolygon)
    # Get a list of the coordinates:
    stratumPolygonList <- lapply(StratumPolygon@polygons, function(x) data.table::as.data.table(x@Polygons[[1]]@coords))
    # Set the names of the list:
    names(stratumPolygonList) <- polygonNames
    
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
    RstoxData::setRstoxPrecisionLevel(areaDT)
    
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
    
    laea.CRS <- paste0(
        "+proj=laea +lat_0=", 
        stratumPolygon@polygons[[1]]@labpt[2], 
        " +lon_0=", 
        stratumPolygon@polygons[[1]]@labpt[1], 
        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=kmi +no_defs"
    )
    
    
    stratumPolygonSF <- sf::st_transform(stratumPolygonSF, laea.CRS)
    
    
    # Project data points from longlat to given laea
            #stratumPolygon <- sp::spTransform(stratumPolygon, laea.CRS)
            #area <- rgeos::gArea(stratumPolygon, byid = TRUE)
    stratumPolygon <- sf::as_Spatial(stratumPolygonSF)
    area <- rgeos::gArea(stratumPolygon, byid = TRUE)
    
    
    
    output <- data.table::data.table(
        Stratum = stratumPolygon$polygonName,
        Area = area
    )
    return(output)
}

