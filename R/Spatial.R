# We will support three different polygon files formats:
# 1. StoxWKT files, which are tables with two columns, one for the polygon names and one for the multipolygon WKT strings in a file with file extension "txt".
# 2. Shape files, which are folders with files with the following file extensions: "shp", shx" and "dbf".
# 3. GEOJSON files, which are text files with a GEOJSON string, with file extension "geojson".

# StoX will detect file extension and apply the appropriate reading function, returning an object of class SpatialPolygons. (readShapeFiles(), readGSOJSON(), readStoxMultipolygonWKT())

# We will also have a function converting from SpatialPolygons to GSOJSON string, and vice versa, for use when writing to the project.xml.





readStoxMultipolygonWKTFromFile <- function(FilePath) {
    # If the input is an existing file path instead of a data.table from the project.xml:
    if(!file.exists(FilePath) || (file.exists(FilePath) && isTRUE(file.info(FilePath)$isdir))) {
        stop("The StoX multipolygon WKT file ", FilePath, " does not exist or is a directory.")
    }
    tab <- data.table::fread(FilePath, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
    names(tab) <- c("Stratum", "Polygon")
    tab
}




dataTable2SpatialPolygons <- function(DataTable) {

    # 
    polygonName <- as.character(DataTable$Stratum)
    multipolygon <- DataTable$Polygon

    # Convert each WKT strings to SpatialPolygons:
    spatialPolygonsList <- lapply(multipolygon, rgeos::readWKT)
    # Extract the Polygons objects to modify the IDs and merge to a SpatialPolygons:
    polygonsList <- lapply(spatialPolygonsList, function(x) slot(x, "polygons")[[1]])
    # Add the polygon names as IDs:
    for (ind in seq_along(polygonsList)) {
        polygonsList[[ind]]@ID <- polygonName[ind]
    }
    # Merge to a SpatialPolygons object:
    spatialPolygons = sp::SpatialPolygons(polygonsList)
    #plot(SpP, col = 1:5, pbg="white")

    spatialPolygons
}



stoxMultipolygonWKT2SpatialPolygons <- function(FilePath) {

    # Read the file as data.table:
    dataTable <- readStoxMultipolygonWKTFromFile(FilePath)
    # Convert to SpatialPolygons:
    dataTable2SpatialPolygons(dataTable)
}


##################################################
##################################################
#' Define stratum multipolygon
#' 
#' This function reads a goejson file, shape file or a \code{\link{StratumPolygon}} Stox multipolygon WKT file and returns an object of StoX data type \code{\link{StratumPolygon}} object.
#' 
#' @param processData       The current data produced by a previous instance of the function.
#' @param FileName          The path to a goejson file, shape file (folder) or a Stox multipolygon WKT file.
#' @param UseProcessData    Logical: If TRUE use the existing funciton output in the process. 
#' 
#' @details
#' The parameter \code{UseProcessData} is always set to TRUE when running a process, and needs to be explicitely set to FALSE to enable reading a file. 
#' 
#' @return
#' An object of StoX data type \code{\link{StratumPolygon}}.
#' 
#' @examples
#' 
#' @seealso \code{\link[RstoxBase]{StratumArea}} for calculating the area of the strata.
#' 
#' @export
#' 
DefineStratum <- function(processData, FileName, UseProcessData = FALSE) {
    if(UseProcessData) {
        return(processData)
    }
    stoxMultipolygonWKT2SpatialPolygons(FileName)
}

##################################################
##################################################
#' StoX data type StratumPolygon
#' 
#' The StratumPolygon data type contains the polygons defining the strata of a survey, stored as an object of type \code{\link[sp]{SpatialPolygons}}. 
#' 
#' @details
#' The polygons are stored 
#' 
#' @seealso \code{\link[RstoxData]{DataTypes}} for a list of all StoX data types.
#' 
#' @name StratumPolygon
#' 
NULL




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
#' @seealso \code{\link[RstoxBase]{DefineStratum}} for the \code{StratumPolygon} input to the function.
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
#' @param StratumPolygon    An object of StoX class StratumPolygon, which is s SpatialPolygons object (sp package).
#' @param AreaMethod        The method to use for the area calculation, defaulted to "Accurate", which applied a lambert azimuthal equal area projection. 
#' 
#' @details
#' The \code{AreaMethod} "Simple" is deprecated, but kept for backwards campatibility.
#' 
#' @return
#' A table of stratum name and area.
#' 
#' @examples
#' 
#' @seealso \code{\link[RstoxBase]{DefineStratum}} for the \code{StratumPolygon} input to the function.
#' 
#' @export
#' 
StratumArea <- function(StratumPolygon, AreaMethod = c("Accurate", "Simple")) {
    # Accept StratumPolygon contained in a list:
    if(is.list(StratumPolygon) && "StratumPolygon" %in% names(StratumPolygon)) {
        StratumPolygon <- StratumPolygon$StratumPolygon
    }
    # Get the polygon areas:
    polygonAreaSP(StratumPolygon)
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
    polygonAreaPolygonDT(as.data.frame(xy.coords(x, y)[c("x", "y")], stringsAsFactors = FALSE))
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


# polygonAreaSP calculates simple GCD polygon area taking SpatialPolygons
polygonAreaSP <- function(sp) {
    
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
    d <- lapply(sp@polygons, stratumAreaDT)
    # Extract the multipolygon area and rbind to a data table:
    stratumAreas <- data.table::rbindlist(lapply(d, stratumArea))
    stratumAreas
}


# Try this e.g. with the files downloaded from this link: https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip:

readShapeFilesToSpatialPolygons <- function(FilePath) {
    shape <- rgdal::readOGR(dsn = FilePath)
    shape
}



# The following are attempts on using s3 methods for coverting between different objects holding multipolygons. This may not be used, since the comments above define that 
# Small test of S3 method getSpatialPolygons
#getSpatialPolygons <- function (x, ...) {
#        UseMethod("getSpatialPolygons", x)
#}
#getJSON <- function (x, ...) {
#        UseMethod("getJSON", x)
#}
#getMatrixList <- function (x, ...) {
#        UseMethod("getMatrixList", x)
#}
## No rush to implement these:
#getShape <- function (x, ...) {
#        UseMethod("getSpatialPolygons", x)
#}
#getStoxWKT <- function (x, ...) {
#        UseMethod("getSpatialPolygons", x)
#}
#
#
#
#
#
#getSpatialPolygons.StoxWKT <- function(x) readWKTSplit(x)
#
#getJSON.SpatialPolygons <- function(x) geojson_json(x)
#
## Example_
#m <- "MULTIPOLYGON(((-100 40, -90 50, -85 45, -100 40)))"
#
#class(m) <- "StoxWKT"
#
#getSpatialPolygons(m)








# Read shapefiles. See line 59:
#' 
#' @export
#' 
readStrataPolygons <- function(projectName, strata = "all", ...) {

    # Function for reading a WKT string to a list of geographical data:
    readWKTstring <- function(x) {
        # Function for converting the list of polygon matrices to data frames named with "longitude", "latitude":
        as.data.frameAddLonLatColnames <- function(x) {
            as.data.frameAddLonLatColnamesOne <- function(x) {
                setNames(as.data.frame(x, stringsAsFactors = FALSE), c("longitude", "latitude"))
            }
            if (is.list(x)) {
                lapply(x, as.data.frameAddLonLatColnamesOne)
            }
            else {
                as.data.frameAddLonLatColnamesOne(x)
            }
        }

        if (!is.data.frame(x)) {
            x <- read.table(x, sep = "\t", stringsAsFactors = FALSE)
        }

        # Interpret which of the columns contain the multipolygons:
        multipolygonCol <- which(tolower(sapply(x, function(x) substr(head(x, 1), 1, 5))) == "multi")
        if (length(multipolygonCol) == 0) {
            warning("There is no column with MULTIPOLYGON() strings")
        }

        # Get strata names:
        strataNames <- x[, 1]

        # Get a list of matrices:
        lonlatFull <- lapply(x[, multipolygonCol], getMatrixList)
        lonlatFull <- lapply(lonlatFull, as.data.frameAddLonLatColnames)
        names(lonlatFull) <- strataNames

        # Check the number of polygons in each multipolygon:
        areMultipolygons <- sapply(lonlatFull, function(x) length(nrow(x)) == 0)
        if (any(areMultipolygons)) {
            warning(paste0("The following strata contains multiple polygons: ", paste0(strataNames[areMultipolygons], collapse = ", "), ". Only the first polygon kept in the output 'lonlat' and 'lonlatAll' (see 'lonlatFull' for all the multipolygons)."))
            lonlat <- lapply(lonlatFull, function(x) if (!is.data.frame(x)) x[[1]] else x)
        }
        else {
            lonlat <- lonlatFull
        }
        lonlatAll <- as.data.frame(data.table::rbindlist(lonlat, idcol = "stratum"), stringsAsFactors = FALSE)

        # Return:
        list(lonlat = lonlat, lonlatAll = lonlatAll, lonlatFull = lonlatFull, strataNames = strataNames)
    }

    # Function for reading shapefiles:
    readShapefiles <- function(x, shapenames = list(longitude = "long", latitude = "lat", stratum = "id"), ...) {
        dsn <- dirname(path.expand(x))
        layer <- tools::file_path_sans_ext(basename(x))
        shape <- rgdal::readOGR(dsn = dsn, layer = layer)
        shape <- ggplot2::fortify(shape)
        #lonlatAll <- data.frame(longitude=shape$long, latitude=shape$lat, stratum=shape$id)
        lonlatAll <- data.frame(longitude = shape[[shapenames$longitude]], latitude = shape[[shapenames$latitude]], stratum = shape[[shapenames$stratum]], stringsAsFactors = FALSE)
        lonlat <- split(lonlatAll, lonlatAll$stratum)
        lonlat <- lapply(lonlat, "[", c("longitude", "latitude"))
        strataNames <- unique(lonlatAll$stratum)

        # Return:
        list(lonlat = lonlat, lonlatAll = lonlatAll, strataNames = strataNames)
    }

    # If the 'projectName' is a project, get the strata from getBaseline():
    if (all(isProject(projectName))) {
        # Get the baseline output and number of strata:
        g <- getBaseline(projectName, endProcess = "ReadProcessData", input = "proc", proc = NULL, drop = FALSE)
        strataNames <- g$processData$stratumpolygon$Stratum

        # Get the strata polygons in geographic coordinates (longitude, latitude) in a list named with the strata names:
        lonlat <- lapply(g$processData$stratumpolygon$Polygon, getMatrixList, data.frame.out = TRUE)
        names(lonlat) <- strataNames
        lonlat <- lapply(lonlat, "colnames<-", c("longitude", "latitude"))
        # Test of southern hemisphere:
        #lonlat <- lapply(lonlat, function(x) {x$latitude <- -x$latitude; x})

        # Create a single data frame version of the strata polygons, with stratum as the third column, and get a common projection definition using the centroid of the system:
        lonlatAll <- as.data.frame(data.table::rbindlist(lonlat, idcol = "stratum"), stringsAsFactors = FALSE)

        strataPolygons <- list(lonlat = lonlat, lonlatAll = lonlatAll, strataNames = strataNames, projectName = projectName)
    }
    # Read the strata from shapefiles, a WKT file ot WKT string:
    else if (is.character(projectName)) {
        if (all(file.exists(projectName))) {
            # Assure that the 'projectName' is a vector of files and not a directory, for convenience:
            if (length(projectName) == 1 && isTRUE(file.info(projectName)$isdir)) {
                projectName <- list.files(projectName, full.names = TRUE)
            }
            # Read from shapefiles:
            if (any(tolower(tools::file_ext(projectName)) == "shp")) {
                strataPolygons <- readShapefiles(projectName[1], ...)
            }
            # Read from a WKT file:
            else {
                strataPolygons <- readWKTstring(projectName[1])
            }
        }
        # Read from a WKT string:
        else {
            strataPolygons <- readWKTstring(textConnection(projectName[1]))
        }
    }
    # Support for giving a dara frame with stratum name in the first column, and a column of MULTIPOLYGON strings:
    else if (is.data.frame(projectName) && nrow(projectName) > 0) {
        strataPolygons <- readWKTstring(projectName)
    }
    # Support for a list as returned from getBaseline():
    else if (is.list(projectName) && is.data.frame(projectName$processData$stratumpolygon)) {
        strataPolygons <- readWKTstring(projectName$processData$stratumpolygon)
    }
    else {
        warning("'projectName' is not a project, path to shapefiles/folder of shapefiles, or a path to a WKT file or the content of the WKT file (string)")
        return()
    }

    ## Add a spatial polygons object and number of strata:
    strataPolygons$lonlatSP <- getSpatial(strataPolygons$lonlat)

    # Apply the subset:
    strataPolygons <- subsetStrata(strataPolygons, strata)

    strataPolygons <- mergeStrata(strataPolygons, strata)

    strataPolygons$nstrata <- length(strataPolygons$lonlat)

    return(strataPolygons)
}


# geojsonio::geojson_sp to convert from json to SpatialPolygons
# geojsonio::geojson_json to convert from SpatialPolygons to json

#*********************************************
#*********************************************
#' Get polygon area and convert to or from geographic and Cartesian coordinates.
#'
#' \code{polyArea} calculates the area in nautical mile squared of a multipolygon (for one stratum). \cr \cr
#' \code{geo2xy} converts from geographic to or from Cartesian coordinates or the inverse. \cr \cr
#' \code{getMatrixList} converts the input to a list of matrices. \cr \cr
#' \code{getMultipolygon} converts the input to a multipolygon wkt string. \cr \cr
#' \code{getSpatial} converts the input to a Spatial object. \cr \cr
#' \code{matrix2multipolygon} identical to \code{getMultipolygon}. \cr \cr
#' \code{multipolygon2matrix} identical to \code{getMatrixList}. \cr \cr
#' 
#' @param x					Either a two column matrix of x and y coordinates, indicating only one polygon, or a list of such matrices, indicating several polygons. If a list of lists of two column matrices are given, the first matrix of each list is the polygon, and the following are subtractions. \code{wkt} cal also be a wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))".
#' @param requireClosed		Logical: If TRUE (default) require polygons to be closed in the sense that the last point should equal the first. Set this to FALSE to allow adding the first point as the last point.
#' @param par				A list of proj4 parameters.
#' @param ...				Further proj4 parameters overriding those in \code{par}.
#' @param longlat			Logical: If TRUE (default) the input to polyArea_test is longitude, latitude.
#' @param inv				Logical: If TRUE, do the inverse conversion in rgdal::project().
#' @param data.frame.out	Logical: If TRUE convert the matrices to data frames with columns x and y.
#' @param drop				Logical: If TRUE drop the list if only one multipolygon or only one polygon is given.
#' 
#' @return \code{polyArea} returns area in nmi squared, and \code{matrix2multipolygon} returns a MULTIPOLYGON wkt.
#'
#' @examples
#' projectName <- "Test_Rstox"
#' g <- getBaseline(projectName, input="proc", proc=NULL, drop=FALSE)
#' # Get the stratum polygons:
#' multipolygon <- g$processData$stratumpolygon$Polygon
#' # Get stratum area in square nautical miles:
#' lapply(g$processData$stratumpolygon$Polygon, polyArea)
#' # Get cartesian locations using Azimuthal Equidistant projection (preserving distance):
#' ###proj <- getProjString(multipolygon)
#' ###xy <- lapply(g$processData$stratumpolygon$Polygon, geo2xy)
#' ###xlim=range(unlist(lapply(xy, "[", , "x")))
#' ###ylim=range(unlist(lapply(xy, "[", , "y")))
#' ###plot(NULL, xlim=xlim, ylim=ylim)
#' ###lapply(xy, lines, col='black', pbg='white')
#' ###lapply(xy, polyArea, input="xy")
#' 
#' @export
#' @importFrom rgeos readWKT
#' @rdname polyArea
#' 
readWKTSplit <- function(x, ...) {
    # Why was the strwrap function used? It is extremely slow for polygons with many border points:
    #x <- paste(strwrap(x, ...), collapse='\r')
    x <- paste(x, collapse = '\r')
    rgeos::readWKT(x)
}


g6 <- rgeos::readWKT("MULTIPOLYGON(((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)),((6 3,9 2,9 4,6 3))), MULTIPOLYGON(((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)),((6 3,9 2,9 4,6 3)))")



#' 
#' @export
#' @importFrom rgeos gArea
#' @importFrom sp CRS spTransform proj4string
#' @rdname polyArea
#' 
polyArea <- function(x, requireClosed = TRUE) {
    # We need rgdal when AreaMethod=Acurate in StratumArea!!!!
    ###if(is.numeric(x)){
    ###	x <- paste0("MULTIPOLYGON(((", paste(apply(x, 1, paste, collapse=" "), collapse=", "), ")))")
    ###}
    #write(x, "~/Desktop/Aktuelt/test1.txt")
    x <- matrix2multipolygon(x, requireClosed = requireClosed)
    #write(x, "~/Desktop/Aktuelt/test3.txt")

    p <- readWKTSplit(x)
    # Define projection for the wkt
    sp::proj4string(p) <- sp::CRS("+proj=longlat +ellps=WGS84")
    # define the proj4 definition of Lambert Azimuthal Equal Area (laea) CRS with origo in wkt center:
    # Units: international nautical miles:
    laea.CRS <- sp::CRS(paste0("+proj=laea +lat_0=", p@polygons[[1]]@labpt[2], " +lon_0=", p@polygons[[1]]@labpt[1],
                                                             " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=kmi +no_defs"))
    # project data points from longlat to given laea
    p1 <- sp::spTransform(p, laea.CRS)
    sum(rgeos::gArea(p1, byid = T)) # Returns area
    # The result is very near compared known online geodesic planimeters (+- 0.001 naut.m)
}
#'
#' @export
#' @importFrom rgeos gArea
#' @rdname polyArea
#' 
polyArea_test <- function(x, par = list(proj = "laea", units = "kmi", lon_0 = NA, lat_0 = NA, x_0 = 0, y_0 = 0, ellps = "WGS84", datum = "WGS84"), longlat = TRUE, ...) {
    # Convert to xy if given in longlat:
    if (longlat) {
        x <- geo2xy(x, par = par, inv = FALSE, ...)
    }
    # Get the spatial object and use gArea() to get the area:
    out <- getSpatial(x)
    out <- sum(rgeos::gArea(out, byid = T))
    return(out)
}
#' 
#' @export
#' @noRd
#' @rdname polyArea
#' 
geo2xy <- function(x, par = list(proj = "aeqd", units = "kmi", lon_0 = NA, lat_0 = NA, x_0 = 0, y_0 = 0, ellps = "WGS84", datum = "WGS84"), inv = FALSE, data.frame.out = FALSE, ...) {

    # cs2cs -lu
    #                     km 1000.                                Kilometer
    #                        m 1.                                     Meter
    #                     dm 1/10                                 Decimeter
    #                     cm 1/100                                Centimeter
    #                     mm 1/1000                             Millimeter
    #                    kmi 1852.0                             International Nautical Mile
    #                     in 0.0254                             International Inch
    #                     ft 0.3048                             International Foot
    #                     yd 0.9144                             International Yard
    #                     mi 1609.344                         International Statute Mile
    #                 fath 1.8288                             International Fathom
    #                     ch 20.1168                            International Chain
    #                 link 0.201168                         International Link
    #                us-in 1./39.37                         U.S. Surveyor's Inch
    #                us-ft 0.304800609601219        U.S. Surveyor's Foot
    #                us-yd 0.914401828803658        U.S. Surveyor's Yard
    #                us-ch 20.11684023368047        U.S. Surveyor's Chain
    #                us-mi 1609.347218694437        U.S. Surveyor's Statute Mile
    #             ind-yd 0.91439523                     Indian Yard
    #             ind-ft 0.30479841                     Indian Foot
    #             ind-ch 20.11669506                    Indian Chain


    # Get projection string:
    par <- getProjString(par = par, ..., x = x, requireClosed = FALSE)

    # Convert to a list of matrices and run the project() function on all elements:
    #out <- x[ ,c("x", "y"), drop=FALSE]
    out <- getMatrixList(x, data.frame.out = data.frame.out)
    #x <- rapply(x, rgdal::project, proj=par, inv=inv, how="replace")
    out <- projectMatrixList(out, par = par, inv = inv, data.frame.out = data.frame.out)
    #if(!inv){
    colnames(out) <- if (inv) c("longitude", "latitude") else c("x", "y")
    #}
    # Add the other columns:
    #if(ncol(x)>2){
    #	out <- cbind(out, x[, -(1:2)])
    #}
    #if(add){
    #	out <- cbind(out, x)
    #}

    attr(out, "proj") <- par
    out
}
#' 
#' @export
#' @rdname polyArea
#' 
getMatrixList <- function(x, drop = TRUE, data.frame.out = FALSE) {
    if (isSpatial(x)) {
        x <- spatial2matrixList(x, drop = drop, data.frame.out = data.frame.out)
    }
    else if (isMultipolygon(x)) {
        x <- multipolygon2spatial(x)
        x <- spatial2matrixList(x, drop = drop, data.frame.out = data.frame.out)
    }
    else if (isMatrixList(x) && data.frame.out) {
        x <- rapplyKeepDataFrames(x, as.data.frame)
    }
    else if (!isMatrixList(x)) {
        warning("Unrecognized input by isSpatial(), isMatrixList() or isMultipolygon(). Returned unaltered")
    }
    x
}
#'
#' @export
#' @rdname polyArea
#' 
getMultipolygon <- function(x, drop = TRUE, data.frame.out = FALSE, requireClosed = TRUE) {
    if (isSpatial(x)) {
        x <- spatial2matrixList(x, drop = drop, data.frame.out = data.frame.out)
        x <- matrixList2multipolygon(x, requireClosed = requireClosed)
    }
    else if (isMatrixList(x)) {
        x <- matrixList2multipolygon(x, requireClosed = requireClosed)
    }
    else if (!isMultipolygon(x)) {
        warning("Unrecognized input by isSpatial(), isMatrixList() or isMultipolygon(). Returned unaltered")
    }
    x
}
#'
#' @export
#' @importFrom rgeos readWKT
#' @importFrom sp SpatialPolygons Polygons Polygon
#' @rdname polyArea
#' 
getSpatial <- function(x) {
    if (isMatrixList(x)) {
        #x <- matrixList2multipolygon(x)
        #x <- readWKTSplit(x)
        if (matrixListLevel(x) == 1) {
            x <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(x)), ID = 1)))
        }
        else {
            x <- lapply(seq_along(x), function(i) sp::Polygons(list(sp::Polygon(x[[i]])), ID = i))
            x <- sp::SpatialPolygons(x)
        }
    }
    else if (isMultipolygon(x)) {
        x <- readWKTSplit(x)
    }
    else if (!isSpatial(x)) {
        warning("Unrecognized input by isSpatial(), isMatrixList() or isMultipolygon(). Returned unaltered")
    }
    x
}
#' 
#' @export
#' @rdname polyArea
#' 
matrix2multipolygon <- getMultipolygon
#' 
#' @export
#' @rdname polyArea
#' 
multipolygon2matrix <- getMatrixList


#*********************************************
#*********************************************
#' Utility functions for coordinate transformation between geographic and Cartesian coordinates, and calculation of multipolygon area.
#'
#' \code{getProjString} generates a projection string in the proj4 format. \cr \cr
#' \code{projectMatrixList} projects a list of matrices. \cr \cr
#' \code{spatial2matrixList} converts a Spatial object to a list of matrices. \cr \cr
#' \code{matrixList2multipolygon} converts a list of matrices to a multipolygon wkt string. \cr \cr
#' \code{multipolygon2spatial} converts a multipolygon wkt string to a Spatial object. \cr \cr
#' \code{isSpatial} determines whether the input is of Spatial class. \cr \cr
#' \code{isMatrixList} determines whether the input is a list of matrices or data frames. \cr \cr
#' \code{isMultipolygon} determines whether the input is a multipolygon wkt (well known text) string. \cr \cr
#' \code{matrixListLevel} determines the number of levels in a list of matrices, where 1 denotes a matrix, 2 denotes a list of matrices, and 3 denotes a list of lists of matrices. \cr \cr
#' \code{rapplyKeepDataFrames} lapplies the funciton \code{FUN} throughout the first two levels of a list but not into any data frames. \cr \cr
#' 
#' @param par				A list of proj4 parameters.
#' @param ...				Further proj4 parameters overriding those in \code{par}.
#' @param x					One of three onjects depending on the funciton: (1) a two column matrix of x and y coordinates, indicating only one polygon, or a list (of lists) of such matrices, indicating several polygons in a multipolygon. If a list of lists of two column matrices are given, the first matrix of each list is the polygon, and the following are subtractions. (2) A wkt string such as "MULTIPOLYGON(((4 55, 15 56, 15 59, 4 59, 4 55)))". (3) A spatial object.
#' @param list.out			Logical: If TRUE the projection info is returned as a list instead of a concatenate string.
#' @param requireClosed		Logical: If TRUE (default) require polygons to be closed in the sense that the last point should equal the first. Set this to FALSE to allow adding the first point as the last point.
#' @param inv				Logical: If TRUE, do the inverse conversion in rgdal::project().
#' @param data.frame.out	Logical: If TRUE convert the matrices to data frames with columns x and y.
#' @param drop				Logical: If TRUE drop the list if only one multipolygon or only one polygon is given.
#' @param FUN				The funciton to apply to the elements in \code{rapplyKeepDataFrames}.
#'
#' @export
#' @importFrom rgeos readWKT
#' @noRd
#' @rdname getProjString
#' 
getProjString <- function(par = list(proj = "laea", units = "kmi", lon_0 = NA, lat_0 = NA, x_0 = 0, y_0 = 0, ellps = "WGS84", datum = "WGS84"), ..., x = NULL, list.out = FALSE, requireClosed = TRUE) {

    # NOTE: There is a difference between how rgeos::gCentroid calculates the centroid for a closed polyogn and a line string (in which case the center of mass of the mere points is calculated, as per the reply to the issue on this page: https://stackoverflow.com/questions/35720614/gcentroid-rgeos-r-vs-actual-centroid-in-python)

    #if(length(x) && is.character(attr(x, "proj"))){
    #	return(attr(x, "proj"))
    #}

    if (is.character(par)) {
        par <- lapply(strsplit(par, "+", fixed = TRUE)[[1]][-1], strsplit, "=", fixed = TRUE)
        par <- lapply(par, unlist, recursive = FALSE)
        parNames <- sapply(par, head, 1)
        par <- lapply(par, "[", 2)
        names(par) <- parNames
    }

    # Include the freely specified args, set to override existing definitions:
    par <- c(list(...), par)
    # If lon_0 or lat_0 are given as NA, use the center of the polygon:
    # SHOULD WE USE rgeos::gCentroid() INSTEAD HERE, SINCE THIS CELECTS A POINT WHICH MAY BE LOCATED BETWEEN POSSIBLE SUB POLYGONS????:
    if (is.na(par$lon_0)) {
        #getCoordsPolygon <- function(x){
        #	do.call(rbind, lapply(p, function(x) x@polygons[[1]]@Polygons[[1]]@coords))
        #}
        p <- getMultipolygon(x, requireClosed = requireClosed)
        #if(is.list(p)){
        #	p <- getCoordsPolygon(p)
        #}
        p <- readWKTSplit(p)
        #p <- lapply(p, readWKTSplit)
        #p[[1]]@polygons[[1]]@Polygons[[1]]@coords <- getCoordsPolygon(p)
        #p <- p[[1]]
        temp <- rgeos::gCentroid(p)@coords
        par$lon_0 <- temp[1]
        par$lat_0 <- temp[2]
    }
    # Get the CRS, using only the first of duplicatedly specified parameters:
    par <- par[!duplicated(names(par))]
    # Convert the args to a vector of strings:
    if (!list.out) {
        par <- paste(paste0("+", names(par), "=", unlist(par, recursive = FALSE)), collapse = " ")
    }
    #par <- sp::CRS(par)
    par
}
#'
#' @export
#' @importFrom rgdal project
#' @noRd
#' @rdname getProjString
#' 
projectMatrixList <- function(x, par = list(proj = "laea", units = "kmi", lon_0 = NA, lat_0 = NA, x_0 = 0, y_0 = 0, ellps = "WGS84", datum = "WGS84"), inv = FALSE, data.frame.out = FALSE) {
    out <- rapplyKeepDataFrames(x, function(y) rgdal::project(data.matrix(y), proj = par, inv = inv))
    if (data.frame.out) {
        out <- rapplyKeepDataFrames(out, as.data.frame)
    }
    out
}
#'
#' @export
#' @noRd
#' @rdname getProjString
#' 
spatial2matrixList <- function(x, drop = TRUE, data.frame.out = FALSE, ...) {

    applyDataFrame <- function(x, data.frame.out = FALSE) {
        if (data.frame.out) {
            if (is.list(x)) {
                x <- lapply(x, as.data.frame)
            }
            else {
                x <- as.data.frame(x, ...)
            }
        }
        x
    }

    # Function for extracting the coordinates of spatialPolygon:
    getCoordsMultipolygon <- function(y, data.frame.out) {
        out <- lapply(y@Polygons, methods::slot, "coords")
        applyDataFrame(out, data.frame.out)
        #######out <- y@Polygons[[1]]@coords
        ###if(data.frame.out){
        ###	out <- lapply(out, as.data.frame)
        ###}
        ###out
    }
    # Function for extracting the coordinates of spatialPoints:
    getCoordsSpatialPoints <- function(y, data.frame.out) {
        out <- y@coords
        applyDataFrame(out, data.frame.out)
        ###if(data.frame.out){
        ###	out <- lapply(out, as.data.frame)
        ###}
        ###out
    }
    # Function for extracting the coordinates of spatialLines:
    getCoordsSpatialLines <- function(y, data.frame.out) {
        out <- lapply(y@Lines, methods::slot, "coords")
        applyDataFrame(out, data.frame.out)
        #######out <- y@Polygons[[1]]@coords
        ###if(data.frame.out){
        ###	if(is.list(out)){
        ###		out <- lapply(out, as.data.frame)
        ###	}
        ###	else{
        ###		out <- as.data.frame(out)
        ###	}
        ###}
        ###out
    }

    #' @importFrom sp disaggregate
    ### # Disaggregate the polygons:
    ### out <- sp::disaggregate(x)
    # Extract the coordinates:
    if ("polygons" %in% methods::slotNames(x)) {
        out <- sp::disaggregate(x)
        out <- lapply(out@polygons, getCoordsMultipolygon, data.frame.out = data.frame.out)
    }
    else if ("coords" %in% methods::slotNames(x)) {
        out <- getCoordsSpatialPoints(x, data.frame.out = data.frame.out)
    }
    else if ("lines" %in% methods::slotNames(x)) {
        out <- lapply(x@lines, getCoordsSpatialLines, data.frame.out = data.frame.out)
    }
    if (drop) {
        # Drop when only one multipolygon:
        out <- lapply(out, function(x) if (length(x) == 1) x[[1]] else x)
        # Drop when only one polygon:
        if (length(out) == 1) {
            out <- out[[1]]
        }
    }

    return(out)
}
#'
#' @export
#' @noRd
#' @importFrom utils head tail
#' @rdname getProjString
#' 
matrixList2multipolygon <- function(x, requireClosed = TRUE) {
    # Merge to pairs of x, y:
    mergeToPairs <- function(x) {
        x <- apply(x, 1, paste, collapse = " ")
        x <- paste(x, collapse = ", ")
        x <- paste0("(", x, ")")
        return(x)
    }
    addParantheseis <- function(x) {
        paste0("(", paste0(x, collapse = ", "), ")")
    }

    isConnected <- all(utils::head(x, 1) == utils::tail(x, 1))

    # Try to append the start point as end point in the polygon if requireClosed=FALSE:
    #write(x, "test2.txt")
    #if(requireClosed){
    if (!requireClosed) {
        x <- rbind(x, utils::head(x, 1))
        isConnected <- TRUE
    }
    if (!isConnected) {
        warning("Points do not form a closed polygon. Use requireClosed=FALSE to add the first point as the last point.")
    }

    # Convert to text string of pairs:
    x <- rapplyKeepDataFrames(x, mergeToPairs)

    x <- lapply(x, addParantheseis)
    x <- addParantheseis(x)
    x <- paste0("MULTIPOLYGON", x)
    write(x, "test.txt")

    ### if(isConnected){
    ### 	x <- lapply(x, addParantheseis)
    ### 	x <- addParantheseis(x)
    ### 	x <- paste0("MULTIPOLYGON", x)
    ### 	write(x, "test.txt")
    ### }
    ### else{
    ### 	warning("Points do not form a closed polygon. LINESTRING returned instead of MULTIPOLYGON. Use requireClosed=FALSE to add the first point as the last point.")
    ### 	x <- paste0("LINESTRING", x)
    ### }
    return(x)
}
#'
#' @export
#' @importFrom rgeos readWKT
#' @noRd
#' @rdname getProjString
#' 
multipolygon2spatial <- function(x) {
    readWKTSplit(x)
}
#'
#' @export
#' @noRd
#' @rdname getProjString
#' 
isSpatial <- function(x) {
    isS4(x) && any(c("lines", "coords", "polygons") %in% methods::slotNames(x))
}
#'
#' @export
#' @noRd
#' @rdname getProjString
#' 
isMatrixList <- function(x) {
    length(matrixListLevel(x)) > 0
}
#'
#' @export
#' @noRd
#' @rdname getProjString
#' 
isMultipolygon <- function(x) {
    is.character(x) && length(grep("MULTIPOLYGON", x)) > 0
}
#'
#' @export
#' @noRd
#' @rdname getProjString
#' 
matrixListLevel <- function(x) {
    isM <- function(y) {
        is.matrix(y) || is.data.frame(y)
    }
    isM1 <- isM(x)
    isM2 <- is.list(x) && isM(x[[1]])
    isM3 <- is.list(x) && isM(x[[1]][[1]])
    isM <- c(isM1, isM2, isM3)
    if (any(isM)) {
        return(which(isM)[1])
    }
    else {
        return(NULL)
    }
}
#'
#' @export
#' @noRd
#' @rdname getProjString
#' 
rapplyKeepDataFrames <- function(x, FUN, ...) {
    level <- matrixListLevel(x)
    FUN0 <- function(x, ...) {
        #do.call(FUN, c(list(data.matrix(x)), ...))
        do.call(FUN, c(list(x), ...))
    }
    FUN1 <- function(x, ...) {
        lapply(x, FUN0, ...)
    }
    FUN2 <- function(x, ...) {
        lapply(x, FUN1, ...)
    }
    if (level == 1) {
        FUN0(x, ...)
    }
    else if (level == 2) {
        FUN1(x, ...)
    }
    else if (level == 3) {
        FUN2(x, ...)
    }
}