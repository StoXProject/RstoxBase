# 1. plotMap
# 2. plotTrack
# 3. plotPoints
# 4. plotSegments(, angle = 90, adjust = 0.5)
# 5. plotMultipleSegments - for NASC with depth


##########################
#' Plot NASCData
#' 
#' Plots a map with cruise line and points with size and (optionally) color representing NASC.
#' 
#' @inheritParams ModelData
#' @inheritParams MeanNASC
#' @inheritParams DefineLayer
#' @inheritParams general_plot_arguments
#' @inheritParams general_file_plot_arguments
#' @inheritParams general_map_plot_arguments
#' @inheritParams general_track_plot_arguments
#' @inheritParams general_stratum_plot_arguments
#' @inheritParams general_map_aspect_plot_arguments
#' @inheritParams general_AcousticPSU_plot_arguments
#' 
#' @return
#' A \code{\link{PlotAcousticTrawlSurveyData}} object.
#' 
#' @export
#' 
PlotAcousticTrawlSurvey <- function(
    
    NASCData, 
    SumNASCData, 
    AcousticPSU, 
    
    LayerDefinition = c("FunctionParameter", "FunctionInput", "PreDefined"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    AcousticLayer = NULL, 
    
    ShowOnlyAcousticPSU = FALSE, 
    ShowAcousticPSULabel = TRUE, 
    UseDefaultAcousticPSULabelSettings = TRUE,
    AcousticPSULabelSize = numeric(), 
    AcousticPSULabelColor = character(), 
    AcousticPSULabelPosition = c("mean", "atMinLongitude", "atMaxLongitude", "atMinLatitude", "atMaxLatitude"), 
    AcousticPSULabelHjust = numeric(), 
    AcousticPSULabelVjust = numeric(), 
    
    
    
    #PointTransparency = 1, 
    
    # Options for the track line and points:
    UseDefaultTrackSettings = TRUE, 
    #PointColorScale = c("combined.color", "rainbow", "hcl.colors", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"),     #PointColorScale = character(),  
    TrackColor = character(), 
    TrackLineWidth = numeric(), 
    TrackPointColor = character(), 
    MaxTrackPointSize = numeric(), 
    #MinPointSize = numeric(), 
    TrackPointShape = numeric(), 
    
    
    
    
    # Options for the stratum polygons:
    ShowStratumPolygon = FALSE, 
    StratumPolygon, 
    UseDefaultStratumPolygonSettings = TRUE, 
    StratumPolygonColor = character(), 
    StratumPolygonBorderColor = character(), 
    StratumPolygonBorderLineWidth = 0.5, 
    
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
) {
    
    LayerDefinition <- RstoxData::match_arg_informative(LayerDefinition)
    
    
    # Get the formals:
    plotArguments <- allargs()
    
    # Get the layer definition:
    if(LayerDefinition != "PreDefined" && "data.table" %in% class(plotArguments$NASCData) && NROW(plotArguments$NASCData) > 0) {
        plotArguments$SumNASCData <- SumNASC(
            NASCData = plotArguments$NASCData, 
            LayerDefinition = plotArguments$LayerDefinition, 
            LayerDefinitionMethod = plotArguments$LayerDefinitionMethod, 
            Resolution = plotArguments$Resolution, 
            LayerTable = plotArguments$LayerTable, 
            AcousticLayer = plotArguments$AcousticLayer
        )
    }
    # If not PreDefined assume SumNASCData is given and remove the NASCData, since this will cause problems when calling plot_lon_lat
    else {
        plotArguments$NASCData <- NULL
    }
    
    
    # Check function inputs specifically, as these have been included in plotArguments:
    if(!length(plotArguments$SumNASCData)) {
        stop("argument \"SumNASCData\" must be given.")
    }
    
     
    #plotArguments <- setDefaultsInStoxFunction(plotArguments, StoxFunctionName = "PlotAcousticTrawlSurvey", stoxFunctionAttributes = stoxFunctionAttributes)
    
    # # Set default general options:
    #plotArguments <- setDefaults(plotArguments, getRstoxBaseDefinitions("defaultPlotOptions"))
    # 
    
    # Set the default of the StoX function. These are defaults defined in the stoxFunctionAttributes.
    plotArguments <- setDefaultsInStoxFunction(plotArguments, StoxFunctionName = "PlotAcousticTrawlSurvey", stoxFunctionAttributes = stoxFunctionAttributes)
    
    # Split axis titles into x and y:
    plotArguments <- splitAxisTitleSize(plotArguments)
    
    # Set hard coded values used in plot_lon_lat():
    plotArguments <- c(
        plotArguments, 
        list(
            lon_name = "Longitude", 
            lat_name = "Latitude", 
            color.track.point = "NASC",
            trackType = "lp", 
            size = "NASC"
        )
    )
    
    # Define the translation from the inputs to this function to the inputs to plot_lon_lat:
    translation <- c(
        trackData = "SumNASCData", 
        polygon = "StratumPolygon", 
        showMap = "ShowMap",
        # Group by PSUs later if ShowOnlyAcousticPSU == TRUE.
        linewidth.track = "TrackLineWidth", 
        linewidth.polygon.border = "StratumPolygonBorderLineWidth", 
        size.max = "MaxTrackPointSize", 
        # Leave the strokeToPointFactor untouched:
        # strokeToPointFactor
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

    
    
    
    
    
    # Use only the Data table of the SumNASCData:
    if("Data" %in% names(plotArguments$SumNASCData)) {
        plotArguments$SumNASCData <- data.table::copy(plotArguments$SumNASCData$Data)
    }
    
    # Discard NA Layer:
    if(plotArguments$SumNASCData[, any(is.na(Layer))]) {
        totalNASC <- plotArguments$SumNASCData[, .(totalNASC = sum(NASC, na.rm = TRUE)), by = "Layer"]
        fractionOfNASCInNALayer <- totalNASC[is.na(Layer), totalNASC] / totalNASC[, sum(totalNASC, na.rm = TRUE)]
        message("StoX: Removing missing Layer (fraction of summed NASC: ", fractionOfNASCInNALayer, ").")
        plotArguments$SumNASCData[is.na(Layer), NASC := NA]
        #plotArguments$SumNASCData <- subset(plotArguments$SumNASCData, !is.na(Layer))
    }
    
    
    
    # Discard the transits if ShowOnlyAcousticPSU:
    if(ShowOnlyAcousticPSU) {
        # Merge in the PSUs and subset to remove the transit:
        plotArguments$SumNASCData <- merge(plotArguments$SumNASCData, AcousticPSU$EDSU_PSU, by = "EDSU")
        plotArguments$SumNASCData <- subset(plotArguments$SumNASCData, !is.na(PSU))
        # Add the PSU as a grouping variable:
        plotArguments$trackLinesBy <- "PSU"
        
        # Add PSU labels if specified:
        if(ShowAcousticPSULabel) {
            AcousticPSULabelPosition <- RstoxData::match_arg_informative(AcousticPSULabelPosition)
            AcousticPSULabelPositionFun <- switch (
                AcousticPSULabelPosition,
                mean = function(Longitude, Latitude) {
                    list(
                        mean(Longitude, na.rm = TRUE), 
                        mean(Latitude, na.rm = TRUE)
                    )
                }, 
                atMinLongitude = function(Longitude, Latitude) {
                    atMinLongitude <- which.min(Longitude)
                    list(
                        Longitude[atMinLongitude], 
                        Latitude[atMinLongitude]
                    )
                }, 
                atMaxLongitude = function(Longitude, Latitude) {
                    atMaxLongitude <- which.max(Longitude)
                    list(
                        Longitude[atMaxLongitude], 
                        Latitude[atMaxLongitude]
                    )
                }, 
                atMinLatitude = function(Longitude, Latitude) {
                    atMinLatitude <- which.min(Latitude)
                    list(
                        Longitude[atMinLatitude], 
                        Latitude[atMinLatitude]
                    )
                }, 
                atMaxLatitude = function(Longitude, Latitude) {
                    atMaxLatitude <- which.max(Latitude)
                    list(
                        Longitude[atMaxLatitude], 
                        Latitude[atMaxLatitude]
                    )
                }, 
            )
            
            PSUText <- plotArguments$SumNASCData[, .(
                label = PSU[1], 
                size = plotArguments$AcousticPSULabelSize, 
                hjust = plotArguments$AcousticPSULabelHjust,
                vjust = plotArguments$AcousticPSULabelVjust, 
                color = plotArguments$AcousticPSULabelColor
            ), by = "PSU"]
            
            PSUPos <- plotArguments$SumNASCData[, AcousticPSULabelPositionFun(Longitude, Latitude), by = "PSU"]
            names(PSUPos) <- c("PSU", "x", "y")
            
            PSUText <- cbind(
                PSUText, 
                subset(PSUPos, select = c("x", "y"))
            )
            
            # Add the text to the plotArguments:
            plotArguments$text <- PSUText$label
            plotArguments$size.text <- PSUText$size
            plotArguments$hjust.text <- PSUText$hjust
            plotArguments$vjust.text <- PSUText$vjust
            plotArguments$color.text <- PSUText$color
            plotArguments$x.text <- PSUText$x
            plotArguments$y.text <- PSUText$y
        }
    }
    # If not PreDefined assume SumNASCData is given and remove the NASCData, since this will cause problems when calling plot_lon_lat
    else {
        plotArguments$AcousticPSU <- NULL
    }
    
    
    # Add xlim and ylim and center for zooming:
    plotArguments <- set_xlim_ylim_zoomCenter(plotArguments)
    
    
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
    
    # Run the plot_lon_lat function:
    PlotAcousticTrawlSurveyData <- do.call(plot_lon_lat, plotArguments)
    
    # Set the plot attributes to the output:
    PlotAcousticTrawlSurveyData <- setPlotAttributes(
        plotObject = PlotAcousticTrawlSurveyData, 
        plotArguments = plotArguments
    )
    
    
    return(PlotAcousticTrawlSurveyData)
}




splitAxisTitleSize <- function(plotArguments) {
    # Do the split:
    plotArguments$axis.title.size.x <- plotArguments$AxisTitleSize
    plotArguments$axis.title.size.y <- plotArguments$AxisTitleSize
    plotArguments$axis.text.size.x <- plotArguments$AxisTickSize
    plotArguments$axis.text.size.y <- plotArguments$AxisTickSize
    # Clear the inputs:
    plotArguments$AxisTitleSize <- NULL
    plotArguments$AxisTickSize <- NULL
    
    return(plotArguments)
}



set_xlim_ylim_zoomCenter <- function(plotArguments) {
    # Set the xlim, ylim and zoomCenter:
    plotArguments$xlim <- c(plotArguments$LongitudeMin, plotArguments$LongitudeMax)
    plotArguments$ylim <- c(plotArguments$LatitudeMin, plotArguments$LatitudeMax)
    plotArguments$zoomCenter <- c(plotArguments$LongitudeCenter, plotArguments$LatitudeCenter)
    
    # Clear the input:
    plotArguments$LongitudeMin <- NULL
    plotArguments$LongitudeMax <- NULL
    plotArguments$LatitudeMin <- NULL
    plotArguments$LatitudeMax <- NULL
    plotArguments$LongitudeCenter <- NULL
    plotArguments$LatitudeCenter <- NULL
    
    return(plotArguments)
}




#' Set plot attributes to a plot object
#' 
#' Sets the following attributes to the plot object (class ggplot): Format, Width, Height, DotsPerInch.
#' 
#' @param plotObject The plot object of class ggplot.
#' @param plotArguments A list of the arguments to the plot function.
#' 
#' @export
#' 
setPlotAttributes <- function(plotObject, plotArguments) {
    attr(plotObject, "Format") <- plotArguments$Format
    attr(plotObject, "Width") <- plotArguments$Width
    attr(plotObject, "Height") <- plotArguments$Height
    attr(plotObject, "DotsPerInch") <- plotArguments$DotsPerInch
    return(plotObject)
}





#' Apply zoom using ggplot2::coord_sf based on positions in the data and limiting parameters
#' 
#' This function is applied by plot_lon_lat() to set the zoom based on 0 or more arguments and the actual data to be plotted. Providing no arguments returns the full extent of the data.
#' 
#' @param trackData A data.table with track data, including the longitude and latitude variables specified by the arguments \code{lon_name} and \code{lat_name}.
#' @param stationData A data.table with track data, including the longitude and latitude variables specified by the arguments \code{lon_name_station} and \code{lat_name_station}.
#' @param lon_name,lat_name Character: The name of the longitude and latitude variable in the data \code{trackData}.
#' @param lon_name_end,lat_name_end Character: The name of the end point of the longitude and latitude variable in the data \code{trackData}, used in the case that \code{trackType} contains "s" (segments).
#' @param lon_name_station,lat_name_station Character: The name of the longitude and latitude variable in the data \code{stationData}.
#' @param xmin,xmax,ymin,ymax Numeric: The optional extremes of the plot, where x is longitude and y is latitude. Setting only xmin sets only the lower longitude limit, etc. 
#' @param zoom Numeric: The zoom of the plot, where 1 is no zoom and 2 zooms out to double extent of the plot, etc. 
#' @param zoomCenter Numeric: The center of the plot relative to the limits of the plot before zooming.
#' 
zoom_lon_lat <- function(
    trackData, 
    stationData = NULL, 
    lon_name = "lon", lat_name = "lat", 
    lon_name_end = NULL, lat_name_end = NULL, 
    lon_name_station = "lon", lat_name_station = "lat", 
    xmin = NA, xmax = NA, 
    ymin = NA, ymax = NA, 
    zoom = 1, zoomCenter = c(0.5, 0.5)
) {
    
    # Zooms first from the range of the values,:
    temp <- zoom_data(
        x = c(
            if(NROW(trackData)) unlist(subset(trackData, select = c(lon_name, lon_name_end))),
            if(NROW(stationData)) unlist(subset(stationData, select = lon_name_station))
        ), 
        y = c(
            if(NROW(trackData)) unlist(subset(trackData, select = c(lat_name, lat_name_end))),
            if(NROW(stationData)) unlist(subset(stationData, select = lat_name_station))
        ), 
        zoom = zoom, 
        zoomCenter = zoomCenter
    )
    
    # If xmin is not present:
    if(isNotGiven(xmin)){
        xmin <- temp$xlim[1]
    }
    # If xmax is not present:
    if(isNotGiven(xmax)){
        xmax <- temp$xlim[2]
    }
    # If xmin is not present:
    if(isNotGiven(ymin)){
        ymin <- temp$ylim[1]
    }
    # If xmin is not present:
    if(isNotGiven(ymax)){
        ymax <- temp$ylim[2]
    }
    
    xlim <- c(xmin, xmax)
    ylim <- c(ymin, ymax)
    
    # Adjust the aspect ratio by latitude:
    ymid <- mean(ylim)
    aspectratio <- 1 / cos(ymid * pi/180)
    
    # Return the coordinates as a ggplot object to add to a plot:
    #ggplot2::coord_fixed(aspectratio, xlim = xlim, ylim = ylim)
    
    ggplot2::coord_sf(xlim = xlim, ylim = ylim)
}



isNotGiven <- function(x) {
    length(x) == 0 || length(unlist(x)) == 0 || is.na(x)
}


#' Plot a map, an optional (multi)polygon, and points and a track given by the first argument.
#' 
#' @inheritParams zoom_lon_lat
#' @param polygon An sf object with multipolygons.
#' @param showMap Logical: If TRUE show the map.
#' @param trackLinesBy Character: A grouping variable for the track lines. Typically this could be Stratum for a plot of a TransectDesignData so that the transit between strata is not shown.
#' @param trackType The type of plot. See Details.
#' @param linetype.track The line type as described in \code{\link[ggplot2]{aes_linetype_size_shape}}.
#' @param linewidth.track Numeric: The width of the track lines.
#' @param linewidth.polygon.border Numeric: The width of the polygon borders.
#' @param size Either a numeric value or the name of the variable to set the size by.
#' @param size.station.point Numeric: The size of the station points.
#' @param size.max Numeric: The maximum size of the points (presumably ignored if \code{size} is numeric).
#' @param strokeToPointFactor Numeric: A value between 0 and 1 setting the width of the point symbols relative to their size.
#' @param color.track.point Character: The color of the track points
#' @param color.track Character: The color of the track lines.
#' @param color.scale Character: The name of the function defining the color scale in case \code{color} names a variable in the data \code{x}.
#' @param color.station.point Character: The color to use for the station points, defaulted to "black".
#' @param color.land,color.border,color.ocean,color.grid Character: The colors to use for the land, the borders between countries, the ocean and the grid in the map.
#' @param color.polygon The color to use for the polygons, given either as a single color equal for all strata or a color palette. The default, "hue", is the default color ggplot palette. See the \code{Palettes} section in \code{\link[ggplot2]{scale_fill_brewer}} for a list of options (both Diverging, Qualitative and Sequential color palettes are possible).
#' @param color.polygon.border Character: The color of the borders between poygons.
#' @param shape Numeric: The shape of the points as specified in \code{\link[graphics]{points}}. Could possibly also be a character.
#' @param shape.station.point The shape of the station points, as defined for the argument \code{pch} in \code{\link{points}}.
#' @param alpha.point,alpha.track,alpha.polygon Numeric: The alpha (transparency) values between 0 and 1 for the points, track lines and polygons.
#' @param xlim,ylim The optional x and y limit of the plot. These are vectors of length 2. NA can be used to leave on of the values unset. The full extent of the data are used when these are not specified.
#' @param main Character: The main title of the plot.
#' @param text Character: Text to be added to the plot using \code{\link[ggplot2]{annotate}} at the positions specified by \code{x.text}, \code{y.text}, \code{hjust.text} and \code{vjust.text}.
#' @param x.text,y.text Numeric: The positions of the text.
#' @param color.text Character: The color of the text.
#' @param hjust.text,vjust.text Adjustment of the text position. See \code{\link[ggplot2]{annotate}}.
#' @param size.text Character: The size of the text.
#' @param ... Optional arguments, specifically axis.title.size.x, axis.title.size.y, axis.text.size.x,axis.text.size.y, legend.text.size and legend.title.size.
#' 
#' @details
#' The \code{trackType} can have the following values:
#'  \describe{
#'    \item{pl}{Track lines on top of points}
#'    \item{lp}{Points on top of track lines}
#'    \item{ps}{Segments on top of points}
#'    \item{sp}{Points on top of segments}
#'    \item{p}{Only points}
#'    \item{s}{Only segments}
#'    \item{l}{Olny lines}
#'  }
#' 
plot_lon_lat <- function(
    trackData, 
    stationData = NULL, 
    polygon, 
    showMap = TRUE, 
    trackLinesBy = NULL, 
    lon_name = "lon", lat_name = "lat", 
    lon_name_end = NULL, lat_name_end = NULL, 
    lon_name_station = "lon", lat_name_station = "lat", 
    trackType = c("pl", "lp", "ps", "sp", "p", "s", "l"), 
    linetype.track = 1, 
    linewidth.track = 1, linewidth.polygon.border = 0.5, 
    size = 1, size.max = 10, size.station.point = 2, 
    strokeToPointFactor = 0.1, 
    color.track.point = 1, 
    color.track = 1, color.scale = combined.color(2), color.station.point = 1, color.land = "grey50", color.border = "grey10", color.ocean = "grey90", color.grid = "white", color.polygon = "hue", color.polygon.border = "blue", 
    shape = 16, shape.station.point = 0, 
    alpha.point = 1, alpha.track = 1, alpha.polygon = 1, 
    zoom = 1, xlim = NA, ylim = NA, 
    zoomCenter = c(0.5, 0.5), 
    main = NULL, 
    text = NULL, size.text = 4, color.text = "black", hjust.text = 0.5, vjust.text = 0.5, x.text = NULL, y.text = NULL, 
    ...
)
{
    
    # Get the trackType:
    trackType <- RstoxData::match_arg_informative(trackType)
    
    # Get the optional arguments:
    lll <- list(...)
    
    # Get the map:
    gmap <- ggplot2::map_data("world")
    
    # Initiate the plot:
    #p <- ggplot2::ggplot(data = trackData) 
    p <- ggplot2::ggplot() 
    
    # Plot the map:
    if(showMap) {
        p <- p + ggplot2::geom_polygon(
            data = gmap, 
            ggplot2::aes_string(
                x = "long", 
                y = "lat", 
                group = "group"
            ), 
            fill = color.land, 
            color = color.border
        )
    }
    
    # Add polygon:
    if(!missing(polygon)) {
        
        if(length(color.polygon) > 1) {
            stop("Only color/color scale of length 1 is allowed.")
        }
        
        if(color.polygon %in% grDevices::colors()) {
            # One single color for the polygon fill:
            p <- p + ggplot2::geom_sf(data = polygon, fill = color.polygon, color = color.polygon.border, linewidth = linewidth.polygon.border)
        }
        else if(tolower(color.polygon) %in% grDevices::colors()) {
            stop("The color must either be a valid color from colors() (all lower case) or a color palette as recognized by ggplot2. Otherwise the default applied by ggplot2::scale_fill_brewer, which currently seems to be \"Greens\"")
        }
        else {
            p <- p + ggplot2::geom_sf(data = polygon, ggplot2::aes(fill = StratumName), color = color.polygon.border, linewidth = linewidth.polygon.border)
        }
        if(tolower(color.polygon) != "hue") {
            p <- p + ggplot2::scale_fill_discrete(type = ggplot2::scale_fill_brewer , palette = color.polygon)
        }
    }
    
    
    # Zoom the data:
    p <- p + zoom_lon_lat(
        trackData = trackData, 
        stationData = stationData, 
        lon_name = lon_name, 
        lat_name = lat_name, 
        lon_name_end = lon_name_end, 
        lat_name_end = lat_name_end, 
        lon_name_station = lon_name_station, 
        lat_name_station = lat_name_station, 
        xmin = xlim[1], 
        xmax = xlim[2], 
        ymin = ylim[1], 
        ymax = ylim[2], 
        zoom = zoom, 
        zoomCenter = zoomCenter
    ) + 
    # Add the labels:
    ggplot2::xlab(lon_name) + 
    ggplot2::ylab(lat_name) #+ 
    #ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5)))
    
    
    # Plot the track:
    if(grepl("l", trackType)) {
        plotspec <- list(
            data = trackData, 
            ggplot2::aes_string(
                x = lon_name, 
                y = lat_name, 
                group = trackLinesBy
            ), 
            color = color.track, 
            linewidth = linewidth.track, 
            alpha = alpha.track, 
            show.legend = FALSE
        )
        
        track_plot <- do.call(ggplot2::geom_path, plotspec)
    }
    else if(grepl("s", trackType)) {
        
        if(!length(lon_name_end) || !length(lat_name_end)) {
            stop("When plotting segments ('s' in trackType) xend and yend must be given.")
        }
        
        
        linestrings <- dataTable2sf_LINESTRING(
            trackData, 
            x1y1x2y2 = c(lon_name, lat_name, lon_name_end, lat_name_end), 
            idCol = linetype.track, 
            crs = 4326
        )
        
        # Add points along the great circle and extract the coordinates, in order to force segments along the great circle in the plot:
        linestrings_segmentized <- sf::st_segmentize(linestrings, units::set_units(10, km))
        linestrings_segmentized_coordinates <- sf::st_coordinates(linestrings_segmentized)
        linestrings_segmentized_coordinates <- cbind(
            linestrings_segmentized_coordinates, 
            linestrings_segmentized[[linetype.track]][linestrings_segmentized_coordinates[, "L1"]]
        )
        
        
        plotspec <- list(
            data = linestrings_segmentized_coordinates, 
            ggplot2::aes(
                x = X, 
                y = Y, 
                group = L1, 
                #linetype = eval(linetype.track)
                linetype = .data[[linetype.track]]
            ), 
            color = color.track, 
            linewidth = linewidth.track, 
            alpha = alpha.track, 
            show.legend = FALSE
        )
        
        track_plot <- do.call(ggplot2::geom_path, plotspec)
        
        
        
        
        
        
        ### #plotspec <- list(
        ### #    data = linestrings, 
        ### #    ggplot2::aes(
        ### #        linetype = eval(linetype.track)
        ### #    ), 
        ### #    color = color.track, 
        ### #    linewidth = linewidth.track, 
        ### #    alpha = alpha.track, 
        ### #    show.legend = FALSE
        ### #)
        ### 
        ### 
        ### plotspec <- list(
        ###     data = trackData, 
        ###     ggplot2::aes_string(
        ###         x = lon_name, 
        ###         y = lat_name, 
        ###         xend = lon_name_end, 
        ###         yend = lat_name_end, 
        ###         group = L1, 
        ###         linetype = linetype.track
        ###     ), 
        ###     color = color.track, 
        ###     linewidth = linewidth.track, 
        ###     alpha = alpha.track, 
        ###     show.legend = FALSE
        ### )
        ### 
        ### #track_plot <- do.call(ggplot2::geom_segment, plotspec)
        ### track_plot <- do.call(ggplot2::geom_sf, plotspec)
        
        
        
    }
    # If we are not plotting the track, we set the track_plot to NULL, and then remove empty elements in the toPlot below (not sure that this is ideal coding...):
    else {
        track_plot <- NULL
    }
    
    # Plot the points on the track:
    if(grepl("p", trackType)) {
        
        # If the data variable is a column in the data, but the user has specified a single color, use this color instead:
        if(!isCategorical(trackData[[color.track.point]]) && length(color.scale) == 1 && !isColorScaleFunction(color.scale)) {
            color.track.point <- color.scale
        }
        
        plotspec <- list(
            data = trackData, 
            alpha = alpha.point
        )
        
        # If segments AND points are plotted, we need to plot both start and end points:
        if(grepl("s", trackType)) {
            
            tempDataPoints <- data.table::data.table(
                c(t(subset(trackData, select = c(lon_name, lon_name_end)))), 
                c(t(subset(trackData, select = c(lat_name, lat_name_end))))
            )
            names(tempDataPoints) <- c(lon_name, lat_name)
            tempDataPoints <- cbind(
                tempDataPoints, 
                data.table::as.data.table(lapply(subset(trackData, select = ! names(trackData) %in% c(lon_name, lat_name)), function(y) rep(y, each = 2)))
            )
            
            plotspec$data <- tempDataPoints
        }
        else {
            plotspec$data <- trackData
        }
        
        # If the color.track.point and size are given as the name of a column in the data, plot color and size as a function of these columns:
        if(length(color.track.point) && color.track.point %in% names(trackData) && length(size) && size %in% names(trackData)) {
            # Fix the stroke to the strokeToPointFactor times the size.max (multiply by 1.5 since this seems to be needed in ggplot2:
            strokeWidth <- 1.5 * size.max * strokeToPointFactor
            mapping <- ggplot2::aes_string(
                x = lon_name, 
                y = lat_name, 
                color = color.track.point, 
                size = size,
                stroke = strokeWidth
            )
        }
        # If only the color.track.point is given as the name of a column in the data, plot color as a function of this column, whereas size is assumed to be a numeric:
        else if(length(color.track.point) && color.track.point %in% names(trackData)) {
            mapping <- ggplot2::aes_string(
                x = lon_name, 
                y = lat_name, 
                color = color.track.point
            )
            plotspec$size <- size
        }
        # If only the size is given as the name of a column in the data, plot size as a function of this column, whereas color.track.point is assumed to be a valid color:
        else if(length(size) && size %in% names(trackData)) {
            # Fix the stroke to the strokeToPointFactor times the size.max (multiply by 1.5 since this seems to be needed in ggplot2:
            strokeWidth <- 1.5 * size.max * strokeToPointFactor
            mapping <- ggplot2::aes_string(
                x = lon_name, 
                y = lat_name, 
                size = size,
                stroke = strokeWidth
            )
            
            plotspec$color <- color.track.point
        }
        # Otherwise, use the size as a numeric and color.track.point as a valid color:
        else {
            mapping <- ggplot2::aes_string(
                x = lon_name, 
                y = lat_name
            )
            plotspec$color <- color.track.point
            plotspec$size <- size
        }
        
        plotspec$mapping <- mapping
        
        plotspec$shape <- shape
        
        track_point_plot <- do.call(ggplot2::geom_point, plotspec)
    }
    # If we are not plotting the track points, we set the track_point_plot to NULL, and then remove empty elements in the toPlot below:
    else {
        track_point_plot <- NULL
    }
    
    
    # Plot the stations:
    if(NROW(stationData)) {
        
        # Define the aes:
        mapping <- ggplot2::aes_string(
            x = lon_name_station, 
            y = lat_name_station
        )
        # Define the color and size outside of the aes, since these should be static for station points:
        plotspec <- list(
            data = stationData, 
            mapping = mapping, 
            color = color.station.point, 
            size = size.station.point, 
            shape = shape.station.point
        )
        
        station_plot <- do.call(ggplot2::geom_point, plotspec)
    }
    # If we are not plotting the station points, we set the station_plot to NULL, and then remove empty elements in the toPlot below:
    else {
        station_plot <- NULL
    }
    
    
    
    # Order the track_plot and track_point_plot:
    if(startsWith(trackType, "l")) {
        toPlot <- list(track_plot, track_point_plot)
    }
    else {
        toPlot <- list(track_point_plot, track_plot)
    }
    
    # Add the station_plot:
    toPlot <- list(toPlot, station_plot)
    
    # Remove empty elements:
    toPlot <- toPlot[lengths(toPlot) > 0]
    
    # Add the plots to the output:
    for(this in toPlot) {
        p <- p + this
    }
    
    # Scale the points:
    p <- p + ggplot2::scale_size_continuous(range = c(0, size.max))
    
    
    # Use scale_color_manual() for categorical variables and scale_color_gradientn() for numeric:
    if(is.character(color.track.point) && isCategorical(trackData[[color.track.point]])) {
        if(length(color.scale)) {
            numberOfLevelsInTheColorVariable <- length(unique(trackData[[color.track.point]]))
            
            if(isColorScaleFunction(color.scale)) {
                color.scale <- do.call(color.scale, list(numberOfLevelsInTheColorVariable))
            }
            else {
                numberOfDescreteColors <- length(color.scale)
                if(length(color.scale) != length(unique(trackData[[color.track.point]]))) {
                    stop("The number of discrete colors (length of TrackPointColor = ", numberOfDescreteColors, ") must match the number of levels of the categorical variable determining the colors of the points (", color.track.point, " has ", numberOfLevelsInTheColorVariable, " levels).")
                }
            }
            
            p <- p + ggplot2::scale_color_manual(values = color.scale, guide = "legend")
        }
    }
    else {
        # Hard code to use 10 color steps:
        if(isColorScaleFunction(color.scale)) {
            ncolors <- 10
            color.scale <- do.call(color.scale, list(ncolors))
            p <- p + ggplot2::scale_color_gradientn(colors = color.scale, guide = "legend")
        }
        else {
            p <- p + ggplot2::scale_color_gradientn(colors = color.scale, guide = "legend")
        }
    }
    
    ## User thicker lines in legend:
    p <- p + ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(stroke = size.max * strokeToPointFactor * 1.5, linewidth = size.max * strokeToPointFactor * 1.5)))
    
    
    
    p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(size = if(length(lll$axis.title.size.x)) lll$axis.title.size.x else 10), 
        axis.title.y = ggplot2::element_text(size = if(length(lll$axis.title.size.y)) lll$axis.title.size.y else 10), 
        axis.text.x = ggplot2::element_text(size = if(length(lll$axis.text.size.x)) lll$axis.text.size.x else 10), 
        axis.text.y = ggplot2::element_text(size = if(length(lll$axis.text.size.y)) lll$axis.text.size.y else 10), 
        legend.text = ggplot2::element_text(size = if(length(lll$legend.text.size)) lll$legend.text.size else 10), 
        legend.title = ggplot2::element_text(size = if(length(lll$legend.title.size)) lll$legend.title.size else 10), 
        #legend.box = "horizontal",
        panel.background = ggplot2::element_rect(fill = color.ocean, color = "grey80"),
        panel.grid.major = ggplot2::element_line(color = color.grid), 
        panel.grid.minor = ggplot2::element_line(color = color.grid)
    )
    
    ###p <- p + ggplot2::labs(fill = expression(paste("NASC (", m^2, nmi^{-2}, ")")))
    
    if(length(main)) {
        p <- p + ggplot2::ggtitle(main)
    }
    
    if(length(text)) {
        p <- p + ggplot2::annotate(
            "text", 
            x = x.text, 
            y = y.text, 
            label = text, 
            hjust = hjust.text, 
            vjust = vjust.text, 
            size = size.text, 
            color = color.text
        )
    }
    
    ## Set order of the legends to size first then color:
    #p <- p + ggplot2::guides(
    #    size = ggplot2::guide_legend(order = 1), 
    #    colour = ggplot2::guide_legend(order = 2)
    #)
    
    
    return(p)
}


isColorScaleFunction <- function(x) {
    fun <- try(get(x), silent = TRUE)
    is.function(fun) && "n" %in% names(formals(fun))
}




zoom_data <- function(x, y, zoom = 1, zoomCenter = c(0.5, 0.5)) {
    
    if(zoom <= 0){
        warning("StoX: The value of 'zoom' must be positive, with default 1 implying no zoom. The default was used.")
        zoom <- 1
    }
    
    # Get the center of the plot:
    rangex <- range(x, na.rm = TRUE)
    rangey <- range(y, na.rm = TRUE)
    xcenter <- min(rangex) + zoomCenter[1] * diff(rangex)
    ycenter <- min(rangey) + zoomCenter[2] * diff(rangey)
    
    # Zoom:
    fact <- c(-1, 1) * zoom
    xlim <- xcenter + fact * diff(rangex) / 2
    ylim <- ycenter + fact * diff(rangey) / 2
    
    list(xlim = xlim, ylim = ylim)
}










#' Combined rainbow and desaturation color scale
#' 
#' Returns a color vector of rainbow colors which are desaturated by 's' and darkened by 'v' to obtain the "combined" color scale.
#'
#' @param n The number of colors to generate.
#' @param s The range of the saturation values applied to the colors, where lower values suppress saturation. The default applies less strong colors at the higher values of x.
#' @param v The range of the value applied to the colors, where lower values causes darker colors. The default applies less darker colors at the lover values of x.
#' @param colStart The first color, appended to the color scale, possibly repeated \code{nStart} times.
#' @param colEnd The last color, appended to the color scale, possibly repeated \code{nSEnd} times.
#' @param nStart The number of repeated \code{colStart}.
#' @param nEnd The number of repeated \code{colEnd}.
#' @param start The start hue, given as a value in between 0 and 1.
#' @param end The end hue, given as a value in between 0 and 1.
#' @param flip Logical: If TRUE the color scale should be reversed.
#' @param alpha The transparency.
#' @param ... Not used, but allowing for unused arguments.
#'
#' @examples
#' \dontrun{
#' plot(1:10, 1:10, pch = 19, cex = 10, col = combined.color(10))
#' plot(1:10, 1:10 + 1, pch = 19, cex = 10, col = combined.color(10, flip=TRUE))
#' }
#' @export
#'
combined.color <- function(n, colStart = "white", colEnd = NA, nStart = 0, nEnd = floor(n / 5), s = c(0.3, 1), v = c(1, 0.7), start = 0.8, end = 0, flip = FALSE, alpha = 1, ...) {
    
    x <- seq.int(start, end, length.out = n)
    s <- seq.int(s[1], s[2], length.out = n)
    v <- seq.int(v[1], v[2], length.out = n)
    
    # The hue should move from 'start' to 'end':
    h = if(flip) rev(x) else x
    s = if(flip) rev(s) else s
    v = if(flip) rev(v) else v
    
    
    out <- grDevices::hsv(h = h, s = s, v = v, alpha = alpha)
    
    # Add the start and end color:
    out <- c(
        rep(if(is.na(colStart)) out[1] else colStart, nStart), 
        out,
        rep(if(is.na(colEnd)) out[length(out)] else colEnd, nEnd)
    )
    
    return(out)
}


