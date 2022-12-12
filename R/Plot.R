# 1. plotMap
# 2. plotTrack
# 3. plotPoints
# 4. plotSegments(, angle = 90, adjust = 0.5)
# 5. plotMultipleSegments - for NASC with depth


#' Plot NASCData
#' 
#' Plots a map with cruise line and points with size and (optionally) color representing NASC.
#' 
#' @inheritParams ModelData
#' @inheritParams general_plot_arguments
#' @inheritParams general_map_plot_arguments
#' @inheritParams MeanNASC
#' @inheritParams DefineLayer
#' @param UseAcousticPSU Logical: If TRUE plot only EDSUs tagged to acoustic PSUs.
#' @param UseDefaultAcousticPSUSettings Logical: If TRUE (default) use the default settings for indicating acoustic PSU in the plots (text position, color and size). Setting this to FALSE will show all acoustic PSU options of the plotting function in the StoX GUI.
#' @param AcousticPSULabelSize The text size of the plotted acoustic PSU labels.
#' @param AcousticPSULabelColor The text color of the plotted acoustic PSU labels.
#' @param AcousticPSULabelPosition The position of the text of the plotted acoustic PSU labels, one of "mean", "atMinLongitude", "atMaxLongitude", "atMinLatitude" or "atMaxLatitude".
#' @param AcousticPSULabelHjust A value to displace the acoustic PSU label with, where 0 means left-justified and 1 means right-justified, and values outside of [0, 1] are valid.
#' @param AcousticPSULabelVjust  value to displace the acoustic PSU label with, where 0 means bottom-justified and 1 means top-justified, and values outside of [0, 1] are valid.
#' @param LongitudeMin The minimum longitude limit of the plot, overriding the value derived from \code{Zoom}.
#' @param LongitudeMax The maximum longitude limit of the plot, overriding the value derived from \code{Zoom}.
#' @param LatitudeMin The minimum latitude limit of the plot, overriding the value derived from \code{Zoom}.
#' @param LatitudeMax The maximum latitude limit of the plot, overriding the value derived from \code{Zoom}.
#' @param LongitudeCenter The longitude of the point around which the plot is zoomed usinng \code{Zoom}.
#' @param LatitudeCenter The latitude of the point around which the plot is zoomed usinng \code{Zoom}.
#' @param ColorVariable The name of the variable determining the colors of the NASC data points, defaulted to "NASC" (both size AND color reflecting the NASC values). If the variable is a categorical variable (character or integer), the discrete colors can be set by the \code{PointColor} argument. If the variable is a continuous variable (numeric), the color scale can be set by the \code{PointColorScale} argument.
#' @param PointColor The colors to use when plotting the data points. If the \code{ColorVariable} is a discrete variable, a vector of color of the same length as the number of discrete values should be given, defaulted to the default ggplot2 color palette (see the scales package, and specifically the function \code{\link[scales]{hue_pal}} for how to generate these colors). If a continuous variable the colors scale can be given either as vector of colors comprising equally spaced colors of the color scale, or as the name of a color scale function with the first argument being the number of colors. The default is the \code{\link[RstoxBase]{combined.color}}. Other options for color scale function are "rainbow", "hcl.colors", "heat.colors", "terrain.colors", "topo.colors" or "cm.colors".
#' 
#' @return
#' A \code{\link{PlotAcousticTrawlSurveyData}} object.
#' 
#' @export
#' 
PlotAcousticTrawlSurvey <- function(
        #PlotMap
    #PlotHistogram
    #
    #
    #
    #MapPlotNASC
    #ScatterPlotNASC
    #HistogramPlotNASC
    #BoxPlotNASC
    ##
    #PlotNASC
    #    PlotType = c("MapPlot", "ScatterPlot", "Histogram", "BoxPlot")
    #    AddHauls = TRUE
    #    StoxBioticData 
    ##    
    #    
    #
    #
    #PlotAbundance
    #PlotSuperIndividuals
    #
    LayerDefinition = c("FunctionParameter", "FunctionInput", "PreDefined"), 
    LayerDefinitionMethod = c("WaterColumn", "HighestResolution", "Resolution", "Table"), 
    Resolution = double(), 
    LayerTable = data.table::data.table(), 
    AcousticLayer = NULL, 
    
    NASCData, 
    SumNASCData, 
    AcousticPSU, 
    
    ColorVariable = character(), 
    
    
    
    UseAcousticPSU = FALSE, 
    UseDefaultAcousticPSUSettings = TRUE, 
    AcousticPSULabelSize = numeric(), 
    AcousticPSULabelColor = character(), 
    AcousticPSULabelPosition = c("mean", "atMinLongitude", "atMaxLongitude", "atMinLatitude", "atMaxLatitude"), 
    AcousticPSULabelHjust = numeric(), 
    AcousticPSULabelVjust = numeric(), 
    
    
    #UseStratumPolygon = FALSE, 
    #StratumPolygonColor = character(), 
    #StratumPolygonLabelSize = numeric(), 
    #StratumPolygonLabelColor = character(), 
    
    
    # Options for the colors:
    UseDefaultColorSettings = TRUE, 
    PointColor = character(), 
    #PointColorScale = c("combined.color", "rainbow", "hcl.colors", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"), 
    #PointColorScale = character(),  
    TrackColor = character(), 
    LandColor = character(), 
    BorderColor = character(), 
    OceanColor = character(), 
    GridColor = character(), 
    
    # Options for the point sizes and shapes:
    UseDefaultSizeSettings = TRUE, 
    MaxPointSize = numeric(), 
    MinPointSize = numeric(), 
    TrackSize = numeric(), 
    #PointShape = integer(), 
    
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
    UseDefaultTextSettings = TRUE, 
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
    if(LayerDefinition != "PreDefined") {
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
    
    
    ## Check function inputs specifically, as these have been included in plotArguments:
    #if(missing(SumNASCData)) {
    #    stop("argument \"SumNASCData\" is missing, with no default")
    #}
    
    
    # # Set default general options:
    # plotArguments <- setDefaults(plotArguments, getRstoxBaseDefinitions("defaultPlotOptions"))
    # 
    # # Set default map plotting options:
    # plotArguments <- setDefaults(plotArguments, getRstoxBaseDefinitions("defaultMapPlotNASCOptions"))
    # 
    # # Set default NASC-plotting options:
    # plotArguments <- setDefaults(plotArguments, getRstoxBaseDefinitions("defaultMapPlotOptions"))
    
    plotArguments <- setDefaultsInStoxFunction(plotArguments, StoxFunctionName = "PlotAcousticTrawlSurvey", stoxFunctionAttributes = stoxFunctionAttributes)
    
    
    # # Apply custom specifications:
    # if(!length(plotArguments$ColorVariable)) {
    #     plotArguments$ColorVariable <- "NASC"
    # }
    
    # These must be split up:
    plotArguments$axis.title.size.x <- plotArguments$AxisTitleSize
    plotArguments$axis.title.size.y <- plotArguments$AxisTitleSize
    plotArguments$axis.text.size.x <- plotArguments$AxisTickSize
    plotArguments$axis.text.size.y <- plotArguments$AxisTickSize
    
    # Set hard coded values used in plot_lon_lat():
    plotArguments <- c(
        plotArguments, 
        list(
            lon = "Longitude", 
            lat = "Latitude", 
            type = "lp", 
            size = "NASC", 
            shape = 1, 
            alpha.point = 1, 
            alpha.track = 1
        )
    )
    
    # Rename to the argument names of the plot_lon_lat():
    traslation <- c(
        x = "SumNASCData", 
        size.track = "TrackSize", 
        size.min = "MinPointSize", 
        size.max = "MaxPointSize", 
        color = "ColorVariable",
        color.track = "TrackColor", 
        color.scale = "PointColor",  
        color.land = "LandColor", 
        color.border = "BorderColor", 
        color.ocean = "OceanColor", 
        color.grid = "GridColor", 
        zoom = "Zoom", 
        #xlim = "LongitudeLimits", 
        #ylim = "LatitudeLimits", 
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
    
    # Discard the transports if UseAcousticPSU:
    if(UseAcousticPSU) {
        # Merge in the PSUs:
        plotArguments$SumNASCData <- merge(plotArguments$SumNASCData, AcousticPSU$EDSU_PSU, by = "EDSU")
        plotArguments$SumNASCData <- subset(plotArguments$SumNASCData, !is.na(PSU))
        
        AcousticPSULabelPosition <- match.arg(AcousticPSULabelPosition)
        AcousticPSULabelPositionFun <- switch (AcousticPSULabelPosition,
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
            hjust = AcousticPSULabelHjust,
            vjust = AcousticPSULabelVjust, 
            color = AcousticPSULabelColor
        ), by = "PSU"]
        
        PSUPos <- plotArguments$SumNASCData[, AcousticPSULabelPositionFun(Longitude, Latitude), by = "PSU"]
        names(PSUPos) <- c("PSU", "x", "y")
        
        PSUText <- cbind(
            PSUText, 
            subset(PSUPos, select = c("x", "y"))
        )
        
        plotArguments$text <- as.list(PSUText)
    }
    # If not PreDefined assume SumNASCData is given and remove the NASCData, since this will cause problems when calling plot_lon_lat
    else {
        plotArguments$AcousticPSU <- NULL
    }
    
    
    # Add xlim and ylim and center for zooming:
    plotArguments$xlim <- c(plotArguments$LongitudeMin, plotArguments$LongitudeMax)
    plotArguments$ylim <- c(plotArguments$LatitudeMin, plotArguments$LatitudeMax)
    
    plotArguments$zoomCenter <- c(plotArguments$LongitudeCenter, plotArguments$LatitudeCenter)
    
    
    ### # Add StratumPolygon:
    ### if(UseStratumPolygon) {
    ###     # Create a data.table of longitude, latitude and Stratum name:
    ###     polygon <- data.table::rbindlist(getStratumPolygonList(StratumPolygon), , idcol = "Stratum")
    ###     polygon <- data.table::data.table(
    ###         longitude = polygon, 
    ###         latitude = , 
    ###         longitude = , 
    ###     )
    ###     
    ###     
    ### }
    
    
    plotArguments <- renameListByNames(
        plotArguments, 
        old = traslation,  
        new = names(traslation)
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






zoom_lon_lat <- function(
    x, 
    lon = "lon", lat = "lat", 
    xmin = NA, xmax = NA, 
    ymin = NA, ymax = NA, 
    zoom = 1, zoomCenter = c(0.5, 0.5)
) {
    
    # Zooms first from the range of the values,:
    temp <- zoom_data(x = x[[lon]], y = x[[lat]], zoom = zoom, zoomCenter = zoomCenter)
    
    # If xmin is not present:
    if(length(xmin) == 0 || is.na(xmin)){
        xmin <- temp$xlim[1]
    }
    # If xmax is not present:
    if(length(xmax) == 0 || is.na(xmax)){
        xmax <- temp$xlim[2]
    }
    # If xmin is not present:
    if(length(ymin) == 0 || is.na(ymin)){
        ymin <- temp$ylim[1]
    }
    # If xmin is not present:
    if(length(ymax) == 0 || is.na(ymax)){
        ymax <- temp$ylim[2]
    }
    
    xlim <- c(xmin, xmax)
    ylim <- c(ymin, ymax)
    
    # Adjust the aspect ratio by latitude:
    ymid <- mean(ylim)
    aspectratio <- 1 / cos(ymid * pi/180)
    
    # Return the coordinates as a ggplot object to add to a plot:
    ggplot2::coord_fixed(aspectratio, xlim = xlim, ylim = ylim)
}





plot_lon_lat <- function(
        x, 
        polygon, 
        lon = "lon", lat = "lat", 
        type = c("pl", "lp", "p", "l"), 
        size = 1, size.track = 1, size.min = 0.1, size.max = 10, 
        limitWidthFraction = 0.1, 
        color = 1, 
        color.track = 1, color.scale = combined.color(2), color.land = "grey50", color.border = "grey10", color.ocean = "grey90", color.grid = "white", color.polygon = "grey70", 
        shape = 16, 
        fill.polygon = "grey50", 
        alpha.point = 1, alpha.track = 1, alpha.polygon = 1, 
        zoom = 1, xlim = NA, ylim = NA, 
        zoomCenter = c(0.5, 0.5), 
        main = NULL, 
        text = NULL, 
        ...
)
{
    # Get the type:
    type <- RstoxData::match_arg_informative(type)
    
    # Get the optional arguments:
    lll <- list(...)
    
    # Get the map:
    gmap <- ggplot2::map_data("world")
    
    # Initiate the plot, with the map and zoom:
    p <- ggplot2::ggplot(data = x, x = lon, y = lat) + 
        ggplot2::geom_polygon(
            data = gmap, 
            ggplot2::aes_string(
                x = "long", 
                y = "lat", 
                group = "group"
            ), 
            fill = color.land, 
            color = color.border
            #color = "red"
        ) + 
        zoom_lon_lat(
            x = x, 
            lon = lon, 
            lat = lat, 
            xmin = xlim[1], 
            xmax = xlim[2], 
            ymin = ylim[1], 
            ymax = ylim[2], 
            zoom = zoom, 
            zoomCenter = zoomCenter
        ) #+ 
    #ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5)))
    
    ### # Add polygon:
    ### if(length(polygon)) {
    ###     p <- p + geom_polygon(data = polygon, 
    ###         aes_string(
    ###             x = "longitude", 
    ###             y = "latitude", 
    ###             fill = fill.polygon, 
    ###             color = color.polygon
    ###         ), 
    ###         alpha = alpha.polygon #, 
    ###         #inherit.aes = FALSE
    ###     )
    ###     
    ###     polygonCentroid <- getCentroid(polygon)
    ###     
    ###     p <- p + geom_text(data = polygonCentroid, aes_string(group="stratum", x="lon_centroid", y="lat_centroid", label="stratum"), colour=strataNameCol)
    ### }
    
    
    
    # Plot the track:
    if(grepl("l", type)) {
        plotspec <- list(
            data = x, 
            ggplot2::aes_string(
                x = lon, 
                y = lat
            ), 
            color = color.track, 
            size = size.track, 
            alpha = alpha.track, 
            show.legend = FALSE
        )
        
        path <- do.call(ggplot2::geom_point, plotspec)
    }
    else {
        path <- NULL
    }
    
    
    # Plot the points:
    if(grepl("p", type)) {
        
        # If the color variable is column in the data, but the user has specified a single color, use this color instead:
        if(!isCategorical(x[[color]]) && length(color.scale) == 1 && !is.function(try(get(color.scale), silent = TRUE))) {
            color <- color.scale
        }
        
        plotspec <- list(
            alpha = alpha.point
        )
        
        scaleStroke  <- function(size, maxWidth = 0.1) {
            exp(-size) * (1 - maxWidth) + maxWidth
        }
        
        applyScale <- function(scalefun, size, maxSize, maxWidth = 0.1, add = 0.5) {
            size <- size/max(size, na.rm = TRUE) * maxSize
            scale <- scalefun(size, maxWidth = maxWidth - add/2/maxSize) 
            size * scale + add/2
        }
        
        if(length(color) && color %in% names(x) && length(size) && size %in% names(x)) {
            
            mapping <- ggplot2::aes_string(
                x = lon, 
                y = lat, 
                color = color, 
                size = size,
                stroke = paste0( "applyScale(scaleStroke, ", size, ", maxSize = ", size.max, ", maxWidth = ", limitWidthFraction, ", add = ", size.min, ")" )
            )
        }
        else if(length(color) && color %in% names(x)) {
            mapping <- ggplot2::aes_string(
                x = lon, 
                y = lat, 
                color = color
            )
            plotspec$size <- size
        }
        else if(length(size) && size %in% names(x)) {
            
            mapping <- ggplot2::aes_string(
                x = lon, 
                y = lat, 
                size = size,
                stroke = paste0( "applyScale(scaleStroke, ", size, ", maxSize = ", size.max, ", maxWidth = ", limitWidthFraction, ", add = ", size.min, ")" )
            )
            
            plotspec$color <- color
        }
        else {
            mapping <- ggplot2::aes_string(
                x = lon, 
                y = lat
            )
            plotspec$color <- color
            plotspec$size <- size
        }
        
        plotspec$data <- x
        
        plotspec$mapping <- mapping
        
        plotspec$shape <- shape
        
        point <- do.call(ggplot2::geom_point, plotspec)
    }
    else {
        point <- NULL
    }
    
    if(startsWith(type, "l")) {
        toPlot <- list(path, point)
    }
    else {
        toPlot <- list(point, path)
    }
    toPlot <- toPlot[lengths(toPlot) > 0]
    
    for(this in toPlot) {
        p <- p + this
    }
    
    p <- p + 
        ggplot2::scale_size_continuous(range = c(0, size.max)) + 
        ggplot2::xlab(lon) + 
        ggplot2::ylab(lat)
    
    
    # Use scale_color_manual() for categorical variables and scale_color_gradientn() for numeric:
    if(isCategorical(x[[color]])) {
        if(length(color.scale)) {
            numberOfLevelsInTheColorVariable <- length(unique(x[[color]]))
            
            if(is.function(try(get(color.scale), silent = TRUE))) {
                color.scale <- do.call(color.scale, list(numberOfLevelsInTheColorVariable))
            }
            else {
                numberOfDescreteColors <- length(color.scale)
                if(length(color.scale) != length(unique(x[[color]]))) {
                    stop("The number of discrete colors (length of PointColor = ", numberOfDescreteColors, ") must match the number of levels of the categorical variable determining the colors of the points (", color, " has ", numberOfLevelsInTheColorVariable, " levels).")
                }
            }
            
            p <- p + ggplot2::scale_color_manual(values = color.scale)
            
            # User thicker lines in legend:
            p <- p + ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(stroke = size.max * 0.1, size = size.max)))# + 
            #ggplot2::guides(size = ggplot2::guide_legend(override.aes = list(stroke = 2)))
        }
    }
    else {
        # Hard code to use 10 color steps:
        ncolors <- 10
        if(is.function(try(get(color.scale), silent = TRUE))) {
            color.scale <- do.call(color.scale, list(ncolors))
            p <- p + ggplot2::scale_color_gradientn(colors = color.scale)
        }
        else {
            p <- p + ggplot2::scale_color_gradientn(colors = color.scale)
        }
        
        
        ## User thicker lines in legend:
        #p <- p + ggplot2::guides(size = ggplot2::guide_legend(override.aes = list(stroke = size.max * 0.1)))
        
        
        ## Bigger symbols in legend:
        #p <- p + ggplot2::guides(
        #    size = ggplot2::guide_legend(override.aes = list(stroke = 2)), 
        #    color = ggplot2::guide_colorbar(barheight = ggplot2::unit(50, "cm"))
        #)
    }
    
    p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(size = if(length(lll$axis.title.size.x)) lll$axis.title.size.x else 10), 
        axis.title.y = ggplot2::element_text(size = if(length(lll$axis.title.size.y)) lll$axis.title.size.y else 10), 
        axis.text.x = ggplot2::element_text(size = if(length(lll$axis.text.size.x)) lll$axis.text.size.x else 10), 
        axis.text.y = ggplot2::element_text(size = if(length(lll$axis.text.size.y)) lll$axis.text.size.y else 10), 
        legend.text = ggplot2::element_text(size = if(length(lll$legend.text.size)) lll$legend.text.size else 10), 
        legend.title = ggplot2::element_text(size = if(length(lll$legend.title.size)) lll$legend.title.size else 10),
        panel.background = ggplot2::element_rect(fill = color.ocean, color = "grey80"),
        panel.grid.major = ggplot2::element_line(color = color.grid), 
        panel.grid.minor = ggplot2::element_line(color = color.grid)
    )
    
    p <- p + ggplot2::labs(fill = expression(paste("NASC (", m^2, nmi^{-2}, ")")))
    
    if(length(main)) {
        p <- p + ggtitle(main)
    }
    
    if(length(text) && is.list(text)) {
        p <- p + ggplot2::annotate(
            "text", 
            x = text$x, 
            y = text$y, 
            label = text$label, 
            hjust = text$hjust, 
            vjust = text$vjust, 
            size = text$size, 
            color = text$color
        )
    }
    
    ## Set order of the legends to size first then color:
    #p <- p + ggplot2::guides(
    #    size = ggplot2::guide_legend(order = 1), 
    #    colour = ggplot2::guide_legend(order = 2)
    #)
    
    
    return(p)
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


