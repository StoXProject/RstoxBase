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
#' @param ColorVariable The name of the variable determining the colors of the NASC data points, defaulted to "NASC" (both size AND color reflecting the NASC values). If the variable is a categorical variable (character or integer), the discrete colors can be set by the \code{PointColor} argument. If the variable is a continuous variable (numeric), the color scale can be set by the \code{PointColorScale} argument.
#' @param PointColor The discrete colors to use when plotting the data points, defaulted to the default ggplot2 color palette (see the scales package, and specifically the function \code{\link[scales]{hue_pal}} for how to generate these colors). 
#' @param PointColorScale The continuous color scale to use when plotting the data points, given as the name of a color scale function with the first argument being the number of colors. The default is the \code{\link[RstoxBase]{combined.color}}. Other options are "rainbow", "hcl.colors", "heat.colors", "terrain.colors", "topo.colors" or "cm.colors".
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
    SumNASCData, 
    
    ColorVariable = character(), 
    
    # Options for the colors:
    UseDefaultColorSettings = TRUE, 
    PointColor = character(), 
    #PointColorScale = c("combined.color", "rainbow", "hcl.colors", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"), 
    PointColorScale = character(),  
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
    LongitudeLimits = numeric(), 
    LatitudeLimits = numeric(), 
    
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
    
    
    
    # Get the formals:
    plotArguments <- allargs()
    
    # Check function inputs specifically, as these have been included in plotArguments:
    if(missing(SumNASCData)) {
        stop("argument \"SumNASCData\" is missing, with no default")
    }
    
    # # Set default general options:
    # plotArguments <- setDefaults(plotArguments, getRstoxBaseDefinitions("defaultPlotOptions"))
    # 
    # # Set default map plotting options:
    # plotArguments <- setDefaults(plotArguments, getRstoxBaseDefinitions("defaultMapPlotNASCOptions"))
    # 
    # # Set default NASC-plotting options:
    # plotArguments <- setDefaults(plotArguments, getRstoxBaseDefinitions("defaultMapPlotOptions"))
    
    plotArguments <- setDefaultsInStoxFunction(plotArguments)
    
    
    
    
    
    
    
    # Use only the Data table of the SumNASCData:
    plotArguments$SumNASCData <- plotArguments$SumNASCData$Data
    
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
            alpha = 1, 
            alpha.track = 1, 
            offset = c(0.5, 0.5)
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
        color.scale = "PointColorScale", 
        color.discrete = "PointColor", 
        color.land = "LandColor", 
        color.border = "BorderColor", 
        color.ocean = "OceanColor", 
        color.grid = "GridColor", 
        zoom = "Zoom", 
        xlim = "LongitudeLimits", 
        ylim = "LatitudeLimits", 
        legend.text.size = "LegendTextSize", 
        legend.title.size = "LegendTitleSize", 
        main = "Title"
    )
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
    xlim = NA, ylim = NA, zoom = 1, offset = c(0.5, 0.5)
) {
    
    # If xlim or ylim is not present:
    if(length(xlim) == 0 || is.na(xlim) || length(ylim) == 0 || is.na(ylim)){
        # Get xlim and ylim:
        xlim <- range(x[[lon]], na.rm = TRUE)
        ylim <- range(x[[lat]], na.rm = TRUE)
        
        # Apply the zoom:
        temp <- zoom_xlim_ylim(xlim, ylim, zoom, offset)
        xlim <- temp$xlim
        ylim <- temp$ylim
    }
    
    # Adjust the aspect ratio by latitude:
    ymid <- mean(ylim)
    aspectratio <- 1 / cos(ymid * pi/180)
    
    # Return the coordinates as a ggplot object to add to a plot:
    ggplot2::coord_fixed(aspectratio, xlim = xlim, ylim = ylim)
}






plot_lon_lat <- function(
        x, 
        lon = "lon", lat = "lat", 
        type = c("pl", "lp", "p", "l"), 
        size = 1, size.track = 1, size.min = 0.1, size.max = 10, limitWidthFraction = 0.1, 
        color = 1, color.track = 1, 
        color.scale = combined.color(2), color.discrete = NULL, 
        color.land = "grey50", color.border = "grey10", color.ocean = "grey90", color.grid = "white", 
        shape = 16, 
        alpha = 1, alpha.track = 1, 
        zoom = 1, xlim = NA, ylim = NA, 
        offset = c(0.5, 0.5), 
        main = NULL, 
        ...
)
{
    
    # Get the type:
    type <- match.arg(type)
    
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
            xlim = xlim, 
            ylim = ylim, 
            zoom = zoom, 
            offset = offset
        ) #+ 
    #ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5)))
    
    # Plot the lines:
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
        
        path <- do.call(ggplot2::geom_path, plotspec)
    }
    else {
        path <- NULL
    }
    
    
    
    # Plot the points:
    if(grepl("p", type)) {
        
        plotspec <- list(
            alpha = alpha
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
    if(is.character(x[[color]]) || is.integer(x[[color]])) {
        if(length(color.discrete)) {
            numberOfDescreteColors <- length(color.discrete)
            numberOfLevelsInTheColorVariable <- length(unique(x[[color]]))
            if(length(color.discrete) != length(unique(x[[color]]))) {
                stop("The number of discrete colors (length of PointColor = ", numberOfDescreteColors, ") must match the number of levels of the categorical variable determining the colors of the points (", color, " has ", numberOfLevelsInTheColorVariable, " levels).")
            }
            p <- p + ggplot2::scale_color_manual(values = color.discrete)
        
            
        }
    }
    else {
        # Hard code to use 10 color steps:
        ncolors <- 10
        color.scale <- do.call(color.scale, list(ncolors))
        p <- p + ggplot2::scale_color_gradientn(colors = color.scale)
        
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
    
    
    
    
    return(p)
}





plot_lon_lat_new <- function(
        x, 
        lon = "lon", lat = "lat", 
        type = c("pl", "lp", "p", "l"), 
        size = 1, size.track = 1, size.min = 0.1, size.max = 10, limitWidthFraction = 0.1, 
        color = 1, color.track = 1, 
        color.scale = combined.color(2), color.discrete = NULL, 
        color.land = "grey50", color.border = "grey10", color.ocean = "grey90", color.grid = "white", 
        shape = 16, 
        alpha = 1, alpha.track = 1, 
        zoom = 1, xlim = NA, ylim = NA, 
        offset = c(0.5, 0.5), 
        main = NULL, 
        ...
)
{
    
    # Get the type:
    type <- match.arg(type)
    
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
            xlim = xlim, 
            ylim = ylim, 
            zoom = zoom, 
            offset = offset
        ) #+ 
    #ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5)))
    
    # Plot the lines:
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
        
        path <- do.call(ggplot2::geom_path, plotspec)
    }
    else {
        path <- NULL
    }
    
    
    
    # Plot the points:
    if(grepl("p", type)) {
        
        plotspec <- list(
            alpha = alpha
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
    if(is.character(x[[color]]) && is.integer(x[[color]])) {
        if(length(color.discrete)) {
            numberOfDescreteColors <- length(color.discrete)
            numberOfLevelsInTheColorVariable <- length(unique(x[[color]]))
            if(length(color.discrete) != length(unique(x[[color]]))) {
                stop("The number of discrete colors (length of PointColor = ", numberOfDescreteColors, ") must match the number of levels of the categorical variable determining the colors of the points (", color, " has ", numberOfLevelsInTheColorVariable, " levels).")
            }
            p <- p + ggplot2::scale_color_manual(values = color.discrete)
        }
    }
    else {
        p <- p + ggplot2::scale_color_gradientn(colors = color.scale)
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
    
    # Bigger symbols in legend:
    p <- p + ggplot2::guides(
        size = ggplot2::guide_legend(override.aes = list(stroke = 2)), 
        color = ggplot2::guide_colorbar(barheight = ggplot2::unit(2, "cm"))
    )
    
    
    return(p)
}











zoom_xlim_ylim <- function(xlim, ylim, zoom = 1, offset = c(0.5, 0.5)){
    # Get the center of the plot, applying the offset:
    xcenter <- min(xlim) + offset[1] * diff(xlim)
    ycenter <- min(ylim) + offset[2] * diff(ylim)
    if(zoom <= 0){
        warning("StoX: The value of 'zoom' must be positive, with default 1 implying no zoom. The default was used.")
    }
    
    # Zoom:
    fact <- c(-1, 1) * zoom
    xlim <- xcenter + fact * diff(xlim) / 2
    ylim <- ycenter + fact * diff(ylim) / 2
    list(xlim = xlim, ylim = ylim)
}










#' Combined rainbow and desaturation color scale
#' 
#' Returns a color vector of rainbow colors which are desaturated by 's' and darkened by 'v' to obtain the "combined" color scale.
#'
#' @param n The number of colors to generate.
#' @param s The range of the saturation values applied to the colors, where lower values suppress saturation. The default applies less strong colors at the higher values of x.
#' @param v The range of the value applied to the colors, where lower values causes darker colors. The default applies less darker colors at the lover values of x.
#' @param start The start color, given as a value in between 0 and 1.
#' @param end The end color, given as a value in between 0 and 1.
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
combined.color <- function(n, nStart = 0, nEnd = floor(n / 5), colStart = "white", colEnd = NA, s = c(0.3, 1), v = c(1, 0.7), start = 0.8, end = 0, flip = FALSE, alpha = 1, ...) {
    
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
