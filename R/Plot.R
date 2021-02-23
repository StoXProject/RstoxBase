# 1. plotMap
# 2. plotTrack
# 3. plotPoints
# 4. plotSegments(, angle = 90, adjust = 0.5)
# 5. plotMultipleSegments - for NASC with depth


##################################################
##################################################
#' Plot NASCData
#' 
#' Plots a map with cruise line and points with size and (optionally) color representing NASC.
#' 
#' @inheritParams ModelData
#' @inheritParams general_report_arguments
#'
#' @details This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLenght, or average IndividualTotalLenght for each IndiivdualAge and Stratum.
#' 
#' @return
#' A \code{\link{ReportSuperIndividualsData}} object.
#' 
#' @examples
#' 
#' @seealso
#' @noRd
#' 
PlotNASCData <- function(
    NASCData, 
    ColorScale = c("combined.color", "rainbow", "hcl.colors", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"), 
    Zoom = 1, 
    MinimumSize = 0, 
    MaximumSize = 10, 
    TrackOnTop = FALSE
) {
    
    ColorScale <- match.arg(ColorScale)
    
    # Add a function StoxDataStartMiddleStopPosition later!!!
    
    ncolors <- 10
    plot_lon_lat(
        x = NASCData, 
        lon = "Longitude", lat = "Latitude", 
        type = if(TrackOnTop) "pl" else "lp", 
        size = "NASC", size.track = 1, size.range = c(MinimumSize, MaximumSize), 
        color = "NASC", color.track = 1, 
        color.scale = do.call(ColorScale, list(ncolors)), 
        shape = 19, 
        alpha = 1, alpha.track = 1, 
        zoom = Zoom, xlim = NA, ylim = NA, 
        offset = c(0.5, 0.5), 
        axis.title.size.x = 20, 
        axis.title.size.y = 20, 
        axis.text.size.x = 20, 
        axis.text.size.y = 20, 
        legend.text.size = 20, 
        legend.title.size = 20
    )
}


##################################################
##################################################
#' Plot NASCData
#' 
#' Plots a map with cruise line and points with size and (optionally) color representing NASC.
#' 
#' @inheritParams ModelData
#' @inheritParams general_report_arguments
#'
#' @details This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLenght, or average IndividualTotalLenght for each IndiivdualAge and Stratum.
#' 
#' @return
#' A \code{\link{ReportSuperIndividualsData}} object.
#' 
#' @examples
#' 
#' @seealso 
#' @noRd
#' 
PlotSumNASCData <- function(
    SumNASCData, 
    ColorScale = c("combined.color", "rainbow", "hcl.colors", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"), 
    Zoom = 1, 
    MinimumSize = 0, 
    MaximumSize = 10, 
    TrackOnTop = FALSE
) {
    
    ColorScale <- match.arg(ColorScale)
    
    # Add a function StoxDataStartMiddleStopPosition later!!!
    
    ncolors <- 10
    plot_lon_lat(
        x = SumNASCData$Data, 
        lon = "Longitude", lat = "Latitude", 
        type = if(TrackOnTop) "pl" else "lp", 
        size = "NASC", size.track = 1, size.range = c(MinimumSize, MaximumSize), 
        color = "NASC", color.track = 1, 
        color.scale = do.call(ColorScale, list(ncolors)), 
        shape = 19, 
        alpha = 1, alpha.track = 1, 
        zoom = Zoom, xlim = NA, ylim = NA, 
        offset = c(0.5, 0.5), 
        axis.title.size.x = 20, 
        axis.title.size.y = 20, 
        axis.text.size.x = 20, 
        axis.text.size.y = 20, 
        legend.text.size = 20, 
        legend.title.size = 20
    )
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
        
        # Apply the zoom_
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
    size = 1, size.track = 1, size.range = c(0.1, 10), 
    color = 1, color.track = 1, 
    color.scale = combined.color(2), 
    shape = 16, 
    alpha = 1, alpha.track = 1, 
    zoom = 1, xlim = NA, ylim = NA, 
    offset = c(0.5, 0.5), 
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
            )
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
            data = x, 
            alpha = alpha
        )
        
        if(length(color) && color %in% names(x) && length(size) && size %in% names(x)) {
            mapping <- ggplot2::aes_string(
                x = lon, 
                y = lat, 
                color = color, 
                size = size
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
                size = size
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
        
        plotspec$mapping <- mapping
        
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
        ggplot2::scale_color_gradientn(colours = color.scale) + 
        ggplot2::scale_size_continuous(range = size.range) + 
        ggplot2::xlab(lon) + 
        ggplot2::ylab(lat)
    
    
    
    p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(size = if(length(lll$axis.title.size.x)) lll$axis.title.size.x else 10), 
        axis.title.y = ggplot2::element_text(size = if(length(lll$axis.title.size.y)) lll$axis.title.size.y else 10), 
        axis.text.x = ggplot2::element_text(size = if(length(lll$axis.text.size.x)) lll$axis.text.size.x else 10), 
        axis.text.y = ggplot2::element_text(size = if(length(lll$axis.text.size.y)) lll$axis.text.size.y else 10), 
        legend.text = ggplot2::element_text(size = if(length(lll$legend.text.size)) lll$legend.text.size else 10), 
        legend.title = ggplot2::element_text(size = if(length(lll$legend.title.size)) lll$legend.title.size else 10), 
    )
    
    return(p)
}



zoom_xlim_ylim <- function(xlim, ylim, zoom = 1, offset = c(0.5, 0.5)){
    # Get the center of the plot, applying the offset:
    xcenter <- min(xlim) + offset[1] * diff(xlim)
    ycenter <- min(ylim) + offset[2] * diff(ylim)
    if(zoom <= 0){
        warning("The value of 'zoom' must be positive, with default 1 implying no zoom. The default was used.")
    }
    
    # Zoom:
    fact <- c(-1, 1) * zoom
    xlim <- xcenter + fact * diff(xlim) / 2
    ylim <- ycenter + fact * diff(ylim) / 2
    list(xlim = xlim, ylim = ylim)
}










#' Returns a color vector of rainbow colors which are desaturated by 's' and darkened by 'v' to obtain the "combined" color scale.
#'
#' @param n The number of colors to generate.
#' @param s The range of the saturation values applied to the colors, where lower values suppress saturation. The default applies less strong colors at the higher values of x.
#' @param v The range of the value applied to the colors, where lower values causes darker colors. The default applies less daker colors at the lover values of x.
#' @param start,end The start and end colors, given as values in [0,1].
#' @param flip Logical: If TRUE the color scale should be reversed.
#' @param alpha The transparency.
#' @param ... Not used, but allowing for unused arguments.
#'
#' @return
#' @noRd
#'
#' @examples
#' \dontrun{
#' plot(1:10, 1:10, pch = 19, cex = 10, col = combined.color(10))
#' plot(1:10, 1:10 + 1, pch = 19, cex = 10, col = combined.color(10, flip=TRUE))
#' }
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
    
    # Add the start and end colour:
    out <- c(
        rep(if(is.na(colStart)) out[1] else colStart, nStart), 
        out,
        rep(if(is.na(colEnd)) out[length(out)] else colEnd, nEnd)
    )
    
    
    
    return(out)
}
