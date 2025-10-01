# Trick to avoid warning for imported lwgeom and maps
# lwgeom: Used for calculation of area of sf object in simplifyStratumPolygon(). Added here since it is only suggested by sf, but RstoxBase needs it.
# Taken from ?lwgeom ::st_is_polygon_cw:
test_lwgeom <- function() {
    polys <- sf::st_sf(cw = c(FALSE, TRUE), sf::st_as_sfc(c('POLYGON ((0 0, 1 0, 1 1, 0 0))', 'POLYGON ((1 1, 2 2, 2 1, 1 1))')))
    lwgeom::st_is_polygon_cw(polys)
}

# maps: Only suggested by ggplot2, but needed for map_data in plotting functions.
test_maps <- function() {
    maps::map('france', fill = TRUE, col = 1:10)
}

##################################################
##################################################
#' StoX multipolygon WKT
#'
#'This data format is used in \eqn{\le} \href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13250}{StoX 2.7} to define strata polygons. 
#' 
#' @details 
#' A data frame used in early versions of \href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13250}{StoX} to define strata polygons, which combine one variable with strata names and one variable consisting of \href{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}{multipolygon.wkt} strings. Well-known text (WKT) is a text markup language for representing vector geometry objects.
#' 
#' @references 
#' Additonal information on WKT multipolygon specification and StoX can be found here:
#' 
#' \href{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}{https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry}
#' 
#' Johnsen, E., Totland, A., Skålevik, Å., Holmin, A. J., Dingsør, G. E., Fuglebakk, E., & Handegard, N. O. (2019). StoX: An open source software for marine survey analyses. Methods in Ecology and Evolution, 10(9), 1523-1528. \doi{10.1111/2041-210X.13250}
#' 
#' @name StoX_multipolygon_WKT
#'
NULL

##################################################
##################################################
#' General parameters of RstoxBase
#' 
#' All functions referring to a project, a model, a process or an output table use the same parameters, listed here.
#' 
#' @param processData The current data produced by a previous instance of the function.
#' @param UseProcessData Logical: If TRUE use the existing function output in the process. 
#' @param LayerType Character: A string naming the type of model, either "Acoustic" or "Biotic".
#' @param PSUType Character: A string naming the type of model, either "Acoustic" or "Biotic" 
#' 
#' @name general_arguments
#' 
NULL

##################################################
##################################################
#' General report parameters of RstoxBase
#' 
#' @param ReportFunction The name of a function to report the Baseline process output by. This must be a function returning a single value. See \code{\link{ReportFunctions}} for implemented funcitons.
#' @param TargetVariable The variable to report.
#' @param TargetVariableUnit The unit to use for the \code{TargetVariable}. See RstoxData::StoxUnits for possible units (look for the appropriate quantity, e.g. "length" for IndividualTotalLength, and use the shortname in the \code{TargetVariableUnit}).
#' @param GroupingVariables The variables to report by. For most applications \code{GroupingVariables} should include "Survey" and "SpeciesCategory", unless the user needs to sum over all Survey or SpeciesCategory.
#' @param InformationVariables Variables to include as columns to the end of the report table. These cannot have more unique combinations than the \code{GroupingVariables}.
#' @param RemoveMissingValues Logical: If TRUE, remove missing values (NAs) from the \code{TargetVariable}. The default (FALSE) implies to report NA if at least one of the values used in the \code{ReportFunction} is NA. Use \code{RemoveMissingValues} = TRUE with extreme caution, as it may lead to under-estimation. E.g., if \code{RemoveMissingValues} = TRUE and a super-individual lacks \code{IndividualRoundWeight}, \code{Biomass} will be NA, and the portion of \code{Abundance} distributed to that super-individual will be excluded when summing \code{Biomass} (but included when summing \code{Abundance}). It is advised to always run with \code{RemoveMissingValues} = FALSE first, and make a thorough investigation to identify the source of any missing values. The function \code{link{ImputeSuperIndividuals}} can be used to impute the missing information from other super-individuals.
#' @param Filter A string with an R expression to filter out unwanted rows of the report, e.g. "IndividualAge \%notin\% NA" or "Survey \%notin\% NA & SpeciesCategory \%notin\% NA".
#' @param WeightingVariable The variable to weight by. Only relevant for \code{ReportFunction} "weighted.mean". Note that missing values in the \code{WeightingVariable} results in missing value from the \code{weighted.mean} function as per the documentation of this function, regardles of any \code{RemoveMissingValues}. 
#' @param ConditionOperator,ConditionValue Expressions (strings) giving the condition for the \code{ReportFunction} \code{number} and \code{fractionOfOccurrence}. Supported values for \code{ConditionOperator} are "\%in\%", "\%notin\%", "==", "!=", "\%notequal\%", "<", "<=", ">=", ">". The \code{ConditionOperator} and \code{ConditionValue} are pasted for use in data.table.
#' 
#' @param FractionOverVariable When \code{ReportFunction} is a fraction ("fractionOfOccurrence" or "fractionOfSum") \code{FractionOverVariable} is a string naming the variable (one of the \code{GroupingVariables}) to sum over in the denominator of the fraction.
#' 
#' @name general_report_arguments
#' 
NULL



##################################################
##################################################
#' The fraction of occurrence function
#' 
#' The \code{ReportFunction} \code{fractionOfOccurrence} represents the fraction of occurrence for two different grouping variables, where the \code{GroupingVariables} is used in the numerator and the \code{GroupingVariables} except the \code{FractionOverVariable} is used in the denominator. The occurrence is defined as 1 if the \code{TargetVariable} meets the condition defined by \code{ConditionOperator} and \code{ConditionValue}. 
#' 
#' E.g., if \code{GroupingVariables} is c("Survey", "SpeciesCategory", "Stratum"), \code{ractionOverVariable} is "Stratum", \code{TargetVariable} is "IndividualAge", \code{ConditionOperator} is ">" and \code{ConditionValue} is 0, the number of individuals with age larger than 0 for each stratum is divided by the total number of individuals of age large than 0 of the survey (for each SpeciesCategory).
#' 
#' Warning! This function is used only to construct an expression to be evaluated in a data.table. Do not use this in R as a regular function like the other \code{\link{ReportFunctions}}.
#' 
#' @name fractionOfOccurrence
#' 
NULL


##################################################
##################################################
#' The fraction of occurrence function
#' 
#' The \code{ReportFunction} \code{fractionOfSum} represents the fraction of the sum of the \code{TargetVariable} for two different grouping variables, where the \code{GroupingVariables} is used in the numerator and the \code{GroupingVariables} except the \code{FractionOverVariable} is used in the denominator. 
#' 
#' E.g., if \code{GroupingVariables} is c("Survey", "SpeciesCategory", "Stratum"), \code{FractionOverVariable} is "Stratum", \code{TargetVariable} is "CatchFractionWeight", the total catch fraction weight for each stratum is divided by the total catch fraction weight of the survey (for each SpeciesCategory).
#' 
#' Warning! This function is used only to construct an expression to be evaluated in a data.table. Do not use this in R as a regular function like the other \code{\link{ReportFunctions}}.
#' 
#' @name fractionOfSum
#' 
NULL




##################################################
##################################################
#' Report functions for baseline process output
#' 
#' The following functions are used by StoX to report from a Baseline process output:
#'  \itemize{
#'    \item{\code{\link[base]{sum}}}
#'    \item{\code{\link[base]{mean}}}
#'    \item{\code{\link[stats]{weighted.mean}}}
#'    \item{\code{\link[stats]{median}}}
#'    \item{\code{\link[base]{min}}}
#'    \item{\code{\link[base]{max}}}
#'    \item{\code{\link[stats]{sd}}}
#'    \item{\code{\link[stats]{var}}}
#'    \item{\code{\link{cv}}}
#'    \item{\code{\link{number}}}
#'    \item{\code{\link{fractionOfOccurrence}}}
#'    \item{\code{\link{fractionOfSum}}}
#'  }
#' 
#' @name ReportFunctions
#' 
NULL


##################################################
##################################################
#' General ploting parameters of RstoxBase
#' 
#' @param UseDefaultMapSettings Logical: If TRUE (default) use the default settings of the map, specifically LandColor, BorderColor, OceanColor and GridColor. Setting this to FALSE will show all map options of the plotting function in the StoX GUI.
#' @param UseDefaultPointSettings Logical: If TRUE (default) use the default point settings of the plot, specifically PointColor, MaxPointSize and MinPointSize. Setting this to FALSE will show all point size options of the plotting function in the StoX GUI.
#' @param UseDefaultTrackSettings Logical: If TRUE (default) use the default point size settings of the plotting function. Setting this to FALSE will show all point size options of the plotting function in the StoX GUI.
#' @param UseDefaultAspectSettings Logical: If TRUE (default) use the default aspect settings of the plot, specifically Zoom, LongitudeMin, LongitudeMax, LatitudeMin, LatitudeMax, LongitudeCenter and LatitudeCenter. Setting this to FALSE will show all aspect options of the plotting function in the StoX GUI.
#' @param UseDefaultLabelSettings Logical: If TRUE (default) use the default text settings of the plot, specifically Title, AxisTitleSize, AxisTickSize, LegendTitleSize and LegendTextSize. Setting this to FALSE will show all text options of the plotting function in the StoX GUI.
#' @param UseDefaultFileSettings Logical: If TRUE (default) use the default file settings of the output file of the plot, specifically Format, Width, Height and DotsPerInch. Setting this to FALSE will show all file options of the plotting function in the StoX GUI.
#' @param UseDefaultAcousticPSULabelSettings Logical: If TRUE (default) use the default settings for acoustic PSU labels in the plots (text position, color and size). Setting this to FALSE will show all acoustic PSU options of the plotting function in the StoX GUI.
#' @param UseDefaultStratumPolygonSettings Logical: If TRUE (default) use the default settings for plotting the StratumPolygon. Setting this to FALSE will show all Stratum plotting options of the plotting function in the StoX GUI.
#' @param Title Character: The title of the plot, defaulted to no title.
#' @param AxisTitleSize Numeric: The size of the axis titles, defaulted to 20.
#' @param AxisTickSize Numeric: The size of the axis tick marks, defaulted to 20.
#' @param LegendTitleSize Numeric: The size of the legend title, defaulted to 20.
#' @param LegendTextSize Numeric: The size of the legend text, defaulted to 20.
#' 
#' @name general_plot_arguments
#' 
NULL

##################################################
##################################################
#' General file writing parameters for plots in RstoxBase
#' 
#' @param Format Character: The format of the output file from plotting functions, defaulted to "png". Other options are defined by \code{\link[ggplot2]{ggsave}}, including "tiff", "jpeg"  and "pdf".
#' @param Width Numeric: The width of the plot in centimeters, defaulted to 17, as per the instructions to authors for submission to the ICES Journal og Marine Science.
#' @param Height Numeric: The height of the plot in centimeters, defaulted to 17.
#' @param DotsPerInch Numeric: The number of dots per inch (DPI), defaulted to 1200.
#' 
#' @name general_file_plot_arguments
#' 
NULL


##################################################
##################################################
#' General map plot parameters of RstoxBase (mostly colors)
#' 
#' @param ShowMap Logical: If TRUE include the map in the plot.
#' @param LandColor Character: The color to use for the land, defaulted to the color used for land in the StoX GUI map (yellow color, rgb(253, 254, 204, maxColorValue = 255)).
#' @param BorderColor Character: The color to use for the borders on land, defaulted to "grey50".
#' @param OceanColor Character: The color to use for the (ocean) background, defaulted to "white", as in the StoX GUI map.
#' @param GridColor Character: The color to use for the longitude/latitude grid lines, defaulted to the color used for the longitude/latitude grid in the StoX GUI map (blue color, rgb(223, 242, 255, maxColorValue = 255).
#' 
#' @name general_map_plot_arguments
#' 
NULL


##################################################
##################################################
#' General track plotting parameters of RstoxBase
#' 
#' @param TrackPointColor Character: The colors to use when plotting the data points. The colors scale can be given either as vector of colors comprising equally spaced colors of the color scale, or as the name of a color scale function with the first argument being the number of colors. The default is the \code{\link[RstoxBase]{combined.color}}. Other options for color scale function are "rainbow", "hcl.colors", "heat.colors", "terrain.colors", "topo.colors" or "cm.colors".
#' @param MaxTrackPointSize Numeric: The maximum size of the points..
#' @param TrackPointSize Numeric: The size of the points.
#' @param TrackPointShape The shape of the points, as defined for the argument \code{pch} in \code{\link{points}}.
#' @param TrackColor Character: The color to use for the vessel track, defaulted to "black".
#' @param TrackLineWidth Numeric: The width of the track, defaulted to 1.
#' 
#' @name general_track_plot_arguments
#' 
NULL


##################################################
##################################################
#' General stratum plotting parameters of RstoxBase
#' 
#' @param ShowStratumPolygon Logical: If TRUE include the stratumPolygon. in the plot.
#' @param StratumPolygonColor Character: The color palette to use for the strata. The default ("hue") is the default HUE color used by ggplot2. See the \code{Palettes} section in \code{\link[ggplot2]{scale_fill_brewer}} for a list of options (both Diverging, Qualitative and Sequential color palettes are possible).
#' @param StratumPolygonBorderColor Character: The single color to use for the polygon borders.
#' @param StratumPolygonBorderLineWidth Numeric: The line width of the polygon borders.
#' 
#' @name general_stratum_plot_arguments
#' 
NULL


##################################################
##################################################
#' General map plot aspect parameters of RstoxBase
#' 
#' @param Zoom Numeric: The zoom of the plot, defaulted to 1, which uses the range of the data as the plot limits. Higher values zooms out and lower values zooms in.
#' @param LongitudeMin Numeric: The minimum longitude limit of the plot, overriding the value derived from \code{Zoom}.
#' @param LongitudeMax Numeric: The maximum longitude limit of the plot, overriding the value derived from \code{Zoom}.
#' @param LatitudeMin Numeric: The minimum latitude limit of the plot, overriding the value derived from \code{Zoom}.
#' @param LatitudeMax Numeric: The maximum latitude limit of the plot, overriding the value derived from \code{Zoom}.
#' @param LongitudeCenter Numeric: The longitude of the point around which the plot is zoomed usinng \code{Zoom}.
#' @param LatitudeCenter Numeric: The latitude of the point around which the plot is zoomed usinng \code{Zoom}.
#' 
#' @name general_map_aspect_plot_arguments
#' 
NULL


##################################################
#' General map plot parameters of RstoxBase
#' 
#' @param AcousticPSULabelSize Numeric: The size of the AcousticPSU label.
#' @param AcousticPSULabelColor Character: The color of the AcousticPSU label.
#' @param AcousticPSULabelPosition Character: A string determinig where to place the AcousticPSU label, one of "mean", "atMinLongitude", "atMaxLongitude", "atMinLatitude" or "atMaxLatitude".
#' @param AcousticPSULabelHjust,AcousticPSULabelVjust Numeric: The hjust and vjust of the AcousticPSU label for more detailed positioning in addition to the \code{AcousticPSULabelPosition}.
#' @param ShowOnlyAcousticPSU Logical: If TRUE plot only EDSUs tagged to acoustic PSUs.
#' @param ShowAcousticPSULabel Logical: If TRUE show the labels of the acoustic PSUs in the plot.
#' 
#' @name general_AcousticPSU_plot_arguments
#' 
NULL


##################################################
##################################################
#' Model data used by RstoxBase
#' 
#' The model data of a StoX model are the data generated during the model run based on input data and user settings and resources given in the project description (project.json file). Model data are transient and only exists from a process has been run until the project is closed.
#' 
#' @param StratumAreaData The \code{\link{StratumAreaData}} data.
#' @param TransectDesignData The \code{\link{TransectDesignData}} data.
#' @param ReportTransectDesignData The \code{\link{ReportTransectDesignData}} data.
#' @param WriteTransectDesignData The \code{\link{WriteTransectDesignData}} data.
#' @param PlotTransectDesignData The \code{\link{PlotTransectDesignData}} data.
#' @param NASCData The \code{\link{NASCData}} data.
#' @param SumNASCData The \code{\link{SumNASCData}} data.
#' @param MeanNASCData The \code{\link{MeanNASCData}} data.
#' @param LengthDistributionData The \code{\link{LengthDistributionData}} data.
#' @param SumLengthDistributionData The \code{\link{SumLengthDistributionData}} data.
#' @param MeanLengthDistributionData The \code{\link{MeanLengthDistributionData}} data.
#' @param AssignmentLengthDistributionData The \code{\link{AssignmentLengthDistributionData}} data.
#' @param DensityData The \code{\link{DensityData}} data.
#' @param ReportDensityData  The \code{\link{ReportDensityData}} data.
#' @param MeanDensityData The \code{\link{MeanDensityData}} data.
#' @param SpeciesCategoryCatchData The \code{\link{SpeciesCategoryCatchData}} data.
#' @param ReportSpeciesCategoryCatchData The \code{\link{ReportSpeciesCategoryCatchData}} data.
#' @param SumSpeciesCategoryCatchData The \code{\link{SumSpeciesCategoryCatchData}} data.
#' @param MeanSpeciesCategoryCatchData The \code{\link{MeanSpeciesCategoryCatchData}} data.
#' @param PreySpeciesCategoryCatchData The \code{\link{PreySpeciesCategoryCatchData}} data.
#' @param ReportPreySpeciesCategoryCatchData The \code{\link{ReportPreySpeciesCategoryCatchData}} data.
#' @param QuantityData The \code{\link{QuantityData}} data.
#' @param ReportQuantityData The \code{\link{ReportQuantityData}} data.
#' @param IndividualsData The \code{\link{IndividualsData}} data.
#' @param SuperIndividualsData The \code{\link{SuperIndividualsData}} data.
#' @param ReportSuperIndividualsData The \code{\link{ReportSuperIndividualsData}} data.
#' @param PlotAcousticTrawlSurveyData The \code{\link{PlotAcousticTrawlSurveyData}} data.
#' @param BioticData \code{\link[RstoxData]{BioticData}} (defined in RstoxData).
#' @param AcousticData \code{\link[RstoxData]{AcousticData}} (defined in RstoxData).
#' @param LandingData \code{\link[RstoxData]{LandingData}} (defined in RstoxData).
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} (defined in RstoxData).
#' @param StoxAcousticData \code{\link[RstoxData]{StoxAcousticData}} (defined in RstoxData).
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} (defined in RstoxData).
#' @param MergeStoxBioticData \code{\link[RstoxData]{MergeStoxBioticData}} (defined in RstoxData).
#' @param MergeStoxAcousticData \code{\link[RstoxData]{MergeStoxAcousticData}} (defined in RstoxData).
#' @param ICESBioticData \code{\link[RstoxData]{ICESBioticData}} (defined in RstoxData).
#' @param ICESAcousticData \code{\link[RstoxData]{ICESAcousticData}} (defined in RstoxData).
#' @param ICESDatrasData \code{\link[RstoxData]{ICESDatrasData}} (defined in RstoxData).
#' @param WriteICESBioticData \code{\link[RstoxData]{WriteICESBioticData}} (defined in RstoxData).
#' @param WriteICESAcousticData \code{\link[RstoxData]{WriteICESAcousticData}} (defined in RstoxData).
#' @param WriteICESDatrasData \code{\link[RstoxData]{WriteICESDatrasData}} (defined in RstoxData).
#' 
#' @name ModelData
#' 
#' @seealso \code{\link{ProcessData}} for process data types and \code{\link{DataTypes}} for all data types produced by \code{\link{RstoxBase}}..
#' 
NULL

##################################################
##################################################
#' Process data used by RstoxBase
#' 
#' The process data of a StoX model are data that are saved to the project description (project.json file), typically manual settings (tagging of \code{\link{EDSU}}s to \code{\link{PSU}}s) or data read from resource files other than acoustic, biotic and landing input data files stored in the input folder. #' 
#' @param StratumPolygon The \code{\link{StratumPolygon}} process data.
#' @param TransectParameter The \code{\link{TransectParameter}} process data.
#' @param Survey The \code{\link{Survey}} process data.
#' @param AcousticLayer The \code{\link{AcousticLayer}} process data.
#' @param AcousticPSU The \code{\link{AcousticPSU}} process data.
#' @param BioticLayer The \code{\link{BioticLayer}} process data.
#' @param BioticPSU The \code{\link{BioticPSU}} process data.
#' @param BioticAssignment The \code{\link{BioticAssignment}} process data.
#' @param AcousticTargetStrength The \code{\link{AcousticTargetStrength}} process data.
#' @param Regression The \code{\link{Regression}} process data.
#' @param Translation \code{\link[RstoxData]{Translation}} (defined in RstoxData).
#' 
#' @name ProcessData
#' 
#' @seealso \code{\link{ModelData}} for model data types and \code{\link{DataTypes}} for all data types produced by \code{\link{RstoxBase}}..
#' 
NULL


##################################################
##################################################
#' StoX data types of the RstoxBase package
#' 
#' StoX data types are the data types used to transfer data and information between processes in a StoX estimation model. The data types are divided into two types, the \code{\link{ModelData}} and \code{\link{ProcessData}}.
#' 
#' @name DataTypes
#' 
NULL


##################################################
##################################################
#' Resolution
#' 
#' Horizontal and vertical resolution in StoX
#' 
#' @details
#' Stox defines specific horizontal and vertical resolutions which are applied at different stages of a StoX model. The resolutions are grouped into the input resolution, the processing resolution and the report resolution.
#' 
#' The input resolution is \code{\link{Station}}/\code{\link{Haul}} (horizontal/vertical) for swept-area models and \code{\link{EDSU}}/\code{\link{Channel}} for acoustic-trawl models, and is the resolution of the input data.  \cr \cr
#' 
#' The process resolution is \code{\link{PSU}}/\code{\link{Layer}} (horizontal/vertical) for both swept-area and acoustic-trawl models, and is the resolution at which density is calculated. All data used as input when calculating density must be in this resolution.  \cr \cr
#' 
#' The report resolution is \code{\link{Stratum}}/\code{\link{Layer}} (horizontal/vertical) for both swept-area and acoustic-trawl models,, and is the resolution of the abundance estimates. 
#' 
#' @name Resolution
#' 
NULL

##################################################
##################################################
#' Stratum horizontal resolution
#' 
#' The horizontal resolution \code{Stratum} is the lowest horizontal resolution in StoX. 
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Stratum
#' 
NULL

##################################################
##################################################
#' Acoustic and biotic primary sampling units
#' 
#' The horizontal resolution \code{PSU} is common for acoustic-trawl and swept-area estimation models, and defines the primary sampling units, typically transects for acoustic-trawl models and single trawl stations for swept-area models. 
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name PSU
#' 
NULL

##################################################
##################################################
#' Horizontal input resolution for acoustic-trawl models
#' 
#' The horizontal resolution \code{EDSU} is the input resolution for acoustic data (convensionally named Log). 
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name EDSU
#' 
NULL

##################################################
##################################################
#' Horizontal input resolution for swept-area models
#' 
#' The horizontal resolution \code{Station} is the input resolution for biotic data. One \code{Station} can contain several \code{\link{Haul}}, which represents the vertical resolution of a \code{Station}.
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Station
#' 
NULL


##################################################
##################################################
#' Layer vertical resolution
#' 
#' The vertical resolution \code{Layer} is the report resolution of a StoX project along with the horizontal report resolution \code{\link{Stratum}}.
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Layer
#' 
NULL


##################################################
##################################################
#' Input vertical resolution for swept-area models
#' 
#' The vertical resolution \code{Haul} is the input resolution for swept-area models. Each \code{\link{Station}} can contain several hauls.
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Haul
#' 
NULL


##################################################
##################################################
#' Input vertical resolution for acoustic-trawl models
#' 
#' The vertical resolution \code{Channel} is the input resolution for acoustic-trawl models. Each \code{\link{EDSU}} can contain several acoustic channels.
#' 
#' @seealso \code{\link{Resolution}} for other resolutions.
#' 
#' @name Channel
#' 
NULL


##################################################
##################################################
#' StoX data type StratumPolygon
#' 
#' The StratumPolygon data type contains the polygons defining the strata of a survey, stored as an object of type \code{\link[sp]{SpatialPolygons}}. 
#' 
#' @details
#' The polygons are stored as gsojson in the project.json file and in output text files.
#' 
#' @seealso This data type is produced by \code{\link{DefineStratumPolygon}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name StratumPolygon
#' 
NULL


##################################################
##################################################
#' Transect parameter table
#' 
#' The TransectParameter process data type contains a table of the columns "StratumName", "TransectType", "Bearing", "BearingAngle", "Retour", "SurveyTime", "SurveyDistance", "SurveySpeed", "Seed", where either "Bearing" or "BearingAngle" must be present and either SurveyTime" and "SurveySpeed" or only "SurveyDistance" must be present.
#' 
#' @details
#' The polygons are stored as gsojson in the project.json file and in output text files.
#' 
#' @seealso This data type is produced by \code{\link{DefineTransectParameter}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name TransectParameter
#' 
NULL


##################################################
##################################################
#' StoX data type Survey
#' 
#' The Survey data is a table of the columns Stratum and Survey, defining which strata comprise the different surveys. 
#' 
#' @details
#' This data type can be useful if there are particular strata that are e.g. experimental, and others that are to be included in the official estimate, and replaces the "include in total" concept of StoX 2.7.
#' 
#' @seealso This data type is produced by \code{\link{DefineSurvey}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name Survey
#' 
NULL



##################################################
##################################################
#' Acoustic Layer
#' 
#' The Acoustic Layer process data is a table of the three columns Layer, MinLayerDepth and MaxLayerDepth specifying the depth ranges for the acoustic layers on which the acoustic density should be calculated, where the Layer column gives the names of the layers.
#' 
#' @seealso This data type is produced by \code{\link{DefineAcousticLayer}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name AcousticLayer
#' 
NULL


##################################################
##################################################
#' Biotic Layer
#' 
#' The biotic Layer process data is a table of the three columns Layer, MinLayerDepth and MaxLayerDepth specifying the depth ranges for the biotic layers on which the biotic density should be calculated, where the Layer column gives the names of the layers.
#' 
#' @seealso This data type is produced by \code{\link{DefineBioticLayer}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name BioticLayer
#' 
NULL


##################################################
##################################################
#' Acoustic PSU
#' 
#' The AcousticPSU process data contains the following three tables: Stratum_PSU, EDSU_PSU and PSUByTime. The table Stratum_PSU contains two columns, Stratum and PSU, linking each acoustic PSUs with a Stratum. The table EDSU_PSU contains two columns, EDSU and PSU, linking each EDSU with and acoustic PSU (possibly missing PSU). The table PSUByTime is a representation the tables Stratum_PSU and EDSU_PSU as start and end time for each un-broken sequence of EDSUs, containing the columns Stratum, PSU, Cruise, StartDateTime and StopDateTime. Note that the table PSUByTime is not updated if UseProcessData is set to TRUE in \code{\link{DefineAcousticPSU}}. To update PSUByTime, set UseProcessData to FALSE and DefinitionMethod to "Manual", and re-run the process.
#' 
#' @seealso This data type is produced by \code{\link{DefineAcousticPSU}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name AcousticPSU
#' 
NULL


##################################################
##################################################
#' Biotic PSU
#' 
#' The BioticPSU process data contains two tables Stratum_PSU and Station_PSU, where Stratum_PSU has the two column Stratum and PSU giving the biotic PSUs defined in each stratum (possibly none), and Station_PSU has the two column Station listing every Station in the data and PSU which tags each Station to a PSU. Stations can be un-tagged, which is indicated with PSU = "".
#' 
#' @seealso This data type is produced by \code{\link{DefineBioticPSU}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name BioticPSU
#' 
NULL


##################################################
##################################################
#' Stratum area data
#' 
#' The StratumAreaData model data is a table of the area in square nautical miles of each stratum.
#' 
#' @seealso This data type is produced by \code{\link{StratumArea}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name StratumAreaData
#' 
NULL


##################################################
##################################################
#' Transect design
#' 
#' The TransectDesignData model data is a table of the columns Stratum (character), Transect (character), Segment (character), LongitudeStart (decimal degrees), LatitudeStart (decimal degrees), LongitudeEnd (decimal degrees), LatitudeEnd (decimal degrees), Distance (nautical mile), Direction (character) and Speed (knots), where each row represents one segment of a transect design. The segments do not need to be connected, so there may be transport between the segments. Use \code{\link{ReportTransectDesign}} to add rows with transport between segments.
#' 
#' @seealso This data type is produced by \code{\link{TransectDesign}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name TransectDesignData
#' 
NULL


##################################################
##################################################
#' Report transect design
#' 
#' The ReportTransectDesignData model data is a list of three tables Segment, SegmentWithTransport and Stratum. 
#' 
#' The table Segment stores the segments from the input TransectDesignData and contains the columns Stratum (character), Segment (character), LongitudeStart (decimal degrees), LatitudeStart (decimal degrees), LongitudeEnd (decimal degrees), LatitudeEnd (decimal degrees), Speed (knots) and Distance (nautical mile), 
#' 
#' The table SegmentWithTransport contains the segments from the table Segment but adds the transport between each segment. This table contains the additional column SegmentType with values "Segment" and "Transport".
#' 
#' The table Stratum stores one row per stratum with the columns Stratum (character), DistanceSegment (nautical mile), DistanceTransport (nautical mile), Distance (nautical mile), Speed (knots), Area (square nautical mile) and Coverage (DistanceSegment / square root of Area).
#' 
#' @seealso This data type is produced by \code{\link{ReportTransectDesign}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name ReportTransectDesignData
#' 
NULL


##################################################
##################################################
#' Transect design as GPX 
#' 
#' The WriteTransectDesignData model data is an sf object which can be written as GPX tracks (one segment with waypoints. 
#' 
#' @seealso This data type is produced by \code{\link{WriteTransectDesign}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name WriteTransectDesignData
#' 
NULL


##################################################
##################################################
#' Transect design plot
#' 
#' The PlotTransectDesignData model data is a ggplot2 object which can be plotted at a later stage.
#' 
#' @seealso This data type is produced by \code{\link{PlotTransectDesign}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name PlotTransectDesignData
#' 
NULL


##################################################
##################################################
#' Total catch per SpeciesCategory and Haul
#' 
#' The SpeciesCategoryCatchData model data is a table with one row per Haul and SpeciesCategory holding the TotalCatchWeigth and TotalCatchNumber which are summed CatchFractionWeight and CatchFractionNumber over all samples of the Haul for each SpeciesCategory. All variables of the input StoxBioticData are kept except keys (variables with name ending with "Key").
#' 
#' @seealso This data type is produced by \code{\link{SpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name SpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Total catch per SpeciesCategory and Station
#' 
#' The SumSpeciesCategoryCatchData model data is a list of two elements; the Data, which are similar to \code{\link{SpeciesCategoryCatch}} but added the Layer resolution column and summed vertically; and the Resolution, which is a table of the three columns Station, Layer and Haul containing the link between the vertical resolution variables before summing. Specifically, the Data table holds the following variables: "Station",, "Layer", "SpeciesCategory", "TotalCatchWeight", "TotalCatchNumber", "MinLayerDepth", "MaxLayerDepth", "SumSpeciesCategoryCatchWeight", "SpeciesCategoryCatchType", "Cruise", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude".
#' 
#' @seealso This data type is produced by \code{\link{SumSpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name SumSpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Total catch per SpeciesCategory and biotic PSU
#' 
#' The MeanSpeciesCategoryCatchData model data is a list of two elements; the Data, which are similar to \code{\link{SpeciesCategoryCatch}} but added the Layer vertical resolution column and the Stratum and PSU horizontal resolution columns, and summed vertically and averaged horizontally; and the Resolution, which is a table of the five columns Stratum, PSU, Station, Layer and Haul containing the link between the horizontal and vertical resolution variables before summing and averaging. Specifically, the Data table holds the following variables: "Survey", "Stratum", "PSU", "Layer", "SpeciesCategory", "TotalCatchWeight", "TotalCatchNumber", "MinLayerDepth", "MaxLayerDepth", "MeanSpeciesCategoryCatchWeight", "SpeciesCategoryCatchType".
#' 
#' @seealso This data type is produced by \code{\link{MeanSpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name MeanSpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Total prey catch per PreySpeciesCategory and Individual
#' 
#' The PreySpeciesCategoryCatchData model data is a table with one row per Individual and PreySpeciesCategory holding the TotalPreyCatchWeigth which is summed PreyCatchWeight over all PreySamples of the Individual for each PreySpeciesCategory. All variables of the input StoxBioticData are kept except keys (variables with name ending with "Key").
#' 
#' @seealso This data type is produced by \code{\link{PreySpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name PreySpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Length distribution data
#' 
#' The LengthDistributionData model data contains the columns Station, Haul, SpeciesCategory, IndividualTotalLength, LengthResolution, WeightedNumber, MinHaulDepth, MaxHaulDepth, MinLayerDepth, MaxLayerDepth, LengthDistributionWeight, Cruise, EffectiveTowDistance, VerticalNetOpening, HorizontalNetOpening, TrawlDoorSpread and LengthDistributionType.
#' 
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' Station \tab Unique Station identifier \tab None \tab Character \tab "2021105-1" \cr   
#' Haul \tab Unique Haul identifier \tab None \tab Character \tab "2021105-1-2" \cr
#' SpeciesCategory \tab The species category \tab None \tab Character \tab "Herring" \cr
#' IndividualTotalLength \tab Total length (from snout to end of fin), given as the lower end of the interval of width given by LengthResolution \tab cm \tab Numeric \tab 14.5 \cr
#' LengthResolution \tab Resolution of IndividualTotalLength \tab cm \tab Numeric \tab 0.5 \cr     
#' WeightedNumber \tab The number of individuals of the length group. \tab None \tab Numeric \tab 12 \cr
#' MinHaulDepth \tab Minimum depth of the haul (trawl headline) \tab m \tab Numeric \tab 65 \cr
#' MaxHaulDepth \tab Maximum depth of the haul (trawl headline) \tab m \tab Numeric \tab 35 \cr
#' LengthDistributionWeight \tab The weight of the WeightedNumber, always 1 \tab None \tab Numeric \tab 1 \cr
#' LengthDistributionType \tab The type of length distribution, one of "Normalized", "Standard" and "Percent" (see \code{\link{LengthDistribution}}) \tab None \tab Character \tab "AreaNumberDensity" \cr
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \cr
#' EffectiveTowDistance \tab Effective tow distance of the Haul \tab nmi \tab Numeric \tab 1.5 \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \cr
#' Gear \tab Identifier of the gear \tab None \tab Character \tab "3270" \cr
#' VerticalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 23 \cr
#' HorizontalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 105 \cr
#' TrawlDoorSpread \tab Distance between the trawl doors. \tab m \tab Numeric \tab 125 \cr
#' }
#' 
#' @seealso This data type is produced by \code{\link{LengthDistribution}}, \code{\link{RegroupLengthDistribution}}, \code{\link{LengthDependentLengthDistributionCompensation}} and \code{\link{RelativeLengthDistribution}}. LengthDistributionData is used in \code{\link{SumLengthDistribution}} with output \code{\link{SumLengthDistributionData}} and \code{\link{MeanLengthDistribution}} with output \code{\link{MeanLengthDistributionData}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name LengthDistributionData
#' 
NULL


##################################################
##################################################
#' Summed length distribution data
#' 
#' The SumLengthDistributionData model data is a list of two elements; the Data, which are similar to \code{\link{LengthDistributionData}} but added the Layer resolution column and summed vertically; and the Resolution, which is a table of the three columns Station, Layer and Haul containing the link between the vertical resolution variables before summing.
#' 
#' \bold{Data}:
#' \tabular{lllll}{
#' Station \tab Unique Station identifier \tab None \tab Character \tab "2021105-1" \cr   
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1"\cr
#' SpeciesCategory \tab The species category \tab None \tab Character \tab "Herring" \cr
#' IndividualTotalLength \tab Total length (from snout to end of fin), given as the lower end of the interval of width given by LengthResolution \tab cm \tab Numeric \tab 14.5 \cr
#' LengthResolution \tab Resolution of IndividualTotalLength \tab cm \tab Numeric \tab 0.5 \cr     
#' WeightedNumber \tab The number of individuals of the length group. \tab None \tab Numeric \tab 12 \cr
#' MinLayerDepth \tab The minimum depth of the depth Layer \tab m \tab Numeric \tab 0 \cr
#' MaxLayerDepth \tab The maximum depth of the depth Layer \tab m \tab Numeric \tab 100 \cr
#' SumLengthDistributionWeight \tab The weight of the WeightedNumber, always 1 \tab None \tab Numeric \tab 1 \cr
#' LengthDistributionType \tab The type of length distribution, one of "Normalized", "Standard" and "Percent" (see \code{\link{LengthDistribution}}) \tab None \tab Character \tab "AreaNumberDensity"\cr
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \cr
#' EffectiveTowDistance \tab Effective tow distance of the Haul \tab nmi \tab Numeric \tab 1.5 \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \cr
#' }
#' 
#' \bold{Resolution}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' Station \tab Unique Station identifier \tab None \tab Character \tab "2021105-1" \cr   
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1"\cr
#' Haul \tab Unique Haul identifier \tab None \tab Character \tab "2021105-1-2" \cr
#' Gear \tab Identifier of the gear \tab None \tab Character \tab "3270" \cr
#' VerticalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 23 \cr
#' HorizontalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 105 \cr
#' TrawlDoorSpread \tab Distance between the trawl doors. \tab m \tab Numeric \tab 125 \cr
#' }
#' 
#' @seealso This data type is produced by \code{\link{SumLengthDistribution}} with input \code{\link{LengthDistributionData}} from \code{\link{LengthDistribution}}. SumLengthDistributionData is used in \code{\link{MeanLengthDistribution}} with output \code{\link{MeanLengthDistributionData}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name SumLengthDistributionData
#' 
NULL


##################################################
##################################################
#' Mean length distribution data
#' 
#' The MeanLengthDistributionData model data is a list of two elements; the Data, which are similar to \code{\link{LengthDistributionData}} but added the Layer vertical resolution column and the Stratum and PSU horizontal resolution columns, and summed vertically and averaged horizontally; and the Resolution, which is a table of the five columns Stratum, PSU, Station, Layer and Haul containing the link between the horizontal and vertical resolution variables before summing and averaging.
#' 
#' \bold{Data}:
#' \tabular{llllll}{
#' Survey \tab The survey identifier \tab None \tab Character \tab "MainSurvey" \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \cr
#' PSU \tab The PSU identifier \tab None \tab Character \tab "PSU1" \cr
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1"\cr
#' SpeciesCategory \tab The species category \tab None \tab Character \tab "Herring" \cr
#' IndividualTotalLength \tab Total length (from snout to end of fin), given as the lower end of the interval of width given by LengthResolution \tab cm \tab Numeric \tab 14.5 \cr
#' LengthResolution \tab Resolution of IndividualTotalLength \tab cm \tab Numeric \tab 0.5 \cr     
#' WeightedNumber \tab The number of individuals of the length group. \tab None \tab Numeric \tab 12 \cr
#' MinLayerDepth \tab The minimum depth of the depth Layer \tab m \tab Numeric \tab 0 \cr
#' MaxLayerDepth \tab The maximum depth of the depth Layer \tab m \tab Numeric \tab 100 \cr
#' MeanLengthDistributionWeight \tab The weight of the WeightedNumber, equal to the number of Stations of the PSU \tab None \tab Numeric \tab 1 \cr
#' LengthDistributionType \tab The type of length distribution, one of "Normalized", "Standard" and "Percent" (see \code{\link{LengthDistribution}}) \tab None \tab Character \tab "AreaNumberDensity" \cr
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \cr
#' EffectiveTowDistance \tab Effective tow distance of the Haul \tab nmi \tab Numeric \tab 1.5 \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \cr
#' }
#' 
#' \bold{Resolution}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \cr
#' PSU \tab The PSU identifier \tab None \tab Character \tab "PSU1" \cr
#' Station \tab Unique Station identifier \tab None \tab Character \tab "2021105-1" \cr   
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1"\cr
#' Haul \tab Unique Haul identifier \tab None \tab Character \tab "2021105-1-2" \cr
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \cr
#' EffectiveTowDistance \tab Effective tow distance of the Haul \tab nmi \tab Numeric \tab 1.5 \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \cr
#' Gear \tab Identifier of the gear \tab None \tab Character \tab "3270" \cr
#' VerticalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 23 \cr
#' HorizontalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 105 \cr
#' TrawlDoorSpread \tab Distance between the trawl doors. \tab m \tab Numeric \tab 125 \cr
#' }
#' 
#' @seealso This data type is produced by \code{\link{MeanLengthDistribution}} with input \code{\link{LengthDistributionData}} from \code{\link{LengthDistribution}} or \code{\link{SumLengthDistributionData}} from \code{\link{SumLengthDistribution}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name MeanLengthDistributionData
#' 
NULL


##################################################
##################################################
#' Assignment length distribution data
#' 
#' The AssignmentLengthDistributionData model data is a table of averaged length distribution for each combination of Stratum (acoustic)PSU and (acoustic)Layer.
#' 
#' @seealso This data type is produced by \code{\link{AssignmentLengthDistribution}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name AssignmentLengthDistributionData
#' 
NULL


##################################################
##################################################
#' Nautical area scattering coefficient (NASC) data
#' 
#' The NASCData model data gives the NASC for each acoustic channel or Layer, and ESDU, PSU or Stratum. 
#' 
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' EDSU \tab Unique elementary distance sampling unit (EDSU/Log) identifier \tab None \tab Character \tab "2020821/2020-04-24T03:51:26.000Z" \cr   
#' Channel \tab Unique NASC depth channel identifier \tab None \tab Character \tab "2" \cr
#' Beam \tab Unique Beam identifier \tab None \tab Character \tab "38000/2" \cr
#' AcousticCategory \tab The acoustic category \tab None \tab Character \tab "HER" \cr
#' Frequency \tab The acoustic frequency of the Beam \tab hertz \tab Numeric \tab 38000 \cr
#' NASC \tab The nautical area scattering coefficient. \tab m^2/nmi^2 \tab Numeric \tab 59.24813 \cr
#' MinChannelDepth \tab The minimum depth of the depth Channel \tab m \tab Numeric \tab 0 \cr
#' MaxChannelDepth \tab The maximum depth of the depth Channel \tab m \tab Numeric \tab 10 \cr
#' NASCWeight \tab The LogDistance of the EDSU \tab None \tab None \tab 0.1 \cr
#' ChannelReferenceType \tab Unique ChannelReference identifier \tab None \tab Character \tab "P" \cr
#' ChannelReferenceDepth \tab The depth of the ChannelReference origin. 0 for pelagic channels. Not yet given for bottom channels, as BottomDepth is not yet defined for NMDEchosounder data \tab m \tab Numeric \tab 0 \cr
#' ChannelReferenceTilt \tab The tilt angle of the beam, where 180 is vertically downwards and 0 is vertically upwards \tab degree \tab Numeric \tab 180 \cr 
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \cr
#' EffectiveLogDistance \tab The effective length of the EDSU \tab nmi \tab Numeric \tab 0.09 \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \cr
#' }
#' 
#' @seealso This data type is produced by \code{\link{NASC}}. NASCData is used in \code{\link{SumNASC}} with output \code{\link{SumNASCData}} and \code{\link{MeanNASC}} with output \code{\link{MeanNASCData}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name NASCData
#' 
NULL


##################################################
##################################################
#' Summed NASC data
#' 
#' The SumNASCData model data is a list of two elements; the Data, which are similar to \code{\link{NASCData}} but added the Layer resolution column and summed vertically; and the Resolution, which is a table of the three columns EDSU, Layer and Channel containing the link between the vertical resolution variables before summing.
#' 
#' \bold{Data}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' EDSU \tab Unique elementary distance sampling unit (EDSU/Log) identifier \tab None \tab Character \tab "2020821/2020-04-24T03:51:26.000Z" \cr   
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1"\cr
#' Beam \tab Unique Beam identifier \tab None \tab Character \tab "38000/2" \cr
#' AcousticCategory \tab The acoustic category \tab None \tab Character \tab "HER" \cr
#' Frequency \tab The acoustic frequency of the Beam \tab hertz \tab Numeric \tab 38000 \cr
#' NASC \tab The nautical area scattering coefficient. \tab m^2/nmi^2 \tab Numeric \tab 59.24813 \cr
#' MinLayerDepth \tab The minimum depth of the depth Layer \tab m \tab Numeric \tab 0 \cr
#' MaxLayerDepth \tab The maximum depth of the depth Layer \tab m \tab Numeric \tab 100 \cr
#' SumNASCWeight \tab The LogDistance of the EDSU (same as NASCWeight of the \code{\link{NASCData}}) \tab None \tab None \tab 0.1 \cr
#' ChannelReferenceType \tab Unique ChannelReference identifier \tab None \tab Character \tab "P" \cr
#' ChannelReferenceDepth \tab The depth of the ChannelReference origin. 0 for pelagic channels. Not yet given for bottom channels, as BottomDepth is not yet defined for NMDEchosounder data \tab m \tab Numeric \tab 0 \cr
#' ChannelReferenceTilt \tab The tilt angle of the beam, where 180 is vertically downwards and 0 is vertically upwards \tab degree \tab Numeric \tab 180 \cr 
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \cr
#' EffectiveLogDistance \tab The effective length of the EDSU \tab nmi \tab Numeric \tab 0.09 \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \cr
#' }
#' 
#' \bold{Resolution}:
#' \tabular{llllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' EDSU \tab Unique elementary distance sampling unit (EDSU/Log) identifier \tab None \tab Character \tab "2020821/2020-04-24T03:51:26.000Z" \cr   
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1" \cr
#' Channel \tab Unique NASC depth channel identifier \tab None \tab Character \tab "2" \cr
#' }
#' 
#' @seealso This data type is produced by \code{\link{SumNASC}} with input \code{\link{NASCData}} from \code{\link{NASC}}. SumNASCData is used in \code{\link{MeanNASC}} with output \code{\link{MeanNASCData}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name SumNASCData
#' 
NULL


##################################################
##################################################
#' Mean NASC data
#' 
#' The MeanNASCData model data is a list of two elements; the Data, which are similar to \code{\link{NASCData}} but added the Layer vertical resolution column and the Stratum and PSU horizontal resolution columns, and summed vertically and averaged horizontally; and the Resolution, which is a table of the five columns Stratum, PSU, EDSU, Layer and Channel containing the link between the horizontal and vertical resolution variables before summing and averaging.
#' 
#' \bold{Data}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' Survey \tab The survey identifier \tab None \tab Character \tab "MainSurvey" \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \cr
#' PSU \tab The PSU identifier \tab None \tab Character \tab "PSU1" \cr
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1" \cr
#' Beam \tab Unique Beam identifier \tab None \tab Character \tab "38000/2" \cr
#' AcousticCategory \tab The acoustic category \tab None \tab Character \tab "HER" \cr
#' Frequency \tab The acoustic frequency of the Beam \tab hertz \tab Numeric \tab 38000 \cr
#' NASC \tab The nautical area scattering coefficient. \tab m^2/nmi^2 \tab Numeric \tab 59.24813 \cr
#' MinLayerDepth \tab The minimum depth of the depth Layer \tab m \tab Numeric \tab 0 \cr
#' MaxLayerDepth \tab The maximum depth of the depth Layer \tab m \tab Numeric \tab 100 \cr
#' MeanNASCWeight \tab The total LogDistance of the PSU \tab None \tab None \tab 0.1 \cr
#' ChannelReferenceType \tab Unique ChannelReference identifier \tab None \tab Character \tab "P" \cr
#' ChannelReferenceDepth \tab The depth of the ChannelReference origin. 0 for pelagic channels. Not yet given for bottom channels, as BottomDepth is not yet defined for NMDEchosounder data \tab m \tab Numeric \tab 0 \cr
#' ChannelReferenceTilt \tab The tilt angle of the beam, where 180 is vertically downwards and 0 is vertically upwards \tab degree \tab Numeric \tab 180 \cr 
#' }
#' 
#' \bold{Resolution}:
#' \tabular{lllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \cr
#' PSU \tab The PSU identifier \tab None \tab Character \tab "PSU1" \cr
#' EDSU \tab Unique elementary distance sampling unit (EDSU/Log) identifier \tab None \tab Character \tab "2020821/2020-04-24T03:51:26.000Z" \cr   
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1" \cr
#' Channel \tab Unique NASC depth channel identifier \tab None \tab Character \tab "2" \cr
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \cr
#' EffectiveLogDistance \tab The effective length of the EDSU \tab nmi \tab Numeric \tab 0.09 \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \cr
#' }
#' 
#' @seealso This data type is produced by \code{\link{MeanNASC}} with input \code{\link{NASCData}} from \code{\link{NASC}} or \code{\link{SumNASCData}} from \code{\link{SumNASC}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name MeanNASCData
#' 
NULL


##################################################
##################################################
#' Acoustic-Trawl Survey plot
#' 
#' The PlotAcousticTrawlSurveyData model data is a ggplot2 object which can be plotted at a later stage.
#' 
#' @seealso This data type is produced by \code{\link{PlotAcousticTrawlSurvey}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name PlotAcousticTrawlSurveyData
#' 
NULL


##################################################
##################################################
#' Biotic assignment
#' 
#' The BioticAssignment process data is a table containing the columns Stratum, PSU, Layer, Haul and WeightingFactor, where Haul is a list of all Hauls assigned to each acoustic PSU of each acoustic layer. 
#' 
#' @seealso This data type is produced by \code{\link{DefineBioticAssignment}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name BioticAssignment
#' 
NULL


##################################################
##################################################
#' Density data
#' 
#' The DensityData model data is a common datatype for acoustic and swept-area density produced by the functions \code{\link{AcousticDensity}} and \code{\link{SweptAreaDensity}}, respectively, and contains the tables Data and Resolution with variables given in the following tables:
#' 
#' \bold{Data}:
#' \tabular{llllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{Occurrence} \cr
#' Survey \tab The survey identifier \tab None \tab Character \tab "MainSurvey" \tab Always \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \tab Always \cr
#' PSU \tab The PSU identifier \tab None \tab Character \tab "PSU1" \tab Always \cr
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1" \tab Always \cr
#' Beam \tab Unique Beam identifier \tab None \tab Character \tab "38000/2" \tab \code{\link{AcousticDensity}} \cr
#' SpeciesCategory \tab The species category \tab None \tab Character \tab "Herring" \tab Always \cr
#' IndividualTotalLength \tab Total length (from snout to end of fin), given as the lower end of the interval of width given by LengthResolution \tab cm \tab Numeric \tab 14.5 \tab \code{\link{SweptAreaDensity}} \cr
#' LengthResolution \tab Resolution of IndividualTotalLength \tab cm \tab Numeric \tab 0.5 \tab \code{\link{SweptAreaDensity}} \cr     
#' Frequency \tab The acoustic frequency of the Beam \tab hertz \tab Numeric \tab 38000 \tab \code{\link{AcousticDensity}} \cr
#' Density \tab The density, either given as number of individuals or weight per square nautical mile (when DensityType is "AreaNumberDensity" or AreaWeightDensity, respectively) \tab nmi^-2 if DensityType is "AreaNumberDensity", and kg nmi^-2 if DensityType is "AreaWeightDensity" (only relevant when SweptAreaDensityMethod is "TotalCatch") \tab Numeric \tab 123.4 \tab Always \cr
#' MinLayerDepth \tab The minimum depth of the depth Layer \tab m \tab Numeric \tab 0 \tab Always \cr
#' MaxLayerDepth \tab The maximum depth of the depth Layer \tab m \tab Numeric \tab 100 \tab Always \cr
#' DensityWeight \tab The weight of the density value associated with the PSU, i.e., the number of Stations for \code{\link{SweptAreaDensity}} and total LogDistance for \code{\link{AcousticDensity}} \tab None \tab Numeric \tab 8 \tab Always \cr
#' DensityType \tab The type of density, currently one of "AreaNumberDensity" and "AreaWeightDensity" \tab None \tab Character \tab "AreaNumberDensity" \tab Always \cr
#' }
#' 
#' \bold{Resolution}:
#' \tabular{llllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{Occurrence} \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \tab Always \cr
#' PSU \tab The PSU identifier \tab None \tab Character \tab "PSU1" \tab Always \cr
#' EDSU \tab Unique elementary distance sampling unit (EDSU/Log) identifier \tab None \tab Character \tab "2020821/2020-04-24T03:51:26.000Z" \tab \code{\link{AcousticDensity}} \cr   
#' Station \tab Unique Station identifier \tab None \tab Character \tab "2021105-1" \tab \code{\link{SweptAreaDensity}} \cr   
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1" \tab Always \cr
#' Channel \tab Unique NASC depth channel identifier \tab None \tab Character \tab "2" \tab \code{\link{AcousticDensity}} \cr
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \tab Always \cr
#' EffectiveLogDistance \tab The effective length of the EDSU \tab nmi \tab Numeric \tab 0.09 \tab \code{\link{AcousticDensity}} \cr
#' EffectiveTowDistance \tab Effective tow distance of the Haul \tab nmi \tab Numeric \tab 1.5 \tab \code{\link{SweptAreaDensity}} \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \tab Always \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \tab Always \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \tab Always \cr
#' VerticalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 23 \tab \code{\link{SweptAreaDensity}} \cr
#' HorizontalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 105 \tab \code{\link{SweptAreaDensity}} \cr
#' TrawlDoorSpread \tab Distance between the trawl doors. \tab m \tab Numeric \tab 125 \tab \code{\link{SweptAreaDensity}} \cr
#' }
#' 
#' @seealso This data type is produced by \code{\link{AcousticDensity}} and \code{\link{SweptAreaDensity}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name DensityData
#' 
NULL


##################################################
##################################################
#' Mean density data
#' 
#' The MeanDensityData model data holds the density of individuals as number per square nautical mile, by Stratum and Layer. The data type contains the tables Data and Resolution. The variables of the MeanDensityData are the same as in \code{\link{DensityData}}, except that the variable PSU is not present in the table Data, and that the variable DensityWeight is replaced by the variable MeanDensityWeight:
#' 
#' \bold{Data}:
#' \tabular{llllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{Occurrence} \cr
#' Survey \tab The survey identifier \tab None \tab Character \tab "MainSurvey" \tab Always \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \tab Always \cr
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1" \tab Always \cr
#' Beam \tab Unique Beam identifier \tab None \tab Character \tab "38000/2" \tab \code{\link{AcousticDensity}} \cr
#' SpeciesCategory \tab The species category \tab None \tab Character \tab "Herring" \tab Always \cr
#' IndividualTotalLength \tab Total length (from snout to end of fin), given as the lower end of the interval of width given by LengthResolution \tab cm \tab Numeric \tab 14.5 \tab \code{\link{SweptAreaDensity}} \cr
#' LengthResolution \tab Resolution of IndividualTotalLength \tab cm \tab Numeric \tab 0.5 \tab \code{\link{SweptAreaDensity}} \cr     
#' Frequency \tab The acoustic frequency of the Beam \tab hertz \tab Numeric \tab 38000 \tab \code{\link{AcousticDensity}} \cr
#' Density \tab The density, either given as number of individuals or weight per square nautical mile (when DensityType is "AreaNumberDensity" or AreaWeightDensity, respectively) \tab nmi^-2 if DensityType is "AreaNumberDensity", and kg nmi^-2 if DensityType is "AreaWeightDensity" (only relevant when SweptAreaDensityMethod is "TotalCatch") \tab Numeric \tab 123.4 \tab Always \cr
#' MinLayerDepth \tab The minimum depth of the depth Layer \tab m \tab Numeric \tab 0 \tab Always \cr
#' MaxLayerDepth \tab The maximum depth of the depth Layer \tab m \tab Numeric \tab 100 \tab Always \cr
#' MeanDensityWeight \tab The weight of the density value associated with the Stratum, i.e., the number of Stations for \code{\link{SweptAreaDensity}} and total LogDistance for \code{\link{AcousticDensity}} \tab None \tab Numeric \tab 8 \tab Always \cr
#' DensityType \tab The type of density, currently one of "AreaNumberDensity" and "AreaWeightDensity" \tab None \tab Character \tab "AreaNumberDensity" \tab Always \cr
#' }
#' 
#' \bold{Resolution}:
#' \tabular{llllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{Occurrence} \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \tab Always \cr
#' PSU \tab The PSU identifier \tab None \tab Character \tab "PSU1" \tab Always \cr
#' EDSU \tab Unique elementary distance sampling unit (EDSU/Log) identifier \tab None \tab Character \tab "2020821/2020-04-24T03:51:26.000Z" \tab \code{\link{AcousticDensity}} \cr   
#' Station \tab Unique Station identifier \tab None \tab Character \tab "2021105-1" \tab \code{\link{SweptAreaDensity}} \cr   
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1" \tab Always \cr
#' Channel \tab Unique NASC depth channel identifier \tab None \tab Character \tab "2" \tab \code{\link{AcousticDensity}} \cr
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \tab Always \cr
#' EffectiveLogDistance \tab The effective length of the EDSU \tab nmi \tab Numeric \tab 0.09 \tab \code{\link{AcousticDensity}} \cr
#' EffectiveTowDistance \tab Effective tow distance of the Haul \tab nmi \tab Numeric \tab 1.5 \tab \code{\link{SweptAreaDensity}} \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \tab Always \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \tab Always \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \tab Always \cr
#' VerticalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 23 \tab \code{\link{SweptAreaDensity}} \cr
#' HorizontalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 105 \tab \code{\link{SweptAreaDensity}} \cr
#' TrawlDoorSpread \tab Distance between the trawl doors. \tab m \tab Numeric \tab 125 \tab \code{\link{SweptAreaDensity}} \cr
#' }
#' 
#' @seealso This data type is produced by \code{\link{MeanDensity}} based on \code{\link{DensityData}} See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name MeanDensityData
#' 
NULL


##################################################
##################################################
#' Acoustic target strength
#' 
#' The AcousticTargetStrength process data is a list of two tables, the AcousticTargetStrengthModel, holding only the string naming the model to use, and the AcousticTargetStrengthTable, holding the table of the parameters/values of the model. See \code{\link{DefineAcousticTargetStrength}} for details of the different models.
#' 
#' @seealso This data type is produced by \code{\link{DefineAcousticTargetStrength}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name AcousticTargetStrength
#' 
NULL


##################################################
##################################################
#' Regression
#' 
#' The Regression process data is a list of two tables, the RegressionModel, holding only the string naming the model to use, the RegressionTable, holding the table of the parameters/values of the model. See \code{\link{DefineRegression}} for details of the different models. The function \code{\link{EstimateBioticRegression}} also outputs this datatype.
#' 
#' @seealso This data type is produced by \code{\link{DefineRegression}} and \code{\link{EstimateBioticRegression}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name Regression
#' 
NULL



##################################################
##################################################
#' Quantity data
#' 
#' The Quantity model data holds the abundance and biomass of individuals as number and weight, respectively, per Stratum and Layer. The data type contains the tables Data and Resolution with variables given in the following tables:
#' 
#' \bold{Data}:
#' \tabular{llllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{Occurrence} \cr
#' Survey \tab The survey identifier \tab None \tab Character \tab "MainSurvey" \tab Always \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \tab Always \cr
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1" \tab Always \cr
#' Beam \tab Unique Beam identifier \tab None \tab Character \tab "38000/2" \tab \code{\link{AcousticDensity}} \cr
#' SpeciesCategory \tab The species category \tab None \tab Character \tab "Herring" \tab Always \cr
#' IndividualTotalLength \tab Total length (from snout to end of fin), given as the lower end of the interval of width given by LengthResolution \tab cm \tab Numeric \tab 14.5 \tab \code{\link{SweptAreaDensity}} \cr
#' LengthResolution \tab Resolution of IndividualTotalLength \tab cm \tab Numeric \tab 0.5 \tab \code{\link{SweptAreaDensity}} \cr     
#' Frequency \tab The acoustic frequency of the Beam \tab hertz \tab Numeric \tab 38000 \tab \code{\link{AcousticDensity}} \cr
#' Abundance \tab The number of individuals \tab individuals \tab Numeric \tab 1234567.8 \tab If MeanDensityType is"AreaNumberDensity" in the input MeanDensityData \cr
#' Biomass \tab The total weight of the species \tab kg \tab Numeric \tab 123456.7 \tab If DensityType is "AreaWeightDensity" (only relevant when SweptAreaDensityMethod is "TotalCatch" in \code{\link{SweptAreaDensity}}) \cr
#' MinLayerDepth \tab The minimum depth of the depth Layer \tab m \tab Numeric \tab 0 \tab Always \cr
#' MaxLayerDepth \tab The maximum depth of the depth Layer \tab m \tab Numeric \tab 100 \tab Always \cr
#' }
#' 
#' \bold{Resolution}:
#' \tabular{llllll}{
#' \bold{Variable} \tab \bold{Description} \tab \bold{Unit} \tab \bold{Data type} \tab \bold{Example} \tab \bold{Occurrence} \cr
#' Stratum \tab The stratum identifier \tab None \tab Character \tab "Stratum1" \tab Always \cr
#' PSU \tab The PSU identifier \tab None \tab Character \tab "PSU1" \tab Always \cr
#' EDSU \tab Unique elementary distance sampling unit (EDSU/Log) identifier \tab None \tab Character \tab "2020821/2020-04-24T03:51:26.000Z" \tab \code{\link{AcousticDensity}} \cr   
#' Station \tab Unique Station identifier \tab None \tab Character \tab "2021105-1" \tab \code{\link{SweptAreaDensity}} \cr   
#' Layer \tab The depth Layer identifier \tab None \tab Character \tab "Layer1" \tab Always \cr
#' Channel \tab Unique NASC depth channel identifier \tab None \tab Character \tab "2" \tab \code{\link{AcousticDensity}} \cr
#' Cruise \tab Unique Cruise identifier (see \code{\link[RstoxData]{StoxAcousticFormat}} for output from \code{\link{AcousticDensity}} and \code{\link[RstoxData]{StoxBioticFormat}} for output from \code{\link{SweptAreaDensity}}) \tab None \tab Character \tab "2021105" \tab Always \cr
#' EffectiveLogDistance \tab The effective length of the EDSU \tab nmi \tab Numeric \tab 0.09 \tab \code{\link{AcousticDensity}} \cr
#' EffectiveTowDistance \tab Effective tow distance of the Haul \tab nmi \tab Numeric \tab 1.5 \tab \code{\link{SweptAreaDensity}} \cr
#' DateTime \tab UTC time at start of the EDSU or Station, stored as \code{\link{POSIXct}} \tab ISO8601 \tab Character \tab 2020-09-09T01:02:03.456Z \tab Always \cr
#' Longitude \tab Longitude at start of the EDSU or Station \tab degree east \tab Numeric \tab 62.5 \tab Always \cr
#' Latitude \tab Latitude at start of the EDSU or Station \tab degree north \tab Numeric \tab 5.1 \tab Always \cr
#' VerticalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 23 \tab \code{\link{SweptAreaDensity}} \cr
#' HorizontalNetOpening \tab Vertical span of the net \tab m \tab Numeric \tab 105 \tab \code{\link{SweptAreaDensity}} \cr
#' TrawlDoorSpread \tab Distance between the trawl doors. \tab m \tab Numeric \tab 125 \tab \code{\link{SweptAreaDensity}} \cr
#' }
#' 
#' @seealso This data type is produced by \code{\link{Quantity}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name QuantityData
#' 
NULL


##################################################
##################################################
#' Individuals data
#' 
#' The IndividualsData model data are the individuals from \code{\link[RstoxData]{StoxBioticData}} used in the estimate. In Individuals() the \code{\link[RstoxData]{StoxBioticData}} is merged with \code{\link{BioticAssignment}} in the case of acoustic-trawl models and with \code{\link{MeanLengthDistributionData}} in the case of swept-area models, by the Haul identifier stored in the \code{\link[RstoxData]{StoxBioticData}}, the \code{\link{BioticAssignment}}, and in the Resolution table of the \code{\link{MeanLengthDistributionData}}. As the hauls may be linked to a different stratum than the one containing the haul, the Stratum column of the \code{\link{IndividualsData}} may not correspond to the actual stratum of the haul.
#' 
#' @seealso This data type is produced by \code{\link{Individuals}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name IndividualsData
#' 
NULL


##################################################
##################################################
#' Super-individuals data
#' 
#' The SuperIndividualsData model data is the \code{\link{IndividualsData}} added Abundance and Biomass. Note that the Abundance is number of individuals and the Biomass is weight in g.
#' 
#' @seealso This data type is produced by \code{\link{SuperIndividuals}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name SuperIndividualsData
#' 
NULL


##################################################
##################################################
#' Reported super-individuals data
#' 
#' The ReportSuperIndividualsData model data is a report of the \code{\link{SuperIndividualsData}}.
#' 
#' @seealso This data type is produced by \code{\link{ReportSuperIndividuals}}. See \code{\link{DataTypes}} for a list of all data types produced by \code{\link{RstoxBase}}.
#' 
#' @name ReportSuperIndividualsData
#' 
NULL


##################################################
##################################################
#' Reported SpeciesCategoryCatch data
#' 
#' The ReportSpeciesCategoryCatchData model data is a report of the \code{\link{SpeciesCategoryCatchData}} which combines Haul info and a table of the catch of each SpeciesCategory.
#' 
#' @seealso This data type is produced by \code{\link{ReportSpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name ReportSpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Reported PreySpeciesCategoryCatch data
#' 
#' The ReportPreySpeciesCategoryCatchData model data is a report of the \code{\link{PreySpeciesCategoryCatchData}}.
#' 
#' @seealso This data type is produced by \code{\link{ReportPreySpeciesCategoryCatch}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxBase}}.
#' 
#' @name ReportPreySpeciesCategoryCatchData
#' 
NULL


##################################################
##################################################
#' Reported Density data
#' 
#' The ReportDensityData model data is a report of the \code{\link{DensityData}}.
#' 
#' @seealso This data type is produced by \code{\link{ReportDensity}}. See \code{\link{DataTypes}} for a list of all data types produced by \code{\link{RstoxBase}}.
#' 
#' @name ReportDensityData
#' 
NULL


##################################################
##################################################
#' Reported Quantity data
#' 
#' The ReportQuantityData model data is a report of the \code{\link{QuantityData}}.
#' 
#' @seealso This data type is produced by \code{\link{ReportQuantity}}. See \code{\link{DataTypes}} for a list of all data types produced by \code{\link{RstoxBase}}.
#' 
#' @name ReportQuantityData
#' 
NULL


##################################################
##################################################
#' Write StratumPolygon data
#' 
#' The WriteStratumPolygonData model data is used to write \code{\link{StratumPolygon}}.
#' 
#' @seealso This data type is produced by \code{\link{WriteStratumPolygon}}. See \code{\link{DataTypes}} for a list of all data types produced by \code{\link{RstoxBase}}.
#' 
#' @name WriteStratumPolygonData
#' 
NULL

