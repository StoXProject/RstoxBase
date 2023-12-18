##################################################
##################################################
#' Definitions stored in the RstoxBase environment
#' 
#' This function declares the RstoxBase environment and writes vital definitions to it.
#' 
#' @return
#' A list of definitions.
#' 
#' @noRd
#' @seealso Use \code{\link{getRstoxBaseDefinitions}} to get the definitions.
#' 
initiateRstoxBase <- function(){
    
    # Define the variables of the main data types used in estimation models:
    PSUByTime = list(
        horizontalResolution = c("Stratum", "PSU"), 
        categoryVariable = "Cruise", 
        groupingVariables = c("StartDateTime", "StopDateTime")
    )
    
    #### Data type definitions: ####
    dataTypeDefinition <- list(
        # NASC: 
        NASCData = list(
            horizontalResolution = "EDSU", 
            verticalResolution = "Channel", 
            obserationVariable = "Beam",
            categoryVariable = "AcousticCategory", 
            groupingVariables = c("Beam", "Frequency"), 
            data = "NASC", 
            verticalRawDimension = c("MinChannelDepth", "MaxChannelDepth"), 
            verticalLayerDimension = NULL, 
            weighting = "NASCWeight", 
            type = c("ChannelReferenceType"), 
            other = c("ChannelReferenceDepth", "ChannelReferenceTilt", "Cruise", "EffectiveLogDistance", "DateTime", "Longitude", "Latitude")
        ), 
        SumNASCData = list(
            Data = list(
                horizontalResolution = "EDSU", 
                verticalResolution = "Layer", 
                obserationVariable = "Beam",
                categoryVariable = "AcousticCategory", 
                groupingVariables = c("Beam", "Frequency"), 
                data = "NASC", 
                verticalRawDimension = NULL, 
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = "SumNASCWeight", 
                type = c("ChannelReferenceType"), 
                other = c("ChannelReferenceDepth", "ChannelReferenceTilt", "Cruise", "EffectiveLogDistance", "DateTime", "Longitude", "Latitude")
            ), 
            Resolution = list(
                horizontalResolution = "EDSU", 
                verticalResolution = c("Layer", "Channel")
            )
        ), 
        MeanNASCData = list(
            Data = list(
                surveyDefinition = "Survey", 
                horizontalResolution = c("Stratum", "PSU"), 
                verticalResolution = "Layer", 
                obserationVariable = "Beam",
                categoryVariable = "AcousticCategory", 
                groupingVariables = c("Beam", "Frequency"), 
                data = "NASC", 
                verticalRawDimension = NULL, 
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = "MeanNASCWeight", 
                type = c("ChannelReferenceType"), 
                other = c("ChannelReferenceDepth", "ChannelReferenceTilt")
            ), 
            Resolution = list(
                horizontalResolution = c("Stratum", "PSU", "EDSU"), 
                verticalResolution = c("Layer", "Channel"), 
                other = c("Cruise", "EffectiveLogDistance", "DateTime", "Longitude", "Latitude")
            )
        ), 
        # LengthDistribution:
        LengthDistributionData = list(
            horizontalResolution = "Station", 
            verticalResolution = "Haul", 
            obserationVariable = NULL,
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = "WeightedNumber",
            verticalRawDimension = c("MinHaulDepth", "MaxHaulDepth"), 
            verticalLayerDimension = NULL, 
            weighting = "LengthDistributionWeight", 
            type = "LengthDistributionType", 
            other = c("Cruise", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude", "Gear", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
        ), 
        SumLengthDistributionData = list(
            Data = list(
                horizontalResolution = "Station", 
                verticalResolution = "Layer", 
                obserationVariable = NULL,
                categoryVariable = "SpeciesCategory", 
                groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
                data = "WeightedNumber",
                verticalRawDimension = NULL, 
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = "SumLengthDistributionWeight", 
                type = "LengthDistributionType", 
                other = c("Cruise", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude")
            ), 
            Resolution = list(
                horizontalResolution = "Station", 
                verticalResolution = c("Layer", "Haul"), 
                other = c("Gear", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
            )
        ), 
        MeanLengthDistributionData = list(
            Data = list(
                surveyDefinition = "Survey", 
                horizontalResolution = c("Stratum", "PSU"), 
                verticalResolution = "Layer", 
                obserationVariable = NULL,
                categoryVariable = "SpeciesCategory", 
                groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
                data = "WeightedNumber",
                verticalRawDimension = NULL, 
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = "MeanLengthDistributionWeight", 
                type = "LengthDistributionType", 
                other = NULL
            ), 
            Resolution = list(
                horizontalResolution = c("Stratum", "PSU", "Station"), 
                verticalResolution = c("Layer", "Haul"), 
                other = c("Cruise", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude", "Gear", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
            )
        ), 
        
    
        
        # SpeciesCategoryCatch:
        SpeciesCategoryCatchData = list(
            horizontalResolution = "Station", 
            verticalResolution = "Haul", 
            obserationVariable = NULL,
            categoryVariable = "SpeciesCategory", 
            groupingVariables = NULL, 
            data = c(
                Weight = "TotalCatchWeight", 
                Number = "TotalCatchNumber"
            ),
            verticalRawDimension = c("MinHaulDepth", "MaxHaulDepth"), 
            verticalLayerDimension = NULL, 
            weighting = "SpeciesCategoryCatchWeight", 
            type = "SpeciesCategoryCatchType", 
            other = c("Cruise", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude", "Gear", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
        ), 
        SumSpeciesCategoryCatchData = list(
            Data = list(
                horizontalResolution = "Station", 
                verticalResolution = "Layer", 
                obserationVariable = NULL,
                categoryVariable = "SpeciesCategory", 
                groupingVariables = NULL, 
                data = c(
                    Weight = "TotalCatchWeight", 
                    Number = "TotalCatchNumber"
                ),
                verticalRawDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                verticalLayerDimension = NULL, 
                weighting = "SumSpeciesCategoryCatchWeight", 
                type = "SpeciesCategoryCatchType", 
                other = c("Cruise", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude")
            ), 
            Resolution = list(
                horizontalResolution = "Station", 
                verticalResolution = c("Layer", "Haul"), 
                other = c("Gear", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
            )
        ), 
        MeanSpeciesCategoryCatchData = list(
            Data = list(
                surveyDefinition = "Survey", 
                horizontalResolution = c("Stratum", "PSU"), 
                verticalResolution = "Layer", 
                obserationVariable = NULL,
                categoryVariable = "SpeciesCategory", 
                groupingVariables = NULL, 
                data = c(
                    Weight = "TotalCatchWeight", 
                    Number = "TotalCatchNumber"
                ),
                verticalRawDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                verticalLayerDimension = NULL, 
                weighting = "MeanSpeciesCategoryCatchWeight", 
                type = "SpeciesCategoryCatchType", 
                other = NULL
            ), 
            Resolution = list(
                horizontalResolution = c("Stratum", "PSU", "Station"), 
                verticalResolution = c("Layer", "Haul"), 
                other = c("Cruise", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude", "Gear", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
            )
        ), 
        
        AssignmentLengthDistributionData = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = c("Layer"), 
            obserationVariable = NULL,
            categoryVariable = "SpeciesCategory", 
            groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
            data = "WeightedNumber",
            verticalLayerDimension = NULL, # Not needed, as this datatype is only used in AcousticDensity.
            weighting = NULL, 
            type = "LengthDistributionType", 
            other = c("NumberOfAssignedHauls")
        ), 
        
        # Density:
        DensityData = list(
            Data = list(
                surveyDefinition = "Survey", 
                horizontalResolution = c("Stratum", "PSU"), 
                verticalResolution = c("Layer"), 
                obserationVariable = "Beam",
                categoryVariable = "SpeciesCategory", 
                groupingVariables = c(
                    "IndividualTotalLength", "LengthResolution", 
                    "Beam", "Frequency" # The relevant acoustic variables
                ), 
                groupingVariables_biotic = c("IndividualTotalLength", "LengthResolution"), 
                groupingVariables_acoustic = c("Beam", "Frequency"), 
                data = "Density",
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = "DensityWeight", 
                type = "DensityType", 
                other = NULL
            ), 
            Resolution = list(
                horizontalResolution = c("Stratum", "PSU", "EDSU", "Station"), 
                verticalResolution = c("Layer", "Channel"), 
                other = c("Cruise", "EffectiveLogDistance", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
            )
        ), 
        MeanDensityData = list(
            Data = list(
                surveyDefinition = "Survey", 
                horizontalResolution = "Stratum", 
                verticalResolution = c("Layer"), 
                obserationVariable = "Beam",
                categoryVariable = "SpeciesCategory", 
                groupingVariables = c(
                    "IndividualTotalLength", "LengthResolution", 
                    "Beam", "Frequency" # The relevant acoustic variables
                ), 
                groupingVariables_biotic = c("IndividualTotalLength", "LengthResolution"), 
                groupingVariables_acoustic = c("Beam", "Frequency"), 
                data = "Density",
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = "MeanDensityWeight", 
                type = "DensityType", 
                other = NULL
            ), 
            Resolution = list(
                horizontalResolution = c("Stratum", "PSU", "EDSU", "Station"), 
                verticalResolution = c("Layer", "Channel"), 
                other = c("Cruise", "EffectiveLogDistance", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
            )
        ), 
        # Quantity:
        QuantityData = list(
            Data = list(
                surveyDefinition = "Survey", 
                horizontalResolution = "Stratum", 
                verticalResolution = c("Layer"), 
                categoryVariable = "SpeciesCategory", 
                groupingVariables = c(
                    "IndividualTotalLength", "LengthResolution", 
                    "Beam", "Frequency" # The relevant acoustic variables
                ), 
                groupingVariables_biotic = c("IndividualTotalLength", "LengthResolution"), 
                groupingVariables_acoustic = c("Beam", "Frequency"), 
                data = c("Abundance", "Biomass"), 
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = NULL, 
                type = NULL, 
                other = NULL
            ), 
            Resolution = list(
                horizontalResolution = c("Stratum", "PSU", "EDSU", "Station"), 
                verticalResolution = c("Layer", "Channel"), 
                other = c("Cruise", "EffectiveLogDistance", "EffectiveTowDistance", "DateTime", "Longitude", "Latitude", "VerticalNetOpening", "HorizontalNetOpening", "TrawlDoorSpread")
            )
        ), 
        IndividualsData = list(
            Data = list(
                horizontalResolution = c("Stratum"), 
                #verticalResolution = c("Layer", "Haul"), 
                verticalResolution = c("Layer"), 
                #categoryVariable = "SpeciesCategory", 
                #groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
                #groupingVariables = c("Haul", "Individual"), 
                data = NULL, 
                verticalLayerDimension = NULL, # Not relevant
                weighting = NULL, 
                other = NULL
            ), 
            VariableNames = list()
        ), 
        # Prioritise the aggregation variables (horizontalResolution, verticalResolution, categoryVariable and groupingVariables) of the QuantityData, followed by the IndividualRoundWeight, which is used to calculate Biomass (dividing Abundance by it); and finally the aggregation variables of the IndividualsData, as the purpose of the SuperIndividualsData is to distribute Abundance and Biomass onto the individuals:
        SuperIndividualsData = list(
            Data = list(
                surveyDefinition = "Survey", 
                horizontalResolution = "Stratum", 
                #verticalResolution = c("Layer", "Haul"), 
                verticalResolution = c("Layer"), 
                categoryVariable = "SpeciesCategory", 
                #groupingVariables = c("IndividualTotalLength", "LengthResolution"), 
                #groupingVariables = c("IndividualTotalLength", "LengthResolution", "IndividualRoundWeight", "Haul", "Individual"), 
                groupingVariables = c(
                    # Add the keys here to comply with the column order of the IndividualsData, which is inherited from StoxBiotic:
                    "CruiseKey", 
                    "StationKey", 
                    "HaulKey", 
                    "SpeciesCategoryKey", 
                    "SampleKey", 
                    "IndividualKey", 
                    "IndividualTotalLength", "LengthResolution", "IndividualRoundWeight", 
                    "Beam", "Frequency" # The relevant acoustic variables
                ), 
                data = c("Abundance", "Biomass"), 
                verticalLayerDimension = c("MinLayerDepth", "MaxLayerDepth"), 
                weighting = NULL, 
                other = NULL
            ), 
            VariableNames = list()
        ), 
        BioticAssignment = list(
            horizontalResolution = c("Stratum", "PSU"), 
            verticalResolution = "Layer", 
            categoryVariable = "Haul", 
            weighting = "WeightingFactor", 
            other = NULL
        ), 
        BioticPSU = list(
            Stratum_PSU = list(
                horizontalResolution = c("Stratum", "PSU")
            ), 
            Station_PSU = list(
                horizontalResolution = c("Station", "PSU")
            )
        ), 
        AcousticPSU = list(
            Stratum_PSU = list(
                horizontalResolution = c("Stratum", "PSU")
            ), 
            EDSU_PSU = list(
                horizontalResolution = c("EDSU", "PSU")
            ), 
            PSUByTime = PSUByTime
        )#, 
        #AcousticPSUByTime = list(
        #    horizontalResolution = c("Stratum", "PSU"), 
        #    categoryVariable = "Cruise", 
        #    groupingVariables = c("StartDateTime", "StopDateTime")
        #)
    )
    ####
    
    #### Data type units: ####
    dataTypeUnits <- list(
        # DensityData
        list(dataType = "DensityData", variableName = "Density", quantity = "area_number_density", unit = "individuals/nmi^2"), 
        # QuantityData
        list(dataType = "QuantityData", variableName = "Abundance", quantity = "cardinality", unit = "individuals"), 
        list(dataType = "QuantityData", variableName = "Biomass", quantity = "mass", unit = "kg"),
        # SpeciesCategoryCatchData
        list(dataType = "SpeciesCategoryCatchData", variableName = "TotalCatchNumber", quantity = "cardinality", unit = "individuals"), 
        list(dataType = "SpeciesCategoryCatchData", variableName = "TotalCatchWeight", quantity = "mass", unit = "kg"),
        # SuperIndividualsData
        list(dataType = "SuperIndividualsData", variableName = "IndividualTotalLength", quantity = "length", unit = "cm"), 
        list(dataType = "SuperIndividualsData", variableName = "IndividualRoundWeight", quantity = "mass", unit = "g"), 
        list(dataType = "SuperIndividualsData", variableName = "Abundance", quantity = "cardinality", unit = "individuals"), 
        list(dataType = "SuperIndividualsData", variableName = "Biomass", quantity = "mass", unit = "g"), 
        list(dataType = "SuperIndividualsData", variableName = "TowDistance", quantity = "length", unit = "nmi"), 
        list(dataType = "SuperIndividualsData", variableName = "EffectiveTowDistance", quantity = "length", unit = "nmi"), 
        list(dataType = "SuperIndividualsData", variableName = "CatchFractionWeight", quantity = "mass", unit = "kg"), 
        list(dataType = "SuperIndividualsData", variableName = "CatchFractionNumber", quantity = "cardinality", unit = "individuals"), 
        list(dataType = "SuperIndividualsData", variableName = "SampleWeight", quantity = "mass", unit = "kg"), 
        list(dataType = "SuperIndividualsData", variableName = "SampleNumber", quantity = "cardinality", unit = "individuals"), 
        list(dataType = "SuperIndividualsData", variableName = "IndividualAge", quantity = "age", unit = "year"),
        # Copied from RstoxData:
        list(dataType = "SuperIndividualsData", variableName = "Longitude", quantity = "angle", unit = "degree east"), 
        list(dataType = "SuperIndividualsData", variableName = "Latitude", quantity = "angle", unit = "degree north"), 
        list(dataType = "SuperIndividualsData", variableName = "BottomDepth", quantity = "length", unit = "m"), 
        list(dataType = "SuperIndividualsData", variableName = "MinHaulDepth", quantity = "length", unit = "m"), 
        list(dataType = "SuperIndividualsData", variableName = "MaxHaulDepth", quantity = "length", unit = "m"), 
        list(dataType = "SuperIndividualsData", variableName = "VerticalNetOpening", quantity = "length", unit = "m"), 
        list(dataType = "SuperIndividualsData", variableName = "HorizontalNetOpening", quantity = "lengthlength", unit = "m"), 
        list(dataType = "SuperIndividualsData", variableName = "TrawlDoorSpread", quantity = "length", unit = "m")
    )
    dataTypeUnits <- data.table::rbindlist(dataTypeUnits)

    
    #### Resolution: ####
    resolutionClasses <- list(
        NASC = list(
            vertical= c("Layer", "Channel"), 
            horizontal = c("Stratum", "PSU", "EDSU")
        ), 
        LengthDistribution = list(
            vertical= c("Layer", "Haul"), 
            horizontal = c("Stratum", "PSU", "Station")
        ), 
        Density = list(
            vertical= c("Layer"), 
            horizontal = c("Stratum", "PSU")
        ), 
        Quantity = list(
            vertical= c("Layer"), 
            horizontal = c("Stratum")
        )
    )
    
    allResolution = list(
        NASCData = "NASC", 
        SumNASCData = "NASC", 
        MeanNASCData = "NASC", 
        LengthDistributionData = "LengthDistribution", 
        SumLengthDistributionData = "LengthDistribution", 
        MeanLengthDistributionData = "LengthDistribution", 
        DensityData = "Density",
        MeanDensityData = "Density",
        QuantityData = "Quantity"
    )
    
    
    # This was in use for the discareded detectDataType():
    ## Define the variables of the main data types used in estimation models:
    #getRequiredVariables <- function(x) {
    #    c(
    #        utils::tail(x$horizontalResolution, 1), 
    #        utils::tail(x$verticalResolution, 1), 
    #        x$categoryVariable, 
    #        x$groupingVariables, 
    #        x$weighting
    #        #utils::head(x$groupingVariables, 1), 
    #        #x$data, 
    #        #x$weighting
    #    )
    #}
    #dataTypeRequiredVariables <- lapply(dataTypeDefinition, getRequiredVariables)
    
    #proj4string <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    proj4string_longlat <- "+proj=longlat +no_defs +ellps=WGS84 +towgs84=0,0,0"
    #proj4string_aeqd <- "+proj=aeqd +no_defs +ellps=WGS84 +towgs84=0,0,0"
    proj4string_laea <- "+proj=laea +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # Define an empty SpatialPolygonsDataFrame, with no projection, as it is not possible to assign projection to an empty SpatialPolygonsDataFrame:
    
    # Removing sp altogether:
    # # Should we use new("SpatialPolygonsDataFrame") instead???????????????
    # emptyStratumPolygon <- sp::SpatialPolygonsDataFrame(
    #     sp::SpatialPolygons(list()), 
    #     data = data.frame()
    # )
    # # This failed due to the above note:
    # #suppressWarnings(sp::proj4string(emptyStratumPolygon) <- proj4string)
    emptyStratumPolygon <- sf::st_sf(sf::st_sfc())
    suppressWarnings(sf::st_crs(emptyStratumPolygon) <- proj4string_longlat)
    
    emptyStratumPolygonGeojson <- "{\n\t\"type\": \"FeatureCollection\",\n\t\"features\": []\n}\n"
    
    
    ##### Definitions of implemented model classes, such as target strength and regression: #####
    implementedModelClasses <- c(
        "AcousticTargetStrength", 
        "Regression"
    )
    
    # The names of the models for each model class:
    modelNames <- list(
        AcousticTargetStrength = c(
            "LengthDependent", 
            "LengthAndDepthDependent", 
            "LengthExponent", 
            "TargetStrengthByLength"
        ), 
        Regression = list(
            "SimpleLinear", 
            "Power"
        )
    )
    
    # The model types, either function or table:
    modelTypes  <- list(
        AcousticTargetStrength = list(
            LengthDependent = "Function", 
            LengthAndDepthDependent = "Function", 
            LengthExponent = "Function", 
            TargetStrengthByLength = "Table"
        ), 
        Regression = list(
            SimpleLinear = "Function", 
            Power = "Function"
        )
    )
    
    # The model parameters:
    modelParameters <- list(
        AcousticTargetStrength = list(
            LengthDependent = c("TargetStrength0", "LengthExponent"), 
            LengthAndDepthDependent = c("TargetStrength0", "LengthExponent", "DepthExponent"), 
            LengthExponent = "LengthExponent", 
            TargetStrengthByLength = c("TargetStrength", "TotalLength")
        ), 
        Regression = list(
            SimpleLinear = c("Intercept", "Slope"), 
            Power = c("Factor", "Exponent")
        )
    )
    
    defaultEstimationMethod <- list(
        Regression = list(
            SimpleLinear = "SimpleLinear", 
            Power = "LogLogLinear"
        )
    )
    
    
    # The model functions:
    modelFunctions  <- list(
        AcousticTargetStrength = list(
            
            # Length dependent: 
            # TS = TS0 + M log10(Lcm): 
            LengthDependent = function(midIndividualTotalLength, TargetStrength0, LengthExponent, Depth) {
                TargetStrength0 + 
                LengthExponent * log10(midIndividualTotalLength)
            }, 
            
            # Length and depth dependent: 
            # TS = TS0 + M log10(Lcm) + D log10(1 + D/10) # Ona 2003:
            LengthAndDepthDependent = function(midIndividualTotalLength, TargetStrength0, LengthExponent, DepthExponent, Depth) {
                TargetStrength0 + 
                LengthExponent * log10(midIndividualTotalLength) + 
                DepthExponent * log10(1 + Depth/10)
            }, 
            
            # Length exponent: 
            # TS = M log10(Lcm):
            LengthExponent = function(midIndividualTotalLength, LengthExponent) {
                LengthExponent * log10(midIndividualTotalLength)
            }, 
            
            # Target strength by lngth is given as a table: 
            TargetStrengthByLength = NA
        ), 
        Regression = list(
            
            # Simple linear regression Y = a + bX:
            SimpleLinear = function(independentVariable, parameters) {
                parameters$Intercept + parameters$Slope * independentVariable
            }, 
            
            # Power regression Y = aX^b:
            Power = function(independentVariable, parameters) {
                parameters$Factor * independentVariable^parameters$Exponent
            }
        )
    )
    
    
    # The estimation functions:
    estimationFunctions  <- list(
        Regression = list(
            # Simple linear regression Y = a + bX:
            SimpleLinear = function(dependentVariable, independentVariable, data) {
                fit <- stats::lm(
                    get(dependentVariable) ~ get(independentVariable), 
                    data = data, 
                    na.action = stats::na.exclude
                )
                return(fit)
            }, 
            # Power regression Y = aX^b:
            Power = function(dependentVariable, independentVariable, data) {
                # Throw an error if any values are 0, causing -Inf in log():
                is0 <- which(data[, c(dependentVariable, independentVariable), with = FALSE] == 0, arr.ind = TRUE)
                if(NROW(is0)) {
                    stop("There are zeros in the data, causing -Inf in the regression. This can happen when regressing IndividualTotalLength in SuperIndividualsData, where the length resolution may have been reduced to that some individuals end up with IndividualTotalLength = 0.")
                }
                
                fit <- stats::lm(
                    log(get(dependentVariable)) ~ log(get(independentVariable)), 
                    data = data, 
                    na.action = stats::na.exclude
                )
                # After log-log we get log(Y) = log(a) + b log(X), so a = exp(fit$coefficients[1])
                fit$coefficients[1] <- exp(fit$coefficients[1])
                return(fit)
            }
        )
    )
    
    
    # Some diagnostics to check that all model specifications contain the implemented model classes, and only the implemented model classes:
    if(!identical(sort(implementedModelClasses), sort(names(modelNames)))) {
        stop("All implemented model classes must be present in the RstoxBase definition 'modelNames'. Please notify the developers to fix this.")
    }
    if(!identical(sort(implementedModelClasses), sort(names(modelParameters)))) {
        stop("All implemented model classes must be present in the RstoxBase definition 'modelParameters'. Please notify the developers to fix this.")
    }
    if(!identical(sort(implementedModelClasses), sort(names(modelTypes)))) {
        stop("All implemented model classes must be present in the RstoxBase definition 'modelTypes'. Please notify the developers to fix this.")
    }
    if(!identical(sort(implementedModelClasses), sort(names(modelFunctions)))) {
        stop("All implemented model classes must be present in the RstoxBase definition 'modelFunctions'. Please notify the developers to fix this.")
    }
    # The estimationFunctions are not required for AcousticTargetStrength.
    
    
    
    # Define the PSU prefix and the SSU label:
    getPSUPrefix <- function(PSUType) {
        if(PSUType  == "Acoustic") {
            prefix <- "PSU"
        }
        else if(PSUType == "Biotic") {
            prefix <- "PSU"
        }
        else {
            stop("PSUType must be one of \"Acoustic\" or \"Biotic\"")
        }
        return(prefix)
    }
    getSSULabel <- function(PSUType) {
        if(PSUType  == "Acoustic") {
            SSULabel <- "EDSU"
        }
        else if(PSUType == "Biotic") {
            SSULabel <- "Station"
        }
        else {
            stop("PSUType must be one of \"Acoustic\" or \"Biotic\"")
        }
        return(SSULabel)
    }
    getStationLevel <- function(PSUType) {
        if(PSUType  == "Acoustic") {
            SSULabel <- "Log"
        }
        else if(PSUType == "Biotic") {
            SSULabel <- "Station"
        }
        else {
            stop("PSUType must be one of \"Acoustic\" or \"Biotic\"")
        }
        return(SSULabel)
    }
    
    nauticalMileInMeters <- 1852
    
    # List of functions avilable for report functions:
    reportFunctions <- list(
        functionName = c(
            "summaryStox", 
            "sum", 
            "mean", 
            "weighted.mean", 
            "median", 
            "min", 
            "max", 
            "sd", 
            "var", 
            "cv"#, 
            #"summary", 
            #"quantile", 
            #"percentile_5_95"
        ), 
        packageName = c(
            "RstoxBase", 
            "base", 
            "base", 
            "stats", 
            "stats", 
            "base", 
            "base", 
            "stats", 
            "stats", 
            "RstoxBase"#, 
            #"base", 
            #"stats", 
            #"RstoxBase"
        ), 
        weighted = c(
            FALSE, 
            FALSE, 
            FALSE, 
            TRUE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE#, 
            #FALSE, 
            #FALSE, 
            #FALSE
        ), 
        weightingParameter = c(
            "", 
            "", 
            "", 
            "w", 
            "", 
            "", 
            "", 
            "", 
            "", 
            ""#, 
            #"", 
            #"", 
            #""
        ), 
        specified = c(
            TRUE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE#, 
            #FALSE, 
            #FALSE, 
            #FALSE
        ), 
        specificationParameter = c(
            "percentages", 
            "", 
            "", 
            "", 
            "", 
            "", 
            "", 
            "", 
            "", 
            ""#, 
            #"", 
            #"", 
            #""
        ), 
        specificationParameterDisplayName = c(
            "Percentages", 
            "", 
            "", 
            "", 
            "", 
            "", 
            "", 
            "", 
            "", 
            ""#, 
            #"", 
            #"", 
            #""
        ), 
        multiple = c(
            TRUE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE, 
            FALSE#, 
            #TRUE, 
            #TRUE, 
            #TRUE
        )
    )
    
    
    # Define the length of the sequence to draw seeds from:
    seedSequenceLength <- 1e7
    
    # Warning when using RemoveMissingValues:
    #RemoveMissingValuesWarning <- "StoX: Using RemoveMissingValues = TRUE implies the risk of under-estimation. E.g., if RemoveMissingValues = TRUE and a super-individual lacks IndividualRoundWeight, Biomass will be NA, and the portion of Abundance distributed to that super-individual will be excluded when summing Biomass (but included when summing Abundance). It is advised to always run with RemoveMissingValues = FALSE first, and make a thorough investigation to identify the source of any missing values. The function ImputeSuperIndividuals can be used to impute the missing information from other super-individuals."
    RemoveMissingValuesWarning <- function(TargetVariable) {
        paste0("StoX: The TargetVariable (", TargetVariable, ") has missing values. Use RemoveMissingValues = TRUE with extreme caution!!! Instead, GroupingVariables can be used to isolate missing values in the TargetVariable. E.g., using SpeciesCategory and Survey often suffices to isolate missing values to missing SpeciesCategory and missing Survey. In addition, the Baseline function ImputeSuperIndividuals can be used to fill in missing information from other super-individuals.")
    }
    
    
    # Add the plot defaults for use by other packages:
    defaultPlotOptions <- list(
        # Default general options:
        defaultPlotGeneralOptions = defaultPlotGeneralOptions, 
        # Default file options:
        defaultPlotFileOptions = defaultPlotFileOptions, 
        # Default map plotting options:
        defaultMapPlotNASCOptions = defaultMapPlotNASCOptions, 
        # Default NASC-plotting options:
        defaultMapPlotOptions = defaultMapPlotOptions, 
        # Defaults for the AcousticPSU (potting PSU names) text size and shift (from the mean EDSU position):
        defaultAcousticPSUPlotOptions = defaultAcousticPSUPlotOptions, 
        # Defaults color variable:
        defaultColorVariableOptions = defaultColorVariableOptions
    )
    
    
    
    #### Assign to RstoxBaseEnv and return the definitions: ####
    definitionsNames <- ls()
    definitions <- lapply(definitionsNames, get, pos = environment())
    names(definitions) <- definitionsNames
    
    
    
    #### Create the RstoxBaseEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxBase:::RstoxBaseEnv: ####
    assign("RstoxBaseEnv", new.env(), parent.env(environment()))
    assign("definitions", definitions, envir = get("RstoxBaseEnv"))
    
    #### Return the definitions: ####
    definitions
}


##################################################
##################################################
#' Get RstoxBase definitions
#' 
#' This function gets vital definitions from the RstoxBase environment.
#' 
#' @param name  An optional string vector denoting which definitions to extract.
#' @param ...   values overriding the values of definitions.
#' 
#' @return
#' A list of definitions.
#' 
#' @examples
#' getRstoxBaseDefinitions()
#' 
#' @export
#' 
getRstoxBaseDefinitions <- function(name = NULL, ...) {
    
    # Save the optional inputs for overriding the output:
    l <- list(...)
    
    # Get all or a subset of the definitions:
    definitions <- get("RstoxBaseEnv")$definitions
    if(length(name)){
        definitions <- definitions[[name]]
    }
    
    l <- l[names(l) %in% names(definitions)]
    if(length(l)){
        definitions <- utils::modifyList(definitions, l)
    }
    
    definitions
}



#' Function to format the output of a function returning StoX data (ModelData or ProcessData).
#' 
#' The function removes duplicated columns, orders the columns as per the order defined by \code{\link{getDataTypeDefinition}}, optionally removed undefined columns, orders the rows, and finally deletes any data.table keys from the output tables.
#' 
#' @param data A table or list of tables to return from the StoX function.
#' @param dataType The data type to format against.
#' @param keep.all Logical: If TRUE keep all columns, and if FALSE delete undefined columns.
#' @param allow.missing Logical: If TRUE allow for unrelevant column names defined in \code{secondaryColumnOrder}.
#' @param secondaryColumnOrder,secondaryRowOrder A vector of column names specifying order of column not defined by \code{\link{getDataTypeDefinition}} used to proiritize when ordering columns and rows, respectively.
#' 
#' @export
#' 
formatOutput <- function(data, dataType, keep.all = TRUE, allow.missing = FALSE, secondaryColumnOrder = NULL, secondaryRowOrder = NULL) {
    
    # If data is only one table:
    if(data.table::is.data.table(data)) {
        dataTypeDefinition <- getDataTypeDefinition(dataType, unlist = TRUE)
        formatOutputOneTable(
            table = data, 
            tableDefinition = dataTypeDefinition, 
            keep.all = keep.all, 
            allow.missing = allow.missing, 
            secondaryColumnOrder = secondaryColumnOrder, 
            secondaryRowOrder = secondaryRowOrder
        ) 
    }
    # ... or a list of tables:
    else if(is.list(data) && data.table::is.data.table(data[[1]])) {
        dataTypeDefinition <- lapply(names(data), function(tableName) getDataTypeDefinition(dataType, subTable = tableName, unlist = TRUE))
        mapply(
            formatOutputOneTable, 
            table = data, 
            tableDefinition = dataTypeDefinition, 
            MoreArgs = list(
                keep.all = keep.all, 
                allow.missing = allow.missing, 
                secondaryColumnOrder = secondaryColumnOrder, 
                secondaryRowOrder = secondaryRowOrder
            )
        )
    }
}


formatOutputOneTable <- function(table, tableDefinition, keep.all = TRUE, allow.missing = FALSE, secondaryColumnOrder = NULL, secondaryRowOrder = NULL) {
    
    
    
    # Get the column order:
    columnOrder <- unique(
        c(
            tableDefinition, 
            secondaryColumnOrder
        )
    )
    rowOrder <- unique(
        c(
            tableDefinition, 
            secondaryRowOrder
        )
    )
    
    if(allow.missing) {
        columnOrder <- intersect(columnOrder, names(table))
        rowOrder <- intersect(rowOrder, names(table))
    }
    if(any(!rowOrder %in% columnOrder)) {
        stop("secondaryRowOrder cannot contain values that are not present in the secondaryColumnOrder or tableDefinition.")
    }
    
    # Return immediately if empty table, but add the columns given in the 'columnOrder':
    if(!nrow(table)) {
        table <- data.table::as.data.table(
            structure(
                rep(list(character()), length(columnOrder)), 
                names = columnOrder
            )
        )
        return(table)
    }
    
    # Remove any duplicated columns:
    if(any(duplicated(names(table)))) {
        table[, which(duplicated(names(table))) := NULL]
    }
    # Remove undefined columns:
    if(!keep.all) {
        removeColumnsByReference(
            data = table, 
            toRemove =  setdiff(names(table), columnOrder)
        )
    }
    
    
    # Order the columns:
    data.table::setcolorder(table, columnOrder)
    
    # Order the rows:
    #data.table::setorder(table, na.last = TRUE)
    RstoxData::setorderv_numeric(table, by = rowOrder, na.last = TRUE)
    
    # Delete any keys, as we use the argument 'by' for all merging and aggregation:
    data.table::setkey(table, NULL)
}




# We moved away from using this feature:
#detectDataType <- function(data) {
#    
#    #dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
#    #if(only.data) {
#    #    present <- sapply(dataTypeDefinition, function(var) all(var$data %in% names(data)))
#    #}
#    #else {
#    #    present <- sapply(dataTypeDefinition, function(var) all(unlist(var) %in% names(data)))
#    #}
#    
#    dataTypeRequiredVariables <- getRstoxBaseDefinitions("dataTypeRequiredVariables")
#    present <- sapply(dataTypeRequiredVariables, function(var) all(var %in% names(data)))
#    
#    if(!any(present)) {
#        missing <- lapply(dataTypeRequiredVariables, function(var) setdiff(var %in% names(data)))
#        
#        missing <- lapply(missing, paste, collapse = ", ")
#        missing <- paste(names(missing), missing, sep = ": ", collapse = ". ")
#        
#        
#        warning("StoX: The input data does not contain all the expected variables. The following are needed: ", missing)
#    }
#    else if(sum(present) > 1) {
#        message("More than one element of the input list contains the expected variables (", paste(names(dataTypeRequiredVariables)[present#], collapse = ", "), "). The first selected:")
#    }
#    
#    output <- utils::head(names(dataTypeRequiredVariables)[present], 1)
#    return(output)
#}




getAllAggregationVariables <- function(dataType, exclude.groupingVariables = FALSE) {
    
    # Define the elements to return:
    aggregationElements <- c("horizontalResolution", "verticalResolution", "categoryVariable", if(!exclude.groupingVariables) "groupingVariables")
    
    # Get the definitions:
    aggregateBy <- getDataTypeDefinition(
        dataType = dataType, 
        elements = aggregationElements, 
        unlist = TRUE
    )
    
    return(aggregateBy)
}

getResolutionVariables <- function(dataType = NULL, dimension = c("horizontal", "vertical")) {
    
    # Define the elements to return:
    resolutionElements <- paste0(dimension, "Resolution")
    
    # Get the definitions:
    resolution <- unique(
        getDataTypeDefinition(
            dataType = dataType, 
            elements = resolutionElements, 
            unlist = TRUE
        )
    )
    
    return(resolution)
}

getAllResolutionVariables <- function(dataType, dimension = NULL, other = FALSE) {
    # Define the valid dimensions, and select the other if requested:
    validDimensions <- c("horizontal", "vertical")
    
    if(!length(dimension)) {
        dimension <- validDimensions
    }
    else {
        dimension <- RstoxData::match_arg_informative(dimension, validDimensions)
        if(other) {
            dimension <- setdiff(validDimensions, dimension)
        }
    }
    
    allResolution <- getRstoxBaseDefinitions("allResolution")[[dataType]]
    resolutionClasses <- getRstoxBaseDefinitions("resolutionClasses")
    if(length(allResolution)) {
        unlist(resolutionClasses[[allResolution]][dimension])
    }
    else {
        NULL
    }
}

#' Get data type definitions
#' 
#' @param dataType The name of the data type to get definitions from, or a logical function of one input DataType.
#' @param subTable The sub table to extract, if any. Defaulted to "Data" to return the most relevant part of the data type definition.
#' @param elements A vector of specific elements to extract from the definition.
#' @param unlist Logical: If TRUE unlist the list of column names.
#' 
#' @export
#'
getDataTypeDefinition <- function(dataType, subTable = "Data", elements = NULL, unlist = FALSE) {
    
    # Get the requested type:
    dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
    
    # If given as a function of data type names (names of 'dataTypeDefinition'), apply the function to subset the data types:
    if(is.function(dataType)) {
        dataTypeDefinitionNames <- names(dataTypeDefinition)
        dataType <- subset(dataTypeDefinitionNames, dataType(dataTypeDefinitionNames))
    }
    # Does not make perfect sense, since we are always, or should always be, interested in exactly one dataType:
    ### # If NULL use all data types:
    ### if(length(dataType) == 0) {
    ###     dataType <- names(dataTypeDefinition)
    ### }
    thisDataTypeDefinition <- dataTypeDefinition[[dataType]]
    
    # Extract the elements to return:
    #if(length(elements)) {
    #    thisDataTypeDefinition <- lapply(thisDataTypeDefinition, "[", elements)
    #}
    #if(length(dataType) == 1) {
    #    thisDataTypeDefinition <- thisDataTypeDefinition[[dataType]]
    #}
    
    if(is.list(thisDataTypeDefinition) && is.list(thisDataTypeDefinition[[1]])) {
        if(subTable %in% names(thisDataTypeDefinition)) {
            thisDataTypeDefinition <- thisDataTypeDefinition[[subTable]]
        }
        else {
            stop("The dataType ", dataType, " may be non-existing, or the subTable may not be present for the given dataType")
        }
    }
    
    # Select the elements to return:
    if(length(elements)) {
        thisDataTypeDefinition <- thisDataTypeDefinition[elements]
    }
        
    # Unlist if specified:
    if(unlist) {
        if(length(thisDataTypeDefinition) == 1) {
            thisDataTypeDefinition <- thisDataTypeDefinition[[1]]
        }
        else {
            thisDataTypeDefinition <- unique(unlist(thisDataTypeDefinition))
        }
    }
    
    return(thisDataTypeDefinition)
}



#' Function returning the report functions defined for reporting in StoX, such as sum, summaryStox, etc.
#' 
#' @param getMultiple If given as FALSE or TRUE, select only function names used for baseline or bootstrap, respectively.
#' 
#' @export
#' 
getReportFunctions <- function(getMultiple = NULL) {
    reportFunctions <- getRstoxBaseDefinitions("reportFunctions")
    if(length(getMultiple)) {
        reportFunctions$functionName[reportFunctions$multiple == getMultiple]
    }
    else {
        reportFunctions$functionName
    }
    
}


