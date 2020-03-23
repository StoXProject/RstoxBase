# Rename DefineStrata to DefineStratum:
#rename_DefineStrata_to_DefineStratum <- function(projectDescription) {
#    if(StoxVersion == 2.7) {
#        # Get the function names:
#        functionNames <- sapply(projectDescription$baseline, "[[", "functionName")
#        # Get the position of the process using DefineStrata():
#        atDefineStrata <- which("DefineStrata" %in% functionNames)
#        if(length(atDefineStrata)) {
#            # Split the process:
#            projectDescription$baseline[[atDefineStrata]]$functionName <- "DefineStratum"
#        }
#    }
#    
#    return(projectDescription)
#}

# Split ReadBioticXML into ReadBiotic and StoxBiotic:
split_ReadBioticXML_to_ReadBiotic_and_StoxBiotic <- function(projectDescription) {
    
    # Get the StoxVersion from the attributes:
    resourceVersion <- attr(projectDescription, "resourceversion")
    RstoxFrameworkVersion <- attr(projectDescription, "RstoxFrameworkVersion")
    
    # Do not support backwards compatibility for versoins prior to StoX 2.7:
    if(length(resourceVersion) && resourceversion == "1.92") {
        # Get the function names:
        functionNames <- sapply(projectDescription$baseline, "[[", "functionName")
        # Get the position of the process using ReadBioticXML():
        atReadBioticXML <- which("ReadBioticXML" %in% functionNames)
        if(length(atReadBioticXML)) {
            # Trick to make it possible to split the process:
            projectDescription$baseline <- lapply(projectDescription$baseline, list)
            
            # Split the process:
            projectDescription$baseline[[atReadBioticXML]] <- list(
                projectDescription$baseline[[atReadBioticXML]], 
                projectDescription$baseline[[atReadBioticXML]]
            )
            
            # ... into ReadBiotic:
            projectDescription$baseline[[atReadBioticXML]]$processName = "ReadBiotic"
            projectDescription$baseline[[atReadBioticXML]]$functionName = "RstoxBase::ReadBiotic"
            projectDescription$baseline[[atReadBioticXML]]$functionParameters = list(
                FileNames = unlist(projectDescription$baseline[[atReadBioticXML]]$functionParameters)
            )
            
            # ... and StoxBiotic:
            projectDescription$baseline[[atReadBioticXML]]$processName = "StoxBiotic"
            projectDescription$baseline[[atReadBioticXML]]$functionName = "RstoxBase::StoxBiotic"
            projectDescription$baseline[[atReadBioticXML]]$functionParameters$SpeciesCategory = "commonname"
            projectDescription$baseline[[atReadBioticXML]]$functionInputs$BioticData = "ReadBiotic"
            
            # Flatten the list again:
            projectDescription$baseline <- unlist(projectDescription$baseline, recursive = TRUE)
        }
    }
    
    return(projectDescription)
}

remove_ReadProcessData <- function(projectDescription) {
    if(length(resourceVersion) && resourceversion == "1.92") {
        # Get the function names:
        functionNames <- sapply(projectDescription$baseline, "[[", "functionName")
        # Get the position of the process using ReadBioticXML():
        atReadProcessData <- which("ReadProcessData" %in% functionNames)
        if(length(atReadProcessData)) {
            
            # Remove the process:
            projectDescription$baseline[[atReadProcessData]] <- NULL
        }
    }
    
    return(projectDescription)
}

modifyFilterBiotic <- function(projectDescription) {
    
    # Run only for StoX 2.7:
    if(checkVersion(projectDescription, resourceversion == "1.92")) {
        # Find the process using DefineStrata():
        atProcess <- findProcessFromFunctionName(
            functionName = "FilterBiotic", 
            projectDescription =projectDescription, 
            modelName = "baseline"
        )
        if(length(atProcess)) {
            # Get the filters:
            filterNames <- c("FishStationExpr", "CatchExpr", "SampleExpr", "IndExpr")
            
            # Get the filters:
            FilterExpression <- lapply(filterNames, function(x) projectDescription$baseline[[atProcess]][[x]])
            
            # Convert to R syntax:s
            FilterExpression <- lapply(FilterExpression, JavaJEXL2R)
            
            
            #FilterExpression <- list(
            #    fishstation = projectDescription$baseline[[atProcess]]$
            #)
            
            
            # Backwards compatibility must happen in a separate package, maybe RstoxAPI, or possibly in RstoxFramework!
            
            # General conversions:
            # 1. Get function inputs, by a list of data types in old StoX, except process data, which must be inserted from the last process returning the process data specified in the function inputs of the new function (moev from global to local process data).
            # 2. Get function parameters, and use individual backwards compatibility functions to map to the new parameters.
            # 3. Get function name
            # 4. Get process name
            # 5. Get process parameters
            # 6. Get process data
            
            
            
            
            
            #FishStationExpr	fs.getLengthSampleCount('SILDG03') > 9
            #CatchExpr	species == '161722.G03'
            #SampleExpr	
            #IndExpr	
            #
            #
            #JavaJEXL2R
            
                
        }
    }
    
    return(projectDescription)
}


modify_DefineStrata <- function(projectDescription) {
    
    # Run only for StoX 2.7:
    if(checkVersion(projectDescription, resourceversion == "1.92")) {
        # Find the process using DefineStrata():
        atProcess <- findProcessFromFunctionName(
            functionName = "DefineStrata", 
            projectDescription =projectDescription, 
            modelName = "baseline"
        )
        if(length(atProcess)) {
            # Get the stratum multypolygon WKT table, and ocnvert to SpatialPolygonsDataFrame, and then to JSON
            stratumpolygon_WKT <- projectDescription$processdata$stratumpolygon
            stratumpolygon_sp <- RstoxBase:::dataTable2SpatialPolygonsDataFrame(stratumpolygon_WKT)
            # Add the SpatialPolygonsDataFrame to the process data of the process:
            projectDescription$baseline[[atProcess]]$processData <- stratumpolygon_sp
        }
    }
    
    return(projectDescription)
}

findProcessFromFunctionName <- function(functionName, projectDescription, modelName = "baseline") {
    # Get the function names:
    functionNames <- sapply(projectDescription[[modelName]], "[[", "functionName")
    # Get the position of the process:
    atProcess <- which(functionName %in% functionNames)
    return(atProcess)
}






# A list of functions performing conversions of the projectDescription to ensure backward compatibility:
#' 
#' @export
#' 
backwardCompatibility <- list(
    # rename_DefineStrata_to_DefineStratum, 
    split_ReadBioticXML_to_ReadBiotic_and_StoxBiotic
)
