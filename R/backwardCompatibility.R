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

modify_DefineStrata <- function(projectDescription) {
    
    # Run onl for StoX 2.7:
    if(checkVersion(projectDescription, resourceversion == "1.92")) {
        # Find the process using DefineStrata():
        atProcess <- findProcessFromFunctionName(
            functionName = "DefineStrata", 
            projectDescription =projectDescription, 
            modelName = "baseline"
        )
        if(length(atProcess)) {
            projectDescription$baseline[[atProcess]]
            
            
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

checkVersion <- function(projectDescription, resourceVersion = NULL, RstoxFrameworkVersion = NULL) {
    # Get the StoxVersion from the attributes:
    savedResourceVersion <- attr(projectDescription, "resourceversion")
    savedRstoxFrameworkVersion <- attr(projectDescription, "RstoxFrameworkVersion")
    
    if(length(resourceVersion) && savedResourceVersion == resourceversion) {
        return(TRUE)
    }
    else if(length(RstoxFrameworkVersion) && savedRstoxFrameworkVersion == RstoxFrameworkVersion) {
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}




# A list funcitons performing conversions of the projectDescription to ensure backward compatibility:
#' 
#' @export
#' 
backwardCompatibility <- list(
    # rename_DefineStrata_to_DefineStratum, 
    split_ReadBioticXML_to_ReadBiotic_and_StoxBiotic
)
