# A list funcitons performing conversions of the projectDescription to ensure backward compatibility:
#' 
#' @export
#' 
backwardCompatibility <- list(
    # Split ReadBioticXML into ReadBiotic and StoxBiotic:
    function(projectDescription) {
        if(StoxVersion == 2.7) {
            # Get the function names:
            functionNames <- sapply(projectDescription$Baseline, "[[", "functionName")
            # Get the position of the process using ReadBioticXML():
            atReadBioticXML <- which("ReadBioticXML" %in% functionNames)
            if(length(atReadBioticXML)) {
                # Trick to make it possible to split the process:
                projectDescription$Baseline <- lapply(projectDescription$Baseline, list)
                
                # Split the process:
                projectDescription$Baseline[[atReadBioticXML]] <- list(
                    projectDescription$Baseline[[atReadBioticXML]], 
                    projectDescription$Baseline[[atReadBioticXML]]
                )
                
                # ... into ReadBiotic:
                projectDescription$Baseline[[atReadBioticXML]]$processName = "ReadBiotic"
                projectDescription$Baseline[[atReadBioticXML]]$functionName = "RstoxBase::ReadBiotic"
                projectDescription$Baseline[[atReadBioticXML]]$functionParameters = list(
                    FileNames = unlist(projectDescription$Baseline[[atReadBioticXML]]$functionParameters)
                )
                
                # ... and StoxBiotic:
                projectDescription$Baseline[[atReadBioticXML]]$processName = "StoxBiotic"
                projectDescription$Baseline[[atReadBioticXML]]$functionName = "RstoxBase::StoxBiotic"
                projectDescription$Baseline[[atReadBioticXML]]$functionParameters$SpeciesCategory = "commonname"
                projectDescription$Baseline[[atReadBioticXML]]$functionInputs$BioticData = "ReadBiotic"
                
                # Flatten the list again:
                projectDescription$Baseline <- unlist(projectDescription$Baseline, recursive = TRUE)
            }
        }
        
        return(projectDescription)
    }
    
)



