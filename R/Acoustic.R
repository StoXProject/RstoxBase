
##################################################
##################################################
#' NASC
#' 
#' NASC function converts the StoxAcousticData into
#' a NASCData format
#' 
#' @param StoxAcousticData input in a StoxAcoustcdata format
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{NASCData}} object.
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
NASC <- function(StoxAcousticData = NULL, AcousticLayer = NULL, AcousticPSU = NULL) {
    
    # Merge the StoxAcousticData:
    NASC <- mergeDataTables(StoxAcousticData)$NASC
    # Check that the input StoxAcousticData has the same ChannelReferenceType:
    dataTypeDefinition <- getDataTypeDefinition(dataType = "NASCData")
    ChannelReferenceType <- NASC[[dataTypeDefinition$type]]
    if(!all(ChannelReferenceType == ChannelReferenceType[1])) {
        stop("The StoxAcousticData must have only one ", dataTypeDefinition$type, " in the NASC function. This can be obtained in FilterStoxAcoustic.")
    }
    
    # Add weights:
    NASC[, NASCWeight := EffectiveLogDistance]
    
    # Insert the Stratum and PSU column by the AcousticPSU input, and otherwise by NAs:
    NASC <- addPSUDefinition(NASC, PSUDefinition = AcousticPSU)
    
    # Insert the Layer column by the AcousticLayer input, and otherwise by NAs:
    NASC <- addLayerDefinition(NASC, dataType = "NASCData", layerDefinition = AcousticLayer)
    ## Rename "MinRange" and "MaxRange" to "MinChannelRange" and "MaxChannelRange":
    #setnames(NASC, old=c("MinRange", "MaxRange"), new=c("MinChannelRange", "MaxChannelRange"))
    
    # Extract the relevant variables:
    validVariables <- getAllDataTypeVariables(dataType = "NASCData")
    NASC <- NASC[, ..validVariables]
    
    return(NASC)
}

NASC_sindre <- function(StoxAcousticData = NULL, AcousticLayer = NULL, AcousticPSU = NULL) {
  # Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
  
  
  
  #Add log distances in NASCData
  NASCData<- StoxAcousticData$Log[,c('Log','EffectiveDistance','EDSU','CruiseKey','LogKey')]
  
  
  #Quick fix to remove log info not in nasc
  #This has to be fixed in StoxAcoustic
  NASCData<-NASCData[NASCData$LogKey%in%StoxAcousticData$NASC$LogKey]
  
  
  
  #Grab all Frequency cathegory
  Frequency <- c(unique(StoxAcousticData$Beam$Frequency))
  NASCData<-setDT(NASCData)[, .(Frequency = as.numeric(Frequency)), .(Log,EffectiveDistance,EDSU,CruiseKey,LogKey)]
  BeamKey <- c(unique(StoxAcousticData$Beam$BeamKey))
  NASCData<-setDT(NASCData)[, .(BeamKey = (BeamKey)), .(Log,EffectiveDistance,EDSU,CruiseKey,LogKey,Frequency)]
  
  
  #add channel key
  ChannelKey  <- c(unique(StoxAcousticData$NASC$ChannelKey ))
  NASCData<-setDT(NASCData)[, .(ChannelKey = (ChannelKey)), .(Log,EffectiveDistance,EDSU,CruiseKey,LogKey,Frequency,BeamKey)]
  
  
  #Add channel reference key
  NASCData<- merge(NASCData,StoxAcousticData$NASC[,c('LogKey','CruiseKey','BeamKey','AcousticCategoryKey','ChannelKey','ChannelReferenceKey','MinRange','MaxRange')],by=c('LogKey','CruiseKey','BeamKey','ChannelKey'))
  
  
  #Grab all acoustic cathegory
  AcousticCategory <- c(unique(StoxAcousticData$AcousticCategory$AcousticCategory))
  NASCData<-setDT(NASCData)[, .(AcousticCategory = as.numeric(AcousticCategory)), .(Log,EffectiveDistance,EDSU,CruiseKey,LogKey,Frequency,BeamKey,ChannelKey,ChannelReferenceKey,MinRange,MaxRange)]
  NASCData$AcousticCategoryKey<-as.character(NASCData$AcousticCategory)

  
    
  #Add NASC data to NASCData
  NASCData <- merge(NASCData,StoxAcousticData$NASC[,c('LogKey','CruiseKey','BeamKey','AcousticCategoryKey','ChannelKey','ChannelReferenceKey','NASC')],by=c('LogKey','CruiseKey','BeamKey','AcousticCategoryKey','ChannelKey','ChannelReferenceKey'),all.x=TRUE)
  NASCData$NASC[is.na(NASCData$NASC)]<-0
  
  # NASCData[(NASCData$LogKey=='2017-02-14T23:56:11.000Z')&(NASCData$AcousticCategory==12)]
  
  # 
  # #Fix missing values in NASC
  # NASCData$ChannelReferenceKey[is.na(NASCData$NASC)]<-'P'
  # NASCData$ChannelKey[is.na(NASCData$NASC)]<-'1'
  # NASCData$MinRange[is.na(NASCData$NASC)]<-0
  # NASCData$MaxRange[is.na(NASCData$NASC)]<-Inf
  # NASCData$NASC[is.na(NASCData$NASC)]<-0
  
  
  
  
  #Fiks missing stuff
  NASCData$EDSU <- paste(NASCData$CruiseKey,sprintf("%.1f",round(as.numeric(NASCData$Log),digits = 1)),NASCData$LogKey,sep='/')
  NASCData$Channel <- NASCData$ChannelKey
  NASCData$ChannelReference <- NASCData$ChannelReferenceKey
  NASCData$NASCWeight<-NASCData$EffectiveDistance
  NASCData$EffectiveLogDistance<-NASCData$EffectiveDistance
  
  NASCData$Layer<-NA
  NASCData$PSU <- NA
  NASCData$Stratum <- NA
  
  

  #Fill inn acoustic layer if avaliable
  #TODO:
  # revisit this when this info is avaliable
  if(!is.null(AcousticLayer)){
    print('Add acoustic layer')
    NASCData$Layer<-NULL
    # NASCData<-merge(NASCData,AcousticLayer[c('Channel','Layer')],by='Channel')
    
    NASCData<- cbind(NASCData,AcousticLayer[,c('Channel','Layer')])
  }

  
  #Reshape structure
  NASCData<-NASCData[,c('Stratum','PSU','EDSU','Layer','Channel',
                        'AcousticCategory','ChannelReference','Frequency',
                        'NASC','MinRange','MaxRange','NASCWeight','EffectiveLogDistance')]
  
  
  
  #return data
  return(NASCData)
  
}


##################################################
#' Sum NASC 
#' 
#' This function sums NASC data vertically into layer resolution
#' 
#' @inheritParams SumLengthDistribution
#' @param NASCData The NASC data.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{NASCData}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{NASC}} and \code{\link{SumNASC}}.
#' 
#' @export
#' @import data.table
#' 
SumNASC <- function(NASCData, TargetResolution = "Layer") {
    sumData(NASCData, dataType = "NASCData", targetResolution = TargetResolution)
}




##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
CombineNASC <- function(NASCData) {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}



##################################################
#' (Weighted) Average NASC 
#' 
#' This function averages NASC data horizontally, weighted by the log distance.
#' 
#' @inheritParams MeanNASC
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A \code{\link{NASCData}} object.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link{NASC}} and \code{\link{MeanNASC}}.
#' 
#' @export
#' @import data.table
#' 
MeanNASC <- function(NASCData, TargetResolution = "PSU") {
    meanData(NASCData, dataType = "NASCData", targetResolution = TargetResolution)
}






##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
NASCToAcousticData <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
SplitNASC <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


##################################################
##################################################
#' Some title
#' 
#' Some description
#' 
#' @param parameterName Parameter descrption.
#' 
#' @details
#' This function is awesome and does excellent stuff.
#' 
#' @return
#' A data.table is returned with awesome stuff.
#' 
#' @examples
#' x <- 1
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
#' @import data.table
#' 
SplitNASCAssignment <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


