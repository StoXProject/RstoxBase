
##################################################
##################################################
#' Calculate number density from NASC in length intervals
#' 
#' This function takes NASC and 
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
AcousticDensity <- function(NASCData,m=20,a=-70,d=1) {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
  
  
  
  
  # TS_l = m*log10(l) +a+d*log10(1+r_y/10)
  #Where:
    # TS_l = target strength (dB re 1 m2) of a fish with length l (cm)
    # m = constant in the TS vs length relationship for the given species (user input)
    # a = constant in the TS vs length relationship for the given species (user input)
    # d = constant in the TS vs length relationship related to depth dependent TS (user input)
    # l = length of the fish (cm). Typically, the center length of a length group (data input)
    # ry = average depth (m) of the NASC channel y (data input)
  
  # depth_info <- NASCData[,c('Channel','MinRange','MaxRange')]
  TS_l = c()
  
  
  #In linear domain
  sigma_bs_l = 10**(TS_l/10)
  # where
  #sigma_bs_l = acoustic backscattering cross-section (m2) for a fish of length l
  
  # NASC_l = NASC *(sigma_bs_l*p_l)/(sum(sigma_bs_l*p_l))
  # where:
    # NASC = the total NASC which is used to calculate densities by length
    # NASCl = the proportion of the total NASC which can be attributed to length group l. The sum of
    # NASCl for all length groups in the total length distribution is equal to NASC
    # pl = proportion of fish of length l in the input length distribution. Sum of all pl is 1.
  
  
}













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
#' A data.frame are returned
#' 
#' @seealso \code{\link[roxygen2]{roxygenize}} is used to generate the documentation.
#' 
#' @export
NASC <- function(StoxAcousticData = NULL, AcousticLayer = NULL, AcousticPSU = NULL) {
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
##################################################
#' Sum NASC 
#' 
#' This function sums all the vertical channels into layer per edsu
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
SumNASC <- function(NASCData=NULL,TargetResolution='Layer',AcousticLayer=NULL) {
  
  
  
  #Test if this is a vallid TargetResolution
  if(!TargetResolution%in%ModelVariables$NASCData$verticalResolution)stop('Invalid TargetResolutions')
  
  
  #Add layer
  if(!is.null(AcousticLayer)){
    print('Add acoustic layer')
    NASCData$Layer<-NULL
    NASCData<- cbind(NASCData,AcousticLayer[,c('Channel','Layer')])
  }
    
    
  
  #get the grouping variables
  aggregationVariables <- determineAggregationVariables(data=NASCData, 
                                                        TargetResolution = TargetResolution, 
                                                        dataType = 'NASCData', 
                                                        dimension = "vertical")
  
  
  # Return immediately if TargetResolution equals inputResolution:
  if(TargetResolution == aggregationVariables$finestResolution) {
    print('Did not aggregate')
    return(NASCData)
  }
  # Also return immediately if TargetResolution is not in presentResolutions:
  if(!TargetResolution %in% aggregationVariables$presentResolution) {
    print('Did not aggregate2')
    return(NASCData)
  }
  
  #Test if layer information has been added
  if(any(is.na(NASCData$Layer))) {
      stop('AcousticLayer must be defined when summing to layer')
  }

  
  #Sum
  data<-NASCData[, .(sum(NASC)), by = c(aggregationVariables$by)]  
  
  
  #Fiks variable names
  data$NASC <-data$V1
  data$V1<-NULL
  
  #Add channel info
  colnam <-colnames(data)
  data$x<-NA
  colnames(data)<-c(colnam,toString(aggregationVariables$presentResolution[aggregationVariables$presentResolution!=TargetResolution]))
  
  
  #Add minRange and maxRange
  data<-cbind(data, (AcousticLayer[,c('Layer','MinRange','MaxRange')]))
  
  #Add NASCWeight and EffectiveLogDistance
  data<-cbind(data, unique(NASCData[,c('EDSU','NASCWeight','EffectiveLogDistance')]))
  
  
  #Reshape structure
  data<-data[,c('Stratum','PSU','EDSU','Layer','Channel',
                        'AcousticCategory','ChannelReference','Frequency',
                        'NASC','MinRange','MaxRange','NASCWeight','EffectiveLogDistance')]
  
  
  
  
  return(data)
  
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
##################################################
#' Average NASC for each acoustic PSU
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
MeanNASC <- function(NASCData=NULL,TargetResolution = 'PSU',AcousticPSU=NULL) {

  
  #Check if valid target Resolution
  if(!TargetResolution%in%ModelVariables$NASCData$horizontalResolution)stop('Error: Invalid TargetResolution. ')
  
  
  
  
  #Add acoustic PSU  
  if(!is.null(AcousticPSU)){
    NASCData$PSU<-NULL
    NASCData$Stratum<-NULL
    NASCData<-merge(NASCData,AcousticPSU)     #Added merge here, but see if there exist other ways that is faster
  }
  

  
  #Get variables to aggregate by
  aggregationVariables <- determineAggregationVariables(NASCData, TargetResolution = TargetResolution, dataType = ModelVariables$NASCData$data, dimension = "horizontal")
  
  
  
  # Return immediately if TargetResolution equals inputResolution:
  if(TargetResolution == aggregationVariables$finestResolution) {
    print('Did not aggregate')
    return(NASCData)
  }
  
  
  
  #Grab ranges to be added later
  Ranges <- unique(NASCData[,c('Layer','PSU','Stratum','MaxRange','MinRange')])
  
  
  #Logic test: check if there is multiple ranges per psu and 
  #To be added
  
  
  #Find mean nasc per target resolution
  data <- NASCData[, .(sum(NASC*EffectiveLogDistance)/sum(EffectiveLogDistance)), by = c(aggregationVariables$by)] 
  
  #Fix names
  data$NASC <- data$V1
  data$V1 <- NULL
  
  #Find the transect distance: 
  data$NASCWeight <- NASCData[, .(sum(EffectiveLogDistance)), by = c(aggregationVariables$by)]$V1

    
  #Remove nonrelevant resolution data
  data$EDSU <- NA
  data$EffectiveLogDistance <- NA
  if(TargetResolution=='Stratum'){data$PSU<-NA}

  
  #Add lost information  
  data<-cbind(data,Ranges)
  
  
  #Reshape structure
  data<-data[,c('Stratum','PSU','EDSU','Layer','Channel',
                'AcousticCategory','ChannelReference','Frequency',
                'NASC','MinRange','MaxRange','NASCWeight','EffectiveLogDistance')]
  
  return(data)
  
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


