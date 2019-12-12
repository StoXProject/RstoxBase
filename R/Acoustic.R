

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
AcousticDensity <- function(NASCData,AssignedLengthDistributionData=NULL,m=20,a=-70,d=NULL) {

  
  #Grab the nasc data values
  tmp<-NASCData[,c('Stratum','PSU','Layer','Channel',
                   'AcousticCategory','Frequency','NASC',
                   'MinChannelRange', 'MaxChannelRange', 
                   'MinLayerRange', 'MaxLayerRange')]
  
  
  #Find the mean depth
  if(all(is.na(tmp$MinChannelRange))){
    tmp$MeanDepth <- rowMeans(tmp[,c('MinLayerRange','MaxLayerRange')])
    }else{
    tmp$MeanDepth <- rowMeans(tmp[,c('MinChannelRange','MaxChannelRange')])
    tmp$Layer <- tmp$Channel
  }
  
  
  #Remove redundent stuff
  tmp[,c('Channel','MinChannelRange','MaxChannelRange','MinLayerRange','MaxLayerRange')]<-NULL
  
  
  #Start making DensityData
  DensityData <- AssignedLengthDistributionData
  
  
  #Merge DensityData with NASCData
  DensityData<-merge(DensityData,tmp,by=c("PSU","Stratum","AcousticCategory","Frequency","Layer"),all=TRUE)
  
  
  
  #Add species stuff to DensityData
  #This is a temporaly stuff untill the process data is avaliable
  DensityData$m = 20
  DensityData$a = -72
  DensityData$d = 0
  
  
  #Add target strength to data
  DensityData$TS<-apply(DensityData[,c('LengthGroup','m','a','d','MeanDepth')],1,
                        function(x) x['m']*log10(x['LengthGroup'])+x['a']+x['d']*log10(1+(x['MeanDepth']/10)))
  
  #remove mad stuff
  DensityData[,c('m','a','d')]<-NULL
  
  #Find the weigth per group
  DensityData<-merge(DensityData,DensityData[, .(tmp=sum(10^(TS/10)*WeightedCount)), by = c('PSU','Stratum','AcousticCategory', 'Frequency', 'Layer')]  )
  
  
  #Find nasc per length group
  DensityData$NASC <- apply(DensityData,1,function(x) as.numeric(x['NASC'])*(((10^(as.numeric(x['TS'])/10))*as.numeric(x['WeightedCount']))/as.numeric(x['tmp'])))
  DensityData$tmp <- NULL

  
  #Compute the number of idivids per sqare nautical mile per length group
  DensityData$Density <- apply(DensityData,1,function(x) as.numeric(x['NASC'])/(4*pi*10^(as.numeric(x['TS'])/10)))
  
  
  return(DensityData)
}










##################################################
##################################################
#' NASC
#' 
#' NASC function converts the StoxAcousticData into
#' a NASCData format
#' 
#' @param StoxAcousticData input of acoustic information in a StoxAcoustcdata format
#' @param AcousticLayer input of acoustic layer definition in a NASCData sub format
#' @param AcousticPSU input of acoustic psu/strata definition in a NASCData sub format
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
  #This is all log distances both with and without filtered nasc data per species
  NASCData<- StoxAcousticData$Log[,c('Log','EffectiveDistance','EDSU','CruiseKey','LogKey')]
  
  
  #Grab all Frequency cathegories and stack it to NASCData
  Frequency <- c(unique(StoxAcousticData$Beam$Frequency))
  NASCData<-setDT(NASCData)[, .(Frequency = as.numeric(Frequency)), .(Log,EffectiveDistance,EDSU,CruiseKey,LogKey)]
  BeamKey <- c(unique(StoxAcousticData$Beam$BeamKey))
  NASCData<-setDT(NASCData)[, .(BeamKey = (BeamKey)), .(Log,EffectiveDistance,EDSU,CruiseKey,LogKey,Frequency)]
  
  
  #Grab channel key and stack into NASCData
  ChannelKey  <- c(unique(StoxAcousticData$NASC$ChannelKey ))
  NASCData<-setDT(NASCData)[, .(ChannelKey = (ChannelKey)), .(Log,EffectiveDistance,EDSU,CruiseKey,LogKey,Frequency,BeamKey)]
  
  
  #Add channel reference key to the NASCData
  NASCData<- merge(NASCData,StoxAcousticData$NASC[,c('LogKey','CruiseKey','BeamKey','AcousticCategoryKey',
                                                     'ChannelKey','ChannelReferenceKey','MinChannelRange',
                                                     'MaxChannelRange')],by=c('LogKey','CruiseKey',
                                                                              'BeamKey','ChannelKey'))
  
  
  #Grab all acoustic cathegory
  AcousticCategory <- c(unique(StoxAcousticData$AcousticCategory$AcousticCategory))
  NASCData<-setDT(NASCData)[, .(AcousticCategory = as.numeric(AcousticCategory)), .(Log,EffectiveDistance,EDSU,CruiseKey,LogKey,Frequency,BeamKey,ChannelKey,ChannelReferenceKey,MinChannelRange,MaxChannelRange)]
  NASCData$AcousticCategoryKey<-as.character(NASCData$AcousticCategory)

  
    
  #Add NASC info to NASCData
  NASCData <- merge(NASCData,StoxAcousticData$NASC[,c('LogKey','CruiseKey','BeamKey','AcousticCategoryKey',
                                                      'ChannelKey','ChannelReferenceKey','NASC')],by=c('LogKey','CruiseKey','BeamKey','AcousticCategoryKey','ChannelKey','ChannelReferenceKey'),all.x=TRUE)
  NASCData$NASC[is.na(NASCData$NASC)]<-0
  
  
  #Add stuff that is lost during aggregation
  NASCData$EDSU <- paste(NASCData$CruiseKey,sprintf("%.1f",round(as.numeric(NASCData$Log),digits = 1)),NASCData$LogKey,sep='/')
  NASCData$Channel <- NASCData$ChannelKey
  NASCData$ChannelReference <- NASCData$ChannelReferenceKey
  NASCData$NASCWeight<-NASCData$EffectiveDistance
  NASCData$EffectiveLogDistance<-NASCData$EffectiveDistance
  
  #Add information of payer and psu
  NASCData$Layer<-NA
  NASCData$PSU <- NA
  NASCData$Stratum <- NA
  
  

  #Fill inn acoustic layer if avaliable
  if(!is.null(AcousticLayer)){
    print('Add acoustic layer')
    NASCData$Layer<-NULL
    NASCData<- cbind(NASCData,AcousticLayer)
  }
  
  if(!is.null(AcousticPSU)){
      print('Add acoustic psu/strata')
      NASCData$PSU<-NULL
      NASCData$Stratum<-NULL
      #Do a qick fix here. Will look into more efficient ways of doing this.
      #There is something odd with the datastructure that has to be fixed in StoxAcoustic and in NASC
      NASCData <- as.data.table(merge(as.data.frame(NASCData),as.data.frame(AcousticPSU), by = "EDSU", sort=F))
    }
  
  
  #Reshape structure and remove tables not defined in NASCData
  NASCData<-NASCData[,c('Stratum','PSU','EDSU','Layer','Channel',
                        'AcousticCategory','ChannelReference','Frequency',
                        'NASC','MinChannelRange','MaxChannelRange','MinLayerRange',
                        'MaxLayerRange','NASCWeight','EffectiveLogDistance')]
  
  
  
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
    NASCData<- cbind(NASCData,AcousticLayer)
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
  data<-NASCData[, .(NASC=sum(NASC)), by = c(aggregationVariables$by)]  
  
  #Remove channel info
  data$Channel <- NA
  data$MinChannelRange <- NA
  data$MaxChannelRange <- NA
  
  
  #Add MinChannelRange and MaxChannelRange
  data<-cbind(data, (AcousticLayer[,c('Layer','MinLayerRange','MaxLayerRange')]))
  
  #Add NASCWeight and EffectiveLogDistance
  data<-cbind(data, unique(NASCData[,c('EDSU','NASCWeight','EffectiveLogDistance')]))
  
  
  
  #Reshape structure
  data<-data[,c('Stratum','PSU','EDSU','Layer','Channel',
                        'AcousticCategory','ChannelReference','Frequency',
                        'NASC','MinChannelRange','MaxChannelRange','MinLayerRange',
                        'MaxLayerRange','NASCWeight','EffectiveLogDistance')]
  
  
  
  
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

  #Use this when bugfixing from a test script
  # NASCData<-dl3  
  
  #Add acoustic PSU  
  if(!is.null(AcousticPSU)){
    NASCData$PSU<-NULL
    NASCData$Stratum<-NULL
    #Do a qick fix here. Will look into more efficient ways of doing this.
    #There is something odd with the datastructure that has to be fixed in StoxAcoustic and in NASC
    NASCData <- as.data.table(merge(as.data.frame(NASCData),as.data.frame(AcousticPSU), by = "EDSU", sort=F))
  }
                                                                          
  #Get variables to aggregate by
  aggregationVariables <- determineAggregationVariables(NASCData, TargetResolution = TargetResolution, dataType = ModelVariables$NASCData$data, dimension = "horizontal")
  
  
  
  # Return immediately if TargetResolution equals inputResolution:
  if(TargetResolution == aggregationVariables$finestResolution) {
    print('Did not aggregate')
    return(NASCData)
  }
  
  
  
  #Grab ranges to be added later
  Ranges <- unique(NASCData[,c('Layer','PSU','Stratum','MaxChannelRange','MinChannelRange','MaxLayerRange','MinLayerRange')])
  
  
  #Find mean nasc per target resolution
  data <- NASCData[, .(NASC = sum(NASC*EffectiveLogDistance)/sum(EffectiveLogDistance)), by = c(aggregationVariables$by)] 
  
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
                'NASC','MinChannelRange','MaxChannelRange','MinLayerRange','MaxLayerRange','NASCWeight','EffectiveLogDistance')]
  
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


