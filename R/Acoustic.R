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
AcousticDensity <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
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
  
  
  
  #Add all AcousticCategory in NASCData
  AcousticCategory <- c(unique(StoxAcousticData$AcousticCategory))
  NASCData <-merge(NASCData,AcousticCategory,by=c('LogKey','CruiseKey'),all.x=TRUE)

  
  #Add frequency to NASCData
  NASCData <-merge(NASCData,StoxAcousticData$Beam,by=c('LogKey','CruiseKey','BeamKey'),all.x=TRUE)
  
  
  #Add NASC data to NASCData
  NASCData <- merge(NASCData,StoxAcousticData$NASC,by=c('LogKey','CruiseKey','BeamKey','AcousticCategoryKey'),all.x = TRUE)
  
  
  #Fix missing values in NASC
  NASCData$ChannelReferenceKey[is.na(NASCData$NASC)]<-'P'
  NASCData$ChannelKey[is.na(NASCData$NASC)]<-'1'
  NASCData$MinRange[is.na(NASCData$NASC)]<-0
  NASCData$MaxRange[is.na(NASCData$NASC)]<-Inf
  NASCData$NASC[is.na(NASCData$NASC)]<-0
  
  
  
  
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

  # 
  # 
  # 
  # 
  #   
  #   
  #   
  #   
  # #Fill inn acousticPSU if avaliable
  # #TODO:
  # # revisit this when this info is avaliable
  # if(!is.null(AcousticPSU)){
  # 
  #   print('Add acousticPSU')
  #   NASCData$PSU <- NULL
  #   NASCData$Stratum <- NULL
  #   NASCData$Survey <- NULL
  #   # NASCData<-merge(NASCData,unique(AcousticPSU),all.x = TRUE)
  # 
  # }
  # 
  
  
  #Remove that is not needed
  # NASCData<-NASCData[c('AcousticCategory','Frequency')]
  
  # ,'SampleUnitType','SampleUnit',
                       # 'Distance','NASC','MaxRange','MinRange',
                       # 'Channel','Layer','PSU','Stratum','Survey')]
  
  
  
  
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
SumNASC <- function(data=NULL,TargetResolution='Layer',AcousticLayer=NULL) {
  # Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
  
  
  #get the grouping variables
  aggregationVariables <- determineAggregationVariables(data=NASCData, TargetResolution = TargetResolution, dataType = 'NASCData', dimension = "vertical")
  
  
  # Return immediately if TargetResolution equals inputResolution:
  if(TargetResolution == aggregationVariables$finestResolution) {
    print('Did not aggregate')
    return(data)
  }
  # Also return immediately if TargetResolution is not in presentResolutions:
  if(!TargetResolution %in% aggregationVariables$presentResolution) {
    print('Did not aggregate2')
    return(data)
  }
  

  
  #Sum
  data<-data[, .(sum(NASC)), by = c(aggregationVariables$by)]  
  
  
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
  
  
  
  # 
  # 
  # #Add range i SumNasc
  # 
  # #Add rest of the nasc info
  # 
  # 
  # 
  # 
  # 
  # 
  # #Check if valid target Resolution
  # if(!TargetResolution%in%c('Channel','Layer','AllLayers'))stop('Error: Invalid TargetResolution.')
  # 
  # 
  # 
  # 
  # #Fill inn acoustic layer if avaliable
  # #TODO: 
  # # revisit this when this info is avaliable
  # if(!is.null(AcousticLayer)){
  #   print('Add acoustic layer')
  #   NASCData$MinRange<-NULL
  #   NASCData$MaxRange<-NULL
  #   NASCData$Layer<-NULL
  #   NASCData<-merge(NASCData,unique(AcousticLayer),by.x=c('SampleUnit','Channel'))
  # }
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #Check if layer in NASCData when layer is selected
  # if(!TargetResolution =='Channel'){
  #   if(nrow(NASCData[!is.na(NASCData$Layer),])==0)stop(paste0('Error: TargetResolution ',TargetResolution, 'is selected but there is no layer data in NASCData'))
  # }
  # 
  # 
  # 
  # 
  # 
  # #If TargetResolution = Channel, skip
  # if(TargetResolution=='Channel') SumNASCData<-NASCData
  # 
  # 
  # 
  # 
  # #If TargetResolution = Lauer, sum nasc value per EDSU per Layer
  # if(TargetResolution=='Layer'){
  #   
  #   
  #   
  #   #A bug fix if we want to show layers equal to NA
  #   if(rm.na == TRUE)NASCData$Layer[is.na(NASCData$Layer)]<-'NA'
  #   
  #   
  #   
  #   #Aggregate nasc data in cathegory
  #   tmp <- aggregate(NASCData$NASC, 
  #                            by=list(AcousticCategory=NASCData$AcousticCategory,
  #                                    SampleUnitType=NASCData$SampleUnitType, 
  #                                    SampleUnit=NASCData$SampleUnit, 
  #                                    Distance = NASCData$Distance, 
  #                                    Frequency = NASCData$Frequency, 
  #                                    Layer = NASCData$Layer),
  #                            FUN=sum)
  #   
  #   #Add data
  #   tmp$NASC <- tmp$x
  #   tmp$x<-NULL
  #   
  #   tmp <-((merge(tmp,unique(NASCData[c('AcousticCategory','SampleUnit','Frequency','Layer','PSU','Stratum','Survey')]))))
  #   
  #   
  #   SumNASCData <- tmp  
  #   SumNASCData$Channel<-NA
  #   
  #   
  #   #Find minimum range
  #   tmp <- aggregate(NASCData$MinRange, 
  #                    by=list(AcousticCategory=NASCData$AcousticCategory,
  #                            SampleUnitType=NASCData$SampleUnitType, 
  #                            SampleUnit=NASCData$SampleUnit, 
  #                            Distance = NASCData$Distance, 
  #                            Frequency = NASCData$Frequency, 
  #                            Layer = NASCData$Layer),
  #                    FUN=min)
  #   
  #   
  #   #Add to data
  #   tmp$MinRange <- tmp$x
  #   tmp$x<-NULL
  #   
  #   SumNASCData<-merge(SumNASCData,tmp)
  #   
  #   
  #   #Find maximum range
  #   tmp <- aggregate(NASCData$MaxRange,
  #                    by=list(AcousticCategory=NASCData$AcousticCategory,
  #                            SampleUnitType=NASCData$SampleUnitType, 
  #                            SampleUnit=NASCData$SampleUnit, 
  #                            Distance = NASCData$Distance, 
  #                            Frequency = NASCData$Frequency, 
  #                            Layer = NASCData$Layer),
  #                    FUN=max)
  #   
  #   
  #   #Add to data
  #   tmp$MaxRange <- tmp$x
  #   tmp$x<-NULL
  #   SumNASCData<-merge(SumNASCData,tmp)
  #   SumNASCData$SampleUnitType<-'EPSU'
    
  #   
  # }
  # 
  # 
  # 
  # 
  # if(TargetResolution=='AllLayers'){
  #   
  #   #Aggregate nasc data in cathegory
  #   tmp <- aggregate(NASCData$NASC, 
  #                    by=list(AcousticCategory=NASCData$AcousticCategory,
  #                            SampleUnit=NASCData$SampleUnit, 
  #                            Distance = NASCData$Distance, 
  #                            Frequency = NASCData$Frequency), 
  #                    FUN=sum)
  #   
  #   #Add data
  #   tmp$NASC <- tmp$x
  #   tmp$x<-NULL
  #   
  #   tmp <-((merge(tmp,unique(NASCData[c('AcousticCategory','SampleUnit','Frequency','PSU','Stratum','Survey')]))))
  #   
  #   SumNASCData <- tmp  
  #   SumNASCData$Layer <- 'AllLayers'
  #   SumNASCData$Channel <- NA
  #   
  #   
  #   #Find minimum range
  #   tmp <- aggregate(NASCData$MinRange, 
  #                    by=list(AcousticCategory=NASCData$AcousticCategory,
  #                            SampleUnit=NASCData$SampleUnit, 
  #                            Distance = NASCData$Distance, 
  #                            Frequency = NASCData$Frequency),
  #                    FUN=min)
  #   
  #   
  #   #Add to data
  #   SumNASCData$MinRange <- 0
  #   SumNASCData$MaxRange <- Inf
  #   SumNASCData$SampleUnitType<-'EPSU'
  # }
  # 
  # 
  # #Sort SumNASCData
  # SumNASCData<-SumNASCData[c('AcousticCategory','Frequency','SampleUnitType','SampleUnit',
  #                      'Distance','NASC','MaxRange','MinRange',
  #                      'Channel','Layer','PSU','Stratum','Survey')]  
  # 
  # head(SumNASCData)
  # 
  #Return
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
MeanNASC <- function(NASCData=NULL,TargetResolution = 'PSU',AcousticPSU=NULL,rm.na = TRUE) {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
  
  # NASCData<-droplevels(NASCData)
  
  
  #Check if valid target Resolution
  if(!TargetResolution%in%c('PSU','Stratum','Survey'))stop('Error: Invalid TargetResolution. Valid resolutions is Channel, Layer and AllLayers')
  
  
  
  # 
  # #Fill inn acousticPSU if avaliable
  # #TODO:
  # # revisit this when this info is avaliable
  # if(!is.null(AcousticPSU)){
  #   
  #   if(length(unique(NASCData$SampleUnitType))>1)stop('Error: Several SampleUnitTypes is present in NASCData')
  #   
  #   print('Add acousticPSU')
  #   
  #   if(unique(NASCData$SampleUnitType)=='EDSU'){
  #     NASCData$Stratum<-NULL
  #     NASCData$Survey<-NULL
  #     NASCData$PSU<-NULL
  #     NASCData<-merge(NASCData,unique(AcousticPSU),all.x = TRUE)
  #   }
  #   if(unique(NASCData$SampleUnitType)=='PSU'){
  #     AcousticPSU_sub <- unique(AcousticPSU[c('PSU','Stratum','Survey')])
  #     NASCData$Survey<-NULL
  #     NASCData$PSU<-NULL
  #     NASCData<-merge(NASCData,AcousticPSU_sub,all.x = TRUE)
  #   }
  # 
  # }
  
  
  # 
  # #If unallocated stuff should be visible, change the variable name
  # if(rm.na==FALSE)NASCData$Stratum[is.na(NASCData$Stratum)]<-'Unallocated'
  # if(rm.na==FALSE)NASCData$Survey[is.na(NASCData$Survey)]<-'Unallocated'
  # if(rm.na==FALSE)NASCData$PSU[is.na(NASCData$PSU)]<-'Unallocated'
  

  # 
  # #Remove channel info  
  # NASCData$Channel<-NA
  
  
  # 
  # #Fix layer when they are missing
  # if(length(unique(NASCData$Layer))==1){
  #   if(is.na(unique(NASCData$Layer))){
  #     NASCData$Layer<-1
  #     NASCData$MinRange<-0
  #     NASCData$MaxRange<-Inf
  #     }
  # }else{
  #   NASCData$Layer[is.na(NASCData$Layer)]<-'Unallocated'
  # }
  
  
  ##################################
  #Test to see if the NASCData is ok
  ##################################
  
  # if(TargetResolution=='PSU'){
  #   if(nrow(NASCData[!is.na(NASCData$PSU),])==0)stop(paste0('Error: TargetResolution PSU is selected but no PSU data is within NASCData'))
  #   
  #   if(sum(table((unique(NASCData[!is.na(NASCData$PSU),][c('PSU','Layer','MaxRange','MinRange')])$PSU))>length(unique(NASCData$Layer)))>0)stop('Error: Vertical resolution is different within a psu')
  # }
  # 
  # if(TargetResolution=='Stratum'){
  #   if(nrow(NASCData[!is.na(NASCData$PSU),])==0)stop(paste0('Error: TargetResolution Stratum is selected but no Stratum data is within NASCData'))
  # }
  # 
  # if(TargetResolution=='Survey'){
  #   if(nrow(NASCData[!is.na(NASCData$PSU),])==0)stop(paste0('Error: TargetResolution Survey is selected but no Survey data is within NASCData'))
  # }
  # 
  
  
  
  
  
  
  
  #########################################
  #Find the mean nasc per Target Resolution
  #########################################
  
  
  if(TargetResolution=='PSU'){
    # NASCData[is.na(NASCData$Distance),]
    
    
    #Find the total distance per PSU
    distances_matrix <- unique(NASCData[,c('EDSU','EffectiveLogDistance','PSU')])
    
    
    NASCData<-as.data.frame(NASCData)
    
    #Aggregate distance data in each cathegory
    tmp_distance <- aggregate(distances_matrix$EffectiveLogDistance, 
                     by=list(PSU), 
                     FUN=sum)
    
    
    #Fix the variable name
    tmp_distance$Distance <- tmp_distance$x
    tmp_distance$x <- NULL
    
    
    
    #Aggregate nasc data in each cathegory
    tmp <- aggregate(NASCData$NASC*NASCData$Distance, 
                     by=list(AcousticCategory=NASCData$AcousticCategory,
                             Frequency = NASCData$Frequency,
                             Layer = NASCData$Layer,
                             PSU = NASCData$PSU), 
                     FUN=sum)
    
    
    #Fix the variable name
    tmp$NASC <- tmp$x
    tmp$x<-NULL
    
    
    
    #Add distance and find mean NASC
    tmp<-merge(tmp,tmp_distance,all.y = TRUE)
    tmp$NASC <- tmp$NASC/tmp$Distance
    
    
    
    #Add species cathegory to PSU where they are missing
    test <- unique(merge(tmp[c('PSU','Layer','Frequency','Distance')],unique(tmp$AcousticCategory)))
    test$AcousticCategory<-test$y
    test$y<-NULL  
    tmp<-merge(test,tmp,by.x=c('PSU','Layer','Frequency','Distance','AcousticCategory'),all.x=TRUE)
    

    #Set NA in NASC to 0    
    tmp$NASC[is.na(tmp$NASC)]=0
    
    
    #Store data in MeanNASCData
    MeanNASCData <- tmp
    MeanNASCData$SampleUnit<-MeanNASCData$PSU
    MeanNASCData$SampleUnitType<-'PSU'
    
    
    #Add missing stuff from NASCData
    MeanNASCData<-merge(MeanNASCData,unique(NASCData[c('PSU','Frequency','Layer','Channel','Stratum','Survey','MaxRange','MinRange')]),
                        by=c('PSU','Frequency','Layer'))
    
  }
  
  
  #If target resolution is equal to Stratum
  if(TargetResolution=='Stratum'){
    
    
    #Find the total distance per PSU
    distances_matrix <- unique(NASCData[c('SampleUnit','Distance','PSU','Stratum')])
    
    
    
    #Aggregate distance data in each cathegory
    tmp_distance <- aggregate(distances_matrix$Distance, 
                              by=list(Stratum = distances_matrix$Stratum), 
                              FUN=sum, na.rm=TRUE)
    
    
    #Fix the variable name
    tmp_distance$Distance <- tmp_distance$x
    tmp_distance$x <- NULL
    
    
    
    
    #Aggregate nasc data in each cathegory
    tmp <- aggregate(NASCData$NASC*NASCData$Distance, 
                     by=list(AcousticCategory=NASCData$AcousticCategory,
                             Frequency = NASCData$Frequency,
                             Layer = NASCData$Layer,
                             Stratum = NASCData$Stratum), 
                     FUN=sum)
    
    
    
    #Fix the variable name
    tmp$NASC <- tmp$x
    tmp$x<-NULL
    
    
    #Add distance and find mean NASC
    tmp<-merge(tmp,tmp_distance,all.y = TRUE)
    tmp$NASC <- tmp$NASC/tmp$Distance
    
    
    
    #Add species cathegory to Stratum where they are missing
    test <- unique(merge(tmp[c('Stratum','Layer','Frequency','Distance')],unique(tmp$AcousticCategory)))
    test$AcousticCategory<-test$y
    test$y<-NULL  
    tmp<-merge(test,tmp,by.x=c('Stratum','Layer','Frequency','Distance','AcousticCategory'),all.x=TRUE)
    
    
    #Set NA in NASC to 0    
    tmp$NASC[is.na(tmp$NASC)]=0
    
    
    #Store data in MeanNASCData
    MeanNASCData <- tmp
    MeanNASCData$SampleUnit<-MeanNASCData$Stratum
    MeanNASCData$SampleUnitType<-'Stratum'
    MeanNASCData$PSU<-NA
    
    
    #Add missing stuff from NASCData
    MeanNASCData<-merge(MeanNASCData,unique(NASCData[c('Frequency','Layer','Channel','Stratum','Survey','MaxRange','MinRange')]),
                        by=c('Stratum','Frequency','Layer'))
    
  }
  
  
  #If target resolution is equal to Stratum
  if(TargetResolution=='Survey'){
    
    
    #Find the total distance per PSU
    distances_matrix <- unique(NASCData[c('SampleUnit','Distance','PSU','Stratum','Survey')])
    
    
    
    #Aggregate distance data in each cathegory
    tmp_distance <- aggregate(distances_matrix$Distance, 
                              by=list(Survey = distances_matrix$Survey), 
                              FUN=sum, na.rm=TRUE)
    
    
    #Fix the variable name
    tmp_distance$Distance <- tmp_distance$x
    tmp_distance$x <- NULL
    
    
    
    
    #Aggregate nasc data in each cathegory
    tmp <- aggregate(NASCData$NASC*NASCData$Distance, 
                     by=list(AcousticCategory=NASCData$AcousticCategory,
                             Frequency = NASCData$Frequency,
                             Layer = NASCData$Layer,
                             Survey = NASCData$Survey), 
                     FUN=sum)
    
    
    
    #Fix the variable name
    tmp$NASC <- tmp$x
    tmp$x<-NULL
    
    
    #Add distance and find mean NASC
    tmp<-merge(tmp,tmp_distance,all.y = TRUE)
    tmp$NASC <- tmp$NASC/tmp$Distance
    
    
    
    #Add species cathegory to Survey where they are missing
    test <- unique(merge(tmp[c('Survey','Layer','Frequency','Distance')],unique(tmp$AcousticCategory)))
    test$AcousticCategory<-test$y
    test$y<-NULL  
    tmp<-merge(test,tmp,by.x=c('Survey','Layer','Frequency','Distance','AcousticCategory'),all.x=TRUE)
    
    
    #Set NA in NASC to 0    
    tmp$NASC[is.na(tmp$NASC)]=0
    
    
    #Store data in MeanNASCData
    MeanNASCData <- tmp
    MeanNASCData$SampleUnit<-MeanNASCData$Survey
    MeanNASCData$SampleUnitType<-'Survey'
    MeanNASCData$PSU<-NA
    MeanNASCData$Stratum<-NA
    
    
    #Add missing stuff from NASCData
    MeanNASCData<-merge(MeanNASCData,unique(NASCData[c('Frequency','Layer','Channel','Survey','MaxRange','MinRange')]),
                        by=c('Survey','Frequency','Layer'))
    
  }
  
  
  #Regroup MeanNASCData
  MeanNASCData<-MeanNASCData[c('AcousticCategory','Frequency','SampleUnitType','SampleUnit',
                             'Distance','NASC','MaxRange','MinRange',
                             'Channel','Layer','PSU','Stratum','Survey')]  
  
  #Return
  return(MeanNASCData)
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


