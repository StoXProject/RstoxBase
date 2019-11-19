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
NASC <- function(StoxAcousticData = NULL) {
  # Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
  
  
  
  #Add log distances in NASCData
  NASCData<- as.data.frame(StoxAcousticData$Log)[c('Log','Distance','EDSU','CruiseKey','LogKey')]
  
  #Add all AcousticCategory in NASCData
  AcousticCategoryKey <- c(unique(StoxAcousticData$AcousticCategory$AcousticCategoryKey))
  NASCData <-merge(NASCData,as.data.frame(AcousticCategoryKey))

  #Add frequency to NASCData
  NASCData <-merge(NASCData,as.data.frame(StoxAcousticData$Beam),all = TRUE)
  
  
  #Add NASC data to NASCData
  NASCData <- merge(NASCData,as.data.frame(StoxAcousticData$NASC),all = TRUE)
  
  
  #Fix missing values in NASC
  NASCData$ChannelReferenceKey[is.na(NASCData$NASC)]<-'P'
  NASCData$ChannelKey[is.na(NASCData$NASC)]<-'1'
  NASCData$MinRange[is.na(NASCData$NASC)]<-0
  NASCData$MaxRange[is.na(NASCData$NASC)]<-Inf
  NASCData$NASC[is.na(NASCData$NASC)]<-0
  # 
  # 
  # NASCData[NASCData$LogKey=='2017-02-13T21:00:54.000Z',]
  # StoxAcousticData$NASC[StoxAcousticData$NASC$LogKey=='2017-02-13T21:00:54.000Z',]
  # 
  # 
  # #Grab data from StoxAcoustic, and merge into one dataframe
  # NASCData0 <- merge(merge(merge(as.data.frame(StoxAcousticData$NASC),
  #                         as.data.frame(StoxAcousticData$Beam),all = TRUE),
  #                   as.data.frame(StoxAcousticData$AcousticCategory)),
  #                   as.data.frame(StoxAcousticData$Log)[c('Log','Distance','EDSU','CruiseKey','LogKey')],all = TRUE)
  
  
  
  
  
  #Fiks missing stuff
  NASCData$SampleUnit <- paste(NASCData$CruiseKey,sprintf("%.1f",round(as.numeric(NASCData$Log),digits = 1)),NASCData$LogKey,sep='/')
  NASCData$SampleUnitType <- 'EDSU'
  NASCData$SampleSize <- 1
  NASCData$PosSampleSize <- 1
  NASCData$Layer <- NASCData$ChannelKey
  
  
  #Remove that is not needed
  NASCData<-NASCData[c('AcousticCategoryKey','SampleUnitType','SampleUnit','SampleSize','PosSampleSize',
                       'Distance','MaxRange','MinRange','Frequency','Layer','NASC')]
  
  
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
SumNASC <- function(NASCData) {
  # Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
  
  
   
  
  #Aggregate nasc data in cathegory
  tmp <- aggregate(NASCData$NASC, 
                           by=list(AcousticCategory=NASCData$AcousticCategory,
                                   SampleUnit=NASCData$SampleUnit, 
                                   SampleSize = NASCData$SampleSize,
                                   PosSAmpleSize =NASCData$PosSampleSize, 
                                   Distance = NASCData$Distance, 
                                   Frequency = NASCData$Frequency, 
                                   Layer = NASCData$Layer),
                           FUN=sum)
  
  
  #Add data
  tmp$NASC <- tmp$x
  tmp$x<-NULL
  SumNASCData <- tmp  
  
  
  
  #Find minimum range
  tmp <- aggregate(NASCData$MinRange, 
                         by=list(AcousticCategory=NASCData$AcousticCategory,
                                 SampleUnit=NASCData$SampleUnit, 
                                 SampleSize = NASCData$SampleSize,
                                 PosSAmpleSize =NASCData$PosSampleSize, 
                                 Distance = NASCData$Distance, 
                                 Frequency = NASCData$Frequency, 
                                 Layer = NASCData$Layer),
                         FUN=min)
  
  
  #Add to data
  SumNASCData$MinRange <- tmp$x
  
  
  
  
  #Find maximum range
  tmp <- aggregate(NASCData$MaxRange,
                   by=list(AcousticCategory=NASCData$AcousticCategory,
                           SampleUnit=NASCData$SampleUnit, 
                           SampleSize = NASCData$SampleSize,
                           PosSAmpleSize =NASCData$PosSampleSize, 
                           Distance = NASCData$Distance, 
                           Frequency = NASCData$Frequency, 
                           Layer = NASCData$Layer), FUN=max)
  
  
  #Add to data
  SumNASCData$MaxRange <- tmp$x
  SumNASCData$SampleUnitType<-'EPSU'
  
  #Return
  return(SumNASCData)
  
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
MeanNASC <- function(NASCData=NULL,inn) {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
  
  
  
  #TODO: 
  #     - Ha med EDSU per PSU
  #     - Aggreger NASC
  #     - SampleSize er opptelling av antall EDSU
  #     - PosSampleSize er antall EDSU med NASC >0
  
  
  NASC_table <- NULL
  
  #Loop through each PSU
  for(psu in unique(inn$PSU)){
    
    #Get EPSU id references for each PSU
    epsu_id <- inn$EDSU[inn$PSU==psu]
    
    
    #For backwards compability
    #TODO: 
    #   - this has to be more accuratly checked
    epsu_id<-lapply(epsu_id, function(x) if(!grepl('.000Z',x)){x=paste0(x,'.000Z')}else(x=x))
    NASCData$SampleUnit<-gsub('T','/',NASCData$SampleUnit)
    
    
    #Filter out the data per PSU
    data <-NASCData[NASCData$SampleUnit%in%epsu_id,]
    
    
    #Print warning msg if there is epsu id not in psu
    if(sum(!epsu_id%in%NASCData$SampleUnit)>0)warning(paste0('EPSU not in PSU ', psu))

    
    
    
    
    #Only allow psu with epsu
    if(nrow(data)>0){
      
      
      
      #Distance of the PSU, used in the weighted mean
      distance_PSU <- sum(unique(data[c('SampleUnit','Distance','Frequency','Layer')])$Distance)
      sample_size <- nrow(unique(data[c('SampleUnit','Distance','Frequency','Layer')]))
      
      
      
      #Rename units 
      data$SampleUnit<-psu
      data$SampleUnitType<-'PSU'
      
      
      #Get the sum NASC horisontally*distance each transect
      out <-aggregate(x=data$NASC*data$Distance,
                      by=list(AcousticCategory=data$AcousticCategory,
                              SampleUnitType=data$SampleUnitType, 
                              SampleUnit=data$SampleUnit, 
                              Frequency = data$Frequency, 
                              Layer = data$Layer, 
                              SampleUnitType = data$SampleUnitType),
                      FUN=sum)
      
      #Devide with length of transect to get weighted mean
      out$NASC <- out$x/distance_PSU
      out$x <- NULL
      
      NASC_table=rbind(NASC_table,out)
    }else{}      
  }
  
  
  return(NASC_table)  
  
  
  # PSU <- 'T158'
  
  # tagget <- inn$tagg[inn$PSU==PSU]
  
  
  
  # 
  # 
  # #Filter out the data per PSU
  # data <-NASCData[NASCData$SampleUnit%in%tagget,]
  # tmp_data <- data
  # tmp_data$NASC[tmp_data$NASC>0] <- 1
  # 
  # 
  # 
  # 
  # #Temporarly step to get number of EPSU with data
  # out4<-aggregate(x=tmp_data$NASC,
  #                 by=list(AcousticCategory=tmp_data$AcousticCategory,
  #                         Frequency = tmp_data$Frequency, 
  #                         Layer = tmp_data$Layer),
  #                 FUN=sum)
  # out4$PosSampleSize <-out4$x
  # out4$x<-NULL
  # 
  # 
  # 
  # 
  # #Distance of the PSU, used in the weighted mean
  # distance_PSU <- sum(unique(data[c('SampleUnit','Distance','Frequency','Layer')])$Distance)
  # sample_size <- nrow(unique(data[c('SampleUnit','Distance','Frequency','Layer')]))
  # 
  # 
  # #Fiks stuff 
  # data$SampleUnit<-PSU
  # data$SampleUnitType<-'PSU'
  # 
  # 
  # 
  # #Get the sum NASC horisontally*distance each transect
  # out <-aggregate(x=data$NASC*data$Distance,
  #           by=list(AcousticCategory=data$AcousticCategory,
  #                   SampleUnit=data$SampleUnit, 
  #                   Frequency = data$Frequency, 
  #                   Layer = data$Layer, 
  #                   SampleUnitType = data$SampleUnitType),
  #           FUN=sum)
  # 
  # #Devide with length of transect to get weighted mean
  # out$NASC <- out$x/distance_PSU
  # 
  # #Add data
  # out$x<- NULL
  # out$Distance <- distance_PSU
  # out$SampleSize <- sample_size
  # 
  # 
  # 
  # 
  # 
  # 
  # #Grab the minimum range
  # out2 <-aggregate(x=data$MinRange,
  #                  by=list(AcousticCategory=data$AcousticCategory,
  #                          SampleUnit=data$SampleUnit, 
  #                          Frequency = data$Frequency, 
  #                          Layer = data$Layer, 
  #                          SampleUnitType = data$SampleUnitType),
  #                  FUN=min)
  # 
  # out <- merge(out,out2)
  # out$MinRange <- out$x
  # out$x=NULL
  # 
  # 
  # 
  # 
  # #Gram the maximume range
  # out2 <-aggregate(x=data$MaxRange,
  #                  by=list(AcousticCategory=data$AcousticCategory,
  #                          SampleUnit=data$SampleUnit, 
  #                          Frequency = data$Frequency, 
  #                          Layer = data$Layer, 
  #                          SampleUnitType = data$SampleUnitType),
  #                  FUN=max)
  # 
  # out <- merge(out,out2)
  # out$MaxRange <- out$x
  # out$x=NULL
  # 
  # 
  # 
  # 
  # out<-merge(out,out4)
  # return(out)
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


