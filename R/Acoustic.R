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
  
  
  
  
  #Grab data from StoxAcoustic, and merge into one dataframe
  NASCData <- merge(merge(merge(as.data.frame(StoxAcousticData$NASC),
                          as.data.frame(StoxAcousticData$Beam)),
                    as.data.frame(StoxAcousticData$AcousticCategory)),
                    as.data.frame(StoxAcousticData$Log)[c('Log','Distance','EDSU','CruiseKey','LogKey')])
  
  
  
  #Fiks missing stuff
  NASCData$SampleUnit <- paste(NASCData$CruiseKey,NASCData$Log,NASCData$LogKey,sep='/')
  NASCData$SampleUnitType <- 'EDSU'
  NASCData$SampleSize <- 1
  NASCData$PosSampleSize <- 1
  NASCData$Layer <- NASCData$ChannelKey
  
  
  #Remove that is not needed
  NASCData<-NASCData[c('AcousticCategory','SampleUnitType','SampleUnit','SampleSize','PosSampleSize',
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
#' Average NASC for ecah acoustic PSU
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
MeanNASC <- function() {
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


