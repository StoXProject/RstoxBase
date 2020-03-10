
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

AcousticDensity <- function(NASCData,
                            AssignedLengthDistributionData,
                            DefineAcousticTargetStrength,
                            SpeciesLinkTable,
                            TargetStrengthMethod = 'StandardModel'){
    
    
    
    #################################################################
    #   Define a function to add TS according to selected model     #
    #################################################################
    TargetStrengthFunctions <- function(TargetStrengthMethod,DensityData){
        
        #Check wich model is selected
        if(TargetStrengthMethod=='StandardModel'){
            
            #Check if all parameters is avaliable
            if(!all(c("d",'a','m') %in% colnames(DensityData),T)){
                warning('mad parameters is not added')
            }
            
            #Compute targetstrength
            DensityData$TargetStrength<-apply(DensityData[,c('IndividualTotalLengthCentimeter','m','a','d','MaxLayerDepth','MinLayerDepth')],1,
                                              function(x) x['m']*log10(x['IndividualTotalLengthCentimeter'])+x['a']+x['d']*log10(1+(mean(c(x['MaxLayerDepth'],x['MinLayerDepth']))/10)))
            
        }
        else{
            warning('Selected model do not exist')
        }
        
        return(DensityData)
    }
        
    
    
    #################################################################
    #    Merge TargetStrengthTable with SpeciesLinkTable            #
    #################################################################
    TargetStrengthTable<-merge(DefineAcousticTargetStrength,SpeciesLinkTable,by='AcousticCategory',all.x=T,allow.cartesian=TRUE)
        
    
    #################################################################
    #         Join NASCData with species link                       #
    #################################################################
    key <- c(names(NASCData)[names(NASCData)%in%names(TargetStrengthTable)])
    if(nrow(TargetStrengthTable)>1){
        warning('Several TargetStrength references is included. This has not yet been implemented')}
    
    
    #Small hack as there is different types in data
    TargetStrengthTable$Frequency<-as.integer(TargetStrengthTable$Frequency)
    TargetStrengthTable$AcousticCategory<-as.integer(TargetStrengthTable$AcousticCategory)
    
    
    DensityData<-merge(NASCData,TargetStrengthTable,by=c('AcousticCategory','Frequency'),all.x = T)
    

    
        
    #################################################################
    #         Remove horizontal resolution not in NASCData          #
    ################################################################# 
    rmo <- c(names((colSums(is.na(DensityData[,c('Stratum','PSU')])) == nrow(DensityData[,c('Stratum','PSU')])))[(colSums(is.na(DensityData[,c('Stratum','PSU')])) == nrow(DensityData[,c('Stratum','PSU')]))==T],'assignmentID')
    AssignedLengthDistributionData[,(rmo):=NULL]
    AssignedLengthDistributionData<-unique(AssignedLengthDistributionData)
    
    
    
    
    
    #################################################################
    #         merge DensityData with biology info                   #
    #################################################################
    key <- c(names(DensityData)[names(DensityData)%in%names(AssignedLengthDistributionData)])
    DensityData <- merge(DensityData,AssignedLengthDistributionData,by=key,all.x = T)
    
    
    
    
    
    #################################################################
    #         Compute Target strength when using model              #
    #################################################################
    DensityData<-TargetStrengthFunctions(TargetStrengthMethod,DensityData)

    
        
    #################################################################
    #         Compute the total weight                              #
    #################################################################
    tmp <- DensityData[, sum(10^(TargetStrength/10)*WeightedCount,na.rm = T), by=.(Stratum,Layer,SpeciesCategory)]
    DensityData<-merge(DensityData,tmp,by=c(names(DensityData)[names(DensityData)%in%names(tmp)]))
    
    
    
    
    
    
    #################################################################
    #         Compute the mean nasc per length group                #
    #################################################################
    DensityData$NASC <- apply(DensityData,1,function(x) as.numeric(x['NASC'])*(((10^(as.numeric(x['TargetStrength'])/10))*as.numeric(x['WeightedCount']))/as.numeric(x['V1'])))
    
    
    
    
    
    
    #################################################################
    #          Compute number of individs                           #
    #################################################################
    #Compute the number of idivids per sqare nautical mile per length group
    DensityData$Density <- apply(DensityData,1,function(x) as.numeric(x['NASC'])/(4*pi*10^(as.numeric(x['TargetStrength'])/10)))
    
    
    
    
    
    #################################################################
    #         Cleen DensityData output                              #
    #################################################################
    relevantVariables <- getAllDataTypeVariables(dataType = "DensityData")
    DensityData <- DensityData[, ..relevantVariables]
    
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
StationSpecCatDensity <- function() {
	# Use @noRd to prevent rd-files, and @inheritParams runBaseline to inherit parameters (those in common that are not documented) from e.g. getBaseline. Use @section to start a section in e.g. the details. Use @inheritParams runBaseline to inherit parameters from e.g. runBaseline(). Remove the @import data.table for functions that do not use the data.table package, and add @importFrom packageName functionName anotherFunctionName for importing specific functions from packages. Also use the packageName::functionName convention for the specifically imported functions.
}


##################################################
##################################################
#' Swept-area density
#' 
#' This function calculates the area density of fish as number of individuals per square nautical mile.
#' 
#' @inheritParams RegroupLengthDistribution
#' @param SweptAreaMethod The method used to calculate the density, one of "LengthDependent" and "TotalCatch" (the latter is not )
#' @param SweepWidthMethod Parameter descrption.
#' @param SweepWidthSweepWidth Parameter descrption.
#' @param SweepWidthExpr Parameter descrption.
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
SweptAreaDensity <- function(
    LengthDistributionData, 
    SweptAreaMethod = c("LengthDependent", "TotalCatch"), 
    SweepWidthMethod = c("Constant", "CruiseDependent"), 
    SweepWidth = integer(), 
    SweepWidthExpr = character(), 
    CatchVariable = c("Weight", "Count")
) {
	
    # Get the DefinitionMethod:
    SweptAreaMethod <- match.arg(SweptAreaMethod)
    # Get the DefinitionMethod:
    SweepWidthMethod <- match.arg(SweepWidthMethod)
    
    # This method may require defining an additional data type in StoX:
    if(SweptAreaMethod == "TotalCatch") {
        stop("SweptAreaMethod = \"TotalCatch\" not yet implemented")
    }
    
    
    # Check the horizontal and vertical resolution of the input LengthDistributionData:
    if(any(!is.na(LengthDistributionData$Station)) && any(!is.na(LengthDistributionData$Haul))) {
        stop("The horizontal and vertical resolution of the input LengthDistributionData must be PSU (identified by only NAs in the Station column) and Layer (identified by only NAs in the Haul column)")
    }
    else if(any(!is.na(LengthDistributionData$Station))) {
        stop("The horizontal resolution of the input LengthDistributionData must be PSU (identified by only NAs in the Station column)")
    }
    else if(any(!is.na(LengthDistributionData$Haul))) {
        stop("The vertical resolution of the input LengthDistributionData must be Layer (identified by only NAs in the Haul column)")
    }
    
    
    # Get the length distribution type:
    LengthDistributionType <- utils::head(LengthDistributionData$LengthDistributionType, 1)
    
    # Issue an error if the LengthDistributionType is "Percent" or "Standard":
    validLengthDistributionType <- c("Normalized", "SweepWidthCompensatedNormalized", "SelectivityCompensatedNormalized")
    if(! LengthDistributionType %in% validLengthDistributionType) {
        stop("The LengthDistributionType of the input LengthDistributionData must be one of ", paste(validLengthDistributionType, collapse = ", "))
    }
    
    
    
    # Make a copy of the input, since we are averaging and setting values by reference:
    DensityData = data.table::copy(LengthDistributionData)
    
    # Introduce the DensityWeight as a copy of the LengthDistributionWeight:
    DensityData[, DensityWeight := LengthDistributionWeight]
    
    
    # If LengthDistributionType is "NormalizedLengthDistribution", the effectivev towed distance has been accounted for, but there is still need to account for the sweep width:
    if(LengthDistributionType == "Normalized") {
        
        # Use a constant sweep width for all data by default:
        if(SweepWidthMethod == "Constant") {
            if(length(SweepWidth) == 0) {
                stop("SweepWidth must be given when SweepWidthMethod == \"Constant\"")
            }
            
            # Convert WeightedCount to density:
            DensityData[, Density := WeightedCount / SweepWidth]
        }
        else if(SweepWidthMethod == "CruiseDependent") {
            stop("SweepWidthMethod = \"CruiseDependent\" is not yet implemented")
            if(length(SweepWidthExpr) == 0) {
                stop("SweepWidthExpr must be given when SweepWidthMethod == \"CruiseDependent\"")
            }
        }
        
    }
    # If LengthDistributionType == "SweepWidthCompensatedNormalized" the sweep width is already accounted for, in meters:
    else if(LengthDistributionType == "SweepWidthCompensatedNormalized") {
        # Copy the WeightedCount to the Density:
        DensityData[, Density := WeightedCount]
    }
    else {
        stop("Invalid LengthDistributionType. In StoX 2.7 and earlier, LengthDistType = \"LengthDist\" was allowed, where the distance was multiplied with the weighted count in SweptAreaDensity.")
    }
    
    # Convert to density per nautical mile atwarthship:
    lengthOfOneNauticalMile <- 1852
    DensityData[, Density := Density * lengthOfOneNauticalMile]
    
    # Extract the relevant variables only:
    relevantVariables <- getAllDataTypeVariables(dataType = "DensityData")
    DensityData <- DensityData[, ..relevantVariables]
    
    return(DensityData)
}



##################################################
##################################################
#' Mean density in each stratum
#' 
#' This function calculates the weighted avverage of the density in each stratum (or possibly in each PSU, which impies returning the input DensityData unchanged). The weights are effective log distance for acoustic density and number of hauls ??????????????????? per PSU for swept area density.
#' 
#' @param DensityData An object of \code{\link{DensityData}} data.
#' @param TargetResolution The vertical resolution of the output.
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
MeanDensity <- function(DensityData, TargetResolution = "Stratum") {
    meanData(DensityData, dataType = "DensityData", targetResolution = TargetResolution)
}

