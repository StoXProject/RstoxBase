
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
AcousticDensity <- function(
    NASCData,
    AssignmentLengthDistributionData,
    AcousticTargetStrength,
    SpeciesLinkTable,
    TargetStrengthMethod = "Foote1987"){
    
    # Check that the input SpeciesLinkTable has the appropriate types:
    checkTypes(table = SpeciesLinkTable)
    
    #################################################################
    #    Merge TargetStrengthTable with SpeciesLinkTable            #
    #################################################################
    TargetStrengthTable <- merge(AcousticTargetStrength, SpeciesLinkTable, by='AcousticCategory', all = TRUE, allow.cartesian = TRUE)
    
    
    #################################################################
    #         Join NASCData with species link                       #
    #################################################################
    #key <- c(names(NASCData)[names(NASCData)%in%names(TargetStrengthTable)])
    #if(nrow(TargetStrengthTable)>1){
    #    warning('Several TargetStrength references is included. This has not yet been implemented')}
    #
    #
    ##Small hack as there is different types in data
    #TargetStrengthTable$Frequency<-as.integer(TargetStrengthTable$Frequency)
    #TargetStrengthTable$AcousticCategory<-as.integer(TargetStrengthTable$AcousticCategory)
    
    mergeBy <- getDataTypeDefinition(dataType = "NASCData", elements = c("categoryVariable", "groupingVariables"), unlist = TRUE)
    DensityData <- merge(NASCData, TargetStrengthTable, by = mergeBy, all.x = TRUE)
    
    
    mergeBy <- intersect(names(DensityData), names(AssignmentLengthDistributionData))
    DensityData <- merge(DensityData, AssignmentLengthDistributionData, by = mergeBy, all.x = TRUE)
    
    getTargetStrength(DensityData, TargetStrengthMethod = TargetStrengthMethod)
    
    # Get backscatteringCrossSection:
    DensityData[, backscatteringCrossSection := 10^(TargetStrength/10)]
    
    stop("To be finished")
    
    
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

###########################################################
# Define a function to add TS according to selected model #
###########################################################
getTargetStrength <- function(Data, TargetStrengthMethod = c("Foote1987", "Ona2003")){
    
    TargetStrengthMethod <- match.arg(TargetStrengthMethod)
    
    # Check wich model is selected
    if(grepl("Foote1987", TargetStrengthMethod, ignore.case = TRUE)){
        # Check that all parameters are present: 
        if(!all(c("m", "a") %in% names(Data))){
            stop("The columns \"m\" and \"a\" are required for TargetStrengthMethod \"Foote1987\" (m * log10(L) + a, where L is length in centimeter)")
        }
        
        # Apply the Foote1987 equation: 
        Data[, TargetStrength := m * log10(IndividualTotalLengthCentimeter) + a]
    }
    else if(grepl("Ona2003", TargetStrengthMethod, ignore.case = TRUE)){
        # Check that all parameters are present: 
        if(!all(c("m", "a", "d") %in% names(Data))){
            stop("The columns \"m\", \"a\" and \"d\" are required for TargetStrengthMethod \"Foote1987\" (m * log10(L) + a, where L is length in centimeter)")
        }
        
        # Apply the Ona2003 equation: 
        verticalLayerDimension <- getDataTypeDefinition(dataType = "LengthDistributionData", elements = "verticalLayerDimension", unlist = TRUE)
        Data[, TargetStrength := m * log10(IndividualTotalLengthCentimeter) + a + d * log10(1 + rowMeans(.SD)/10), .SDcols = verticalLayerDimension]
    }
    else{
        warning("Invalid TargetStrengthMethod (Foote1987 and Ona2003 currently implemented)")
    }
}


AcousticDensityOld <- function(NASCData, m = 20, a = -70, d = double()) {
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
#' Swept-area density
#' 
#' This function calculates the area density of fish as number of individuals per square nautical mile.
#' 
#' @inheritParams RegroupLengthDistribution
#' @param SweepWidthMethod The method for calculating the sweep width to multiply the \code{WeightedCount} in the \code{LengthDistributionData} with. Possible options are (1) "Constant", which requires \code{SweepWidth} to be set as the constant sweep width, (2) "PreDefined", impying that the sweep width is already incorporated in the \code{WeightedCount} in the \code{LengthDistributionData}, by using LengthDistributionType == "SweepWidthCompensatedNormalized" in the function \code{\link{LengthDependentCatchCompensation}}, and (3) "CruiseDependent", which requires the \code{SweepWidthTable} to be given.
#' @param SweepWidth The constant sweep width of the project.
#' @param SweepWidthTable A table of two columns, \code{Cruise} and \code{SweepWidth}, giving the sweep width for each cruise.
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
    #SweptAreaMethod = c("LengthDependent", "TotalCatch"), 
    SweepWidthMethod = c("Constant", "PreDefined", "CruiseDependent"), 
    SweepWidth = integer(), 
    SweepWidthTable= data.table::data.table()#, 
    #CatchVariable = c("Weight", "Count")
) {
	
    ## Get the DefinitionMethod:
    #SweptAreaMethod <- match.arg(SweptAreaMethod)
    # Get the DefinitionMethod:
    SweepWidthMethod <- match.arg(SweepWidthMethod)
    
    ## This method may require defining an additional data type in StoX:
    #if(SweptAreaMethod == "TotalCatch") {
    #    stop("SweptAreaMethod = \"TotalCatch\" not yet implemented")
    #}
    
    
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
    if(LengthDistributionType %in% c("Normalized", "SelectivityCompensatedNormalized")) {
        
        # Use a constant sweep width for all data by default:
        if(SweepWidthMethod == "Constant") {
            if(length(SweepWidth) == 0) {
                stop("SweepWidth must be given when SweepWidthMethod == \"Constant\"")
            }
            
            # Convert WeightedCount to density:
            sweepWidthInNauticalMiles <- SweepWidth / 1852
            DensityData[, Density := WeightedCount / sweepWidthInNauticalMiles]
        }
        else if(SweepWidthMethod == "CruiseDependent") {
            stop("SweepWidthMethod = \"CruiseDependent\" is not yet implemented")
            if(length(SweepWidthTable) == 0) {
                stop("SweepWidthTable must be given when SweepWidthMethod == \"CruiseDependent\"")
            }
        }
        else {
            stop("SweepWidthMethod must be \"Constant\" or \"CruiseDependent\" if LengthDistributionType is \"Normalized\" or \"SelectivityCompensatedNormalized\" in the LengthDistributionData")
        }
        
    }
    # If LengthDistributionType == "SweepWidthCompensatedNormalized" the sweep width is already accounted for, in meters:
    else if(LengthDistributionType == "SweepWidthCompensatedNormalized") {
        if(SweepWidthMethod == "PreDefined") {
            # WeightedCount is already in area density when LengthDistributionType is SweepWidthCompensatedNormalized, which is generated by LengthDependentCatchCompensation():
            DensityData[, Density := WeightedCount]
        }
        else {
            stop("SweepWidthMethod must be \"PreDefined\" if LengthDistributionType is \"SweepWidthCompensatedNormalized\" in the LengthDistributionData")
        }
    }
    else {
        # In StoX 2.7 and earlier, LengthDistType = \"LengthDist\" was allowed, where the distance was multiplied with the weighted count in SweptAreaDensity.
        stop("Invalid LengthDistributionType.")
    }
    
    # Convert to density per nautical mile atwarthship:
    #DensityData[, Density := Density * lengthOfOneNauticalMile]
    
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

