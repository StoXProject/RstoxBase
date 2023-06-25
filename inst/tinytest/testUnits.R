# Get all datatype units:
dataTypeUnits_all <- rbind(
    RstoxData::getRstoxDataDefinitions("dataTypeUnits"), 
    RstoxBase::getRstoxBaseDefinitions("dataTypeUnits")
)

# Check whether any equal variable names have different units:
dataTypeUnits_all_splitted <- split(dataTypeUnits_all, by = "variableName")
differingUnit <- sapply(dataTypeUnits_all_splitted, function(x) NROW(unique(subset(x, select = c("variableName", "quantity", "unit"))))) > 1


differing <- dataTypeUnits_all_splitted[differingUnit]
# Allow for Biomass to be defined in units og g in SuperIndividualsData and kg in Quantity when the input MeanDensityData originates from SweptAreaDensity with SweptAreaDensityMethod = "TotalCatch":
differing <- subset(differing, ! names(differing) %in% c("Biomass"))
if(length(differing)) {
    differing <- data.table::rbindlist(differing)
    differing <- do.call(paste, c(differing, list(sep = ",")))
    warning("There are variables across data types that have the same name but different unit. Variables should be uniquely defined. The following datatype,variable,quantity,unit differ in units:\n", RstoxData::printErrorIDs(differing))
}


expect_equal(length(differing), 0)