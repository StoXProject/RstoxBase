# Read the test data:
testFile <- system.file("testresources", "biotic_cruiseNumber_2004703_small.xml", package = "RstoxBase")
b <- RstoxData::ReadBiotic(testFile)
s <- RstoxData::StoxBiotic(b)
#library(data.table)
#Translation <- data.table::data.table(
#    VariableName = "SpeciesCategory", 
#    Value = "torsk/164712/126436/NA", 
#    NewValue = "COD"
#)
#s <- RstoxData::TranslateStoxBiotic(s, Translation = Translation)
ca <- RstoxBase::SpeciesCategoryCatch(s)
#r <- RstoxBase::ReportSpeciesCategoryCatch(ca, Translation = Translation)
#expect_equal(r$COD, c(6, 94), check.attributes = F)

# In StoX 3.0.0 - 3.2.0 delprøver were doubled...
expect_equal(ca$TotalCatchNumber, c(6, 94), check.attributes = F)
