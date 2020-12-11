
context("test-DefineStratumPolygon")
stratumFile <- system.file("testresources", "strata_sandeel_2020_firstCoverage.wkt", package="RstoxBase")

stratumPolygon <- DefineStratumPolygon(
    DefinitionMethod = "ResourceFile", 
    FileName = stratumFile)

stratumNames <- RstoxBase::getStratumNames(stratumPolygon)

expect_equal(
    stratumNames, 
    c("AlbjoernLing", "Engelsk_Klondyke_2020", "Inner_Shoal_East_2016", "Inner_Shoal_North_2020", "Inner_Shoal_West_2018", "Nordgyden", "Ostbanken_2020", "Outer_Shoal_2020_1", "Vestbanken_North_2020", "VestbankenSouthEast", "VestbankenSouthWest", "Vikingbanken")
)
expect_equal(
    class(stratumPolygon)[1], 
    "SpatialPolygonsDataFrame"
)
