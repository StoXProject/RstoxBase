# RstoxBase v1.4.4 (2021-08-18)
* Added more informative error when more than one SpeciesCategory in BioticAssignmentWeighting().
* Fixed bug in SuperIndividuals() where length measured individuals were counted over all beams, whereas per beam was correct. Added DefinitionMethod "ResourceFile" in DefineSurvey(), DefineAcousticPSU() and DefineBioticAssignment(), and support for a StoX 2.7 project.xml file in DefineStratumPolygon().*
* Fixed bug with CompensationTable in GearDependentCatchCompensation.

# RstoxBase v1.3.32 (2021-06-16)
* Added error if the variable specified by ImputeAtMissing is not explicitely inclcuded in ToImpute
* Fixed bug in DistributionMethod == "HaulDensity", where number of individuals in each length group was not taken into account.
* Fixed bug in SplitMeanNASC(), where now acoustic categories not to be split are added to the AcousticCategoryLink only if of positive length.
* Fixed warning 'the condition has length > 1 and only the first element will be used'
* Fixed bug with multiple species in catchCompensationTable, selectivityTable and gearCompensationTable parameter formats.
* Added stop if not all EDSUs are inside a stratum in SplitMeanNASC()Fixed bug where stations with 0 fish were removed in LenghtDistribution()
* Fixed several bugs related to DensityData and onwards being flexible datatypes which may or may not contain Beam and Frequency
* Added parameters to ImputeSuperIndividuals().
* Fixed column order for datatypes *IndividualsData
* Fixed bug ini SuperIndividuals, where number of individuals was counted over all Beams while it should be counted for each Beam.

# RstoxBase v1.3.12 (2021-05-04)
* Changed to only use keys as secondaryColumnOrder in Individuals, speeding up the function.
* Solved bug where estimates were reduced when including several Beams in acoustic-trawl models (added groupingVariables_biotic and groupingVariables_acoustic).

# RstoxBase v1.3.0 (2021-02-11)
* Final version for the release of StoX 3.0.0.

# RstoxBase v1.2.49 (2021-02-08)
* Cleaned up ImputeSuperIndividuals().
* Introduced calculation of centroid of each polygon instead of using labpt.
* Renamed *TowedDistance to *TowDistance. 
* Fixed bug in SuperIndividuals with DistributionMethod = 'HaulDensity'.

# RstoxBase v1.2.44 (2021-01-21)
* Added re-processing PSUByTime even UseProcessData is TRUE in DefineAcousticPSU().
* Renamed TargetStrengthDefinition to TargetStrengthTable.

# RstoxBase v1.2.42 (2021-01-14)
Added projection to StratumPolygon and EDSU points.

# RstoxBase v1.2.41 (2021-01-13)
* Updated documentation for several functions.

# RstoxBase v1.2.39 (2021-01-09)
* Removed dependency of rgdal in StratumArea().

# RstoxBase v1.2.36 (2020-12-21)
* Removed rgdal and replaced geojsonio with geojsonsf.

# RstoxBase v1.2.35 (2020-12-18)
* Added the option DefinitionMethod = 'Manual' in DefineAcousticPSU().
* Renamed ChannelReferenceOrientation to ChannelReferenceTilt.

# RstoxBase v1.2.34 (2020-11-03)
* Preserving all hauls in BioticAssignmentWeighting().

# RstoxBase v1.2.33 (2020-10-08)
* Re-adding Layer to BioticAssignment even if UseProcessData = TRUE.
* Common data type for SuperIndividuals and ImputeSuperIndividuals.

# RstoxBase v1.2.31 (2020-09-09)
* Added LayerDefinition and DefinitionMethod = DeleteAllAssignments to DefineBioticAssignment().

# RstoxBase v1.2.28 (2020-09-07)
* Added ReportBootstrap().

# RstoxBase v1.2.25 (2020-08-28)
* Removed unit in variable and parameter names.

# RstoxBase v1.2.22 (2020-08-14)
* Added ImputeSuperIndividuals().

# RstoxBase v1.2.20 (2020-07-11)
* Introduced Sum* and Mean* data types.
* Renamed SweptAreaPSU and SweptAreaLayer to BioticPSU and BioticLayer.

# RstoxBase v1.2.15 (2019-10-16)
* Created the package, which will be a library for StoX functions used for survey estimation, including defining resolution and calculating density, abundance and super individual data.
