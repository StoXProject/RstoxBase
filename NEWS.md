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
