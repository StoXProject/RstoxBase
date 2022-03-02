# RstoxBase v1.7.8 (2022-03-02)
* Fixed bug in DefinePSU when there are no PSUs.


# RstoxBase v1.7.6 (2022-03-01)
* Reduced time of reports.
* Added na.action = na.exclude to the regression functions applied by EstimateBioticRegression().
* Changed all all sd and cv in reports from 0 to NA. Standard deviation = 0 is no longer accepted by StoX, as it implies either insufficient number of bootstraps or only one value to sample from in the bootstrapping.
* Added error when EstimateBioticRegression() when using the power model and when there are 0 in the data, which result in -Inf in the log used in the power regression.
* Changed the warning when not all assigned hauls have length measured individuals to a warning when not all hauls of the stratum have length measured individuals, as we are bootstrapping within hauls and not within assignment.
* Added a warning for when only one assigned haul has length measured individuals.


# RstoxBase v1.7.5 (2022-02-25)
* Added error when the raising factor calculated from CatchFractionWeight/SampleWeight or CatchFractionNumber/SampleNumber is NA or Inf in one or more samples. This is an indication of error in the data that StoX can take no general approach to handle. The primary solution for this error is to correct the errors in the input data, or preferably in the database holding the data, so that other users may avoid the same error. There are possibilities for filtering out the hauls/samples with error in raising factor in StoX, but this requires COMPLETE KNOWLEDGE of what the different samples and hauls represent. Filtering out a sample with missing raising factor causes the length distribution to be given by the other samples, which may be special samples of e.g. only large fish, resulting in highly biased length distribution. Filtering out entire hauls is also problematic, as one may lose vital information in the data, say if the large catches have a particular problem with extra samples where the raising factor is not given. Another dangerous option in StoX is to translate e.g. CatchFractionWeight and SampleWeight to positive values at the exact knowledge of what the correct value should be. 
* Added possible values for SpeciesLink in AcousticDensity().
* Added notes on the difference between unit for Biomass in the data types QuantityData (kg) and SuperIndividualsData (g) in the documentation of the functions Quantity() and SuperIndividuals().


# RstoxBase v1.7.2 (2022-02-10)
* Added WeightingMethod = "NASC" in BioticAssignmentWeighting().


# RstoxBase v1.7.1 (2022-01-25)
* Fixed bug in drop-down list for DensityType in SweptAreaDensity() when SweptAreaDensityMethod == "LengthDistributed". To avoid error the user had to type in the value manually as ["AreaNumberDensity"]. Moved from testthat to tinytest.
* Added errors as StoX warning in getRegressionTable() to communicate the error. 
* Added drop-down list in parameters DependentVariable and IndependentVariable in EstimateRegression().


# RstoxBase v1.5.4 (2022-01-12)
* Renamed Abundance(), ReportAbundance(), AbundanceData and ReportAbundanceData to Quantity*, as this data now contains both Abundance and Biomass.


# RstoxBase v1.5.3 (2022-01-12)
* Changed all instances of the use of the phrase "count" to "number", in accordance with the terminology of ICESBiotic and the convension "area number density" ("area count density is very rare"). This change affects the following code:

    + In StoxBiotic():
	*SampleCount -> SampleNumber*
	*CatchFractionCount -> CatchFractionNumber*
	**This could affect external scripts that use the StoxBioticData.**
	
    + In LengthDistribution(), SumLengthDistribution(), MeanLengthDistribution(), AssignmentLengthDistribution(), RegroupLengthDistribution(), GearDependentCatchCompensation(), LengthDependentCatchCompensation(), RelativeLengthDistribution():
	*Renamed the column WeightedCount to WeightedNumber*
    **This could affect external scripts that use one of the listed datatypes as WeightedCount is no longer found. Other than that the WeightedCount does not exist further in the estimation models in StoX.**

    + In BioticAssignmentWeighting():
	*WeightingMethod = "NormalizedTotalCount"    -> "NormalizedTotalNumber"*
	*WeightingMethod = "SumWeightedCount"        -> "SumWeightedNumber"*
	*WeightingMethod = "InverseSumWeightedCount" -> "InverseSumWeightedNumber"*
	**Backward compatibility should take care of these**

    + LengthDistribution():
	*RaisingFactorPriority = "Count" -> "Number"*
    **Backward compatibility should take care of these**
* Added the function ReportAbundance.
* Added the parameter InformationVariables to reports.


# RstoxBase v1.5.2 (2022-01-10)
* Changed SpeciesCategoryCatch() to return a single table similar to LengthDistributionData, but with TotalCatchWeight and TotalCatchCount instead of WeightedCount. As such, moved the CatchVariable of SpeciesCategoryCatch() to the ReportVariable of ReportSpeciesCategoryCatch(). The latter is a backward compatibility breaking change. Any existing StoX project using SpeciesCategoryCatch() and ReportSpeciesCategoryCatch() will break in ReportSpeciesCategoryCatch(), and the ReportVariable needs to be set to the appropriate value in order to continue. 
* Removed the function input Translation from ReportSpeciesCategoryCatch(). This was used to remove the species categories that were not translated, but such filtering should rather take place in a proecss using FilterStoxBiotic().
* Added SumSpeciesCategoryCatch() and MeanSpeciesCategoryCatch().
* Added the parameter SweptAreaDensityType in SweptAreaDensity() supporting both "LengthDistributed" and "TotalCatch" swept-area density. 
* Added new column DensityType in DensityData with supported values "AreaNumberDensity" (the only option for AcousticDensity() and  SweptAreaDensityType "LengthDistributed") and  "AreaMassDensity".
* Added new column AbundanceType in AbundanceData with supported values "Number" (the only option for AcousticDensity() and  SweptAreaDensityType "LengthDistributed") and "Mass".

# RstoxBase v1.5.1 (2022-01-07)
* Added DefineRegression(), EstimateBioticRegression() and the implementation in ImputeSuperIndividuals(). Refactored so that DefineRegression() and DefineAcousticTargetStrength() both use the underlying DefineModel(), with outputs <Model>Model and <Model>Table. Renamed DefinitionMethod "TargetStrengthTable", "SurveyTable" and "LayerTable" to "Table".

# RstoxBase v1.4.27 (2021-12-12)
* Improved documentation of ImputeSuperIndividuals().
* Avoided unwanted warnings when raising factor is missing for hauls with no fish.

# RstoxBase v1.4.26 (2021-11-23)
* Changed how StoX treats missing values (NA) so that an NA will always propagate to the next step in the calculation (na.rm = FALSE). The only exception is missing values in bootstrap runs, which are treated as 0 to reflect the instances of e.g. a length group being left out in the data due to the random sampling of hauls, in which case the abundance of that length group is to be considered as 0. Previously, in MeanNASC(), MeanLengthDistribution() and MeanDensity(), where the mean is calculated as sum of the data divided by the sum of the weights (e.g. log distance or number of stations), missing values were ignored in the sums. In the new version missing values result in missing values in the mean. A consequence of preserving missing values can be that a missing value in a normalized LengthDistribution (e.g., due to missing sample weights) will propagate through to a missing value in the stratum, and ultimately to a missing value in the survey if summing over strata in a report. The problem must then be solved by either correcting what in the input data that is causing the missing values, or filter out those values (e.g. filter out erroneous or experimental hauls). For acoustic-trawl models there is an extra complication related to bootstrapping, since missing values here are treated as 0. I there are hauls assigned to an acoustic PSU that does not contain any length measured individuals of the target species, there is a positive probability that only these hauls are sampled in a bootstrap run, resulting in missing values in abundance of the stratum of that PSU. Treating this as 0 will lead to under-estimation. Only hauls length measured individuals of the target species should be used in the biotic assignment when bootstrapping is included in the StoX project.
* Added support for multiple beams in SplitNASC(), where the NASC is now distributed to the different species for each beam().
* Fixed bug in SplitNASC() so that NASC for a PSUs with all missing values in the AssignmentLengthDistributionData of a specific species are left un-splitted.
* Added removal of empty PSUs.
* Added the columns NumberOfAssignedHaulsWithCatch and NumberOfAssignedHauls to AsssignmentLengthDistributionData, used in AcousticDensity() to flag PSUs for which hauls with no length measured individuals of the target species are assigned.
* Removed unwanted columns in the output from AcousticDensity(), inherirted from MeanNASCData.
* Added support for Biomass = 0 when Abundance = 0, regardless of IndividualRoundWeight = NA.
* Reverted to using all = TRUE when merging AbnudanceData into Individuals in SuperIndividuals(), as using all.x = TRUE implies the risk of discarding a portion of the abundance.
* Changed error causing trouble in DistributeNASC() in splitOneAcousticCategory() to warning.
* Fixed bug causing empty PSUByTime from DefineAcousticPSU when DefinitionMethod = "Manual".

# RstoxBase v1.4.22 (2021-11-03)
* Refactored SplitNASC to support multiple EDSUs per PSU, and EDSUs outside of any statum.
* Added warning for when EffectiveTowDistance = 0 in Lengthdistribution() with LengthDistributionType = "Normalized".
* Updated processDataSchema.json.

# RstoxBase v1.4.18 (2021-10-21)
* Added the DefinitionMethod \"ResourceFile\" in DefineBioticPSU(), which enables reading BioiticPSU from a StoX 2.7 project.xml file.

# RstoxBase v1.4.17 (2021-10-11)
* Changed to merge SuperIndividualsData and LengthDistributionData with all.x = TRUE in SuperIndividuals(). The effect of this is that hauls that are discarded by the random sampling in a bootstrap run no longer result in a row in SuperIndividualsData with mostly NAs.

# RstoxBase v1.4.16 (2021-10-11)
* Changed SuperIndividuals() to not add rows from EDSUs with no assigned biotic hauls. In the previous version, rows were added with NA in all variables except those from the EDSU (frequency, etc.) if e.g. assignment method "Stratum" was used, and no hauls existed in a stratum with EDSUs.
* Changed aggregateBaselineDataOneTable() so that when used to produce reports using RstoxFramework::BootstraReport() NAs are not change to 0 except for NAs introduced between bootstrap runs (e.g., ages missing in some runs due to resampling of hauls).
* Fixed bug in the parameter formats of ImputeSuperIndividuals() when using SuperIndividualsData from another process using ImputeSuperIndividuals().

# RstoxBase v1.4.15 (2021-10-08)
* Fixed bug in SplitNASC().
* Final version for the release of StoX 3.2.0.

# RstoxBase v1.4.13 (2021-09-28)
* Added the function SplitNASC() intended to replaec SplitMeanNASC(). SplitNASC() uses NASCData and AcousticPSU as input and generates one PSU per EDSU for splitting the NASC based on BioticAssignment, and then returns a NASCData object. Consequently one can skip the MeanNASC() function in the model. Added method for simplifying stratum polygons in DefineStratumPolygon. Fixed bug when using depth TargetStrengthMethod = "LengthAndDepthDependent" .
* Sped up DefineAcousticPSU with DefinitionMethod "EDSUToPSU" by removing loop over EDSUs. Fixed bug when reading shapefiles or GeoJSON, by introducing the parameter StratumNameLabel in DefineStratumPolygon(). Added the option of simplifying stratum polygons by the new parameters SimplifyStratumPolygon and SimplificationFactor.
* Removed updating PSUByTime in DefineAcousticPSU() when UseProcessData = TRUE, as this destroys the information to be passed onto another process using DefineAcousticPSU() where the first process is used as input.
* Fixed bug where LengthDistribution produced a line of NA in IndividualTotalLength for subsamples that were completely empty by the filter, thus resulting in a small percentage of the WeightedCount assigned to this NA length in the percent length distribution, and consequently reducinng the WeightedCount of the valid lengths.
* Changed from [0, Inf] to [min, max] of channel depth when DefinitionMethod "WaterColumn" in DefineLayer(), and added [0, Inf] if [min, max] are NA in channel depth when DefinitionMethod "WaterColumn" in DefineLayer().
* Fixed bugs related to stratum names (using getStratumNames() consistently).
* Added shapefile and GeoJSON to the tests.

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
