# RstoxBase v1.11.4-9008  (2024-04-23)
* Added documentation of ReportFunctions.
* Added the exported getFunctionArgumentHierarchyForSpcificationParameters() used in stoxFunctionAttributes.
* Fixed report functions number(), fractionOfOccurrence() and fractionOfSum().
* Fixed bug where IndividualAge was not avaiable as TargetVariable in reports (due to class integer, which was not accounted for).


# RstoxBase v1.11.4-9007  (2024-04-19)
* Fixed bug in DefineBioticAssignment() when DefinitionMethod = "Stratum".
* Moved warnings for only one PSU to Bootstrap in RstoxFramework.
* Added report functions number(), fractionOfOccurrence() and fractionOfSum().
* Refactored report functions to use a general Specification list for arguments like WeightingVariable.


# RstoxBase v1.11.4-9006  (2023-12-19)
* Added the new function AddHaulDensityToSuperIndividuals() which adds the density of each Haul estimated from the LengthDistribution input.
* Removed the columns AllHaulsHaveAllSpeciesCategory and AllHaulsHaveAnySpeciesCategory from AssignmentLengthDistribution(), which were only used to produce a warning about missing length distribution in AcousticDensity and SplitNASC. This warning has been changed.
* Fixed the warning of duplicated entries in the Individual column in ImputeSuperIndividuals_StoX3().


# RstoxBase v1.11.4-9004  (2023-12-08)
* Added the new function AddHaulDensityToSuperIndividuals() for adding density of the haul of each super-individual.
* Changed sorting of StratumLayerIndividual when creating the StratumLayerIndividualIndex to platform independent locale = "en_US_POSIX". This is actually a bug, but has not been discovered since all known StoX projects have been using input data with Cruise as numbers of only upper case letters (sorting by locale = "en_US_POSIX" arranges capital letters first (India before england)).
* Fixed bug in ImputeSuperIndividuals() when ImputationMethod = "RandomSampling", where individuals used in multiple strata were not uniquely represented by the Individual column. The consequence was that data were imputed from he first of the rows with identical Individual, resulting in possible loss of imputation, e.g. if imputing IndividualAge and only one of the identical individuals had IndividualAge but not the first of them, in which case that age would never be imputed. This was fixed by introducing a new unique ID StratumLayerIndividual, which is a concatenation of Stratum, Layer and Individual, and using this to idenify row o impue from.
* Renamed ImputeSuperIndividuals() to ImputeSuperIndividuals_StoX3() and kept this for backward compatibility.
* Implemented the "single" format class of parameters like DensityType and TargetVariableUnit.


# RstoxBase v1.11.4-9003  (2023-11-07)
* Added support for reading a project.json in DefineSurvey(), DefineAcousticPSU(), DefineBioticPSU(), DefineBioticAssignment() and DefineStratumPolygon.
* Restricted warning for missing or 0 EffectiveTowDistance to only activate when there are there are more than 0 individuals in the Haul. 
* Fixed bug in DefineSurvey with DefinitionMethod = "ResourceFile", where the FileName was the path to a project.xml file.


# RstoxBase v1.11.4-9002  (2023-10-18)
* Added the option ImputationLevels to ImputeSuperIndividuals(), which allows for restricting imputation by random sampling to e.g. only the same haul and stratum (and not survey). 
* Fixed bug in ImputeSuperIndividuals() where more than one row resulted in only the last row being used.
* Fixed bug in readStoxMultipolygonWKTFromFile(), where the columns of data read from wkt files were named Stratum instead of StratumName, causing an error when reading wkt files.


# RstoxBase v1.11.4-9001  (2023-08-30)
* Removed dependency on the retiring package sp.
* Changed GearDependentCatchCompensation() to keep all variables from the input SpeciesCategoryCatchData.
* Improved error message then Percentages is not given (now saying exactly that and not "SpecificationParameter must be given").
* Removed exported functions getStratumPolygonList and readGeoJSON.
* Improved documentation of DefinitionMethod in DefineBioticPSU(), DefineAcousticPSU() and DefineBioticAssignment().
* Added a warning when reading BioticPSUs from a StoX 2.7 project.xml file where Method is Station and not UseProcessData in DefineSweptAreaPSU(), which makes the BioticPSUs of the project.xml file unused.
* Improved warning when there are Individuals in the IndividualsData with IndividualTotalLength that does not match any of the length intervals of the QuantityData.
* Improved warning for when there are positive NASC values with no assignment length distribution, also removing the list of the  affected PSUs.
* Improved simplifyStratumPolygon() used in DefineStratumPolygon() which got stuck in an endless loop in certain cases.
* Exposing PlotAcousticTrawlSurvey().
* Fixed bug in LengthDistribution() where missing raising factor was reported for samples with no individuals.


# RstoxBase v1.11.3  (2023-06-27)
* Version for StoX 3.6.2.
* Changed defined unit of Biomass in QuantityData from g to kg, as Biomass originates from SweptAreaMethod "TotalCatch" which is in kg.
* Added tables listing the variables of the data types NASCData, SumNASCData, MeanNASCData, LengthDistributionData, SumLengthDistributionData, MeanLengthDistributionData, DensityData, MeanDensityData and QuantityData.  
* Relaxed testUnits.R to accept that Biomass has different units in QuantityData and SuperIndividualsData.
* Increased speed of frequently used functions in RstoxFramework by using list instead of data.table for the definition og reportFunctions.
* Skipped gc() in aggregateBaselineDataOneTable() to reduce CPU time.
* Removed SpeciesCategoryKey from ReportSpeciesCategoryCatch(), as the output is per haul.


# RstoxBase 1.11.2 (2023-04-30)
* Fixed bug in EstimateRegression when insufficient data to estimate the regression. Now returning NA for all parameters.


# RstoxBase 1.11.1 (2023-04-18)
* Fixed bug in SuperIndividuals() where bootstrapping could result in artificial rows with missing Abundance for certain length grups only present in biotic PSUs that are not resampled in a bootstrap replicate. This was only a problem when DistributionMethod = "HaulDensity". The result was that when not using IndividualTotalLength in the GroupingVariables in ReportBootstrap(), many rows contained NAs. In the new version the NAs can be isolated by including "Survey" and "SpeciesCategory" in the GroupingVariables.
* Fixed bug in SuperIndividuals() where the test for equal total Abundance from the QuantityData and in the SuperIndividualsData failed when both were 0. This could be a problem if there were no NASC or no catch of the target species in a stratum for acoustic-trawl or swept-area models, respectively.
* Fixed bug in SuperIndividuals() for when all lengths of the QuantityData are NA.
* Fixed slow aggregateBaselineDataOneTable() used by e.g. RstoxFramework::ReportBootstrap() by removing repeated unnecessary call to getReportFunctions(getMultiple = TRUE) to get the column names of the output.
* Added test for non-empty AcousticPSU in BioticAssignment().
* Added support for starting out with no PSUs in DefineAcousticPSU.
* Now reporting a warning if the user tries to set unit to a variable that has no units defined in ReportSuperIndividuals().
* Improved warning when EstimateBioticRegression() returns NA.
* Fixed bug where only the varaibles from the Individual table of StoxBioticData were available as GroupingVariables in EstimateBioticRegression() in the GUI.
* Added support for more than one row in the Regression input to ImputeSuperIndividuals.
* Corrected the documentation of RegroupLengthDistribution().


# RstoxBase 1.12.0-9001 (2023-04-14)
* Improved warning when EstimateBioticRegression() returns NA.
* Fixed bug where only the varaibles from the Individual table of StoxBioticData were available as GroupingVariables in EstimateBioticRegression() in the GUI.
* Added support for more than one row in the Regression input to ImputeSuperIndividuals.
* Corrected the documentation of RegroupLengthDistribution().


# RstoxBase v1.11.0 (2023-01-13)
* Added warning when Percentages are outside of [0, 100] in ReportBootstrap when BootstrapReportFunction = "summaryStox".


# RstoxBase v1.11.0-9005 (2023-01-06)
* Added the parameter percentages defaulted to c(0.05, 0.5, 0.95) in summaryStox().
* Changed defaultPlotGeneralOptions AxisTitleSize (10 to 12) and LegendTitleSize (10 to 12).
* Changed defaultPlotFileOptions Height (17 to 10).


# RstoxBase v1.11.0-9004 (2022-12-23)
* Fixed bug in SuperIndividuals, where the temporary column LengthGroup is generated but might already be present if the user has created a LengthGroup with the new Copy functionality.
* Cleaned up documentation of ModelData.


# RstoxBase v1.11.0-9003 (2022-12-13)
* Improved the documentation EstimateBioticRegression().
* Improved warning when acoustic PSUs are not present in the BioticAssignment processData or have no assigned biotic Hauls.


# RstoxBase v1.11.0-9002 (2022-12-02)
* Changed the check-full.yaml to download and install binary pre-releases on macOS and Windows.


# RstoxBase v1.11.0-9001 (2022-12-01)
* Changed the check-full.yaml to install pre-releases from the package repo (as pre-releases are no longer deployed to the drat repo "https://github.com/StoXProject/repo").


# RstoxBase v1.10.6 (2022-11-30)
* Removed rows of the output from ReportBootstrap() that contained combinations of the GroupingVariables that are not present in the BootstrapData. There rows were created to ensure that all bootstrap runs contain all combinations of the GroupingVariables, but also introduced non-existing combinations.
* Fixed bug in DefineSurvey() when reading from a table text file, which was attempted read as a project.xml file.
* Improved warning when using RemoveMissingValues. This warning now informs the user that GruopingVariables can be useful to isolate missing values out from the relevant rows of the report.
* Fixed error in the documentation of GroupingVariables.
* Fixed bug in pkgdown.yaml.
* Added support for pre-releases, which are not deployed to the StoX repo.


# RstoxBase v1.10.5 (2022-11-22)
* Fixed bug in DefineSurvey(), where reading a table from a text file (e.g. csv) caused a crash.


# RstoxBase v1.10.4 (2022-11-21)
* Improved warning for PSU not present in BioticAssignment.


# RstoxBase v1.10.3 (2022-11-21)
* Added the parameter TargetVariableUnit in ReportSuperIndividuals() and ReportQuantity(), DensityUnit in ReportDensity(), and ReportVariableUnit in ReportSpeciesCategoryCatch(), which all acn be used to set the units for the report.


# RstoxBase v1.10.2 (2022-10-31)
* Fixed possible bug when using a Regression process data where GroupingVariables were set. The indices of the rows of the SuperIndividualsData to be imputed were previously identified before merging in the RegressionTable of the Regression process data. This could possibly change the order so that indices were incorrect when the actual imputation by regression was made. In the new version the indices are identified as the last step before the imputation.


# RstoxBase v1.10.1-9001 (2022-10-31)
* Pre-release before 1.10.1. Errors are expected.
* Added (unfinished) PlotAcousticTrawlSurvey().
* Applied RstoxData::match_arg_informative() to improve warning messages.
* Added error if variables specified in Regression in ImputeSuperIndividuals() are not present in the data (preivously this was only a warning).
* Added error if a LayerTable specified by the user contains missing values.
* Fixed bug where "Linear" was used instead of "SimpleLinear" as EstimationMethod in EstimateBioticRegression().
* Also fixed bug where the EstimationMethod "SimpleLinear" did not work as expected. 
* Moved printErrorIDs() to RstoxData as an exported function.
* Changed behavior of DefinitionMethod "WaterColumn" so that even data with missing depth information will have Layer = "WaterColumn". Before, if MinHaulDepth, MaxHaulDepth, MinChannelDepth or MaxHChannelDepth was missing, Layer would also be missing. 
* Also, changed the MinLayerDepth and MaxLayerDepth from the range of the depths (set to 0 and Inf if min depth and max depth was misssing) to (0, NA), saying that "WaterColumn" means from surface to an unknown bottom, or at least not defined by a single value.
* Added variable selection dialogue for GroupingVariables in DefineRegression() (typing, as there is no list of possible values).
* Added a warning if the variables selected using GroupingVariables and RegressionModel have changed in DefineRegression(), making the RegressionTable not work properly in the current version of the GUI.
* Fixed possible values for AcousticCategory in SpeciesLink in SplitNASC(), from the available AcousticCategory in the NASCData to the SplitAcousticCategory in the AcousticCategoryLink. Also reordered the parameters so that AcousticCategoryLink comes before SpeciesLink.
* Changed to not remove rows with missing Haul in DefineBioticAssignment(). This was introduced when DefinitionMethod == "Stratum" for unknown reasons. The warning when all Hauls are missing is kept.
* Removed error when there are Individuals with IndividualTotalLength smaller than the smallest IndividualTotalLength in the QuantityData in SuperIndividuals(). This was changed to warnings when IndividualTotalLength does not fit into any of the length intervals of the QuantityData.
* Added warning when there are AcousticCategory present in the NASCData but not in the SpeciesLink in AcousticDensity.


# RstoxBase v1.10.0 (2022-08-12)
* Removed from the GUI the warning for EDSUs/Stations detected in more than one stratum. 
* Added warning is all EDSUs included in the AcousticPSU proecss data are missing in the StoxAcousticData, which is an indication of new data in an old project where AcousticPSUs should be re-defined from scratch. 
* Start of using semantic versioning (https://semver.org/). Before this release the two first version numbers represented the major and minor release number, in accordance with semantic versioning, whereas the third version number identified test versions. The major and minor releases (versions ending with 0.0 or 0) were considered as official versions. From this release and onwards, the third version number will represent patches (bug fixes), and are to be considered equally official as the major and minor releases. In fact, as patches are restricted to fixing bugs and not adding new functionality, the latest patch will be the recommended version."


# RstoxBase v1.9.8 (2022-08-10)
* Fixed bug in applyMeanToData() introduced in RstoxBase 1.9.8.


# RstoxBase v1.9.7 (2022-08-10)
* Improved warning when EDSUs/Stations are tagged to a PSU but not present in the data. 
* Turned off spherical geometry with apply_and_set_use_s2_to_FALSE() when locating EDSUs/Stations in Strata. 
* Added warning when no assigned hauls are located in any Stratum of the PSUs. 
* Cleaned up warnings that list up Hauls, PSUs etc, so that alle use printErrorIDs(), which was simplified.


# RstoxBase v1.9.6 (2022-08-08)
* Fixed bug in LengthDistribution() with SampleWeight or SampleCount = 0, where haulsWithInfRaisingFactor and samplesWithInfRaisingFactor were missing on line 208.
* Changed to not split NSAC = 0. 
* Changed SplitNASC to remove rows with NA NASC originating from missing assignment length distribution. 
* Changed to consider species to be split that are not present in the AssignmentLengthDistribution of a NASC value to be split as WeightedNumber = 0 instead of NA. This prevents NA NASC, and doubles the previous change.


# RstoxBase v1.9.5 (2022-06-20)
* Fixed bug in DefineBioticAssignment() where DefinitionMethod "Stratum" failed due to unset attribute "pointLabel".
* Added stop when project.xml file path is not set in a DefinitionMethod "ResourceFile"
* Disabled the EstimationMethod "NonLinear" in the drop-down menu in the RegressionTable of function Regression() in the GUI.
* Changed JSON schema so that all table columns of type "string" allow also type "null", supporting NAs.


# RstoxBase v1.9.4 (2022-06-15)
* Added DependentResolutionVariable and IndependentResolutionVariable in the RegressionTable of DefineRegression() and as parameters in EstimateBioticRegression(), used for adding half the resolution of e.g. length intervals.
* Reverted to identify all AcousticPSUs that have any missing assignment, as the proposed solution did not work.


# RstoxBase v1.9.2 (2022-05-26)
* Added warning occurring when there are samples with positive SampleNumber but no individuals, resulting in positive Abundance in the SuperIndividuals function to be set to NA.
* Replaced all use of functions from the packages rgdal and rgeos by the package sf, as per the planned retirement of these packages. See https://www.r-bloggers.com/2022/04/r-spatial-evolution-retirement-of-rgdal-rgeos-and-maptools/. 
* Refactored location of stations and EDSUs in stratum, and added warnings when locating to multiple or zero stratum.
* Tested and saved for future reference the function StratumArea_supportingIterativeCentroidCalculation which sets the centroid more accurately when transforming to Cartesian coordinates for the area calcuclation.


# RstoxBase v1.9.1 (2022-05-12)
* Fixed bug in AssignmentLengthDistribution(), where the sum of the WeightedNumber did not sum to 100. This did not have any implications on the estimates, as AcousticDensity() normalizes the WeightedNumber from the AssignmentLengthDistributionData.
* Fixed bug in ReportSpeciesCategoryCatch(), where Hauls were duplicated.


# RstoxBase v1.9.0 (2022-04-01)
* Eased warning for single unique value in bootstrap report to when the mean is 0.
* Bypassed warning for manual BioticPSU not yet being implemented when UseProcessData = TRUE.


# RstoxBase v1.8.0 (2022-03-31)
* Final version for StoX 3.4.0.


# RstoxBase v1.7.13 (2022-03-30)
* Changed WeightingMethod "AcousticDensity" to only consider EDSUs that are tagged to an acoustic PSU.


# RstoxBase v1.7.12 (2022-03-29)
* Fixed bug in BioticAssignmentWeighting, WeightingMethod "SumWeightedNumber", "InverseSumWeightedNumber" and "NASC", where weights were overwritten instead of multiplied, with the consequence that the randomness introduced by the bootstrapping of hauls in acoustic-trawl models when the DefineBioticAssignment process is used in BootstrapMethodTable, and with any of these WeightingMethod, will be overwritten by the BioticAssignmentWeighting process, thus cancelling the randomness for hauls (randomness will still be present for the EDSUs).
* Translated WeightingMethod "NASC" to "AcousticDensity" to reflect that AcousticDensity is calcuclated per EDSU around each Haul. Changed the description of the BioticAssignmentWeighting() to reflect that this happens rather than averaging NASC first and then calculating AcousticDensity. Fixed errors in BioticAssignmentWeighting() when WeightingMethod = "AcousticDensity".
* Added error when weigths do not sum to 1 in SuperIndividuals, with a not indicating that this may be due to different input LengthDistributionData compared to that used to  derive the input QuantityData.


# RstoxBase v1.7.11 (2022-03-22)
* Fixed critical bug in acoustic-trawl projects for SuperIndividuals when DistributionMethod = "HaulDensity" and Hauls are assigned to PSUs in more than one stratum, which led to under-estimation, as the number of individuals to distribute the Abundance to was counted over all strata per Haul ID, whereas only inside the stratum was correct.


# RstoxBase v1.7.10 (2022-03-22)
* Changed warning when there are assigned hauls with no length distribution. Now there is a warning in AcousticDensity() if there are hauls in a stratum for which not ALL target species have length distribution. In SplitNASC() the warning is when not ANY target species have length distribution, as we only need one length distribution to distribute NASC between species.
* Changed the warning "that have assigned ONLY ONE haul with length measured individuals" to "that have assigned ONLY ONE haul", since this warning concerns bootstrapping only one Haul, regardless of whether the Haul contains length distribution or not.
* Added the MinNumberOfEDSUs parameter to BioticAssignmentWeighting() when WeightingMethod == "NASC".
* Fixed bug in possible values for speciesLinkTable in  SplitNASC().
* Fixed bug when checking for only one PSU in a stratum.


# RstoxBase v1.7.9 (2022-03-10)
* Fixed bug in JSON schema for AcousticLayer and BioticLayer.
* Added removal of empty string PSUs, originating form a bug in RstoxFramework::removeEDSU().


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
* Added stop if not all EDSUs are inside a stratum in SplitMeanNASC()Fixed bug where stations with 0 fish were removed in LengthDistribution()
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
