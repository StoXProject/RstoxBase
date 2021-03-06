% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Report.R
\name{ReportSuperIndividuals}
\alias{ReportSuperIndividuals}
\title{Report SuperIndividualsData}
\usage{
ReportSuperIndividuals(
  SuperIndividualsData,
  TargetVariable,
  ReportFunction = getReportFunctions(getMultiple = FALSE),
  GroupingVariables = character(),
  RemoveMissingValues = FALSE,
  WeightingVariable = character()
)
}
\arguments{
\item{SuperIndividualsData}{The \code{\link{SuperIndividualsData}} data.}

\item{TargetVariable}{The variable to report.}

\item{ReportFunction}{The function to apply, see RstoxBase::getRstoxBaseDefinitions("reportFunctions")$functionName.}

\item{GroupingVariables}{The variables to report by, e.g. "Stratum" or "SpeciesCategory". Can be given as a comma separated list of variables (with no spaces).}

\item{RemoveMissingValues}{Logical: If TRUE, remove missing values (NAs). The default (FALSE) implies to report NA if at least one of the values used in the \code{ReportFunction} is NA.}

\item{WeightingVariable}{The variable to weight by. Only relevant for \code{ReportFunction} "weighted.mean".}
}
\value{
A \code{\link{ReportSuperIndividualsData}} object.
}
\description{
Reports the sum, mean or other functions on a variable of the \code{\link{SuperIndividualsData}}.
}
\details{
This function is useful to, e.g, sum Biomass for each SpeciesCategory and IndividualTotalLength, or average IndividualTotalLength for each IndiivdualAge and Stratum.
}
