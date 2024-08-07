% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data.R
\name{fractionOfOccurrence}
\alias{fractionOfOccurrence}
\title{The fraction of occurrence function}
\description{
The \code{ReportFunction} \code{fractionOfOccurrence} represents the fraction of occurrence for two different grouping variables, where the \code{GroupingVariables} is used in the numerator and the \code{GroupingVariables} except the \code{FractionOverVariable} is used in the denominator. The occurrence is defined as 1 if the \code{TargetVariable} meets the condition defined by \code{ConditionOperator} and \code{ConditionValue}.
}
\details{
E.g., if \code{GroupingVariables} is c("Survey", "SpeciesCategory", "Stratum"), \code{ractionOverVariable} is "Stratum", \code{TargetVariable} is "IndividualAge", \code{ConditionOperator} is ">" and \code{ConditionValue} is 0, the number of individuals with age larger than 0 for each stratum is divided by the total number of individuals of age large than 0 of the survey (for each SpeciesCategory).

Warning! This function is used only to construct an expression to be evaluated in a data.table. Do not use this in R as a regular function like the other \code{\link{ReportFunctions}}.
}
