% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LengthDistribution.R
\name{AssignmentLengthDistribution}
\alias{AssignmentLengthDistribution}
\title{Length distribution assigned to each cell of acoustic PSU and Layer}
\usage{
AssignmentLengthDistribution(LengthDistributionData, BioticAssignment)
}
\arguments{
\item{LengthDistributionData}{The \code{\link{LengthDistributionData}} data.}

\item{BioticAssignment}{The \code{\link{BioticAssignment}} process data.}
}
\value{
An object of StoX datatype \code{\link{AssignmentLengthDistributionData}}.
}
\description{
This funciton calculates weighted average of the length distribution of hauls assigned to each acoustic PSU and Layer. The weights are set by \code{\link{BioticAssignmentWeighting}}.
}
\details{
The purpose of the \emph{AssignmentLengthDistribution} function is to produces one total length distribution for each combination of assigned biotic stations with corresponding weighting variables. 

If the biotic station length distributions which shall make up the total length distribution is of type \emph{Standard} (length distribution as if the complete catch on deck was measured), this will give an \emph{implicit weighting by catch as well as towing distance}.

If the biotic station length distributions which shall make up the total length distribution is of type \emph{Normalized} (length distribution as if the complete catch on deck was measured and as if the towing distance had been 1 nautical mile), this will give an \emph{implicit weighting by catch}.

If the biotic station length distributions which shall make up the total length distribution is of type \emph{Percent} (sum of percentages for all length groups in the distribution is 100 and the shape of the distribution is the aim), this will give \emph{NO implicit weighting}.

One total length distribution is calculated as follows:

1)	For each biotic station a weighting factor for each station is calculated from the weight variables of the assigned stations:

\deqn{W_s = \frac{w_s}{\sum_{y=1}^{n} w_y}}

where

\eqn{W_s} 		= weighting factor for station \eqn{s}

\eqn{n}		    = number of trawl stations to be combined

\eqn{w_s}  		= the value of the weight variable for station \eqn{s}

\eqn{w_y} 		= the value of the weight variable for station \eqn{y}


2)	For each length distribution by biotic station (\eqn{d_s}), the number or percentage value in each length group is multiplied by \eqn{W_s}:

\deqn{dw_s = W_s \times d_s}

3)	The total length distribution \eqn{d_t} is finally calculated by adding the numbers in each length interval for all stations where \eqn{n} is the number of stations to be combined:

\deqn{d_t = \displaystyle\sum_{y=1}^{n} dw_y}

\eqn{d_t} is the total length distribution for one assignment.
}
\seealso{
\code{\link{LengthDistribution}} to produce the input LengthDistributionData, and \code{\link{DefineBioticAssignment}} to produce the input BioticAssignment.
}
