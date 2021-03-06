% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanNZGxpEA.R
\name{cleanNZGxpEA}
\alias{cleanNZGxpEA}
\title{\code{cleanNZGxpEA} cleans up the raw GXP grid export file from the NZ EA data website}
\usage{
cleanNZGxpEA(dt)
}
\arguments{
\item{dt}{the data.table to clean up}
}
\description{
These are not in pretty form so we clean them up to a long form file and fix the dateTimes.
}
\details{
Note that rDateTime will be NA for the DST breaks which equate to TP49/50. We really dislike DST breaks.
}
\seealso{
Other data: 
\code{\link{alignDates}()},
\code{\link{cleanNZEmbEA}()},
\code{\link{cleanNZGridEA}()},
\code{\link{cleanUkEmbeddedESO}()},
\code{\link{cleanUkGridESO}()},
\code{\link{getNZEmbData}()},
\code{\link{getNZEmbMeta}()},
\code{\link{getNZGridEA}()},
\code{\link{getNZGridMeta}()},
\code{\link{getNZGxpEA}()},
\code{\link{getNZGxpMeta}()},
\code{\link{getUkEmbeddedESO}()},
\code{\link{getUkGridESO}()},
\code{\link{loadNZEAYearlyEmbeddedGenData}()},
\code{\link{loadNZEAYearlyGridGenData}()},
\code{\link{loadUKESOYearlyGenData}()},
\code{\link{loadUKEmbeddedGenData}()},
\code{\link{makeNZYearlyData}()},
\code{\link{nzCalculateCO2e}()},
\code{\link{reshapeEmbeddedGenDT}()},
\code{\link{reshapeGenDT}()},
\code{\link{saveUkEmbeddedESO}()},
\code{\link{saveUkGridESO}()},
\code{\link{setEmbeddedGenTimePeriod}()},
\code{\link{setGridGenTimePeriod}()}

Other grid: 
\code{\link{cleanNZGridEA}()},
\code{\link{cleanUkGridESO}()},
\code{\link{getNZEmbData}()},
\code{\link{getNZGridEA}()},
\code{\link{getNZGridMeta}()},
\code{\link{getNZGxpEA}()},
\code{\link{getNZGxpMeta}()},
\code{\link{makeNZYearlyData}()},
\code{\link{saveUkGridESO}()}

Other NZ: 
\code{\link{cleanNZEmbEA}()},
\code{\link{cleanNZGridEA}()},
\code{\link{getNZEmbData}()},
\code{\link{getNZEmbMeta}()},
\code{\link{getNZGridEA}()},
\code{\link{getNZGridMeta}()},
\code{\link{getNZGxpEA}()},
\code{\link{getNZGxpMeta}()},
\code{\link{makeNZYearlyData}()}
}
\author{
Ben Anderson, \email{b.anderson@soton.ac.uk} (original)
}
\concept{NZ}
\concept{data}
\concept{grid}
