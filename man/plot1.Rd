% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot1.R
\name{plot1}
\alias{plot1}
\title{Make a plot of positives, active and recovered versus time}
\usage{
plot1(data = c("states", "world", "italy"), region, lockdown = NULL)
}
\arguments{
\item{data}{Name of a data set. "states", "world" or "italy"}

\item{region}{Name of a region in the region (location) column of the dataset (dataset$region). Might be country, state, or province. Use regions() to see what is available.}

\item{lockdown}{(optional) Date of the lockdown in "2020-03-11" format.}
}
\description{
Makes a plot of positives, active and recovered versus time. New cases per day is added as a line. 
If lockdown date is given, a line is added showing that. Plot is shown for a specific region (state, country, province, etc). Use regions() to see what locations are available. See ?getdatajhu, ?getdataitaly, ?getdatastates to see the data sources.
}
\examples{
plot1(data="italy", region="Lombardia", lockdown="2020-03-11")
plot1(data="states", region="NY", lockdown="2020-03-21")
}
