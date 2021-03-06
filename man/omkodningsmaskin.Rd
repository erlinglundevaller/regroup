% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/omkodningsmaskin.R
\name{omkodningsmaskin}
\alias{omkodningsmaskin}
\title{omkodningsmaskin
regroups individuals/items over time based on region/group menbership}
\usage{
omkodningsmaskin(data, cutoff = 0.05, startar = NA, slutar = NA,
  djup = 1)
}
\arguments{
\item{data}{A data.frame with "pid", "time", "region"}

\item{cutoff}{how large proportion of pids to merge groups}

\item{startar}{time to start}

\item{slutar}{time to stop}

\item{djup}{for sloving nested knots, not implemented.}
}
\value{
lansdt A data.frame with columns: "New", "Old",  "Freq", "totold", "propold", "totnew", "propnew", "newcode"; a key table.
}
\description{
omkodningsmaskin
regroups individuals/items over time based on region/group menbership
}
\details{
regroups individuals/items over time based on region/group menbership
}
\author{
Erling Haggstrom Lundevaller
}

