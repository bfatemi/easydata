% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classmorph.R
\name{ClassMorph}
\alias{ClassMorph}
\alias{NumMorph}
\alias{pcc}
\title{Functions to Change Column Classes}
\usage{
ClassMorph(DT, old = c("factor", "integer", "character", "numeric", "logical",
  "Date"), new = c("factor", "integer", "character", "numeric"),
  copy = FALSE, force = FALSE)

NumMorph(DT, cols = NULL, copy = FALSE, verbose = FALSE)

pcc(DT, ord = NULL, bret = FALSE)
}
\arguments{
\item{DT}{A data.table to use for any "class" operations}

\item{old}{ClassMorph only. A column type, specified as a character value, to
detect and convert to the class specified by 'new'}

\item{new}{ClassMorph only. A column type, specified as a character value, to
convert all columns of the class specified by 'old' to.}

\item{copy}{A boolean value indicating whether to alter \code{DT} in memory
or whether to return a new (copy) of the input data.table.}

\item{force}{A boolean indicating whether to force conversion from class
factor to class numeric despite NAs being generated.}

\item{cols}{An optional character vector of column names to operate on. Not
applicable for function ClassMorph.}

\item{verbose}{A boolean indicating whether to be chatty}

\item{ord}{For convenience, an optional char vec indicating how to sort rows. Can be one 
of: "CName", "Class", or "Pos" (position)}

\item{bret}{A boolean indicating whether to return the data.table (rather than print by default)}
}
\value{
Returns a data.table that is either a copy of the input DT that has
  been modified, or the same input DT that has been modified in memory
}
\description{
The functions \code{ClassMorph}, and \code{NumMorph}, in addition to
\code{DateMorph}, are functions designed to make dealing with column classes
easy. See examples for how to avoid uncessary frustrations related to
unexpected column classes or date formats.
}
\details{
This function takes care of multiple conversion steps that are sometimes
needed to avoid data loss or unexpected results (e.g. a factor that "looks"
like the number "1" may not convert to a \code{numeric} "1". To get around
this common frustration, this function will perform an indirect conversion to
\code{character} before converting to \code{numeric})
}
\section{Functions}{
\itemize{
\item \code{ClassMorph}: A function to convert all columns of class "old" to class "new". This
function handles the indirect conversions sometimes needed to avoid data loss or
unexpected results (see details).

\item \code{NumMorph}: A function to standards classes to "numeric" for all columns of
DT, where a conversion to numeric would not generate NA values.

\item \code{pcc}: prints (default) or returns a data.table describing column classes
}}

\examples{
pcc(iris)                       # print sorted by ord (default)
pcc(iris, "Class")              # print sorted by "Class"
pcc(iris, "CName", bret = TRUE) # sort and return data.table

}
