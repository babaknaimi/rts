\name{endpoints}

\alias{endpoints}
\alias{endpoints,RasterStackBrickTS-method}


\title{Locate endpoints by time}

\description{
Extract index values of a given \code{Raster*TS} object corresponding
to the \emph{last} observations given a period specified by \code{on}.
}

\usage{
endpoints(x, on="months", k=1)
}

\arguments{
  \item{x}{ a raster time series (\code{Raster*TS}) object created by \code{\link[rts]{rts}} }
  \item{on}{ the periods endpoints to find as a character string }
  \item{k}{ along every k-th element - see notes }
}
\details{
This function is, indeed, \code{\link[xts]{endpoints}} in \pkg{xts} that works with \code{Raster*TS} objects. It returns a numeric vector corresponding to the \emph{last}  observation in each period specified by \code{on}, with a zero added to the beginning of the vector, and the index of the last raster in \code{x} at the end.

Valid values for the argument \code{on} include: \dQuote{us} (microseconds),
\dQuote{microseconds}, \dQuote{ms} (milliseconds),
\dQuote{milliseconds}, \dQuote{secs} (seconds),
\dQuote{seconds}, \dQuote{mins} (minutes), \dQuote{minutes},
\dQuote{hours}, \dQuote{days}, \dQuote{weeks}, \dQuote{months}, \dQuote{quarters},
and \dQuote{years}.
}

\note{
Windows support for subsecond periods is not supported. 
}
\value{
A numeric vector of endpoints beginning with 0 and ending with a value equal to the number of raster layers in the x argument.
}

\author{Babak Naimi

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\seealso{ \code{\link[xts]{endpoints}}}

\examples{
\dontrun{
file <- system.file("external/ndvi", package="rts")

ndvi <- rts(file) # read the ndvi time series from the specified file

endpoints(ndvi,"years")

endpoints(ndvi,"quarters")
}

}

\keyword{ utilities }