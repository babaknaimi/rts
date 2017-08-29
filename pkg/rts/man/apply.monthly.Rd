\name{apply.monthly}
\alias{apply.daily}
\alias{apply.weekly}
\alias{apply.monthly}
\alias{apply.quarterly}
\alias{apply.yearly}

\alias{apply.daily,RasterStackBrickTS-method}
\alias{apply.weekly,RasterStackBrickTS-method}
\alias{apply.monthly,RasterStackBrickTS-method}
\alias{apply.quarterly,RasterStackBrickTS-method}
\alias{apply.yearly,RasterStackBrickTS-method}


\title{Apply a function over calendar periods}

\description{
Apply a specified function to each distinct period in a given raster time series object.
}

\usage{
apply.daily(x, FUN, ...)
apply.weekly(x, FUN, ...)
apply.monthly(x, FUN, ...)
apply.quarterly(x, FUN, ...)
apply.yearly(x, FUN, ...)
}

\arguments{
  \item{x}{ a raster time series (\code{Raster*TS}) object, created by \code{\link[rts]{rts}} }
  \item{FUN}{ an \R function }
  \item{\dots}{ additional arguments to FUN }
}
\details{
These functions offer Simple mechanism to apply a function to non-overlapping time periods, e.g. weekly, monthly, etc, and return a raster time series object including a raster layer for each period in the original data, produced by \code{FUN}. The end of each period of time is assigned to the corresponding raster layer in the output.
}

\value{
A raster time series (\code{Raster*TS}) object
}

\author{Babak Naimi

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\seealso{ \code{\link[rts]{endpoints}}, \code{\link[rts]{period.apply}},
}

\examples{
\dontrun{
file <- system.file("external/ndvi", package="rts")

ndvi <- rts(file) # read the ndvi time series from the specified file
ndvi
ndvi.y <- apply.yearly(ndvi, mean) # apply mean function for each year
ndvi.y
ndvi.q <- apply.quarterly(ndvi,sd) # apply sd function for each quarter of years
ndvi.q 
}
}
