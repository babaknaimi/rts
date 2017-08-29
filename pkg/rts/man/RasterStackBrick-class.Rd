\name{RasterStackBrickTS-class}

\docType{class}

\alias{RasterStackTS-class}
\alias{RasterBrickTS-class}
\alias{RasterStackBrickTS-class}
\alias{rts-class}
\alias{xts-class}

\alias{show,RasterStackTS-method}
\alias{show,RasterBrickTS-method}

\title{ rts classes}

\description{

A raster time series contains a collection of \code{RasterLayer} objects, each corresponds to a time/date.

\code{RasterStackTS} and \code{RasterBrickTS} classes are created by putting together a \code{RasterStack} or \code{RasterBrick} object, from the \pkg{raster} package, and an \code{xts} object, from the \pkg{xts} package. A \code{RasterStack} and \code{RasterBrick} represents a collection of \code{RasterLayer} objects with the same extent and resolution. An \code{xts} object extends the S3 class \code{zoo} from the package of the same name. This object provides the index values that is unique and ordered, and also is a time-based class. Currently acceptable classes include: 'Date', 'POSIXct', 'timeDate', as well as 'yearmon' and 'yearqtr' where the index values remain unique.

\code{rts} is a subclass of \code{xts} class.
}

\section{Slots}{
Slots for Raster*TS object:
	\describe{
    \item{\code{raster}:}{object of class \code{RasterStack} or \code{RasterBrick}} 
    \item{\code{rime}:}{object of class \code{xts}}

See also \code{\link[raster]{Raster-class}} for slots in \code{raster}.
  }
}


\author{Babak Naimi 

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\examples{
showClass("RasterStackTS")
}

\keyword{classes}
\keyword{spatial}
