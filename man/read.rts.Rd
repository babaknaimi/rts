\name{read.rts}
\alias{read.rts}


\alias{read.rts,character-method}


\title{Read raster time Series data from a file}

\description{
Read a raster time series object from a file.
}

\usage{
read.rts(filename)
}

\arguments{
  \item{filename}{ Filename (character)}
  }
\details{
This function reads a raster time series object which has been writen by \code{\link[rts]{write.rts}}. Instead of \code{read.rts}, the \code{rts} function can be used (usage: \code{rts(filename)}). 
}

\value{
\code{RasterBrickTS}
}

\author{Babak Naimi

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\seealso{ \code{\link[rts]{write.rts}, \link[rts]{rts}}}

\examples{
\dontrun{
file <- system.file("external/ndvi", package="rts")

rt <- read.rts(file)

rt

# or alternatively:
rt <- rts(file)
}

}

\keyword{ write, write raster time series}
