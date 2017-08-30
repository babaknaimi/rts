\name{rts}
\alias{rts}

\alias{rts,RasterStack,ANY-method}
\alias{rts,RasterBrick,ANY-method}
\alias{rts,character,ANY-method}
\alias{rts,xts,ANY-method}
\alias{rts,character,missing-method}

\title{Create a Raster Time Series object}

\description{
Constructor function to create a raster time series (\code{Raster*TS}) object. rts object can be created from a vector of image files names,  a RasterStack or a RasterBrick object (defined in \pkg{raster}) together with a vector of time/dates-must be of known time-based class. This function can also be used to read a raster time series file.
}

\usage{
rts(x, time)
}

\arguments{
  \item{x}{A character vector including names of image/raster files, or \code{RasterStack} or \code{RasterBrick} object, or the name (character) of a raster time series file}
  \item{time}{a vector holding date/time data with the same length as rasters in \code{Raster*} object or name of files in character vector}
  
  
}
\details{
A raster time series object is created by combining a \code{RasterStack} or \code{RasterBrick} objct, defined in \pkg{raster} and a \code{\link[xts]{xts}} object in \code{\link[xts]{xts-package}}.
\code{RasterStack} or \code{RasterBrick} can be created by using \code{\link[raster]{stack}} and \code{\link[raster]{brick}} functions, respectively in \code{raster-package}. If a character vector including the name of raster files is used for \code{x}, \code{stack} function is internally called by \code{rts}. \code{time} information is handled by \code{xts} object. The date/time values in the vector of \code{time} should be correspond to the raster files (i.e. first date/time for first raster, ...) and have the same length as the number of rasters in \code{x}. 

If a name of a raster time series file is provided for the \code{x} argument, it acts the same as code{\link[rts]{read.rts}}.
}

\value{
RasterStackTS or RasterBrickTS
}

\author{
Babak Naimi

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\seealso{ \code{\link[raster]{stack}, \link[raster]{brick}, \link[xts]{xts}}}

\examples{

\dontrun{
path <- system.file("external", package="rts") # location of files

lst <- list.files(path=path,pattern='.asc$',full.names=TRUE)
lst # list of raster files


r <- stack(lst) # creating a RasterStack object

r

d <- c("2000-02-01","2000-03-01","2000-04-01","2000-05-01") # corresponding dates to 4 rasters
d <- as.Date(d) # or d <- as.POSIXct(d) 

rt <- rts(r,d) # creating a RasterStackTS object

rt

plot(rt)
}



}

\keyword{ raster, time series }