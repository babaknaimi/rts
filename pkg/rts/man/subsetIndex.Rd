\name{subset by index}

\docType{methods}
\alias{[[,RasterStackBrickTS,ANY,ANY-method}

\title{Subset layers in a raster time series object by index}

\description{
Extract layers from a \code{Raster*TS} object by index (i.e. double bracket, \code{[[}).   
}

\section{Methods}{
\describe{

\code{x[[i, ...]]}


Arguments


\code{i}  - indicates the layers (represented as a vector of numeric or character string relevant to time index, or by a time-based object).

\code{...}  - same as \code{...} in \code{\link[raster]{subset}} function in package \pkg{raster}

}}

\details{
This function can be used to extract a raster layer or a set of raster lasyers based on the time-index using date-like string. The format must left-specied with respect to the standard ISO:8601 time format "CCYY-MM-DD HH:MM:SS". It is also possible to specify a range of times via the index-based subsetting, using ISO-recommended "/" as the range operator. The basic form is \emph{"from/to"}, where both are optional. If either side is missing, it is interpretted as a request to retrieve raster layers from the beginning, or through the end of the raster time series object. Both \code{subset} function and \code{'[['} operator do the same thing.}

\value{
\code{RasterStackTS} or \code{RasterBrickTS}.
}

\author{Babak Naimi

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\seealso{ \code{\link[rts]{subset}}}

\examples{
\dontrun{
file <- system.file("external/ndvi", package="rts")

ndvi <- rts(file) # read the ndvi time series from the specified file

s1 <- ndvi[["2000-01-01/2000-05-31"]]

s1

plot(s1)

}

}

\keyword{ utilities }