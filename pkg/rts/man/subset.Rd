\name{subset}
\alias{subset}
\alias{subset,RasterStackBrickTS-method}


\title{Subset layers in a raster time series object}

\description{
Extract layers from a \code{Raster*TS} object.   
}

\section{Methods}{
\describe{

\code{subset(x, subset, ...)}


Arguments

  \code{x} is a raster time series (\code{Raster*TS}) object created by \code{\link[rts]{rts}}
  
  \code{subset} is indicates the layers (represented as a vector of numeric or character string relevant to time index, or by a time-based object).
  
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

\seealso{ \code{\link[raster]{subset}} and \code{\link[xts]{[.xts}}}

\examples{
\dontrun{
file <- system.file("external/ndvi", package="rts")

ndvi <- rts(file) # read the ndvi time series from the specified file

s1 <- subset(ndvi,1:5) # subset the first 5 raster layers into a new raster time series object

s1

plot(s1)

s2 <- subset(ndvi,"/2000") # subset all layers till end of year 2000

s2

plot(s2)

s3 <- subset(ndvi,"2000-01-01/2000-05-31")

s3

plot(s3)

}

}

\keyword{ utilities }