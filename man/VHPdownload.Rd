\name{VHPdownload}
\alias{VHPdownload}
\alias{VHPdownload,character-method}

\title{Download AVHRR-based Vegetation and Drought satellite image products}

\description{
'VHPdownload' downloads a series of AVHRR and VIIRS images in a specific date or a period of dates. The format of the files are downloaded as GeoTiff, and they can be optionally returned as a Raster Time Series Object.
}

\usage{
VHPdownload(x,dates,rts,ncore,...)

}

\arguments{
\item{x}{ product name; it can be either of \code{ c('VHI','VCI','SMN','SMT','TCI')}}
  \item{dates}{ a vector, character or Date, with one or two items, specifiying an individual date or a range of dates as the form of \code{c(from, to)}}
  \item{rts}{logical; specifies whether the downloaded files should be returned as a Raster Time Series object}
  \item{ncore}{numeric; specifies the number of cores to use for parallel downloading of the files}
  
  \item{...}{ additional arguments (Not implemented yet.)}
  
}
\details{
This function assists to download Blended Vegetation Health Indices Product (blended VIIRS (2013-present) and AVHRR (1981-2012), below, referred as Blended-VHP or VHP). These images for are available with a weekly temporal resolution and a spatial resolution of 4 KM.
Five products are available that are specified with the following abbreviations:

- \code{'SMT'}: Smoothed Brightness Temperature
- \code{'SMN'}: Smoothed NDVI
- \code{'TCI'}: Temperature Condition Index
- \code{'VHI'}: Vegetation Health Index
- \code{'VCI'}: Vegetation Condition Index

Data arrays are in geographic projection (grid with equal latitude and longitude interval). 



}


\author{Babak Naimi

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}



\examples{
\dontrun{
library(raster)

library(RCurl)


# download Vegetation Health Index for two months
vhi <- VHPdownload(x='VHI',dates=c('2015.01.01','2015.02.28'),rts=TRUE) # output is as rts object

vhi

plot(vhi[[1]])

plot(vhi[2120000]) # plot time series at the specified cell number


# to make sure the dates are appropriately specified, use a Date object:

dates <- as.Date(c('2015.01.01','2016.12.31'),format="%Y.%m.%d")

dates

class(dates)

dates <- as.Date(c('2012-01-01','2012-12-31'),format="%Y-%m-%d")

dates
 
# If your machine has multiple cores, you can use parallel downloading to speed up the downloads
# Vegetation Condition Index for two years
vci <- VHPdownload(x='VCI',dates=dates,rts=TRUE,ncore=4)

vci

plot(vci[[1:2]])



}

}

\keyword{ utilities }