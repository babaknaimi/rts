# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2012
# Last Update: Oct. 2018
# Version 1.1
# Licence GPL v3

cellFromXY <- function(object,xy) {
  if ( missing(xy)) { stop('you must provide XY coordinates as a vector, matrix, or SpatialPoints') }
  cellFromXY(object@raster,xy)
}

setMethod("cellFromRowCol", "RasterStackBrickTS",
          function(object, rownr, colnr) {
            if ( missing(rownr) | missing(colnr)) { stop('you must provide row and col number(s)') }
            as.vector(cellFromRowCol(object@raster,rownr=rownr,colnr=colnr))
          })
#-----------

setMethod("cellFromXY", "RasterStackBrickTS",
          function(object,xy) {
            if ( missing(xy)) { stop('you must provide XY coordinates as a vector, matrix, or SpatialPoints') }
            cellFromXY(object@raster,xy)
          })