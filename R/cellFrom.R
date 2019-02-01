# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2012
# Last Update: Feb. 2019
# Version 1.2
# Licence GPL v3

cellFromXY <- function(object,xy) {
  if ( missing(xy)) { stop('you must provide XY coordinates as a vector, matrix, or SpatialPoints') }
  cellFromXY(object@raster,xy)
}

cellFromRowCol <- function(object,row, col) {
  if ( missing(row) | missing(col)) { stop('you must provide row and col number(s)') }
  as.vector(cellFromRowCol(object@raster,row=row,col=col))
}



setMethod("cellFromRowCol", signature(object="RasterStackBrickTS", row="numeric",col="numeric"),
          function(object, row, col,...) {
            if ( missing(row) | missing(col)) { stop('you must provide row and col number(s)') }
            as.vector(cellFromRowCol(object@raster,row=row,col=col))
          }
)
#-----------

setMethod("cellFromXY", "RasterStackBrickTS",
          function(object,xy) {
            if ( missing(xy)) { stop('you must provide XY coordinates as a vector, matrix, or SpatialPoints') }
            cellFromXY(object@raster,xy)
          })