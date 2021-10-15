# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2015
# Version 1.1
# Licence GPL v3


setMethod("[", c("RasterStackBrickTS", "Spatial","ANY"),
          function(x, i, j, drop=TRUE) {
            if (!missing(j)) {
              if (!inherits(try(j <- x@time[j],T), "try-error")) {
                if (length(j) > 0) {
                  if (drop) {
                    x <- x@raster[[as.vector(j)]][i,drop=drop]
                    rts(xts(t(x),index(j)))
                  } else {
                    x <- x@raster[[as.vector(j)]][i,drop=drop]
                    if (length(j) == 1) {
                      names(x) <- as.character(index(j))
                      x
                    }
                    else rts(x,index(j))  
                  }
                } else {
                  stop("The specified range for time is out of subscribtion")
                  }
              } else {
                stop("Problem with specified time range!")
                }
            } else {
              if(drop){
                rts(xts(t(x@raster[i, drop=drop])[as.vector(x@time),],index(x@time)))
              } else {
                rts(x@raster[i, drop=drop],index(x@time)[as.vector(x@time)])
              }
            }
          })


setMethod("[", c("RasterStackBrickTS", "numeric","ANY"),
          function(x, i, j ,drop=TRUE) {
            if (!missing(j)) {
              if (!inherits(try(j <- x@time[j],T), "try-error")) {
                if (length(j) > 0) {
                  if (drop) {
                    x <- .doExtract(x@raster[[as.vector(j)]],i,drop=drop)
                    rts(xts(t(x),index(j)))
                  } else {
                    x <- .doExtract(x@raster[[as.vector(j)]],i,drop=drop)
                    if (length(j) == 1) {
                      names(x) <- as.character(index(j))
                      x
                    }
                    else rts(x,index(j))  
                  }
                } else {
                  stop("The specified range for time is out of subscribtion")
                }
              } else {
                stop("Problem with specified time range!")
                }
            } else {
              if(drop){
                if (length(i) > 1) rts(xts(t(.doExtract(x@raster,i,drop=drop))[as.vector(x@time),],index(x@time)))
                else rts(xts(t(.doExtract(x@raster,i,drop=drop))[as.vector(x@time)],index(x@time)))
              } else {
                rts(.doExtract(x@raster,i,drop=drop),index(x@time)[as.vector(x@time)])
              }
            }
          })
#-------------

setMethod("[", c("SpatRasterTS", "Spatial","ANY"),
          function(x, i, j, drop=TRUE) {
            if (!missing(j)) {
              if (!inherits(try(j <- x@time[j],TRUE), "try-error")) {
                if (length(j) > 0) {
                  if (drop) {
                    x <- x@raster[[as.vector(j)]][vect(i),drop=drop]
                    rts(xts(t(x),index(j)))
                  } else {
                    x <- x@raster[[as.vector(j)]][vect(i),drop=drop]
                    if (length(j) == 1) {
                      names(x) <- as.character(index(j))
                      x
                    } else rts(x,index(j))  
                  }
                } else {
                  stop("The specified range for time is out of subscribtion")
                }
              } else {
                stop("Problem with specified time range!")
              }
            } else {
              if(drop){
                rts(xts(t(x@raster[vect(i), drop=drop])[as.vector(x@time),],index(x@time)))
              } else {
                rts(x@raster[vect(i), drop=drop],index(x@time)[as.vector(x@time)])
              }
            }
          })
#---------

setMethod("[", c("SpatRasterTS", "SpatVector","ANY"),
          function(x, i, j, drop=TRUE) {
            if (!missing(j)) {
              if (!inherits(try(j <- x@time[j],TRUE), "try-error")) {
                if (length(j) > 0) {
                  if (drop) {
                    x <- x@raster[[as.vector(j)]][i,drop=drop]
                    rts(xts(t(x),index(j)))
                  } else {
                    x <- x@raster[[as.vector(j)]][i,drop=drop]
                    if (length(j) == 1) {
                      names(x) <- as.character(index(j))
                      x
                    } else rts(x,index(j))  
                  }
                } else {
                  stop("The specified range for time is out of subscribtion")
                }
              } else {
                stop("Problem with specified time range!")
              }
            } else {
              if(drop){
                rts(xts(t(x@raster[i, drop=drop])[as.vector(x@time),],index(x@time)))
              } else {
                rts(x@raster[i, drop=drop],index(x@time)[as.vector(x@time)])
              }
            }
          })

#-------

setMethod("[", c("SpatRasterTS", "numeric","ANY"),
          function(x, i, j ,drop=TRUE) {
            if (!missing(j)) {
              if (!inherits(try(j <- x@time[j],T), "try-error")) {
                if (length(j) > 0) {
                  if (drop) {
                    x <- extract(x@raster[[as.vector(j)]],i,drop=drop)
                    rts(xts(t(x),index(j)))
                  } else {
                    x <- extract(x@raster[[as.vector(j)]],i,drop=drop)
                    if (length(j) == 1) {
                      names(x) <- as.character(index(j))
                      x
                    } else rts(x,index(j))  
                  }
                } else {
                  stop("The specified range for time is out of subscribtion")
                }
              } else {
                stop("Problem with specified time range!")
              }
            } else {
              
              if(drop){
                if (length(i) > 1) rts(xts(t(extract(x@raster,i,drop=drop))[as.vector(x@time),],index(x@time)))
                else rts(xts(t(extract(x@raster,i,drop=drop))[as.vector(x@time)],index(x@time)))
              } else {
                rts(extract(x@raster,i,drop=drop),index(x@time)[as.vector(x@time)])
              }
            }
          })
#---------




.doExtract <- function(x, i, drop) {  
  # Copid from raster package
  # Author: Robert J. Hijmans, r.hijmans@gmail.com
  # Date :  January 2009
  # Version 0.9
  # Licence GPL v3
  if (! raster::hasValues(x) ) {
    stop('no data associated with this Raster object')
  }
  if (length(i) < 1) return(NULL) 
  
  nacount <- sum(is.na(i))
  if (nacount > 0) {
    warning('some indices are invalid (NA returned)')
  }  
  
  if (drop) {
    return( raster::extract(x, i) )
    
  } else {
    i <- na.omit(i)
    r <- raster::rasterFromCells(x, i, values=FALSE)
    newi <- raster::cellFromXY(r, raster::xyFromCell(x, i))
    if (raster::nlayers(x) > 1) {
      r <- raster::brick(r)
      v <- matrix(NA, nrow=raster::ncell(r), ncol=raster::nlayers(x))
      v[newi,] <- raster::extract(x, i)
      v <- raster::setValues(r, v)
      return(v)
    } else {
      r[newi] <- raster::extract(x, i)
      return(r)
    }
  }
}

setMethod("[", c("RasterStackBrickTS", "Extent","ANY"),
          function(x, i, j ,drop=TRUE) {
            if (!missing(j)){
              if (!inherits(try(j <- x@time[j],T), "try-error")) {
                if (length(j) > 0) {
                  if (drop) {
                    x <- raster::extract(x@raster[[as.vector(j)]],i)
                    rts(xts(t(x),index(j)))
                  } else {
                    x <- raster::crop(x@raster[[as.vector(j)]],i)
                    rts(x,index(j))  
                  }
                } else {
                  stop("The specified range for time is out of subscribtion")
                  }
              } else {
                stop("Problem with specified time range!")
                #if(drop){
                #  rts(xts(t(extract(x@raster,i))[as.vector(x@time),],index(x@time)))
                #} else {
                #  rts(crop(x@raster,i),index(x@time)[as.vector(x@time)])
                #}
              }
            } else {
              if(drop){
                rts(xts(t(raster::extract(x@raster,i))[as.vector(x@time),],index(x@time)))
              } else {
                rts(raster::crop(x@raster,i),index(x@time)[as.vector(x@time)])
              }
            }
          })


setMethod("[", c("SpatRasterTS", "SpatExtent","ANY"),
          function(x, i, j ,drop=TRUE) {
            if (!missing(j)){
              if (!inherits(try(j <- x@time[j],T), "try-error")) {
                if (length(j) > 0) {
                  if (drop) {
                    x <- x@raster[[as.vector(j)]][i,drop=TRUE]
                    rts(xts(t(x),index(j)))
                  } else {
                    x <- x@raster[[as.vector(j)]][i,drop=FALSE]
                    rts(x,index(j))  
                  }
                } else {
                  stop("The specified range for time is out of subscribtion")
                }
              } else {
                stop("Problem with specified time range!")
              }
            } else {
              if(drop){
                rts(xts(t(x@raster[i,drop=TRUE])[as.vector(x@time),],index(x@time)))
              } else {
                rts(x@raster[i],index(x@time)[as.vector(x@time)])
              }
            }
          })
#----------

if (!isGeneric("extract")) {
  setGeneric("extract", function(x, y, ...)
    standardGeneric("extract"))
}

setMethod('extract', signature(x='RasterStackBrickTS', y='numeric'), 
          function(x, y, time){ 
            if (!missing(time)) x[y,time]
            else x[y]
          })
setMethod('extract', signature(x='RasterStackBrickTS', y='Spatial'), 
          function(x, y, time){ 
            if (!missing(time)) x[y,time]
            else x[y]
          })
setMethod('extract', signature(x='RasterStackBrickTS', y='Extent'), 
          function(x, y, time){ 
            if (!missing(time)) x[y,time]
            else x[y]
          })
#----------
setMethod('extract', signature(x='SpatRasterTS', y='numeric'), 
          function(x, y, time){ 
            if (!missing(time)) x[y,time]
            else x[y]
          })

setMethod('extract', signature(x='SpatRasterTS', y='SpatVector'), 
          function(x, y, time){ 
            if (!missing(time)) x[y,time]
            else x[y]
          })
setMethod('extract', signature(x='SpatRasterTS', y='SpatExtent'), 
          function(x, y, time){ 
            if (!missing(time)) x[y,time]
            else x[y]
          })
