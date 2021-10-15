# Author: Babak Naimi, naimi.b@gmail.com
# First Date :  July 2012
# Last Update :  Oct. 2021
# Version 1.2
# Licence GPL v3


if (!isGeneric("period.apply")) {
  setGeneric("period.apply", function(x, INDEX, FUN, ...)
    standardGeneric("period.apply"))
}


setMethod("period.apply", "RasterStackTS",
          function(x,INDEX,FUN,...) {
            FUN <- match.fun(FUN)
            if (length(FUN(1:10)) > 1) stop("Function (FUN) should return one value!") 
            w <- which(!(INDEX %in% 1:length(x@time)))
            if (length(w) > 0) INDEX <- INDEX[-w]
            if (length(INDEX) == 0) stop("INDEX of endpoints is out of range!")
            ep <- sort(INDEX)
            if(ep[length(ep)] != NROW(x@time)) ep <- c(ep,NROW(x@time))
            #-------
            .dot <- list(...)
            if (length(.dot) > 0) {
              .fo <- formals(FUN)
              if (length(names(.fo)) > 1) {
                .nfo <- names(.fo)
                .nfo <- .nfo[2:length(.nfo)]
                if (length(.dot) > 0 && any(.nfo %in% names(.dot))) {
                  .nfo <- .nfo[.nfo %in% names(.dot)]
                  for (.n in .nfo) {
                    .fo[[.n]] <- .dot[[.n]]
                  }
                  formals(FUN) <- .fo
                }
              }
            }
            #--------
            
            for (i in 1:length(ep)) {
              if (i == 1) {
                if (ep[1] == 1) epr <- 1
                else epr <- 1:ep[1]
                if (length(epr) == 1) xx <- raster::subset(x@raster,1)
                else xx <- raster::calc(raster::subset(x@raster,as.vector(x@time[epr])),FUN)
                xxx <- raster::stack(xx)
                ind <- as.character(index(x@time[ep[1]]))
              } else {
                epr <- (ep[i-1]+1):ep[i]
                if (length(epr) == 1) xx <- raster::subset(x@raster,ep[i])
                else xx <- raster::calc(raster::subset(x@raster,as.vector(x@time[epr])),FUN)
                xxx <- raster::addLayer(xxx,xx)
                ind <- c(ind,as.character(index(x@time[ep[i]])))
              }
            }
            xxx <- rts(xxx,as.POSIXct(ind))
            xxx
          })

setMethod("period.apply", "RasterBrickTS",
          function(x,INDEX,FUN,...) {
            FUN <- match.fun(FUN)
            if (length(FUN(1:10)) > 1) stop("Defined function (FUN) returns more than one value!") 
            w <- which(!(INDEX %in% 1:length(x@time)))
            if (length(w) > 0) INDEX <- INDEX[-w]
            if (length(INDEX) == 0) stop("INDEX of endpoints is out of range!")
            ep <- sort(INDEX)
            if(ep[length(ep)] != NROW(x@time)) ep <- c(ep,NROW(x@time))
            
            #-------
            .dot <- list(...)
            if (length(.dot) > 0) {
              .fo <- formals(FUN)
              if (length(names(.fo)) > 1) {
                .nfo <- names(.fo)
                .nfo <- .nfo[2:length(.nfo)]
                if (length(.dot) > 0 && any(.nfo %in% names(.dot))) {
                  .nfo <- .nfo[.nfo %in% names(.dot)]
                  for (.n in .nfo) {
                    .fo[[.n]] <- .dot[[.n]]
                  }
                  formals(FUN) <- .fo
                }
              }
            }
            #--------
            for (i in 1:length(ep)) {
              if (i == 1) {
                if (ep[1] == 1) epr <- 1
                else epr <- 1:ep[1]
                if (length(epr) == 1) xx <- raster::subset(x@raster,1)
                else xx <- raster::calc(raster::subset(x@raster,as.vector(x@time[epr])),FUN)
                xxx <- raster::stack(xx)
                ind <- as.character(index(x@time[ep[1]]))
              } else {
                epr <- (ep[i-1]+1):ep[i]
                if (length(epr) == 1) xx <- subset(x@raster,ep[i])
                else xx <- raster::calc(subset(x@raster,as.vector(x@time[epr])),FUN)
                xxx <- raster::addLayer(xxx,xx)
                ind <- c(ind,as.character(index(x@time[ep[i]])))
              }
            }
            xxx <- raster::brick(xxx)
            xxx <- rts(xxx,as.POSIXct(ind))
            xxx
          })

#--------------
setMethod("period.apply", "SpatRasterTS",
          function(x,INDEX,FUN,...) {
            FUN <- match.fun(FUN)
            if (length(FUN(1:10)) > 1) stop("Defined function (FUN) returns more than one value!") 
            w <- which(!(INDEX %in% 1:length(x@time)))
            if (length(w) > 0) INDEX <- INDEX[-w]
            if (length(INDEX) == 0) stop("INDEX of endpoints is out of range!")
            ep <- sort(INDEX)
            if(ep[length(ep)] != NROW(x@time)) ep <- c(ep,NROW(x@time))
            
            #-------
            .dot <- list(...)
            if (length(.dot) > 0) {
              .fo <- formals(FUN)
              if (length(names(.fo)) > 1) {
                .nfo <- names(.fo)
                .nfo <- .nfo[2:length(.nfo)]
                if (length(.dot) > 0 && any(.nfo %in% names(.dot))) {
                  .nfo <- .nfo[.nfo %in% names(.dot)]
                  for (.n in .nfo) {
                    .fo[[.n]] <- .dot[[.n]]
                  }
                  formals(FUN) <- .fo
                }
              }
            }
            #--------
            for (i in 1:length(ep)) {
              if (i == 1) {
                if (ep[1] == 1) epr <- 1
                else epr <- 1:ep[1]
                if (length(epr) == 1) xx <- subset(x@raster,1)
                else xx <- app(subset(x@raster,as.vector(x@time[epr])),FUN)
                xxx <- xx
                ind <- as.character(index(x@time[ep[1]]))
              } else {
                epr <- (ep[i-1]+1):ep[i]
                if (length(epr) == 1) xx <- subset(x@raster,ep[i])
                else xx <- app(subset(x@raster,as.vector(x@time[epr])),FUN)
                xxx <- c(xxx,xx)
                ind <- c(ind,as.character(index(x@time[ep[i]])))
              }
            }
            
            xxx <- rts(xxx,as.POSIXct(ind))
            xxx
          })
