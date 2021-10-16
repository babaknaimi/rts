# Author: Babak Naimi, naimi.b@gmail.com
# This is based on 'show' function from raster package 
# Date :  November 2012
# Last Update :  Sep. 2022
# Version 1.4
# Licence GPL v3

setMethod ('show' , 'RasterStackBrickTS',
           function ( object ) {
             if (length(object@time) > 1) {
               p <- periodicity(object@time)
               cat ('Raster Time Series with',p$scale, 'periodicity from',as.character(p$start),'to',as.character(p$end),'\n')
             } 
             cat ('class       :' , class ( object ) , '\n')
                          
             if (raster::filename(object@raster) != '') {
               cat ('raster filename    :' , filename(object@raster), '\n')
             }
             nl <- nlayers(object@raster)
             if (nl == 0) {
               cat ('nlayers     :' , nl, '\n')
             } else {
               cat ('raster dimensions  : ', raster::nrow(object@raster), ', ', raster::ncol(object@raster), ', ', raster::ncell(object@raster), ', ', nl, '  (nrow, ncol, ncell, nlayers)\n', sep="" ) 
               cat ('raster resolution  : ' , raster::xres(object@raster), ', ', raster::yres(object@raster), '  (x, y)\n', sep="")
               cat ('raster extent      : ' , object@raster@extent@xmin, ', ', object@raster@extent@xmax, ', ', object@raster@extent@ymin, ', ', object@raster@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
               cat ('coord. ref. :' , raster::projection(object@raster, TRUE), '\n')
               
               minv <- format(raster::minValue(object@raster), digits=2)
               maxv <- format(raster::maxValue(object@raster), digits=2)
               minv <- gsub('Inf', '?', minv)
               maxv <- gsub('-Inf', '?', maxv)
               if (nl > 10) {
                 minv <- c(minv[1:10], '...')
                 maxv <- c(maxv[1:10], '...')
               }
               cat('min values  :', paste(minv, collapse=' '), '\n')
               cat('max values  :', paste(maxv, collapse=' '), '\n')
             }
             
             z <- getZ(object@raster)
             if (length(z) > 0) {
               name <- names(object@raster@z)
               if (name == '') name <- 'z-value'
               name <- paste(sprintf("%-12s", name), ':', sep='')
               if (length(z) < 10) {
                 cat(name, paste(z, collapse=', '), '\n')
               } else {
                 z <- summary(z)
                 cat(name, paste(z, collapse=' ... '), '(summary)\n')
               }
             }
             
             cat ('\n')
           }
           )
#--------------

setMethod ('show' , 'SpatRasterTS',
           function ( object ) {
             if (length(object@time) > 1) {
               p <- periodicity(object@time)
               cat ('Raster Time Series with',p$scale, 'periodicity from',as.character(p$start),'to',as.character(p$end),'\n')
             } 
             cat ('class       :' , class ( object ) , '\n')
             
             if (length(object@raster@ptr$filenames()[object@raster@ptr$filenames() != '']) > 0) {
               if (length(object@raster@ptr$filenames()) > 3) cat ('raster filename    :' , paste(object@raster@ptr$filenames()[1:3],collapse=', '),'...', '\n')
               else cat ('raster filename    :' , object@raster@ptr$filenames(), '\n')
             }
             nl <- nlyr(object@raster)
             if (nl == 0) {
               cat ('nlayers     :' , nl, '\n')
             } else {
               d <- dim(object@raster)
               xyres <- res(object@raster)
               cat("raster dimensions  : ", d[1], ", ", d[2], ", ", d[3], "  (nrow, ncol, nlyr)\n", sep="" ) 
               cat ('raster resolution  : ' , xyres[1], ', ', xyres[2], '  (x, y)\n', sep="")
               hw <- window(object@raster)
               if (any(hw)) {
                 w <- as.vector(ext(object@raster))
                 if (all(hw)) {
                   txt <- "window      : "
                 } else {
                   txt <- "extent (win): "
                 }
                 cat(txt, w[1], ", ", w[2], ", ", w[3], ", ", w[4], "  (xmin, xmax, ymin, ymax)\n", sep="")
               } else {
                 e <- as.vector(ext(object@raster))
                 cat("raster extent      : " , e[1], ", ", e[2], ", ", e[3], ", ", e[4], "  (xmin, xmax, ymin, ymax)\n", sep="")
               }
               
               
               
               cat ('coord. ref. :' , .name_or_proj4(object@raster), '\n')
               
               minv <- format(object@raster@ptr$range_min, digits=2)
               maxv <- format(object@raster@ptr$range_max, digits=2)
               minv <- gsub('Inf', '?', minv)
               maxv <- gsub('-Inf', '?', maxv)
               if (nl > 10) {
                 minv <- c(minv[1:10], '...')
                 maxv <- c(maxv[1:10], '...')
               }
               cat('min values  :', paste(minv, collapse=' '), '\n')
               cat('max values  :', paste(maxv, collapse=' '), '\n')
             }
             
             cat ('\n')
           }
)

