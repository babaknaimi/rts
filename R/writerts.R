# Author: Babak Naimi, naimi.b@gmail.com
# Date :  November 2012
# Last Update :  Oct 2021
# Version 1.2
# Licence GPL v3


.fileBase <- function (x, overwrite=FALSE) {
  x <- trim(x)
  if (identical(basename(x), x)) x <- file.path(getwd(), x)
  
  if (file.exists(x)) {
    if (!file.info(x)$isdir) stop("rts can not be written when a file with name similar to filename exists!")
    if (!overwrite) stop(paste(x,"exists. use overwrite=TRUE to overwrite it"))
    if (length(dir(x)) > 0 & (length(dir(x,pattern="tif$")) || length(dir(x,pattern="grd$"))) == 0) stop(paste(x,"can not be overwritten, because it is not a raster time series!"))
    unlink(x, recursive = T,force=T)
  }
  dir.create(x,recursive = TRUE)
  file.exists(x)
}


if (!isGeneric("write.rts")) {
  setGeneric("write.rts", function(x, filename, overwrite=FALSE, ...)
    standardGeneric("write.rts"))
}


setMethod ('write.rts' , signature(x='RasterStackBrickTS', filename='character'),
           function (x, filename, overwrite=FALSE, datatype='FLT4S',...) {
             if (missing(filename)) stop("filename is not specified!")
             if (.fileBase(filename,overwrite=overwrite)) {
               writeRaster(rast(x@raster),paste(filename,'/',basename(filename),'.tif',sep=""),datatype=datatype,...)
               write(paste("[General]\nCreator= R package 'rts'\ncreated= ",Sys.time(),
                           "\n[time]\ntclass = ",attr(x@time,"tclass")[1],
                           "\ntzone = ",attr(x@time,"tzone"),"\n[Data]",sep=""),
                     file=paste(filename,'/',basename(filename),'.rts',sep=""))
               write.table(data.frame(strftime(index(x@time)),as.vector(x@time)),
                           row.names=F,file=paste(filename,'/',basename(filename),'.rts',sep=""),
                           sep=",",append=T,col.names=F)
             } else stop("File is not cereated!")
           }
           )
#---------

setMethod ('write.rts' , signature(x='SpatRasterTS', filename='character'),
           function (x, filename, overwrite=FALSE, datatype='FLT4S',...) {
             if (missing(filename)) stop("filename is not specified!")
             if (.fileBase(filename,overwrite=overwrite)) {
               writeRaster(x@raster,paste(filename,'/',basename(filename),'.tif',sep=""),format="raster",datatype=datatype,...)
               write(paste("[General]\nCreator= R package 'rts'\ncreated= ",Sys.time(),
                           "\n[time]\ntclass = ",attr(x@time,"tclass")[1],
                           "\ntzone = ",attr(x@time,"tzone"),"\n[Data]",sep=""),
                     file=paste(filename,'/',basename(filename),'.rts',sep=""))
               write.table(data.frame(strftime(index(x@time)),as.vector(x@time)),
                           row.names=F,file=paste(filename,'/',basename(filename),'.rts',sep=""),
                           sep=",",append=T,col.names=F)
             } else stop("File is not cereated!")
           }
)



if (!isGeneric("read.rts")) {
  setGeneric("read.rts", function(filename,...)
    standardGeneric("read.rts"))
}

setMethod ('read.rts' ,signature(filename='character'),
           function(filename,cls=NULL) {
             
             if (missing(cls)) cls <- NULL
             else if (!is.null(cls)) {
               if (cls %in% c('SpatRasterTS','terra')) cls <- 'SpatRasterTS'
               else if (cls %in% c('RasterStackTS','RasterBrickTS','RasterStackBrickTS','raster')) cls <- 'RasterBrickTS'
               else warning('cls is unknown; default is used!')
             }
             
             
             filename <- trim(filename)
             if (identical(basename(filename), filename)) filename <- file.path(getwd(), filename)
             n <- paste(filename,"/",basename(filename),sep="")
             nn <- paste(n,".rts",sep="")
             if (file.exists(nn)) {
               tclass <- strsplit(readLines(nn,6)[5]," = ")[[1]][2]
               tzone <- strsplit(readLines(nn,6)[6]," = ")[[1]][2]
               dt <- read.table(nn,header=FALSE,skip=7,sep=",")
               dt[,1] <- as.character(dt[,1])
               if (tclass == "POSIXct") time <- as.POSIXct(dt[,1],tz=ifelse(is.na(tzone),"",tzone))
               if (tclass == "Date") time <- as.Date(dt[,1],tz=ifelse(is.na(tzone),"",tzone))
               if (tclass == "yearmon") time <- as.yearmon(dt[,1])
               if (tclass == "yearqtr") time <- as.yearqtr(dt[,1])
               if (!is.null(cls)) {
                 if (cls == "SpatRasterTS") {
                   o <- new("SpatRasterTS")
                   if (file.exists(paste(n,".tif",sep=""))) o@raster <- rast(paste(n,".tif",sep=""))
                   else if (file.exists(paste(n,".grd",sep=""))) o@raster <- rast(paste(n,".grd",sep=""))
                   else stop('file is unknown... seems it has no known raster file (either of geoTiff or RRaster)!')
                 } else {
                   o <- new("RasterBrickTS")
                   if (file.exists(paste(n,".tif",sep=""))) o@raster <- raster::brick(paste(n,".tif",sep=""))
                   else if (file.exists(paste(n,".grd",sep=""))) o@raster <- raster::brick(paste(n,".grd",sep=""))
                   else stop('file is unknown... seems it has no known raster file (either of geoTiff or RRaster)!')
                 }
                 o@time <- xts(dt[,2],time)
               } else {
                 if (file.exists(paste(n,".tif",sep=""))) {
                   o <- try(rast(paste(n,".tif",sep="")),TRUE)
                   if (!inherits(o,'try-error')) o <- new("SpatRasterTS",raster=o,time=xts(dt[,2],time))
                   else {
                     o <- raster::brick(paste(n,".tif",sep=""))
                     o <- new("RasterBrickTS",raster=o,time=xts(dt[,2],time))
                   }
                 } else if (file.exists(paste(n,".grd",sep=""))) {
                   o <- try(rast(paste(n,".grd",sep="")),TRUE)
                   if (!inherits(o,'try-error')) o <- new("SpatRasterTS",raster=o,time=xts(dt[,2],time))
                   else {
                     o <- raster::brick(paste(n,".grd",sep=""))
                     o <- new("RasterBrickTS",raster=o,time=xts(dt[,2],time))
                   }
                 } else stop('file is unknown... seems it has no known raster file (either of geoTiff or RRaster)!') 
               }
               
               return(o)
             } else stop("The specified file does not exist or it doesn't seem to be a raster time series!")
           }
           )