# Author: Babak Naimi, naimi.b@gmail.com
# Date :  August 2017
# Version 1.1
# Licence GPL v3

setOldClass("xts")
setClass("RasterStackTS",
         representation(raster="RasterStack",
                        time="xts"),
         validity=function(object){
           return (nlayers(object@raster) == length(object@time))
         }  
         )


setClass("RasterBrickTS",
         representation(raster="RasterBrick",
                        time="xts"),
         validity=function(object){
           return (nlayers(object@raster) == length(object@time))
         }  
         )


setClassUnion("RasterStackBrickTS", c("RasterStackTS", "RasterBrickTS"))


setClass ('rts', contains = c('xts') )


setRefClass(".rtsOptions",
            fields=list(
              options='list'
            ),
            methods=list(
              addOption=function(n,v) {
                .self$options[[n]] <- v
              },
              getOption=function(n) {
                .self$options[[n]]
              },
              getOptions=function() {
                .self$options
              },
              deleteOption=function(n) {
                if (n %in% names(.self$options)) {
                  .self$options <- .self$options[names(.self$options) != n]
                }
              }
            )
)
.rtsOptions <- new('.rtsOptions')
