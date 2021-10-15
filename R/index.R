# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2012
# Last Update :  October 2021
# Version 1.1
# Licence GPL v3



`index.RasterStackBrickTS` <- function(x,...){
  index(x@time,...)
}

`index<-.RasterStackBrickTS` <- function(x,value){
  index(x@time) <- value
}


`index.SpatRasterTS` <- function(x,...){
  index(x@time,...)
}


`index<-.SpatRasterTS` <- function(x,value){
  index(x@time) <- value
}
