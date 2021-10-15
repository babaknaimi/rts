# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2012
# Last Update :  Oct. 2021
# Version 1.1
# Licence GPL v3

if (!isGeneric("endpoints")) {
  setGeneric("endpoints", function(x, on="months",k=1)
    standardGeneric("endpoints"))
}

setMethod("endpoints", "RasterStackBrickTS",
          function(x, on="months", k=1) {
            endpoints(x@time,on = on, k = k)
          }
          )


setMethod("endpoints", "SpatRasterTS",
          function(x, on="months", k=1) {
            endpoints(x@time,on = on, k = k)
          }
)