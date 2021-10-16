# Author: Babak Naimi, naimi.b@gmail.com
# Date :  Oct. 2021
# Last Update :  March 2022
# Version 1.1
# Licence GPL v3



.name_or_proj4 <- function (x) {
  # copied from the package terra
  d <- eval(parse(text='terra:::.srs_describe(x@ptr$get_crs("wkt"))'))
  r <- x@ptr$get_crs("proj4")
  if (d$name != "unknown") {
    if (substr(r, 1, 13) == "+proj=longlat") {
      r <- paste("lon/lat", d$name)
    }
    else {
      r <- d$name
    }
    if (!is.null(d$EPSG) && !is.na(d$EPSG)) {
      r <- paste0(r, " (EPSG:", d$EPSG, ")")
    }
  }
  else {
    r <- x@ptr$get_crs("proj4")
  }
  r
}