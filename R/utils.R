# Author: Babak Naimi, naimi.b@gmail.com (Thanks Robert Hijman for modifying the code)
# Date :  Oct. 2021
# Last Update :  Sep. 2023
# Version 1.2
# Licence GPL v3
#-------------------------

#----
.name_from.wkt <- function(wkt) { # copied from terra (.name_from_wkt)
  s = strsplit(wkt, ",")[[1]][1]
  strsplit(s, "\"")[[1]][[2]]
}



.name_or_proj4 <- function (x) {
  # copied from the package terra
	d <- crs(x, describe=TRUE)
	r <- crs(x, proj=TRUE)
	if (!(d$name %in% c(NA, "unknown", "unnamed"))) {
		if (substr(r, 1, 13) == "+proj=longlat") {
			r <- paste("lon/lat", d$name)
		} else {
			r <- d$name
		}
		if (!is.na(d$code)) {
			r <- paste0(r, " (", d$authority, ":", d$code, ")")
		}
	}
	if (r == "") {
		rr <- try(.name_from.wkt(wkt), silent=TRUE)
		if (!inherits(rr, "try-error")) {
			r <- rr
		}
	}
	r
}
