# Author: Babak Naimi, naimi.b@gmail.com
# Date :  August 2017
# Version 1.1
# Licence GPL v3

.onAttach <- function(libname, pkgname) {
  pkg.info <- utils::packageDescription('rts') 
  packageStartupMessage(paste("rts ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))
  .setAuth()
  .setMRT()
  #.rtsOptions$addOption('MD_curlHandle',RCurl::getCurlHandle())
  invisible(0)
}


.onUnload <- function(libpath) {
  if (".._MD_curlHandle" %in% ls(envir=as.environment(".GlobalEnv"))) rm('.._MD_curlHandle',envir=as.environment(".GlobalEnv"))
  invisible(0)
}
