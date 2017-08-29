# Title:  ModisDownload 
# Version: 7.1 (last update): August 2017
# Author: Babak Naimi (naimi.b@gmail.com), and (from version 5.4) Pablo Alfaro (ludecan@gmail.com)

# Major changes have been made on this version comparing to the 2.x. Since the FTP is not supported anymore,
# the functions have been adjusted to support HTTP!

# From ver 5.0, a major change is made that is to support authentication required by the main website

# (Thanks Tomislav Hengl as his script [spatial-analyst.net] was used as the main core of the first version)
# Description: This is the source for two functions (ModisDownload and MRTproj) in R language that assist you to download, mosaic, subset and reproject the MODIS image products.
# Dependent library: RCurl
# Reference: http://www.r-gis.net

# MRTproj function is a wrapper to MRT tools, can be used to resample and subset an HDF image and convert to GeoTiff

# Licence GPL v3

# palfaro @ 2017-01-02
# Changes made require a few more packages, most important of which are digest and parallel.
# Digest is used to generate a hash key of the inputs to queries. When downloading a file a .RCache file 
# is saved using as name the hash key. When another download is made you can rebuild the hash and chec if the .RCache file exists to avoid 
# the download altogether.
# parallel is used to call parLapplyLB to do the parallel downloads
# palfaro @ 2017-01-09
# create a RCurl handle which will be reused between connections to enable http keepalives
.._MD_curlHandle <- RCurl::getCurlHandle()
.._MD_curlHandle <<- RCurl::getCurlHandle()
modisProducts <- function(version=NULL) {
  #.ModisLPxxx <- NULL
  #load(system.file("external/ModisLP.RData", package="rts"))
  #return(.ModisLPxxx)
  #rm(.ModisLPxxx)
  p1 <- readRDS(system.file("external/ModisLPV5.rds", package="rts"))
  p2 <- readRDS(system.file("external/ModisLPV6.rds", package="rts"))
  if (is.null(version)) return(list(`Version 4-5.5`=p1,`Version 6`=p2))
  else {
    if (version %in% c(4,5,5.5) || version %in% c('004','005','5.5')) return(p1)
    else if (version == 6 || version == '006') return(p2)
    else return(list(`Version 4-5.5`=p1,`Version 6`=p2))
  }
}

# palfaro @ 2017-01-02
# helpful functions
# gets the native pixel size of a given product from the table
getNativePixelSize <- function(product) {
  products <- modisProducts()
  w <- which(products[[1]]$product==product)
  if (length(w) != 0) {
    pixelSize <- products[[1]]$resolution[w]
  } else {
    w <- which(products[[2]]$product==product)
    pixelSize <- products[[2]]$resolution[w]
  }
  pixelSize <- levels(pixelSize)[pixelSize]
  pixelSize <- as.integer(gsub(pattern = '[^[:digit:]]', replacement='', x=pixelSize))
  return(pixelSize)
}

# gets the native temporal resolution of a given product from the table
getNativeTemporalResolution <- function(product) {
  products <- modisProducts()
  w <- which(products[[1]]$product==product)
  if (length(w) != 0) {
    temporalResolution <- products[[1]]$temporal[w]
  } else {
    w <- which(products[[2]]$product==product)
    temporalResolution <- products[[2]]$temporal[w]
  }
  
  temporalResolution <- levels(temporalResolution)[temporalResolution]
  
  increment <- gsub(pattern = '[^[:digit:]*]', replacement='', x=temporalResolution)
  if (increment == "") { increment <- 1
  } else { increment <- as.integer(increment) }
  units <- gsub(pattern = '[[:digit:]* ]', replacement='', x=temporalResolution)
  if (units == "Daily") { units <- "day"
  } else if (units == "Monthly") { units <- "month"
  } else if (units == "Yearly") { units <- "year"
  }
  
  return(list(increment=increment, units=units))
}
#---------

.getSeparator <- function(x) {
  if (length(strsplit(x,'/')[[1]]) != 1) '/'
  else '\\'
}
#---
.getMRTdata <- function(x) {
  paste0(strsplit(x,'bin')[[1]][1],'data')
}
#----

.normalizePath <- function(x) {
  x <- trim(x)
  xx <- strsplit(x,'')[[1]]
  if (xx[length(xx)] %in% c('/','\\')) {
    xx <- xx[-length(xx)]
    x <- paste(xx,collapse='')
  }
  normalizePath(x,winslash = .getSeparator(x))
}
#-----------------------

.modisHTTP <- function(x,v='005',opt) {
  if (!requireNamespace("RCurl",quietly = TRUE)) stop("Package RCurl is not installed")
  mp <- modisProducts(version=v)
  if (is.numeric(x)) {
    if (x > nrow(mp)) stop("The product code is out of subscription!")
    x <- as.character(mp[x,1])
  }
  x <- trim(x)
  if ('e4ftl01.cr.usgs.gov' %in% strsplit(x,'/')[[1]]) {
    if (!strsplit(x,'/')[[1]][1] %in% c('http:','https:')) x <- paste('http://',x,sep='')
    if (strsplit(x,'')[[1]][length(strsplit(x,'')[[1]])] != "/") x <- paste(x,"/",sep="")
    if (!RCurl::url.exists(x)) stop("the http address does not exist OR Server is down!")
  } else {
    w <- which(mp[,1] == x)
    if (length(w) != 1) stop("The Name does not exist in MODIS Land produnct list!")
    if (as.character(mp[w,2]) == "Terra") ad <- "MOLT"
    else if (as.character(mp[w,2]) == "Aqua") ad <- "MOLA"
    else ad <- "MOTA"
    xx <- paste("http://e4ftl01.cr.usgs.gov/",ad,"/",x,".",v,"/",sep="")
    if (!RCurl::url.exists(xx)) {
      if (!RCurl::url.exists(paste("http://e4ftl01.cr.usgs.gov/",ad,"/",sep=""))) stop("the http address does not exist! Version may be incorrect OR Server is down!")
      else {
        # palfaro @ 2017-01-09
        # I think there was a typo here. .opt instead of .opts
        items <- try(strsplit(RCurl::getURL(paste("http://e4ftl01.cr.usgs.gov/",ad,"/",sep=""),.opts=opt), "\r*\n")[[1]],silent=TRUE)
        dirs <- unlist(lapply(strsplit(unlist(lapply(strsplit(items[-c(1:19)],'href'),function(x){strsplit(x[2],'/')[[1]][1]})),'"'),function(x) {x[2]}))
        dirs <- na.omit(dirs)
        w <- which(unlist(lapply(strsplit(dirs,'\\.'),function(x) x[[1]])) == x)
        if (length(w) > 0) v <- unlist(lapply(strsplit(dirs,'\\.'),function(x) x[[2]]))[w]
        x <- paste("http://e4ftl01.cr.usgs.gov/",ad,"/",x,".",v,"/",sep="")
      }
    } else x <- xx
  }
  x
}

#-----------------
.getModisList <- function(x,h,v,dates,opt,forceReDownload=TRUE,nc) {
  if (!requireNamespace("RCurl",quietly = TRUE)) stop("Package RCurl is not installed")
  if (!requireNamespace("digest",quietly = TRUE)) stop("Package digest is not installed")
  if (inherits(dates,"character")) dates <- as.Date(dates,format='%Y.%m.%d')
  dates <- na.omit(as.Date(dates))
  if (length(dates) == 0) stop("dates is not appropriately selected!")
  dates <- sort(dates)
  
  # palfaro @ 2017-01-02
  # cache the results using a hash digest of the inputs. It avoids having to traverse the whole
  # file structure if you are going to download the exact same product/version/tiles/dates you did in the past
  # for example if a dowload cancelled midway
  pathCache <- paste('RCache/', digest::digest(c(x, h, v, dates)), '.rds', sep='')
  if (forceReDownload | !file.exists(pathCache)) {
    # if forceReDownload or cache doesn't exist then access the MODIS url, else use the cached version
    serverErrorsPattern <- '503 Service Unavailable|500 Internal Server Error'
    try.nr <- 5
    items <- 0
    class(items) <- "try-error"
    ce <- 0
    while(class(items) == "try-error") { 
      # palfaro @ 2017-01-09
      items <- try(strsplit(RCurl::getURL(x,.opts = opt), "\r*\n")[[1]],silent=TRUE)
      
      if (class(items) == "try-error" || (length(items) < 30 && length(grep(pattern = serverErrorsPattern, items)) > 0)) {
        Sys.sleep(15)
        ce <- ce + 1
        if (ce == (try.nr+1)) stop("Download error: Server does not response!")
      }
    }
    items <- items[-1]
    # get the directory names (available dates)
    # palfaro @ 2017-01-17
    # Add fixed = TRUE, speeds up the parsing of the file
    dirs <- unlist(lapply(strsplit(unlist(lapply(strsplit(items,'href',fixed = TRUE),function(x){strsplit(x[2],'/',fixed = TRUE)[[1]][1]})),'"'),function(x) {x[2]}))
    dirs <- na.omit(dirs)
    d <- as.Date(dirs,format='%Y.%m.%d')
    
    # extract the selected dates
    if (length(dates) == 2) {
      dirs <- dirs[which(d >= dates[1] & d <= dates[2])]
    } else dirs <- dirs[unlist(lapply(dates,function(x){which(d == x)}))]
    
    if (length(dirs) < 1) stop("No available data for the selected dates")
    # palfaro @ 2017-01-02
    # I keep the variable allImagesAvailable to know if all images were available for download
    # If they were it means the date range is already available in the server, these files locations 
    # are not expected to change so I cache the ModisList into disk
    # if they aren't I don't cache the results since the file list  may change when the images update
    if (length(dates) > 2 & length(dirs) < length(dates)) {
      allImagesAvailable <- FALSE
      warning("The images are not available for some specified dates!")
    } else {
      allImagesAvailable <- TRUE
    }
    
    # palfaro @ 2017-01-02
    # original sequential version
    # creating a vector of available Modis images in selected tiles and dates
    #Modislist <- list()
    #for (i in 1:length(dirs)) {
    #  getlist <- 0
    #  class(getlist) <- "try-error"
    #  ce <- 0
    #  while(class(getlist) == "try-error") {
    #    getlist <- try(strsplit(RCurl::getURL(paste(x,dirs[i], "/", sep=""),.opts = opt), "\r*\n")[[1]],silent=TRUE)
    #    if (class(getlist) == "try-error") {
    #      Sys.sleep(5)
    #      ce <- ce + 1
    #      if (ce == (try.nr+1)) stop("Download error: Server does not response!")
    #    }
    #  }
    #getlist <- getlist[-c(1:7)]
    #  getlist <- unlist(lapply(strsplit(getlist,"href"),function(x){strsplit(x[2],'"')[[1]][2]}))
    #  w <- which(is.na(getlist))
    #  if (length(w) > 0) getlist <- getlist[-w]
    #  w <- unlist(lapply(lapply(getlist,function(x) strsplit(x,'\\.')[[1]]),function(x) x[length(x)] == 'hdf'))
    #  if (any(w)) getlist <- getlist[w]
    
    #  if (length(grep('h[0-9]',getlist)) > 0) {
    #    m <- c()
    #    for (vv in v) {
    #      for (hh in h) {
    #        if (vv < 10) vc <- paste('0',as.character(vv),sep='')
    #        else vc <- as.character(vv)
    #        if (hh < 10) hc <- paste('0',as.character(hh),sep='')
    #        else hc <- as.character(hh)
    #        ModisName <- grep(".hdf$",grep(paste('h',hc,'v',vc,sep=''),getlist,value=TRUE),value=TRUE)
    #        #if (length(ModisName) == 1) {
    #        m <- c(m,paste(x,dirs[i], "/",ModisName,sep='')[length(ModisName)])
    #        Modislist[[dirs[i]]] <- m
    #        #}
    #      }
    #    }
    #  } else {
    #    Modislist[[dirs[i]]] <- getlist
    #  }
    #}
    
    # palfaro @ 2017-01-02
    # traverse the MODIS file structure using multiple concurrent HTTP connections to mitigate 
    # TCP Slow Start Problems and get a better bandwidth utilization
    # this function gets all filenames for tiles h and v of product productURL in directory dir 
    # I had to rename x to productURL because of the call to parLapplyLB below already has an x parameter
    #dir <- dirs[[1]]
    #productURL <- x
    getModisName <- function(dir, productURL, h, v, opt, serverErrorsPattern,forceReDownload=TRUE) {
      pathCache <- paste('RCache/', basename(productURL), '_', dir, '.rds', sep = '')
      if (forceReDownload | !file.exists(pathCache)) {
        getlist <- 0
        class(getlist) <- "try-error"
        ce <- 0
        while(class(getlist) == "try-error") {
          # palfaro @ 2017-01-09
          # reuse MD_curlHandle to enable http keepalive
          getlist <- try(strsplit(RCurl::getURL(paste(productURL,dir, "/", sep=""),.opts = opt, curl = RCurl::getCurlHandle()), "\r*\n")[[1]],silent=TRUE)
          
          if (class(getlist) == "try-error" || (length(getlist) < 30 && length(grep(pattern = serverErrorsPattern, getlist)) > 0)) {
            Sys.sleep(15)
            ce <- ce + 1
            if (ce == 6) stop(paste("Download error: Server does not response!\n", getlist))
          }
        }
        
        # palfaro @ 2017-01-17
        # Add fixed = TRUE, slightly speeds up the parsing of the file
        getlist <- unlist(lapply(strsplit(getlist,"href", fixed = TRUE),function(x){strsplit(x[2],'"')[[1]][2]}))
        w <- which(is.na(getlist))
        if (length(w) > 0) getlist <- getlist[-w]
        w <- unlist(lapply(lapply(getlist,function(x) strsplit(x,'\\.')[[1]]),function(x) x[length(x)] == 'hdf'))
        if (any(w)) getlist <- getlist[w]
        
        dir.create(dirname(pathCache), showWarnings=FALSE, recursive = TRUE)
        saveRDS(object = getlist, file=pathCache)
      } else {
        getlist <- readRDS(file = pathCache)
      }
      
      if (length(grep('h[0-9]',getlist)) > 0) {
        m <- c()
        for (vv in v) {
          for (hh in h) {
            if (vv < 10) vc <- paste('0',as.character(vv),sep='')
            else vc <- as.character(vv)
            if (hh < 10) hc <- paste('0',as.character(hh),sep='')
            else hc <- as.character(hh)
            # palfaro @ 2017-01-17
            # Add fixed = TRUE, slightly speeds up the parsing of the file
            ModisName <- grep(".hdf$",grep(paste('h',hc,'v',vc,sep=''),getlist,value=TRUE, fixed=TRUE),value=TRUE)
            #if (length(ModisName) == 1) {
            m <- c(m,paste(productURL,dir, "/",ModisName,sep='')[length(ModisName)])
          }
        }
      } else {
        m <- getlist
      }
      
      return(m)
    }
    
    # Browse the directories using multiple concurrent connections to mitigate TCP Slow Start Problems
    nCoresAUsar <- min(length(dirs), nc)
    if (nCoresAUsar > 1) {
      cl <- parallel::makeCluster(getOption("cl.cores", nCoresAUsar))
      # palfaro @ 2017-01-09
      # The curl handles must be created in the processes that are going to use them so we create them
      # using clusterEvalQ
      #parallel::clusterExport(cl,c('.._MD_curlHandle'))
      parallel::clusterEvalQ(cl, expr = { .._MD_curlHandle <- RCurl::getCurlHandle()})
      Modislist <- parallel::parLapplyLB(cl=cl, X=dirs, fun=getModisName, productURL=x, h=h, v=v, opt=opt, serverErrorsPattern=serverErrorsPattern, forceReDownload=forceReDownload)
      parallel::stopCluster(cl)
    } else {
      # palfaro @ 2017-07-13
      # This is a bit convoluted but we need the .rtsOptions$getOption('MD_curlHandle') created in the global environment for
      # getModisName to find it. In the parallel case it gets created in the global environment in the clusterEvalQ call
      # but if we call the same expression here it gets created in the current environment, thus we use assign
      #assign(".rtsOptions$getOption('MD_curlHandle')", RCurl::getCurlHandle(), envir = .GlobalEnv)
      #.._MD_curlHandle <<- RCurl::getCurlHandle()
      Modislist <- lapply(dirs, FUN = getModisName, productURL=x, h=h, v=v, opt=opt, serverErrorsPattern=serverErrorsPattern, forceReDownload=forceReDownload)
      #rm(.rtsOptions$getOption('MD_curlHandle'), envir = .GlobalEnv)
      #rm(.._MD_curlHandle, envir = .GlobalEnv)
    }
    names(Modislist) <- dirs
    
    # palfaro @ 2017-01-02
    # only save cache if all the images that were asked for were found
    # this is to prevent the cache from hiding updates in the server
    if (allImagesAvailable) {
      dir.create(dirname(pathCache), showWarnings=FALSE, recursive = TRUE)
      saveRDS(object = Modislist, file=pathCache)
    }
  } else {
    # if !forceReDownload and cache exists load result from file
    Modislist <- readRDS(file = pathCache)
  }
  Modislist
}
#--------

.downloadHTTP <- function(x,filename,opt, forceReDownload=TRUE, 
                          maxRetries=5, secondsBetweenRetries=15) {
  if (!requireNamespace("RCurl",quietly = TRUE)) stop("Package RCurl is not installed")
  # palfaro @ 2017-07-13 
  # this shouldn't be here, else each time we download a new file the curl handle is being recreated
  # which disables keepalive
  # .rtsOptions$getOption('MD_curlHandle') <- RCurl::getCurlHandle()
  
  # palfaro @ 2017-01-02
  # Sometimes the server returns a response having 299 bytes total like this:
  #<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
  #  <html><head>
  #  <title>503 Service Unavailable</title>
  #  </head><body>
  #  <h1>Service Unavailable</h1>
  #  <p>The server is temporarily unable to service your
  #request due to maintenance downtime or capacity
  #problems. Please try again later.</p>
  #  </body></html>
  #
  # Or a response having 537 bytes like this:
  #<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
  #  <html><head>
  #  <title>500 Internal Server Error</title>
  #  </head><body>
  #  <h1>Internal Server Error</h1>
  #  <p>The server encountered an internal error or
  #misconfiguration and was unable to complete
  #your request.</p>
  #  <p>Please contact the server administrator at 
  #root@e4ftl01.cr.usgs.gov to inform them of the time this error occurred,
  #and the actions you performed just before this error.</p>
  #  <p>More information about this error may be available
  #in the server error log.</p>
  #  </body></html>
  # In those cases we redownload the file
  serverErrorsPattern <- '503 Service Unavailable|500 Internal Server Error'
  
  # palfaro @ 2017-01-02
  # Added forceReDownload parameter to avoid downloading file again if it already exists
  # the filesize check is a minimal check to see if the file has been downloaded correctly
  # The download has been succesfull iff we don't have to redownload and the file exists, 
  # and has a larger size than 1024 bytes or it's smaller but it doesn't contain serverErrorsPattern 
  # in it's lines.
  success <- !forceReDownload && file.exists(filename) && 
    (file.info(filename)$size > 1024 || length(grep(pattern = serverErrorsPattern, readLines(filename))) == 0)
  # Also, adding a few retries if the file can't be downloaded
  # there are several momentary interruptions in connections that 
  # abort downloads but can be addressed by trying again.
  nRetries <- 0
  while (!success & nRetries < maxRetries) {
    # palfaro @ 2017-01-09
    # Write directly to file, without going through memory, should be slightly faster.
    # Also, reuse .rtsOptions$getOption('MD_curlHandle') to enable http keepalive
    f = RCurl::CFILE(filename, mode="wb")
    #er2 <- try(er <- RCurl::curlPerform(url = x, curl=get('.._MD_curlHandle'), writedata = f@ref, .opts = opt))
    er2 <- try(er <- RCurl::curlPerform(url = x, curl=RCurl::getCurlHandle(), writedata = f@ref, .opts = opt))
    RCurl::close(f)
    
    # palfaro @ 2017-01-02
    # Here if we get the service unavailable error we treat the download as an error, wait a little and try again later
    if (!file.exists(filename) || 
        (file.info(filename)$size < 1024 && length(grep(pattern = serverErrorsPattern, readLines(filename))) > 0)) {
      class(er2) <- "try-error"
    }
    
    if (class(er2) == "try-error" || er != 0) {
      nRetries <- nRetries + 1
      Sys.sleep(secondsBetweenRetries)
    } else { success <- TRUE }
  }
  
  if (!success) print("Download Error: Server does not response!!")
  return(success)
}

.getMODIS <- function(x, h, v, dates, version='005',opt, forceReDownload=TRUE,nc) {
  xx <- .modisHTTP(x,v=version,opt=opt)
  Modislist <- .getModisList(x = xx,h=h,v=v,dates=dates,opt=opt, forceReDownload=forceReDownload,nc=nc)
  
  if (length(Modislist) == 0) stop("There is NO available images for the specified product!")
  
  cat(sum(unlist(lapply(Modislist,length))),'images are found for the specified dates!\n')
  
  # palfaro @ 2017-01-02
  # original sequential version
  #out <- data.frame(matrix(nrow=0,ncol=2))
  #names(out) <- c("Date","Name")
  #dirs <- names(Modislist)
  #for (d in dirs) {
  #  cnt <- 1
  #  for (ModisName in Modislist[[d]]) {
  #    n <- strsplit(ModisName,"/")[[1]]
  #    n <- n[length(n)]
  #    if (.downloadHTTP(ModisName,n,opt=opt)) {
  #      out <- rbind(out,data.frame(Date=d,Name=n))
  #      cat('=')
  #    } else cat('0')
  #    cnt <- cnt + 1
  #  }
  #}
  
  # palfaro @ 2017-01-02
  # Download the files using multiple concurrent connections to mitigate TCP Slow Start Problems and get a 
  # better bandwidth utilization
  # We loose the progress bar since the worker processes can't print to the main process' console
  
  # I make the list plain ie: one file per index to achieve parallelism both in dates and tiles.
  plainModisList <- matrix(data='', nrow = 0, ncol = 2)
  for (i in seq_along(Modislist)) {
    date <- names(Modislist)[i]
    for (j in seq_along(Modislist[[i]])) {
      plainModisList <- rbind(plainModisList, c(date, Modislist[[i]][j]))
    }
  }
  # This function gets the file in row i of plainModisList
  getFile <- function(i, plainModisList, opt, forceReDownload) {
    if (!requireNamespace('RCurl')) stop("Package RCurl is not installed")
    n <- strsplit(plainModisList[i, 2],"/")[[1]]
    n <- n[length(n)]
    if (.downloadHTTP(x = plainModisList[i, 2], filename = n, opt=opt, forceReDownload=forceReDownload)) {
      out <- data.frame(Date=plainModisList[i, 1], Name=n)
      cat('=')
    } else {
      out <- data.frame(Date=character(0), Name=character(0))
      cat('0')
    }
    return(out)
  }
  
  # These lines set the amount of worker processes and call getFile over subsets of plainModisList, the complete list of files,
  # while using load balancing
  nCoresAUsar <- min(nrow(plainModisList), nc)
  if (nCoresAUsar > 1) {
    cl <- parallel::makeCluster(getOption("cl.cores", nCoresAUsar))
    # palfaro @ 2017-01-09
    # The curl handles must be created in the processes that are going to use them so we create them
    # using clusterEvalQ
    
    parallel::clusterEvalQ(cl, expr = { .._MD_curlHandle <- RCurl::getCurlHandle()})
    parallel::clusterExport(cl=cl, varlist=c(".downloadHTTP"), envir=environment())
    res <- parallel::parLapplyLB(cl=cl, X = 1:nrow(plainModisList), fun = getFile, plainModisList=plainModisList, opt=opt, forceReDownload=forceReDownload)
    parallel::stopCluster(cl)
  } else {
    # palfaro @ 2017-07-13
    # This is a bit convoluted but we need the .rtsOptions$getOption('MD_curlHandle') created in the global environment for
    # getModisName to find it. In the parallel case it gets created in the global environment in the clusterEvalQ call
    # but if we call the same expression here it gets created in the current environment, thus we use assign
    #assign(".rtsOptions$getOption('MD_curlHandle')", RCurl::getCurlHandle(), envir = .GlobalEnv)
    #.._MD_curlHandle <<- RCurl::getCurlHandle()
    res <- lapply(X = 1:nrow(plainModisList), FUN = getFile, plainModisList=plainModisList, opt=opt, forceReDownload=forceReDownload)
    #rm(.._MD_curlHandle, envir = .GlobalEnv)
  }
  
  out <- do.call('rbind', res)  
  
  cat(' \n')
  out
}

#---------
.setAuth <- function() {
  if (is.null(.rtsOptions$getOption(n = 'nasaAuth'))) {
    if (file.exists(paste0(Sys.getenv('HOME'),'/.netrc'))) {
      if (!file.exists(paste0(Sys.getenv('HOME'),'/.urs_cookies'))) file.create(paste0(Sys.getenv('HOME'),'/.urs_cookies'))
      .rtsOptions$addOption('nasaAuth',list(netrc.file=paste0(Sys.getenv('HOME'),'/.netrc'),
                                            cookiefile=paste0(Sys.getenv('HOME'),'/.urs_cookies')))
    }
  }
}

#------------
.setMRT <- function() {
  if (is.null(.rtsOptions$getOption(n = 'MRTpath'))) {
    if (file.exists(paste0(Sys.getenv('HOME'),'/.MRTpath'))) {
      f <- file(paste0(Sys.getenv('HOME'),'/.MRTpath'),'r')
      a <- readLines(f)
      close(f)
      if (length(a) > 0) .rtsOptions$addOption('MRTpath',lapply(a,function(x) strsplit(x,':')[[1]])[[1]][2])
    }
  }
}

#------------

if (!isGeneric("setNASAauth")) {
  setGeneric("setNASAauth", function(username,password,update,...)
    standardGeneric("setNASAauth"))
}


if (!isGeneric("setMRTpath")) {
  setGeneric("setMRTpath", function(MRTpath,update,...)
    standardGeneric("setMRTpath"))
}

if (!isGeneric("getMODIS")) {
  setGeneric("getMODIS", function(x,h,v,dates,version='005',forceReDownload=TRUE,ncore='auto')
    standardGeneric("getMODIS"))
}

if (!isGeneric("mosaicHDF")) {
  setGeneric("mosaicHDF", function(hdfNames,filename,MRTpath, bands_subset,delete=FALSE)
    standardGeneric("mosaicHDF"))
}
if (!isGeneric("reprojectHDF")) {
  setGeneric("reprojectHDF", function(hdfName,filename,MRTpath, ...)
    standardGeneric("reprojectHDF"))
}

if (!isGeneric("ModisDownload")) {
  setGeneric("ModisDownload", function(x,h,v,dates, ...)
    standardGeneric("ModisDownload"))
}

setMethod("setNASAauth", "ANY",
          function(username,password,update=FALSE,echo=TRUE) {
            if (missing(update)) update <- FALSE
            if (is.null(.rtsOptions$getOption(n = 'nasaAuth'))) {
              if (!file.exists(paste0(Sys.getenv('HOME'),'/.netrc'))) {
                if (missing(username) | missing(password)) stop('username and/or password should be defined!')
                if (!is.character(username) | !is.character(password)) stop('username and password, both should be character!')
                file.create(paste0(Sys.getenv('HOME'),'/.netrc'))
                f <- file(paste0(Sys.getenv('HOME'),'/.netrc'),'w')
                cat("machine urs.earthdata.nasa.gov login",username,"password",password,"\n",file=f)
                close(f)
                Sys.chmod(paste0(Sys.getenv('HOME'),'/.netrc'),"0600")
              } else {
                f <- file(paste0(Sys.getenv('HOME'),'/.netrc'),'r')
                a <- readLines(f)
                close(f)
                if (length(a) > 0) {
                  b <- lapply(a,function(x) strsplit(x,' ')[[1]])
                  b <- lapply(b,function(x) {if ('' %in% x) x[x != ''] else x})
                  w <- which(unlist(lapply(b,length)) == 0)
                  if (length(w) > 0) b <- b[-w]
                  b <- unlist(lapply(b,function(x) {if (x[1] == 'machine' & x[2] == "urs.earthdata.nasa.gov") TRUE else FALSE}))
                  if (!any(b)) {
                    if (missing(username) | missing(password)) stop('username and/or password should be defined!')
                    if (!is.character(username) | !is.character(password)) stop('username and password, both should be character!')
                    f <- file(paste0(Sys.getenv('HOME'),'/.netrc'),'a')
                    cat("\nmachine urs.earthdata.nasa.gov login",username,"password",password,"\n",file=f)
                    close(f)
                    Sys.chmod(paste0(Sys.getenv('HOME'),'/.netrc'),"0600")
                  }
                }
              }
              if (!file.exists(paste0(Sys.getenv('HOME'),'/.urs_cookies'))) file.create(paste0(Sys.getenv('HOME'),'/.urs_cookies'))
              .rtsOptions$addOption('nasaAuth',list(netrc.file=paste0(Sys.getenv('HOME'),'/.netrc'),
                                                    cookiefile=paste0(Sys.getenv('HOME'),'/.urs_cookies')))
              if (echo) cat('username and password are successfully added!')
            } else if (update) {
              if (!file.exists(paste0(Sys.getenv('HOME'),'/.netrc'))) {
                if (missing(username) | missing(password)) stop('username and/or password should be defined!')
                file.create(paste0(Sys.getenv('HOME'),'/.netrc'))
                f <- file(paste0(Sys.getenv('HOME'),'/.netrc'),'w')
                cat("machine urs.earthdata.nasa.gov login",username,"password",password,"\n",file=f)
                close(f)
                Sys.chmod(paste0(Sys.getenv('HOME'),'/.netrc'),"0600")
              } else {
                f <- file(paste0(Sys.getenv('HOME'),'/.netrc'),'r')
                a <- readLines(f)
                close(f)
                if (length(a) > 0) {
                  b <- lapply(a,function(x) strsplit(x,' ')[[1]])
                  b <- lapply(b,function(x) {if ('' %in% x) x[x != ''] else x})
                  w <- which(unlist(lapply(b,length)) == 0)
                  if (length(w) > 0) b <- b[-w]
                  bb <- unlist(lapply(b,function(x) {if (x[1] == 'machine' & x[2] == "urs.earthdata.nasa.gov") TRUE else FALSE}))
                  if (!any(bb)) {
                    f <- file(paste0(Sys.getenv('HOME'),'/.netrc'),'a')
                    cat("\nmachine urs.earthdata.nasa.gov login",username,"password",password,"\n",file=f)
                    close(f)
                    Sys.chmod(paste0(Sys.getenv('HOME'),'/.netrc'),"0600")
                  } else {
                    w <- which(bb)
                    for (i in 1:length(b)) b[[i]] <- paste(b[[i]],collapse=' ')
                    b[w[1]] <- paste("machine urs.earthdata.nasa.gov login",username,"password",password)
                    f <- file(paste0(Sys.getenv('HOME'),'/.netrc'),'w')
                    for (bb in b) cat(bb,'\n',file=f)
                    close(f)
                    Sys.chmod(paste0(Sys.getenv('HOME'),'/.netrc'),"0600")
                  }
                }
              }
              if (!file.exists(paste0(Sys.getenv('HOME'),'/.urs_cookies'))) file.create(paste0(Sys.getenv('HOME'),'/.urs_cookies'))
              .rtsOptions$addOption('nasaAuth',list(netrc.file=paste0(Sys.getenv('HOME'),'/.netrc'),
                                                    cookiefile=paste0(Sys.getenv('HOME'),'/.urs_cookies')))
              if (echo) cat('\nusername and password are successfully updated...!')
            } else {
              if (echo) cat('\nusername and password are already exist; to update, use update=TRUE!')
            }
          }
)


setMethod("setMRTpath", "ANY",
          function(MRTpath,update=FALSE,echo=TRUE) {
            if (missing(update)) update <- FALSE
            if (is.null(.rtsOptions$getOption(n = 'MRTpath'))) {
              
              if (!file.exists(paste0(Sys.getenv('HOME'),'/.MRTpath'))) {
                if (missing(MRTpath)) stop('MRTpath should be defined!')
                if (!is.character(MRTpath)) stop('MRTpath should be a character!')
                
                MRTpath <- .normalizePath(MRTpath)
                
                file.create(paste0(Sys.getenv('HOME'),'/.MRTpath'))
                f <- file(paste0(Sys.getenv('HOME'),'/.MRTpath'),'w')
                cat(paste0("MRTpath:",MRTpath,"\n"),file=f)
                close(f)
                Sys.chmod(paste0(Sys.getenv('HOME'),'/.MRTpath'),"0600")
              } else {
                f <- file(paste0(Sys.getenv('HOME'),'/.MRTpath'),'r')
                a <- readLines(f)
                close(f)
                if (length(a) > 0) {
                  b <- lapply(a,function(x) strsplit(x,':')[[1]])
                  b <- unlist(lapply(b,function(x) {if (x[1] == 'MRTpath' & x[2] == MRTpath) TRUE else FALSE}))
                  if (!b) {
                    if (missing(MRTpath)) stop('MRTpath should be defined!')
                    if (!is.character(MRTpath)) stop('MRTpath should be a character!')
                    
                    MRTpath <- .normalizePath(MRTpath)
                    
                    f <- file(paste0(Sys.getenv('HOME'),'/.MRTpath'),'w')
                    cat(paste0("MRTpath:",MRTpath,"\n"),file=f)
                    close(f)
                    Sys.chmod(paste0(Sys.getenv('HOME'),'/.MRTpath'),"0600")
                  }
                }
              }
              if (echo) cat('MRTpath is successfully added!')
            } else if (update) {
              if (missing(MRTpath)) stop('MRTpath should be defined!')
              if (!is.character(MRTpath)) stop('MRTpath should be a character!')
              MRTpath <- .normalizePath(MRTpath)
              
              file.create(paste0(Sys.getenv('HOME'),'/.MRTpath'))
              f <- file(paste0(Sys.getenv('HOME'),'/.MRTpath'),'w')
              cat(paste0("MRTpath:",MRTpath,"\n"),file=f)
              close(f)
              Sys.chmod(paste0(Sys.getenv('HOME'),'/.MRTpath'),"0600")
              if (echo) cat('\nMRTpath is successfully updated...!')
            } else {
              if (echo) cat('MRTpath does already exist; to update, use update=TRUE!')
            }
            
            f <- file(paste0(Sys.getenv('HOME'),'/.MRTpath'),'r')
            a <- readLines(f)
            close(f)
            #if (length(a) > 0) .rtsOptions$addOption('MRTpath',lapply(a,function(x) strsplit(x,':')[[1]])[[1]][2])
            if (length(a) > 0) .rtsOptions$addOption('MRTpath', lapply(a, function(x) strsplit(x,'MRTpath:')[[1]])[[1]][2])
            
          }
)

setMethod("mosaicHDF", "character",
          function(hdfNames,filename,MRTpath,bands_subset,delete=FALSE) {
            if (length(hdfNames) < 2) stop("mosaic cannot be called for ONE image!")
            if (missing(bands_subset))  bands_subset <- ''
            if (missing(delete)) delete <- FALSE
            
            if (missing(MRTpath)) {
              if (is.null(.rtsOptions$getOption('MRTpath'))) stop('MRTpath is not provided & is not set in the package; you can set it using setMRTpath only one time, and then everytime it can be used everywhere in the package!')
              else MRTpath <- .rtsOptions$getOption('MRTpath')
            } else {
              setMRTpath(MRTpath,echo=FALSE)
              MRTpath <- .rtsOptions$getOption('MRTpath')
            }
            
            
            if (Sys.getenv('MRT_DATA_DIR') == '') {
              Sys.setenv(MRT_DATA_DIR=.getMRTdata(MRTpath))
            }
            
            mosaicname = file(paste(MRTpath, "/TmpMosaic.prm", sep=""), open="wt")
            write(paste(getwd(),"/",hdfNames[1], sep=""), mosaicname)
            for (j in 2:length(hdfNames)) write(paste(getwd(),"/",hdfNames[j], sep=""),mosaicname,append=T)
            close(mosaicname)
            # generate mosaic:
            
            if (bands_subset != '') {
              e <- system(paste(MRTpath, '/mrtmosaic -i ', MRTpath, '/TmpMosaic.prm -s "',bands_subset,'" -o ',getwd(), '/',filename, sep=""))
              if (e != 0) warning ("Mosaic failed! 'bands_subset' may has incorrect structure!")
            } else {
              e <- system(paste(MRTpath, '/mrtmosaic -i ', MRTpath, '/TmpMosaic.prm -o ',getwd(), '/',filename, sep=""))
              if (e != 0) warning ("Mosaic failed!")
            }
            if (delete & e == 0) for (ModisName in hdfNames) unlink(paste(getwd(), '/', ModisName, sep=""))
            if (e == 0) return (TRUE)
            else return (FALSE)
          }
)


setMethod("reprojectHDF", "character",
          function(hdfName,filename,MRTpath,UL="",LR="",resample_type='NEAREST_NEIGHBOR',proj_type='UTM',
                   bands_subset='',proj_params='0 0 0 0 0 0 0 0 0 0 0 0',datum='WGS84',utm_zone=NA,pixel_size=1000) {
            
            if (missing(MRTpath)) {
              if (is.null(.rtsOptions$getOption('MRTpath'))) stop('MRTpath is not provided & is not set in the package; you can set it using setMRTpath only one time, and then everytime it can be used everywhere in the package!')
              else MRTpath <- .rtsOptions$getOption('MRTpath')
            } else {
              setMRTpath(MRTpath,echo=FALSE)
              MRTpath <- .rtsOptions$getOption('MRTpath')
            }
            
            if (Sys.getenv('MRT_DATA_DIR') == '') {
              Sys.setenv(MRT_DATA_DIR=.getMRTdata(MRTpath))
            }
            
            fname = file('tmp.prm', open="wt")
            write(paste('INPUT_FILENAME = ', getwd(), '/',hdfName, sep=""), fname) 
            if (bands_subset != '') {
              write(paste('SPECTRAL_SUBSET = ( ',bands_subset,' )',sep=''),fname,append=TRUE)
            }
            if (UL[1] != '' & LR[1] != '') {
              write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', fname, append=TRUE)
              write(paste('SPATIAL_SUBSET_UL_CORNER = ( ', as.character(UL[1]),' ',as.character(UL[2]),' )',sep=''), fname, append=TRUE)
              write(paste('SPATIAL_SUBSET_LR_CORNER = ( ', as.character(LR[1]),' ',as.character(LR[2]),' )',sep=''), fname, append=TRUE)
            }
            write(paste('OUTPUT_FILENAME = ', filename, sep=""), fname, append=TRUE)
            write(paste('RESAMPLING_TYPE = ',resample_type,sep=''), fname, append=TRUE)
            write(paste('OUTPUT_PROJECTION_TYPE = ',proj_type,sep=''), fname, append=TRUE)
            write(paste('OUTPUT_PROJECTION_PARAMETERS = ( ',proj_params,' )',sep=''), fname, append=TRUE)
            write(paste('DATUM = ',datum,sep=''), fname, append=TRUE)
            if (proj_type == 'UTM') write(paste('UTM_ZONE = ',utm_zone,sep=''), fname, append=TRUE)
            write(paste('OUTPUT_PIXEL_SIZE = ',as.character(pixel_size),sep=''), fname, append=TRUE)
            close(fname)
            e <- system(paste(MRTpath, '/resample -p ',getwd(),'/','tmp.prm', sep=''))
            if (e == 0) return (TRUE)
            else return(FALSE)
          }
          
)


setMethod("getMODIS", "character",
          function(x,h,v,dates,version='005',forceReDownload=TRUE,ncore='auto') {
            if (is.null(.rtsOptions$getOption(n = 'nasaAuth'))) stop('Downloading these data requires a NASA Earthdata Login username and password. \n To obtain a NASA Earthdata Login account, please visit: https://urs.earthdata.nasa.gov/users/new/.\n When you get your username and password, then use the setNASAauth function (only first time) to set the username and password on this machine')
            else {
              opt <- .rtsOptions$getOption(n = 'nasaAuth')
              opt <- RCurl::curlOptions(netrc=TRUE, netrc.file=opt$netrc.file,
                                        cookiefile=opt$cookiefile,
                                        followlocation=TRUE)
            }
            # palfaro @ 2017-01-02
            # Max amount of parallel downloads. If package parallel is not available use only 1
            if (requireNamespace('parallel', quietly = TRUE)) { 
              nc <- parallel::detectCores()
              if (!missing(ncore) && is.character(ncore) && tolower(ncore) %in% c('auto','a','au')) ncore <- min(nc,4)
              else if (is.numeric(ncore)) ncore <- min(nc,ncore)
              else ncore <- 1L
            } else { ncore <- 1L }
            
            if (ncore > 4) {
              ncore <- 4
              warning('ncore is set to 4 (cannot be greater...!)')
            }
            xx <- .modisHTTP(x,v=version,opt=opt)
            Modislist <- .getModisList(xx,h=h,v=v,dates=dates,opt=opt,forceReDownload=forceReDownload,nc=ncore)
            if (length(Modislist) == 0) stop("There is NO available images for the specified product!")
            
            dirs <- names(Modislist)
            out <- data.frame(matrix(nrow=length(dirs),ncol=3))
            out[,1] <- dirs
            dc <- 1
            # palfaro @ 2017-07-13
            # create the curl handle for use in .downloadHTTP
            # This is a bit convoluted but we need the .rtsOptions$getOption('MD_curlHandle') created in the global environment for
            # getModisName to find it. In the parallel case it gets created in the global environment in the clusterEvalQ call
            # but if we call the same expression here it gets created in the current environment, thus we use assign
            #assign(".rtsOptions$getOption('MD_curlHandle')", RCurl::getCurlHandle(), envir = .GlobalEnv)
            #.._MD_curlHandle <<- RCurl::getCurlHandle()
            for (d in dirs) {
              dwnld <- rep(FALSE,length(Modislist[[d]]))
              cnt <- 1
              out[dc,2] <- length(dwnld)
              for (ModisName in Modislist[[d]]) {
                n <- strsplit(ModisName,"/")[[1]]
                n <- n[length(n)]
                if (.downloadHTTP(ModisName,n,opt=opt,forceReDownload=forceReDownload)) dwnld[cnt] <- TRUE
                cnt <- cnt + 1
              }
              out[dc,3] <- length(which(dwnld))
              dc <- dc+1
            }
            #rm(.._MD_curlHandle, envir = .GlobalEnv)
            
            if (sum(out[,3]) > 0) {
              cat(paste('from ', sum(out[,2]),' available images, ',sum(out[,3]),' images are successfully downloaded.',sep=''))
            } else cat('Download is failed!')
            
          }
)


setMethod("getMODIS", "numeric",
          function(x,h,v,dates,version='005',forceReDownload=TRUE,ncore='auto') {
            if (is.null(.rtsOptions$getOption(n = 'nasaAuth'))) stop('Downloading these data requires a NASA Earthdata Login username and password. \n To obtain a NASA Earthdata Login account, please visit: https://urs.earthdata.nasa.gov/users/new/.\n When you get your username and password, then use the setNASAauth function (only first time) to set the username and password on this machine')
            else {
              opt <- .rtsOptions$getOption(n = 'nasaAuth')
              opt <- RCurl::curlOptions(netrc=TRUE, netrc.file=opt$netrc.file,
                                        cookiefile=opt$cookiefile,
                                        followlocation=TRUE)
            }
            
            if (requireNamespace('parallel', quietly = TRUE)) { 
              nc <- parallel::detectCores()
              if (!missing(ncore) && is.character(ncore) && tolower(ncore) %in% c('auto','a','au')) ncore <- min(nc,4)
              else if (is.numeric(ncore)) ncore <- min(nc,ncore)
              else ncore <- 1L
            } else { ncore <- 1L }
            
            if (ncore > 4) {
              ncore <- 4
              warning('ncore is set to 4 (cannot be greater...!)')
            }
            
            xx <- .modisHTTP(x,v=version,opt=opt)
            Modislist <- .getModisList(xx,h=h,v=v,dates=dates,opt=opt,forceReDownload=forceReDownload,nc=ncore)
            if (length(Modislist) == 0) stop("There is NO available images for the specified product!")
            
            dirs <- names(Modislist)
            out <- data.frame(matrix(nrow=length(dirs),ncol=3))
            out[,1] <- dirs
            dc <- 1
            # palfaro @ 2017-07-13
            # create the curl handle for use in .downloadHTTP
            # This is a bit convoluted but we need the .rtsOptions$getOption('MD_curlHandle') created in the global environment for
            # getModisName to find it. In the parallel case it gets created in the global environment in the clusterEvalQ call
            # but if we call the same expression here it gets created in the current environment, thus we use assign
            #assign(".rtsOptions$getOption('MD_curlHandle')", RCurl::getCurlHandle(), envir = .GlobalEnv)
            #.._MD_curlHandle <<- RCurl::getCurlHandle()
            
            for (d in dirs) {
              dwnld <- rep(FALSE,length(Modislist[[d]]))
              cnt <- 1
              out[dc,2] <- length(dwnld)
              for (ModisName in Modislist[[d]]) {
                n <- strsplit(ModisName,"/")[[1]]
                n <- n[length(n)]
                if (.downloadHTTP(ModisName,n,opt=opt,forceReDownload=forceReDownload)) {
                  dwnld[cnt] <- TRUE
                  cat('=')
                } else cat('0')
                cnt <- cnt + 1
              }
              out[dc,3] <- length(which(dwnld))
              dc <- dc+1
            }
            #rm(.._MD_curlHandle, envir = .GlobalEnv)
            
            cat('\n')
            if (sum(out[,3]) > 0) {
              cat(paste('from ', sum(out[,2]),' available images, ',sum(out[,3]),' images are successfully downloaded.',sep=''))
            } else cat('Download is failed!')
            
          }
)


setMethod("ModisDownload", "character",
          function(x,h,v,dates,version='005',MRTpath,mosaic=FALSE,bands_subset='',delete=FALSE,proj=FALSE,UL="",LR="",resample_type='NEAREST_NEIGHBOR',proj_type='UTM', proj_params='0 0 0 0 0 0 0 0 0 0 0 0',datum='WGS84',utm_zone=NA,pixel_size=getNativePixelSize(x),forceReDownload=TRUE,ncore='auto') {
            
            if (is.null(.rtsOptions$getOption(n = 'nasaAuth'))) stop('Downloading these data requires a NASA Earthdata Login username and password. \n To obtain a NASA Earthdata Login account, please visit: https://urs.earthdata.nasa.gov/users/new/.\n When you get your username and password, then use the setNASAauth function (only first time) to set the username and password on this machine')
            else {
              opt <- .rtsOptions$getOption(n = 'nasaAuth')
              opt <- RCurl::curlOptions(netrc=TRUE, netrc.file=opt$netrc.file,
                                        cookiefile=opt$cookiefile,
                                        followlocation=TRUE)
            }
            
            
            if (!missing(MRTpath) && !is.null(MRTpath)) {
              setMRTpath(MRTpath,echo=FALSE)
              MRTpath <- .rtsOptions$getOption('MRTpath')
              if (Sys.getenv('MRT_DATA_DIR') == '') {
                Sys.setenv(MRT_DATA_DIR=.getMRTdata(MRTpath))
              }
            } else {
              if (!is.null(.rtsOptions$getOption('MRTpath'))) {
                MRTpath <- .rtsOptions$getOption('MRTpath')
              }
              if (Sys.getenv('MRT_DATA_DIR') == '') {
                Sys.setenv(MRT_DATA_DIR=.getMRTdata(MRTpath))
              }
            }
            
            if (requireNamespace('parallel', quietly = TRUE)) { 
              nc <- parallel::detectCores()
              if (!missing(ncore) && is.character(ncore) && tolower(ncore) %in% c('auto','a','au')) ncore <- min(nc,4)
              else if (is.numeric(ncore)) ncore <- min(nc,ncore)
              else ncore <- 1L
            } else { ncore <- 1L }
            
            if (ncore > 4) {
              ncore <- 4
              warning('ncore is set to 4 (cannot be greater...!)')
            }
            
            dHDF <- .getMODIS(x, h, v, dates, version,opt=opt, forceReDownload = forceReDownload,nc=ncore)
            dHDF$Date <- as.character(dHDF$Date)
            dHDF$Name <- as.character(dHDF$Name)
            if (nrow(dHDF) < 2) mosaic <- FALSE
            
            if (mosaic) {
              
              du <- unique(dHDF$Date)
              
              for (d in du) {
                dw <- dHDF[which(dHDF$Date == d),]
                if (nrow(dw) > 1){
                  date_name <- sub(sub(pattern="\\.", replacement="-", d), pattern="\\.", replacement="-", d)
                  name <- paste("Mosaic_",date_name,".hdf",sep='')
                  Mosaic.success <- mosaicHDF(dw[,2],name,MRTpath=MRTpath,bands_subset=bands_subset,delete=delete)
                  if (Mosaic.success) {
                    if (delete) for (ModisName in dw[,2]) unlink(paste(getwd(), '/', ModisName, sep=""))
                    if (proj) {
                      pref <- strsplit(dw[1,2],'\\.')[[1]][1]
                      e <- reprojectHDF(name,filename=paste(pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                      if (e & delete) unlink(paste(name))
                      if (!e) warning (paste("The procedure has failed to REPROJECT the mosaic image for date ",d,"!",sep=""))
                    }
                  } else {
                    warning(paste("The procedure has failed to MOSAIC the images for date ",d,"!",sep=""))
                    if (proj) {
                      warning ("Since the mosaic is failed, the individual hdf images are reprojected...")
                      pref <- strsplit(dw[1,2],'\\.')[[1]]
                      pref <- paste(pref[1],"_",pref[3],sep="")
                      for (ModisName in dw[,2]) {
                        e <- reprojectHDF(ModisName,filename=paste(pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,bands_subset=bands_subset,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                        if (e & delete) unlink(paste(ModisName))
                        if (!e) warning (paste("The procedure has failed to REPROJECT the individual HDF image ",ModisName,"!",sep=""))
                      }
                    }
                  } 
                }
              }   
            } else {
              if (proj) {
                for (i in 1:nrow(dHDF)) {
                  ModisName <- dHDF[i,2]
                  pref <- strsplit(ModisName,'\\.')[[1]]
                  pref <- paste(pref[1],"_",pref[3],sep="")
                  d <- dHDF[i,1]
                  date_name <- sub(sub(pattern="\\.", replacement="-", d), pattern="\\.", replacement="-", d)
                  e <- reprojectHDF(ModisName,filename=paste(pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,bands_subset=bands_subset,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                  if (e & delete) unlink(paste(ModisName))
                  if (!e) warning (paste("The procedure has failed to REPROJECT the individual HDF image ",ModisName,"!",sep=""))
                }
              }
            }
            
          }
)


setMethod("ModisDownload", "numeric",
          function(x,h,v,dates,version='005',MRTpath,mosaic=FALSE,bands_subset='',delete=FALSE,proj=FALSE,UL="",LR="",resample_type='NEAREST_NEIGHBOR',proj_type='UTM', proj_params='0 0 0 0 0 0 0 0 0 0 0 0',datum='WGS84',utm_zone=NA,pixel_size,forceReDownload=TRUE,ncore='auto') {
            if (is.null(.rtsOptions$getOption(n = 'nasaAuth'))) stop('Downloading these data requires a NASA Earthdata Login username and password. \n To obtain a NASA Earthdata Login account, please visit: https://urs.earthdata.nasa.gov/users/new/.\n When you get your username and password, then use the setNASAauth function (only first time) to set the username and password on this machine')
            else {
              opt <- .rtsOptions$getOption(n = 'nasaAuth')
              opt <- RCurl::curlOptions(netrc=TRUE, netrc.file=opt$netrc.file,
                                        cookiefile=opt$cookiefile,
                                        followlocation=TRUE)
            }
            
            if (!missing(MRTpath) && !is.null(MRTpath)) {
              setMRTpath(MRTpath,echo=FALSE)
              MRTpath <- .rtsOptions$getOption('MRTpath')
              if (Sys.getenv('MRT_DATA_DIR') == '') {
                Sys.setenv(MRT_DATA_DIR=.getMRTdata(MRTpath))
              }
            } else {
              if (!is.null(.rtsOptions$getOption('MRTpath'))) {
                MRTpath <- .rtsOptions$getOption('MRTpath')
              }
              if (Sys.getenv('MRT_DATA_DIR') == '') {
                Sys.setenv(MRT_DATA_DIR=.getMRTdata(MRTpath))
              }
            }
            
            if (requireNamespace('parallel', quietly = TRUE)) { 
              nc <- parallel::detectCores()
              if (!missing(ncore) && is.character(ncore) && tolower(ncore) %in% c('auto','a','au')) ncore <- min(nc,4)
              else if (is.numeric(ncore)) ncore <- min(nc,ncore)
              else ncore <- 1L
            } else { ncore <- 1L }
            
            if (ncore > 4) {
              ncore <- 4
              warning('ncore is set to 4 (cannot be greater...!)')
            }
            
            dHDF <- .getMODIS(x,h,v,dates,version,forceReDownload=forceReDownload,nc=ncore)
            dHDF$Date <- as.character(dHDF$Date)
            dHDF$Name <- as.character(dHDF$Name)
            if (nrow(dHDF) < 2) mosaic <- FALSE
            if (mosaic) {
              
              du <- unique(dHDF$Date)
              
              for (d in du) {
                dw <- dHDF[which(dHDF$Date == d),]
                if (nrow(dw) > 1){
                  date_name <- sub(sub(pattern="\\.", replacement="-", d), pattern="\\.", replacement="-", d)
                  name <- paste("Mosaic_",date_name,".hdf",sep='')
                  Mosaic.success <- mosaicHDF(dw[,2],name,MRTpath=MRTpath,bands_subset=bands_subset,delete=delete)
                  if (Mosaic.success) {
                    if (delete) for (ModisName in dw[,2]) unlink(paste(getwd(), '/', ModisName, sep=""))
                    if (proj) {
                      pref <- strsplit(dw[1,2],'\\.')[[1]][1]
                      e <- reprojectHDF(name,filename=paste(pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                      if (e & delete) unlink(paste(name))
                      if (!e) warning (paste("The procedure has failed to REPROJECT the mosaic image for date ",d,"!",sep=""))
                    }
                  } else {
                    warning(paste("The procedure has failed to MOSAIC the images for date ",d,"!",sep=""))
                    if (proj) {
                      warning ("Since the mosaic is failed, the individual hdf images are reprojected...")
                      pref <- strsplit(dw[1,2],'\\.')[[1]]
                      pref <- paste(pref[1],"_",pref[3],sep="")
                      for (ModisName in dw[,2]) {
                        e <- reprojectHDF(ModisName,filename=paste(pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,bands_subset=bands_subset,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                        if (e & delete) unlink(paste(ModisName))
                        if (!e) warning (paste("The procedure has failed to REPROJECT the individual HDF image ",ModisName,"!",sep=""))
                      }
                    }
                  } 
                }
              }   
            } else {
              if (proj) {
                for (i in 1:nrow(dHDF)) {
                  ModisName <- dHDF[i,2]
                  pref <- strsplit(ModisName,'\\.')[[1]]
                  pref <- paste(pref[1],"_",pref[3],sep="")
                  d <- dHDF[i,1]
                  date_name <- sub(sub(pattern="\\.", replacement="-", d), pattern="\\.", replacement="-", d)
                  e <- reprojectHDF(ModisName,filename=paste(pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,bands_subset=bands_subset,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                  if (e & delete) unlink(paste(ModisName))
                  if (!e) warning (paste("The procedure has failed to REPROJECT the individual HDF image ",ModisName,"!",sep=""))
                }
              }
            }
            
          }
)
