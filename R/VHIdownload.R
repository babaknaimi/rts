# Author: Babak Naimi, naimi.b@gmail.com
# First Date :  March 2018
# Last Update :  Oct. 2018
# Version 1.1
# Licence GPL v3


.updateVHIfileList <- function() {
  .website <- 'ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/'
  .vhiList <- RCurl::getURL(.website)
  .vhiList <- strsplit(.vhiList,'\n')[[1]]
  .vhiList <- lapply(.vhiList,function(s) strsplit(s,' ')[[1]])
  .vhiList <- unlist(lapply(.vhiList,function(s) s[length(s)]))
  .Prdoucts <- unlist(lapply(.vhiList,function(s) strsplit(s,'\\.')[[1]][7]))
  .d <- strsplit(.vhiList,'\\.')
  .d <- unlist(lapply(.d,function(x) .strRM(x[5])))
  .d <- as.Date(unlist(lapply(.d,.yw2ymd)))
  .vhiProd <- data.frame(VHI_products=.vhiList,date=.d)
  .path <- system.file("external", package="rts")
  saveRDS(.vhiProd,file = paste0(.path,'/VHIproducts.rds'))
}
#--------------
.getVHIFileList <- function(d) {
  .vhiProd <- system.file("external/VHIproducts.rds", package="rts")
  if (.vhiProd == "") {
    .updateVHIfileList() 
    .vhiProd <- system.file("external/VHIproducts.rds", package="rts")
    .vhiProd <- readRDS(.vhiProd)
  } else {
    .vhiProd <- readRDS(.vhiProd)
    if (!any(.vhiProd$date >= as.Date(d))) {
      .updateVHIfileList()
      .vhiProd <- readRDS(system.file("external/VHIproducts.rds", package="rts"))
    }
  }
  .vhiProd
}

#--------
.trim <- function (x) {
  x <- strsplit(x, "")[[1]]
  paste(x[x != " "], collapse = "")
}



#----
# convert YearWeek (e.g., 1981001, 1981051, for the weeks 1 and 51 of the year 1981)
# to Year-Month-Day
.yw2ymd <- function(x) {
  x <- strsplit(as.character(.trim(x)),'')[[1]]
  y <- paste0(x[1:4],collapse = '')
  d <- (as.numeric(paste0(x[6:7],collapse = '')) - 1) * 7 + 1
  if (d < 10) d <- paste0('00',d)
  else if (d < 100) d <- paste0('0',d)
  else d <- as.character(d)
  y <- paste0(y,d)
  as.character(as.POSIXct(y,format='%Y%j'))
}
#-------------
.strRM <- function(x,w=1) {
  # remove w character from the begining of a string
  paste(strsplit(x,'')[[1]][-c(1:w)],collapse='')
}

#-----------
.getProductIndex <- function(pList,product) {
  s <- strsplit(as.character(pList),'\\.')
  s <- sapply(s, function(x) x[length(x)-1])
  u <- unique(s)
  product <- product[toupper(product) %in% u]
  if (length(product) == 0) stop(paste('the input product(s) should be selected from:',paste(u,collapse = ', ')))
  which(s %in% product)
}
#------
#---------

.getVHI <- function(x) {
  f = RCurl::CFILE(x, mode="wb")
  xx <- paste0('ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/',x)
  #xx <- paste0('ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/VHP_4km/geo_TIFF/',x)
  er2 <- try(RCurl::curlPerform(url = xx, curl=RCurl::getCurlHandle(), writedata = f@ref))
  RCurl::close(f)
}
#---------

if (!isGeneric("VHPdownload")) {
  setGeneric("VHPdownload", function(x,dates,rts,ncore,...)
    standardGeneric("VHPdownload"))
}

setMethod("VHPdownload", "character",
          function(x,dates,rts=TRUE,ncore=1L,...) {
            if (!x[1] %in%  c('VHI','VCI','SMN','SMT','TCI')) stop("x should be one of 'VHI','VCI','SMN','SMT', or'TCI'...!")
            else  x <- x[1]
            #------
            if (missing(rts)) rts <- TRUE
            # -----
            if (requireNamespace('parallel', quietly = TRUE)) { 
              nc <- parallel::detectCores()
              if (!missing(ncore) && is.character(ncore) && tolower(ncore) %in% c('auto','a','au')) ncore <- min(nc,4)
              else if (is.numeric(ncore)) ncore <- min(nc,ncore)
              else ncore <- 1L
            } else ncore <- 1L
            #------
            if (inherits(dates,"character")) {
              if ('.' %in% strsplit(dates[1],'')[[1]]) dates <- as.Date(dates,format='%Y.%m.%d')
              else if ('-' %in% strsplit(dates[1],'')[[1]]) dates <- as.Date(dates,format='%Y-%m-%d')
              
              if (any(is.na(dates))) stop('dates is not appropriately specified; the safe way is to introduce them as  a Date object!')
            }
            if (length(dates) == 0) stop("dates is not appropriately selected!")
            dates <- sort(dates)
            
            .fileList <- .getVHIFileList(d=dates[2])
            .fileList <- .fileList[.fileList$date >= dates[1] & .fileList$date <= dates[2],]
            if (nrow(.fileList) == 0) stop('no products are available within the specified dates range!')
            #----------
            w <- .getProductIndex(.fileList$VHI_products,x)
            dL <- as.character(.fileList$VHI_products)[w]
            if (ncore > 1L) {
              cl <- parallel::makeCluster(getOption("cl.cores", ncore))
              parallel::clusterExport(cl, c("dL"))
              
              parallel::clusterEvalQ(cl, {
                library(RCurl)
                library(rts)
              })
              
              ww <- parallel::parLapply(cl,dL,.getVHI)
              parallel::stopCluster(cl)
            } else {
              ww <- lapply(dL, .getVHI)
            }
            
            ww <- sapply(dL, file.exists)
            if (all(ww)) cat(paste0('\n All ',length(ww),' files of ',x,' product are successfully downloaded...!\n'))
            else if (any(ww)) cat(paste0('\n From ',length(ww),' existing files of ',x,' product, ',length(which(ww)),' files are successfully downloaded...!\n'))
            else stop('\nNone of the files are downloaded!!')
            
            if (rts) {
              r <- try(rts(stack(as.character(.fileList$VHI_products[w])[ww]),.fileList$date[w][ww]),silent = TRUE)
              if (inherits(r,'try-error')) cat('Raster Time Series is not created, but the files are downloaded in the working directory!')
              else {
                for (i in 1:nlayers(r@raster)) {
                  r@raster@layers[[i]]@file@nodatavalue <- -9999
                }
                return(r)
              }
            } else TRUE
            
          }
)

