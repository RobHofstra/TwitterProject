library(BMS)

unic2utf8 <- function(hex.org) {
  hex.cln <- gsub("U+", "", hex.org, fixed = TRUE)
  hex.spl <- strsplit(hex.cln, " ", fixed = TRUE)
  hex.unl <- unlist(hex.spl)
  res     <- list()
  
  for (i in (1:length(hex.unl))) {
    bcp <- hex2bin(hex.unl[i])
    while(length(bcp) < 21){
      bcp <- c(0, bcp)
    }
    
    byte1 <- c(1, 1, 1, 1, 0, bcp[1:3])
    byte2 <- c(1, 0, bcp[4:9])
    byte3 <- c(1, 0, bcp[10:15])
    byte4 <- c(1, 0, bcp[16:21])
    
    hex1 <- bin2hex(byte1)
    hex2 <- bin2hex(byte2)
    hex3 <- bin2hex(byte3)
    hex4 <- bin2hex(byte4)
    
    res[i] <- paste0("<", hex1, "><", hex2, "><", hex3, "><", hex4, ">")
  }
  
  return(paste(unlist(res),sep = " ", collapse = " "))
}

emoji5$R.Encoding <- lapply(emoji5$Code, FUN = unic2utf8)
emoji5$R.Encoding <- as.character(emoji5$R.Encoding)