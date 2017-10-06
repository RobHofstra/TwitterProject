packages <- function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x, repos="file:///Y:/miniCRAN", lib = "file:///C:/Program Files/Microsoft/R Client/R_SERVER/library")
    require(x,character.only=TRUE)
  }
}

packages(stringr)
packages(rebus)
packages(qdap)
# load emoticon conversion table
emojis <- read.csv2("emoticonsExt.csv", stringsAsFactors = F)
# remove spaces in R.Encoding
emojis$R.Encoding <- gsub(" ", "", emojis$R.Encoding)
# create a copy of the names with emoticons in byte notation
name.bytes <- iconv(name, "Latin1", "ASCII", "byte")

############## BEGIN LOOP

err.txt <- tryCatch(sort.list(name), error=function(e) e)
err.txt

# create search string
chars<- str_split(err.txt,"'")[[1]][2]
chars.bytes <- iconv(chars, "Latin1", "ASCII", "byte")
bytes <- unlist(ex_non_ascii(chars))
for (byte in bytes) {
  chars.bytes <- sub(byte, "", chars.bytes)
} 
chars <- chars.bytes
# locate all names with search string
namId <- grep(chars, name, fixed = T)
# extract emoticon in byte notation
bytes <- sub(chars, "", name.bytes[namId])[1]
name.bytes[namId]

# remove symbols (only leave emoticons)
temp <- ""
while (TRUE) {
  if (substr(bytes, 2, 2) == "e") {
    bytes <- substr(bytes, 13, (nchar(bytes) - 12))
  }
  if (substr(bytes, 2, 2) == "f") {
    temp <- paste0(temp, substr(bytes, 1, 16))
    bytes <- substr(bytes, 17, nchar(bytes))
  }
  if (nchar(bytes) == 0) break()
}
bytes <- temp
bytes

temp <- ""
desc <- ""
while (TRUE) {
  if (nchar(bytes) >= 80 && length(grep(substr(bytes, 1, 80), emojis$R.Encoding))) {
    emoID <- grep(substr(bytes, 1, 80), emojis$R.Encoding)
    desc  <- paste0(desc, "(", emojis$CLDR.Short.Name[emoID], ")")
    temp  <- paste0(temp, substr(bytes, 1, 80))
    bytes <- substr(bytes, 81, nchar(bytes))
  } else {
    if (nchar(bytes) >= 64 && length(grep(substr(bytes, 1, 64), emojis$R.Encoding))) {
      emoID <- grep(substr(bytes, 1, 64), emojis$R.Encoding)
      desc  <- paste0(desc, "(", emojis$CLDR.Short.Name[emoID], ")")
      temp  <- paste0(temp, substr(bytes, 1, 64))
      bytes <- substr(bytes, 65, nchar(bytes))
    } else {
      if (nchar(bytes) >= 48 && length(grep(substr(bytes, 1, 48), emojis$R.Encoding))) {
        emoID <- grep(substr(bytes, 1, 48), emojis$R.Encoding)
        desc  <- paste0(desc, "(", emojis$CLDR.Short.Name[emoID], ")")
        temp  <- paste0(temp, substr(bytes, 1, 48))
        bytes <- substr(bytes, 49, nchar(bytes))
      } else {
        if (nchar(bytes) >= 32 && length(grep(substr(bytes, 1, 32), emojis$R.Encoding))) {
          emoID <- grep(substr(bytes, 1, 32), emojis$R.Encoding)
          desc  <- paste0(desc, "(", emojis$CLDR.Short.Name[emoID], ")")
          temp  <- paste0(temp, substr(bytes, 1, 32))
          bytes <- substr(bytes, 33, nchar(bytes))
        } else {
          emoID <- grep(substr(bytes, 1, 16), emojis$R.Encoding)
          desc  <- paste0(desc, "(", emojis$CLDR.Short.Name[emoID], ")")
          temp  <- paste0(temp, substr(bytes, 1, 16))
          bytes <- substr(bytes, 17, nchar(bytes))
        }
      }
    }
  }
  if (nchar(bytes) == 0) break()
}

desc
name[namId] <- gsub(temp, desc, name.bytes[namId])
name[namId]



