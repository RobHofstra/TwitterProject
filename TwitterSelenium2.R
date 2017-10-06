#
# Script for Twitter webscraping with RSelenium
# 
library('RSelenium')

# emoji                  <- read.csv2("emDict.csv",stringsAsFactors = FALSE, encoding = "UTF-8")
# emoji$R.Native         <- iconv(emoji$Native,"latin1","ASCII","byte")

tweet_id               <- vector(mode = "numeric")
item_id                <- vector(mode = "numeric")
permalink_path         <- vector(mode = "character")
conversation_id        <- vector(mode = "numeric")
is_reply_to            <- vector(mode = "logical")
has_parent_tweet       <- vector(mode = "logical")
tweet_nonce            <- vector(mode = "character")
tweet_stat_initialized <- vector(mode = "logical")
screen_name            <- vector(mode = "character")
name                   <- vector(mode = "character")
user_id                <- vector(mode = "numeric")
mentions               <- vector(mode = "character")
tweet_timestamp        <- vector(mode = "character")
tweet_text             <- vector(mode = "character")
reply_to               <- vector(mode = "numeric")
has_cards              <- vector(mode = "logical")
card_url               <- vector(mode = "character")
card_title             <- vector(mode = "character")


rD <- rsDriver()
remDr <- rD[["client"]]

# remDr$navigate("https://twitter.com/search?q=Trump&src=typd")
# webElem <- remDr$findElement(using = 'class', "ItemSearch")
# webElem$highlightElement()
# webElem$sendKeysToElement(list("Trump", key = "enter"))

# remDr$navigate("https://twitter.com/search?q=LoggerFestival&src=typd")
remDr$navigate("https://twitter.com/search?q=Vlaardingen")

# first      <- 1
lastHeight <- list(0)
newHeight  <- remDr$executeScript("return document.body.scrollHeight")
while (as.numeric(newHeight[[1]]) != as.numeric(lastHeight[[1]])) {
  lastHeight <- newHeight
  remDr$executeScript(script = "window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(2)
  first <- length(tweet_id) + 1
  
  webElems <- remDr$findElements(using = 'css selector', ".js-original-tweet")
  for (i in (first:length(webElems))) {
    webElem <- webElems[[i]]
    remDr$mouseMoveToLocation(webElement = webElem)
    if (i==1) Sys.sleep((2))
    webElem$highlightElement()
    tweet_id[i]               <- as.numeric(webElem$getElementAttribute("data-tweet-id")) 
    item_id[i]                <- as.numeric(webElem$getElementAttribute("data-item-id")) 
    permalink_path[i]         <- as.character(webElem$getElementAttribute("data-permalink-path")) 
    conversation_id[i]        <- as.numeric(webElem$getElementAttribute("data-conversation-id")) 
    is_reply_to[i]            <- if (length(webElem$getElementAttribute("data-is-reply-to")) != 0 && webElem$getElementAttribute("data-is-reply-to") == "true") TRUE else FALSE
    has_parent_tweet[i]       <- if (length(webElem$getElementAttribute("data-has-parent-tweet")) != 0 && webElem$getElementAttribute("data-has-parent-tweet") == "true") TRUE else FALSE
    tweet_nonce[i]            <- as.character(webElem$getElementAttribute("data-tweet-nonce")) 
    tweet_stat_initialized[i] <- if (webElem$getElementAttribute("data-tweet-stat-initialized") == "true") TRUE else FALSE
    screen_name[i]            <- as.character(webElem$getElementAttribute("data-screen-name")) 
    name[i]                   <- as.character(webElem$getElementAttribute("data-name")) 
    user_id[i]                <- as.character(webElem$getElementAttribute("data-user-id")) 
    mentions[i]               <- if (length(webElem$getElementAttribute("data-mentions"))!=0) as.character(webElem$getElementAttribute("data-mentions")) else ""
    webElemTim                <- webElem$findChildElement(using = "css selector", ".tweet-timestamp")
    tweet_timestamp[i]        <- as.character(webElemTim$getElementAttribute("title"))
    webElemTxt                <- webElem$findChildElement(using = "css selector", ".tweet-text")
    tweet_text[i]             <- as.character(webElemTxt$getElementText())
    if (is_reply_to[i]) {
      webElemRpl                <- webElem$findChildElement(using = "css selector", ".js-user-profile-link")
      reply_to[i]               <- as.character(webElemRpl$getElementAttribute("data-user-id"))
    } else {
      reply_to[i]               <- ""
    }
    has_cards[i]              <- if (length(webElem$getElementAttribute("data-has-cards")) != 0 && webElem$getElementAttribute("data-has-cards") == "true") TRUE else FALSE
    # if (has_cards[i]) {
    #   webElemCrd                <- webElem$findChildElement(using = "css selector", ".card-type-summary")
    #   card_url[i]               <- as.character(webElemCrd$getElementAttribute("data-card-url"))
    # } else {
    #   card_url[i]               <- ""
    # }
    # webElems                  <- remDr$findElements(using = 'css selector', ".js-original-tweet")
  }
  if (tweet_timestamp[i] != "" && strftime(strptime(gsub(". ", " ", tweet_timestamp[i], fixed = TRUE), format = "%I:%M %p - %d %b %Y")) < as.Date("2017-1-1")) break()
  newHeight <- remDr$executeScript("return document.body.scrollHeight")
}


# Put the timestamp in 'standard' notation by switching back and forth to Posix notation after removing the abbreviation dot.
# tweet_timestamp  <- strftime(strptime(gsub(". ", " ", tweet_timestamp, fixed = TRUE), format = "%H:%M - %d %b %Y"))
# tweet_ts2 <- tweet_timestamp
tweet_timestamp2  <- strftime(strptime(gsub(". ", " ", tweet_timestamp, fixed = TRUE), format = "%I:%M %p - %d %b %Y"))

twitter.df <- data.frame(tweet_id, item_id, permalink_path, conversation_id, is_reply_to, has_parent_tweet,
                         tweet_nonce, tweet_stat_initialized, screen_name, name, user_id, mentions,
                         tweet_timestamp2, tweet_text)

write.csv2(twitter.df, "tweets.vlaardingen.csv", row.names = FALSE)

rD[["server"]]$stop() 
rm(rD)
