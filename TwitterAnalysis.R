packages <- function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x, repos="file:///Y:/miniCRAN", lib = "file:///C:/Program Files/Microsoft/R Client/R_SERVER/library")
    require(x,character.only=TRUE)
  }
}

packages(tm)
packages(qdap)

# Read the twitter file
setwd("C:/BiData")
tweets <- read.csv2("TweetsLoggerFestival.csv", stringsAsFactors = FALSE)
# Transform date fields into dates
tweets$tweet_timestamp <- as.POSIXct(tweets$tweet_timestamp)
# Select tweets from this year only
tweets.2017 <- tweets[tweets$tweet_timestamp >= format.Date("2017-01-1"),]
# tweets.old <- tweets[tweets$tweet_timestamp < format.Date("2017-01-1"),]
# Make a vector source: coffee_source
tweet.source <- VectorSource(tweets.2017$tweet_text)
# Make a volatile corpus: coffee_corpus
tweet.corpus <- VCorpus(tweet.source)
# Apply various preprocessing functions
tm_map(tweet.corpus, removePunctuation)
tm_map(tweet.corpus, stripWhitespace)
tm_map(tweet.corpus, removeNumbers)
tm_map(tweet.corpus, content_transformer(tolower))
tm_map(tweet.corpus, removeWords, stopwords("dutch"))
# Create a Term Document Matrix
tweet.tdm <- TermDocumentMatrix(tweet.corpus)
# Convert to regular Matrix
tweet.m   <- as.matrix(tweet.tdm)
term.freq <- rowSums(tweet.m)
term.freq <- sort(term.freq, decreasing = TRUE)
barplot(term.freq[1:10], col = "tan", las = 2)


# Method 2
tweets_qd <- tweets.2017$tweet_text
tweets_qd1 <- tolower(tweets_qd)
tweets_qd2 <- replace_abbreviation(tweets_qd1)
tweets_qd3 <- replace_contraction(tweets_qd2)
tweets_qd4 <- replace_number(tweets_qd3)
tweets_qd5 <- replace_ordinal(tweets_qd4)
tweets_qd6 <- replace_symbol(tweets_qd5)
tweets_qd7 <- removeWords(tweets_qd6, stopwords("dutch"))
tweets_freq <- termFreq(tweets_qd7)
tweets_frq1 <- sort(tweets_freq, decreasing = TRUE)
barplot(tweets_frq1[1:10], col = "tan", las = 2)
tweets_df <- data.frame(term = names(tweets_frq1), num = tweets_frq1)
wordcloud::wordcloud(tweets_df$term, tweets_df$num.Freq, max.words = 100, colors = "red")
