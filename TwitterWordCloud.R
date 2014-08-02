library(twitteR)
library(tm)
library(wordcloud)
library(SnowballC)


getUser <- function(handle ='Wacken') {
        return(handle)
}

connectToTwitter <- function() {
        APIKey <- 'APIKey'
        APISecret <- 'APISecret'
        AcessToken <- 'AcessToken'
        AcessTokenSecret <- 'AcessTokenSecret'
        
        setup_twitter_oauth(consumer_key = APIKey, consumer_secret = APISecret,
                            access_token = AcessToken, access_secret = AcessTokenSecret)
        
}


getTwitterDf <- function(x = getUser()) {
        user <- searchTwitter(x, n=100, lang='en')
        user <- twListToDF(user)
        message('Received tweets:', dim(user)[1])
        message('Since:', min(as.Date(user$created)))
        return(user)
}

buildACorpus <- function( rawDF = getTwitterDf() ) {
        #To remove bad enconding signaling character encoding failures
        rawDF$text <- gsub("[^[:alnum:]///' ]", "", rawDF$text)
        corpus <- VCorpus(VectorSource(rawDF$text))
        inspect(corpus[1:3])
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, content_transformer(removeNumbers))
        corpus <- tm_map(corpus, content_transformer(removePunctuation))
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        #dictionary <- corpus
        corpus <- tm_map(corpus, stemDocument)
        #Since stemCompletion is causing strwidth problems, I'll avoid it ATM
        #corpus <- tm_map(corpus, content_transformer(stemCompletion), dictionary=dictionary)
        return (TermDocumentMatrix(corpus))
}

builWordCloud <- function( x = buildACorpus() ) {
        x <- as.matrix(x)
        View(x)
        sorted <- sort(rowSums(x), decreasing=TRUE)
        dfnames <- names(sorted)
        myDF <- data.frame(word=dfnames, freq=sorted)
        View(myDF)
        png(filename='TwitterWordCloud.png')
        wordcloud(myDF$word, myDF$freq, min.freq=3)
        dev.off()
}


builWordCloud()