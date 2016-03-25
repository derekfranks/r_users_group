library('RCurl')
library('XML')


readTxt <- function(x) {
  tmp <- paste(readLines(x), collapse=" ")
  output <- gsub("_", " ", tmp)
  return(output)
}

getConcepts <- function(x, key) {
  endpoint <- "http://gateway-a.watsonplatform.net/calls/text/TextGetRankedConcepts"
  tmp <- postForm(endpoint, apikey = key, text = x, linkedData = 0, style = 'post')
  tree <- xmlTreeParse(tmp, useInternalNodes = T)
  top <- xmlRoot(tree)
  
  concepts <- top[["concepts"]]
  dat <- xmlToDataFrame(concepts, stringsAsFactors = FALSE)
  names(dat)[1] <- "concept"
  dat$relevance <- as.numeric(dat$relevance)
  
  return(dat)
}

getKeywords <- function(x, key) {
  endpoint <- "http://gateway-a.watsonplatform.net/calls/text/TextGetRankedKeywords"
  foo <- postForm(endpoint, apikey = key, text = x, sentiment = 1, style = 'post')
  bar <- xmlTreeParse(foo, useInternalNodes = T)
  top <- xmlRoot(bar)
  
  keywords <- top[["keywords"]]
  
  baz <- xmlElementsByTagName(keywords, "sentiment", recursive = TRUE)
  sentiment <- xmlToDataFrame(baz, stringsAsFactors = FALSE)
  
  names(sentiment) <- c("mixed", "score", "sentiment")
  sentiment[is.na(sentiment)] <- 0
  sentiment$score <- as.numeric(sentiment$score)
  sentiment$mixed <- as.logical(as.numeric(sentiment$mixed))
  sentiment <- sentiment[, c(3,2,1)]
  
  
  dat <- xmlToDataFrame(keywords, stringsAsFactors = FALSE)
  dat <- dat[, c(3,1)]
  names(dat)[1] <- "keyword"
  dat$relevance <- as.numeric(dat$relevance)
  
  output <- data.frame(dat, sentiment)
  return(output)
}


getEntities <- function(x, key) {
  endpoint <- "http://gateway-a.watsonplatform.net/calls/text/TextGetRankedNamedEntities"
  foo <- postForm(endpoint, apikey = key, text = x, sentiment = 1, style = 'post')
  bar <- xmlTreeParse(foo, useInternalNodes = T)
  top <- xmlRoot(bar)
  
  entities <- top[["entities"]]
  baz <- xmlElementsByTagName(entities, "sentiment", recursive = TRUE)
  sentiment <- xmlToDataFrame(baz, stringsAsFactors = FALSE)
  
  names(sentiment)[1] <- "sentiment"
  sentiment[is.na(sentiment)] <- 0
  sentiment$score <- as.numeric(sentiment$score)
  sentiment$mixed <- as.logical(as.numeric(sentiment$mixed))
  
  dat <- xmlToDataFrame(entities, stringsAsFactors = FALSE)
  names(dat)[5] <- "entity"
  
  dat$type <- factor(dat$type)
  dat$relevance <- as.numeric(dat$relevance)
  dat$count <- as.numeric(dat$count)
  dat <- dat[, c(5,1,2,4,6)]
  
  
  output <- data.frame(dat[, 1:4], sentiment, disambiguation = dat[,5])
  return(output)
}



analyzeText <- function(docList, apikey, type) {
  dat <- lapply(docList, readTxt)
  if (type == "concept") {
    tmp <- vector(mode = "list", length = length(docList))
    for (i in seq_along(docList)) {
      tmp[[i]] <- getConcepts(dat[i], key = apikey)
      tmp[[i]]$docID <- i
    }
    output <- do.call(rbind, tmp)
    
  }
  
  if (type == "keyword") {
    tmp <- vector(mode = "list", length = length(docList))
    for (i in seq_along(docList)) {
      tmp[[i]] <- getKeywords(dat[i], key = apikey)
      tmp[[i]]$docID <- i
    }
    output <- do.call(rbind, tmp)
    
  }
  
  if (type == "entity") {
    tmp <- vector(mode = "list", length = length(docList))
    for (i in seq_along(docList)) {
      tmp[[i]] <- getEntities(dat[i], key = apikey)
      tmp[[i]]$docID <- i
    }
    output <- do.call(rbind, tmp)
    
  }
  return(output)
  
}


