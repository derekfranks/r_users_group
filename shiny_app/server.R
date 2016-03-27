
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  readTxt <- function(x) {
    tmp <- paste(readLines(x), collapse=" ")
    output <- gsub("\\t", " ", tmp)
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
    sentiment$score[is.na(sentiment$score)] <- 0
    sentiment$score <- as.numeric(sentiment$score)
    sentiment$mixed[is.na(sentiment$mixed)] <- 0
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
    sentiment$score[is.na(sentiment$score)] <- 0
    sentiment$score <- as.numeric(sentiment$score)
    sentiment$mixed[is.na(sentiment$mixed)] <- 0
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
  
  plotTreeMap <- function(x, type) {
    if (type == "concept") {
      df <- x %>% group_by(concept) %>%
        summarize(count = n(), relevanceAvg = mean(relevance)) %>%
        arrange(desc(relevanceAvg)) %>% as.data.frame()
      treemap(df, index = "concept", vSize = "count", 
              vColor = "relevanceAvg", type = "value",
              title = "Concept Treemap", title.legend = "Relevance")
    }
    if (type == "entity") {
      df <- x %>% group_by(entity, type) %>% 
        summarize(countSum = sum(count), 
                  relevanceAvg = weighted.mean(relevance, count),
                  scoreAvg = weighted.mean(score, count)) %>%
        arrange(desc(relevanceAvg), desc(scoreAvg)) %>%
        as.data.frame()
      treemap(df, index = "entity", vSize = "countSum", 
              vColor = "scoreAvg", type = "value",
              title = "Entity Treemap", title.legend = "Sentiment Score")
    }
    if (type == "keyword") {
      df <- x %>% group_by(keyword) %>% 
        summarize(count = n(), 
                  relevanceAvg = mean(relevance),
                  scoreAvg = mean(score)) %>%
        arrange(desc(relevanceAvg), desc(scoreAvg)) %>%
        as.data.frame()
      treemap(df, index = "keyword", vSize = "count", 
              vColor = "scoreAvg", type = "value",
              title = "Keyword Treemap", title.legend = "Sentiment Score")
      
    }
  }
  
  createTable <- reactive({
    analyzeText(input$textFile[[4]], 
                apikey = "YOUR_KEY_HERE",
                type = input$type)
  })
  
  createPlot <- reactive({
    plotTreeMap(x = createTable(), type = input$type)
  })
  
  
  
  observeEvent(input$go, {
    
    output$outputTable <- renderDataTable({
      
      if (is.null(input$textFile))
        return(NULL)
      
      createTable()
      
    })
    
    output$treeMap <- renderPlot({
      createPlot()
    })
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {paste(input$type,'.csv', sep = "")},
    content = function(file) {write.csv(createTable(), file, row.names = FALSE)})
  
  
})
