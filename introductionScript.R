# IBM Watson and R

# First we need a couple of packages
# We'll use RCurl to create the API requests and XML to parse the results
library('RCurl')
library('XML')



# We also need an API Key
apikey <- "YOUR_API_KEY_HERE"



# Since we'll be accessing the alchemyLanguage APIs in this case, we also need
# some text for "Watson" to analyze
source <- "YOUR .TXT DOC HERE"
dat <- paste(readLines(source), collapse = " ")



# Let's start with extracting concepts from the text
# First, we need an endpoint
endpointConcept <- "http://gateway-a.watsonplatform.net/calls/text/TextGetRankedConcepts"



# Once we have our endpoint (found in the API documentation), we can submit a
# request using `postForm` from the RCurl package
concepts <- postForm(endpointConcept, apikey = apikey, text = dat, linkedData = 0, style = 'post')
concepts



# The API returns the data in XML form, which is sort of readable, but we can 
# make it much easier to work with
conceptsXML <- xmlTreeParse(concepts, useInternalNodes = T)
conceptsXML
root <- xmlRoot(conceptsXML)



# Now the data is much more readable, and we could actually work with it from 
# here if we wanted
root[[2]]
root[[4]]
root[[5]]
root[["concepts"]]
root[[5]][[2]]
root[[5]][[2]][[1]]

# But it would be a lot easier if we could just move this into a data frame
conceptsdf <- xmlToDataFrame(root[["concepts"]], stringsAsFactors = FALSE)
conceptsdf
names(conceptsdf)[1] <- "concept"
conceptsdf

# We do have one other issue - our numbers have been read in as characters!
sapply(conceptsdf, class)
conceptsdf[[2]] <- as.numeric(conceptsdf[[2]])
conceptsdf


# Now we have a dataframe we can work with - getting Entities and Keywords are 
# very similar, but a bit more complicated
endpointEntities <- "http://gateway-a.watsonplatform.net/calls/text/TextGetRankedNamedEntities"
entities <- postForm(endpointEntities, apikey = apikey, text = dat, sentiment = 1, style = 'post')
entitiesXML <- xmlTreeParse(entities, useInternalNodes = T)
rootEntities <- xmlRoot(entitiesXML)
rootEntities
entitiesdf <- xmlToDataFrame(rootEntities[[6]])

entitiesdf

# What happened?  The sentiment variable isn't being correctly parsed
rootEntities[[6]][[1]][[3]]
rootEntities[[6]][[2]][[3]]


# Thankfully, there's a way to deal with this
rootEntitiesSentiment<- xmlElementsByTagName(rootEntities[[6]], "sentiment", recursive = TRUE)
rootEntitiesSentiment
sentimentdf<- xmlToDataFrame(rootEntitiesSentiment)
sentimentdf

# Now we just replace the NAs with 0 and makre sure the values are numeric/logical
sentimentdf[is.na(sentimentdf)] <- 0
sentimentdf

sapply(sentimentdf, class)
sentimentdf$score <- as.numeric(sentimentdf$score)
sentimentdf$mixed <- as.logical(as.numeric(sentimentdf$mixed))
sentimentdf

# rename the columns
names(sentimentdf)[1:2] <- c("sentiment", "sentimentScore")
sentimentdf


# now let's reorder and fix the classes for the entitiesdf
sapply(entitiesdf, class)
names(entitiesdf)[5] <- "entity"

entitiesdf$type <- factor(entitiesdf$type)
entitiesdf$relevance <- as.numeric(entitiesdf$relevance)
entitiesdf$count <- as.numeric(entitiesdf$count)

entitiesdf <- entitiesdf[, c(5,1,2,4,6)] # we drop column 3 (sentiment)
entitiesdf <- data.frame(entitiesdf, sentimentdf)
entitiesdf



# We can save ourselves a little bit of this effort if we use JSON output instead
require(jsonlite)
entities <- postForm(endpointEntities, apikey = apikey, text = dat, sentiment = 1, style = 'post', outputMode = "json")
tmp <- fromJSON(entities)
tmp
df <- tmp[[6]]

# We still have to correct the column classes, but it's simpler than the previous approach
sapply(df, class)

df2 <- df[[3]]
df3 <- df[[6]]
df <- df[, -c(3, 6)]
df <- data.frame(df, df2, df3)

sapply(df, class)  #There's still work to be done