require(dplyr)
require(treemap)

documents <- dir("YOUR_DATA_DIRECTORY_HERE", full.names = TRUE)
key <- "YOUR_API_KEY_HERE"

concepts <- analyzeText(documents, key, type = "concept")
entities <- analyzeText(documents, key, type = "entity")
keywords <- analyzeText(documents, key, type = "keyword")

# write.csv(concepts, "sample interviews/concepts.csv", row.names = FALSE)
# write.csv(entities, "sample interviews/entities.csv", row.names = FALSE)
# write.csv(keywords, "sample interviews/keywords.csv", row.names = FALSE)

head(concepts)
head(entities[,-8])
head(keywords)

df <- concepts %>% group_by(concept) %>%
  summarize(count = n(), relevanceAvg = mean(relevance)) %>%
  arrange(desc(relevanceAvg)) %>% as.data.frame()
treemap(df, index = "concept", vSize = "count", vColor = "relevanceAvg", type = "value")



df <- entities %>% group_by(entity, type) %>% 
  summarize(countSum = sum(count), 
            relevanceAvg = weighted.mean(relevance, count),
            scoreAvg = weighted.mean(score, count)) %>%
  arrange(desc(relevanceAvg), desc(scoreAvg)) %>%
  as.data.frame()
treemap(df, index = "entity", vSize = "countSum", vColor = "scoreAvg", type = "value")


df <- keywords %>% group_by(keyword) %>% 
  summarize(count = n(), 
            relevanceAvg = mean(relevance),
            scoreAvg = mean(score)) %>%
  arrange(desc(relevanceAvg), desc(scoreAvg)) %>%
  as.data.frame()
treemap(df, index = "keyword", vSize = "count", vColor = "scoreAvg", type = "value")
