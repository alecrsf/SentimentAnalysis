#==================== Setup =======================
setwd("~/Desktop/Study/Informatics/R/Analysis/Text Mining/Sentiment Analysis of Twitter")
library("rtweet")

token <- create_token(
	app = "alecrsf",
	consumer_key = "y2UYQ5ztwCFjO9Ev58NHpqOlj",
	consumer_secret = "eW1oBMT4eGE5OU0BDNHcRdyfMzKUa2l5AowGwNUWgBWPaewG0k",
	access_token = "1542104426558722049-UdmAqlcSdHVOnsvKpWAsTl482nNaE1",
	access_secret = "OY7vnJcNzUpT8gnP7hYKR9UbRxjCmKYdqPG7ECkXiZwrP")

#==================== Tweets =====================

# fast_fashion <- search_tweets(
# 	'"fashion rent"', since='2022-03-01', until='2022-05-02'
# 	n = 10000, include_rts = FALSE,
# 	type = "mixed", verbose = T)
# 
# 
# fast_fashion %>% write_as_csv("twitter.csv")

fast_fashion <- read_twitter_csv("twitter.csv")

#==================== Tidy =======================
library(tidyverse)
library(tidytext)

data <- fast_fashion %>% 
	select(screen_name, text, lang, location) 


clean_tweets <- function(x) {
	#data$text <- gsub("http\\s+","", data$text)
	x %>%
		# Remove URLs
		str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
		# Remove mentions e.g. "@my_account"
		str_remove_all("@[[:alnum:]_]{4,}") %>%
		# Remove hashtags
		str_remove_all("#[[:alnum:]_]+") %>%
		# Replace "&" character reference with "and"
		str_replace_all("&amp;", "and") %>%
		# Remove puntucation, using a standard character class
		str_remove_all("[[:punct:]]") %>%
		# Remove "RT: " from beginning of retweets
		str_remove_all("^RT:? ") %>%
		# Replace any newline characters with a space
		str_replace_all("\\\n", " ") %>%
		# Make everything lowercase
		str_to_lower() %>%
		# Remove any trailing whitespace around the text
		str_trim("both")
}

data$text %<>% clean_tweets()
data$location %<>% clean_tweets() 


#==================== Stemming =======================
# Stemming words and removing "stopwords"
stemmed <- data %>% select(text) %>% 
	unnest_tokens(word, text) %>% 
	anti_join(stop_words)

# Get sentiments
stemmed %<>% inner_join(get_sentiments("bing")) %>% 
	count(word, sentiment, sort=T) %>% ungroup()

stemmed <- stemmed %>% mutate(
	word = as.factor(word),
	sentiment = as.factor(sentiment),
	n = as.numeric(n)) 



#==================== Viz =======================
stemmed %>% 
	arrange(desc(n)) %>% 
	top_n(21, "n") %>% 
	slice(2:20) %>% 
	data.frame() %>% 
  ggplot(aes(x = fct_reorder(word, n), 
  			 		y = n,
  					fill = sentiment)) +
	ggchicklet::geom_chicklet(stat = "identity") +
	theme_bw() +
	coord_flip() +
	labs(
		title = "Parole più usate nei tweet", 
		subtitle = "Escluso ovviamente 'fast' che è compresa in ogni ricerca", 
		x = "",
		y = "")



ts_plot(fast_fashion, "days") +
	labs(title = "#fastfashion tweets per day", 
			 x = "Time/day", y = "Number of tweets") +
	theme_bw()

#==================== Geo =======================
key = "BFeqRCP2VvtUdmOcApTZ20z4rhll50wA"
# location <- osm_search(data$location, key)

location %<>% select(name = display_name,
										 lat, lon, class, type) 


library(leaflet)
leaflet() %>% addTiles() %>% 
	addCircleMarkers(
		data = location,
		lat = ~lat, lng = ~lon,
		clusterOptions = markerClusterOptions()
	)

#==================== Table =======================

