setwd('~/Desktop/IAP_15S41/SoftwareTools-IAP2018/Session2/')
library(tidyr)
library(dplyr)
library(ggplot2)
listings <- read.csv('data/listings.csv')
ls(listings)

# the 5 metrics I will be looking at are:
metrics = listings %>% select('neighbourhood', 'price', 'review_scores_rating', 'maximum_nights', 'bedrooms', 'accommodates')
metrics %>% head

# want to convert the price column to a numerical price in order to average
clean_price <- function(price) as.numeric(gsub('\\$|,', '', price))
metrics %>% mutate(numeric_price = clean_price(price))
metrics %>% head

# displaying avg price in neighborhoods where avg price is over 200, ignoring null values
avg_high_price_graph <- metrics %>% 
  group_by(neighbourhood) %>%
  summarise(average_price = mean(price, na.rm = TRUE)) %>%
  filter(average_price > 200) %>%
  ggplot(aes(x=neighbourhood, y=average_price)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title='average price is over 200')
avg_high_price_graph

# displaying avg price in neighbourhoods where avg price is lower than 100, ignoring null values
avg_low_price_graph <- metrics %>% 
  group_by(neighbourhood) %>%
  summarise(average_price = mean(price, na.rm = TRUE)) %>%
  filter(average_price < 100) %>%
  ggplot(aes(x=neighbourhood, y=average_price)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title='average price is under 100')
avg_low_price_graph

# graph of neighbourhoods with average rating above 95
review_graph <- metrics %>%
  group_by(neighbourhood) %>%
  summarise(average_rating = mean(review_scores_rating, na.rm = TRUE)) %>%
  filter(average_rating > 95) %>%
  ggplot(aes(x=neighbourhood, y=average_rating)) + 
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title='graph of neighbourhoods with average rating above 95')
review_graph

# graph of neighbourhoods with distributions of maximum nights with an upper bound of one year
stay_length_graph <- metrics %>%
  group_by(neighbourhood) %>%
  ggplot(aes(x=neighbourhood, y=maximum_nights)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=60, hjust=1))+
  ylim(0, 365) +
  labs(title='neighbourhoods vs maximum nights stayable')
stay_length_graph

# accomodations graph in Cambridge
accomodates_graph <- metrics %>% 
  group_by(neighbourhood, price) %>%
  filter(neighbourhood=='Cambridge') %>%
  ggplot(aes(x=accommodates, y=price, color=factor(neighbourhood))) +
  geom_point() +
  labs(title='accomodations in cambridge')
accomodates_graph


  


  
