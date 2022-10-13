# 11464 AR/VR for Data Analysis and Communication
# Project on Data Visualisation 
#
# Group: (v) forgo(r) 
# Tutorial: Tuesday 3:30-5.30
#
# Viriya Duch Sam u3211355
# Matthew Borowski u3214653
# Ethan Bendeich u3216357

# Load libraries
library(dplyr)
library(ggplot2)

# Set working directory
wd <- getwd()
setwd(wd)
getwd()

# Read data sets 
netflix <- read.csv("netflix_titles.csv")
hulu <- read.csv("hulu_titles.csv")
disney <- read.csv("disney_plus_titles.csv")
amazon <- read.csv("amazon_prime_titles.csv")

# Preview data frames
head(netflix)
head(hulu)
head(disney)
head(amazon)

# Add new column for each streaming platform
netflix <- netflix %>% mutate(platform = "Netflix")
hulu <- hulu %>% mutate(platform = "Hulu")
disney <- disney %>% mutate(platform = "Disney+")
amazon <- amazon %>% mutate(platform = "Amazon Prime")

# Concatenate data frames

df <- do.call("rbind", list(netflix, disney, hulu, amazon))

# Preview
head(df)

# Breakdown data cleaning tasks
# Check for missing values in any column
print(sapply(df, function(x) sum(is.na(x))))

# Check data types
str(df)

# Convert date_added column to Date format
df$date_added <- as.Date.character(df$date_added, format="%B %d, %Y")
str(df$date_added)

# Comparison across contents (e.g., Movies vs TV Show, Distributions of Genres, 
# Ratings, Duration types…)

## Comparison of Movies vs TV Shows in Total
ggplot(df, aes(x = type)) +
  coord_flip() +
  geom_bar(fill=c("lightblue", "navy")) +
  ggtitle("Comparison of Total Movies vs TV Shows")

## Comparison of Movies vs TV Shows Across Streaming Platforms
ggplot(df, aes(x = platform, fill=type)) +
  geom_bar(position = position_dodge()) +
  ggtitle("Comparison of Movies vs TV Shows Across Streaming Platforms")

## Top 10 Rating
top10ratingsdf <- as.data.frame(sort(table(df$rating), 
                                     decreasing=TRUE)[1:10])
top10ratingsdf_nx <- as.data.frame(sort(table(df[df$platform == 'Netflix', ]$rating), 
                                        decreasing=TRUE)[1:10])
top10ratingsdf_hl <- as.data.frame( sort(table(df[df$platform == 'Hulu', ]$rating), 
                                         decreasing=TRUE)[1:10])
top10ratingsdf_dn <- as.data.frame(sort(table(df[df$platform == 'Disney+', ]$rating), 
                                        decreasing=TRUE)[1:10])
top10ratingsdf_ap <- as.data.frame(sort(table(df[df$platform == 'Amazon Prime', ]$rating), 
                                        decreasing=TRUE)[1:10])

colnames(top10ratingsdf) <- c("rating", "count")
colnames(top10ratingsdf_nx) <- c("rating", "count")
colnames(top10ratingsdf_hl) <- c("rating", "count")
colnames(top10ratingsdf_dn) <- c("rating", "count")
colnames(top10ratingsdf_ap) <- c("rating", "count")

## Comparison of Top 10 Ratings Distribution
top10ratingsdf %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = paste0(rating,"\n", round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar("y", start=0) +
  ggtitle("Top 10 Ratings")

## Comparison of Netflix's Top 10 Ratings Distribution
top10ratingsdf_nx %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = paste0(rating,"\n", round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Netflix's Top 10 Ratings")

## Comparison of Hulu's Top 10 Ratings Distribution
top10ratingsdf_hl %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = paste0(rating,"\n", round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Hulu's Top 10 Ratings")

## Comparison of Disney+'s Top 10 Ratings Distribution
top10ratingsdf_dn %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = paste0(rating,"\n", round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Disney+'s Top 10 Ratings")

## Comparison of Amazon Prime's Top 10 Ratings Distribution
top10ratingsdf_ap %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = paste0(rating,"\n", round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Amazon Prime's Top 10 Ratings")