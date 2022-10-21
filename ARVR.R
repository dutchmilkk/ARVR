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
library(tseries)
library(tidyverse)
library(astsa, quietly=TRUE, warn.conflicts=FALSE)

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
print(sapply(df, function(x) sum(x == "")))

# Check data types
str(df)

# Convert date_added column to Date format
df$date_added <- as.Date.character(df$date_added, format="%B %d, %Y")
str(df$date_added)

# Comparison across contents (e.g., Movies vs TV Show, Distributions of 
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
clean_rating <- df[!(is.na(df$rating) | df$rating==""), ]

top10ratingsdf <- as.data.frame(sort(table(clean_rating$rating), 
                                     decreasing=TRUE)[1:10])
top10ratingsdf_nx <- as.data.frame(sort(table(
  clean_rating[clean_rating$platform == 'Netflix', ]$rating), 
                                        decreasing=TRUE)[1:10])
top10ratingsdf_hl <- as.data.frame( sort(table(
  clean_rating[clean_rating$platform == 'Hulu', ]$rating), 
                                         decreasing=TRUE)[1:10])
top10ratingsdf_dn <- as.data.frame(sort(table(
  clean_rating[clean_rating$platform == 'Disney+', ]$rating), 
                                        decreasing=TRUE)[1:9]) # Disney plus only has 9 unique ratings
top10ratingsdf_ap <- as.data.frame(sort(table(
  clean_rating[clean_rating$platform == 'Amazon Prime', ]$rating), 
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
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Top 10 Ratings")

## Comparison of Netflix's Top 10 Ratings Distribution
top10ratingsdf_nx %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=2) +
  coord_polar("y", start=0) +
  ggtitle("Netflix's Top 10 Ratings")

## Comparison of Hulu's Top 10 Ratings Distribution
top10ratingsdf_hl %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
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
  geom_text(aes(label = paste0(round(perc, 2),"%")), 
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
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Amazon Prime's Top 10 Ratings")

## Comparison of Top 10 Duration Types
clean_duration <- df[!(is.na(df$duration) | df$duration==""), ]

top10durationdf <- as.data.frame(sort(table(clean_duration$duration), 
                                      decreasing=TRUE)[1:10])
top10durationdf_nx <- as.data.frame(sort(table(
  clean_duration[clean_duration$platform == 'Netflix', ]$duration), 
  decreasing=TRUE)[1:10])
top10durationdf_hl <- as.data.frame( sort(table(
  clean_duration[clean_duration$platform == 'Hulu', ]$duration), 
  decreasing=TRUE)[1:10])
top10durationdf_dn <- as.data.frame(sort(table(
  clean_duration[clean_duration$platform == 'Disney+', ]$duration), 
  decreasing=TRUE)[1:9]) # Disney plus only has 9 unique ratings
top10durationdf_ap <- as.data.frame(sort(table(
  clean_duration[clean_duration$platform == 'Amazon Prime', ]$duration), 
  decreasing=TRUE)[1:10])

colnames(top10durationdf) <- c("duration", "count")
colnames(top10durationdf_nx) <- c("duration", "count")
colnames(top10durationdf_hl) <- c("duration", "count")
colnames(top10durationdf_dn) <- c("duration", "count")
colnames(top10durationdf_ap) <- c("duration", "count")

## Top 10 Duration (All)
top10durationdf %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Top 10 Duration on All Platforms")

## Top 10 Duration (Netflix)
top10durationdf_nx %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Top 10 Duration on Netflix")

## Top 10 Duration (Hulu)
top10durationdf_hl %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Top 10 Duration on Hulu")

## Top 10 Duration (Disney+)
top10durationdf_dn %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Top 10 Duration on Disney+")

## Top 10 Duration (Amazon Prime)
top10durationdf_ap %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y", start=0) +
  ggtitle("Top 10 Duration on Amazon Prime")

## Autocorrelation of Total Movies and Shows Added per Year
year_releases = format(df$date_added, format="%Y")
releases_per_year = as.data.frame(sort(table(year_releases)))
colnames(releases_per_year) <- c("year", "releases")

ALLAddedPerYear = releases_per_year$releases

## Plot Autcorrelation of Yearly Added Shows and Movies
acf(ALLAddedPerYear, lag.max=15)

