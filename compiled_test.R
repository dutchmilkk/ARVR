
# Load libraries
library(tseries)
library(tidyverse)
library(astsa, quietly=TRUE, warn.conflicts=FALSE)

# Set working directory
wd <- getwd()
setwd(wd)
getwd()

# Read data sets 
netflix <- read.csv(file.choose())
hulu <- read.csv(file.choose())
disney <- read.csv(file.choose())
amazon <- read.csv(file.choose())

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
# Ratings, Duration types)

## Comparison of Movies vs TV Shows in Total
ggplot(df, aes(x = type)) +
  coord_flip() +
  geom_bar(fill=c("lightblue", "navy")) +
  ggtitle("Comparison of Total Movies vs TV Shows")

## Comparison of Movies vs TV Shows Across Streaming Platforms
ggplot(df, aes(x = platform, fill=type)) +
  geom_bar(position = position_dodge()) +
  ggtitle("Comparison of Movies vs TV Shows Across Streaming Platforms")

## Top 10 Rating and Top 5 across each
clean_rating <- df[!(is.na(df$rating) | df$rating==""), ]

top10ratingsdf <- as.data.frame(sort(table(clean_rating$rating), 
                                     decreasing=TRUE)[1:10])
top5ratingsdf_nx <- as.data.frame(sort(table(
  clean_rating[clean_rating$platform == 'Netflix', ]$rating), 
  decreasing=TRUE)[1:5])
top5ratingsdf_hl <- as.data.frame( sort(table(
  clean_rating[clean_rating$platform == 'Hulu', ]$rating), 
  decreasing=TRUE)[1:5])
top5ratingsdf_dn <- as.data.frame(sort(table(
  clean_rating[clean_rating$platform == 'Disney+', ]$rating), 
  decreasing=TRUE)[1:5])
top5ratingsdf_ap <- as.data.frame(sort(table(
  clean_rating[clean_rating$platform == 'Amazon Prime', ]$rating), 
  decreasing=TRUE)[1:5])

colnames(top10ratingsdf) <- c("rating", "count")
colnames(top5ratingsdf_nx) <- c("rating", "count")
colnames(top5ratingsdf_hl) <- c("rating", "count")
colnames(top5ratingsdf_dn) <- c("rating", "count")
colnames(top5ratingsdf_ap) <- c("rating", "count")

## Comparison of Top 10 Ratings Distribution
top10ratingsdf %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.6), size=4) +
  guides(fill = guide_legend(title = "Group")) +
  coord_polar("y", start=0) +
  ggtitle("Top 10 Ratings")

## Comparison of Netflix's Top 5 Ratings Distribution
top5ratingsdf_nx %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  scale_fill_brewer(palette = "OrRd") +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.6), size=4.5) +
  coord_polar("y", start=0) +
  ggtitle("Netflix's Top 5 Ratings")

## Comparison of Hulu's Top 5 Ratings Distribution
top5ratingsdf_hl %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  scale_fill_brewer(palette = "YlGn") +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=4.5) +
  coord_polar("y", start=0) +
  ggtitle("Hulu's Top 5 Ratings")

## Comparison of Disney+'s Top 5 Ratings Distribution
top5ratingsdf_dn %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  scale_fill_brewer(palette = "PuBu") +
  geom_text(aes(label = paste0(round(perc, 2),"%")), 
            position = position_stack(vjust = 0.5), size=4.5) +
  coord_polar("y", start=0) +
  ggtitle("Disney+'s Top 5 Ratings")

## Comparison of Amazon Prime's Top 5 Ratings Distribution
top5ratingsdf_ap %>% 
  group_by(rating) %>% 
  summarise(rate_sum = sum(count))%>%
  mutate(perc = rate_sum/sum(rate_sum) * 100) %>%
  ggplot(aes(x = "", y = rate_sum, fill = rating))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  scale_fill_brewer(palette = "GnBu") +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=4.5) +
  coord_polar("y", start=0) +
  ggtitle("Amazon Prime's Top 5 Ratings")

## Comparison of Top 10 Duration Types
clean_duration <- df[!(is.na(df$duration) | df$duration==""), ]

top10durationdf <- as.data.frame(sort(table(clean_duration$duration), 
                                      decreasing=TRUE)[1:10])
top5durationdf_nx <- as.data.frame(sort(table(
  clean_duration[clean_duration$platform == 'Netflix', ]$duration), 
  decreasing=TRUE)[1:5])
top5durationdf_hl <- as.data.frame( sort(table(
  clean_duration[clean_duration$platform == 'Hulu', ]$duration), 
  decreasing=TRUE)[1:5])
top5durationdf_dn <- as.data.frame(sort(table(
  clean_duration[clean_duration$platform == 'Disney+', ]$duration), 
  decreasing=TRUE)[1:5]) 
top5durationdf_ap <- as.data.frame(sort(table(
  clean_duration[clean_duration$platform == 'Amazon Prime', ]$duration), 
  decreasing=TRUE)[1:5])

colnames(top10durationdf) <- c("duration", "count")
colnames(top5durationdf_nx) <- c("duration", "count")
colnames(top5durationdf_hl) <- c("duration", "count")
colnames(top5durationdf_dn) <- c("duration", "count")
colnames(top5durationdf_ap) <- c("duration", "count")

## Top 10 Duration (All)
top10durationdf %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.6), size=3.5) +
  coord_polar("y", start=0) +
  ggtitle("Top 10 Duration on All Platforms")

## Top 5 Duration (Netflix)
top5durationdf_nx %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  scale_fill_brewer(palette = "Reds") +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=4.5) +
  coord_polar("y", start=0) +
  ggtitle("Top 5 Duration on Netflix")

## Top 5 Duration (Hulu)
top5durationdf_hl %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  scale_fill_brewer(palette = "Greens") +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=4.5) +
  coord_polar("y", start=0) +
  ggtitle("Top 5 Duration on Hulu")

## Top 10 Duration (Disney+)
top5durationdf_dn %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=4.5) +
  coord_polar("y", start=0) +
  ggtitle("Top 5 Duration on Disney+")

## Top 10 Duration (Amazon Prime)
top5durationdf_ap %>% 
  group_by(duration) %>% 
  summarise(dur_sum = sum(count))%>%
  mutate(perc = dur_sum/sum(dur_sum) * 100) %>%
  ggplot(aes(x = "", y = dur_sum, fill = duration))+
  geom_bar(stat = "identity", width = 1, color="white") + 
  theme_void() +
  scale_fill_brewer(palette = "Purples") +
  geom_text(aes(label = paste0(round(perc,2),"%")), 
            position = position_stack(vjust = 0.5), size=4.5) +
  coord_polar("y", start=0) +
  ggtitle("Top 5 Duration on Amazon Prime")

## Autocorrelation of Total Movies and Shows Added per Year
year_releases = format(df$date_added, format="%Y")
releases_per_year = as.data.frame(sort(table(year_releases)))
colnames(releases_per_year) <- c("year", "releases")

ALLAddedPerYear = releases_per_year$releases

## Plot Autcorrelation of Yearly Added Shows and Movies
acf(ALLAddedPerYear, lag.max=15)

#find all the rows that dont have na for the added_date column
df <- df[complete.cases(df$date_added), ]


#separate out date formatting as its neccesary for the next parts
df$year <- year(ymd(df$date_added))
df$month <- month(ymd(df$date_added)) 
df$day <- day(ymd(df$date_added))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Trends of New Releases across months, seasons, quarters, and years

#mutate so we have weather seasons. (US seasons)
df <- df %>% mutate(season =
                      ifelse(month %in% c(1, 11, 12), "Winter",
                             ifelse(month %in% 2:4, "Spring",
                                    ifelse(month %in% 5:7, "Summer", 
                                           ifelse(month %in% 8:10, "Autumn", "")))))

#set up season order for future graphing
df$season <- factor(df$season, levels = c("Winter", "Spring", "Summer", "Autumn"))

#mutate to have quarters for the year
df <- df %>% mutate(quarter =
                      ifelse(month %in% 1:3, "Q1",
                             ifelse(month %in% 4:6, "Q2",
                                    ifelse(month %in% 7:9, "Q3", 
                                           ifelse(month %in% 10:12, "Q4", "")))))

#unused
#graphs shows added to platform across months of the year.
ggplot(df, aes(x=month, fill=platform))+
  geom_bar(stat="count")+
  #scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_fill_manual( values = c("Netflix"= "#ff0000", "Hulu"   = "#37ff91", "Disney+" = "#001f60","Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  labs(x="Month added to platform",
       y="Number of Releases",
       title="Number of Shows Added Across Each Month",
       color="Platform")+
  theme(axis.text.x=element_text(angle=67.5,hjust=1, vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "hidden")

#used
#graphs across months - facet platform
ggplot(df, aes(x=month, fill=platform))+
  geom_bar(stat="count")+
  #scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_fill_manual( values = c("Netflix"= "#ff0000", "Hulu"   = "#37ff91", "Disney+" = "#001f60","Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  facet_grid(~platform)+
  labs(x="Month added to platform",
       y="Number of Releases",
       title="Number of Shows Added Across Each Month",
       color="Platform")+
  theme(axis.text.x=element_text(angle=67.5,hjust=1, vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "hidden")

#unused
#graph across season - together
ggplot(df, aes(x=season, fill=platform))+
  geom_bar(stat="count")+
  scale_fill_manual( values = c("Netflix"= "#ff0000", "Hulu"   = "#37ff91","Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  labs(x="Season",
       y="Number of Releases",
       title="Content Added by Season",
       color="Platform")+
  theme(axis.text.x=element_text(hjust=0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5))

#used
#graph across season - facet
ggplot(df, aes(x=season, fill=platform))+
  geom_bar(stat="count")+
  scale_fill_manual( values = c("Netflix"= "#ff0000", "Hulu"   = "#37ff91","Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  facet_grid(~platform)+
  labs(x="Season (USA)",
       y="Number of Releases",
       title="Content Added by Season",
       color="Platform")+
  theme(axis.text.x=element_text(hjust=0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "hidden")


#graph across years - together
ggplot(df, aes(x=year, fill=platform))+
  geom_bar(stat="count")+
  scale_fill_manual( values = c("Netflix"= "#ff0000","Hulu" = "#37ff91","Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  labs(x="Year",
       y="Number of Releases",
       title="Content Added Per Year",
       color="Platform")+
  theme(axis.text.x=element_text(hjust=0.5, vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5))

#graph across years - facet platform
ggplot(df, aes(x=year, fill=platform))+
  geom_bar(stat="count")+
  scale_fill_manual( values = c("Netflix"= "#ff0000","Hulu" = "#37ff91","Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  facet_grid(~platform)+
  labs(x="Year",
       y="Number of Releases",
       title="Content Added Per Year",
       subtitle = "separated by platform",
       color="Platform")+
  theme(axis.text.x=element_text(hjust=0.5, vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "hidden")

#used
#graph across years - facet paltform / content type (movie or tv series)
ggplot(df, aes(x=year, fill=platform))+
  geom_bar(stat="count")+
  scale_fill_manual( values = c("Netflix"= "#ff0000","Hulu" = "#37ff91","Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  facet_grid(type~platform)+
  labs(x="Year",
       y="Number of Releases",
       title="Types of Content Added per Year",
       subtitle = "",
       color="Platform")+
  theme(axis.text.x=element_text(hjust=0.5, vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "hidden")

#unused
#graph across years - facet paltform / content type (movie or tv series)
ggplot(df, aes(x=year, fill=platform))+
  geom_bar(stat="count")+
  scale_fill_manual( values = c("Netflix"= "#ff0000","Hulu" = "#37ff91","Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  facet_grid(platform~type)+
  labs(x="Year",
       y="Number of Releases",
       title="Content Added Per Year",
       subtitle = "separated by platform",
       color="Platform")+
  theme(axis.text.x=element_text(hjust=0.5, vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "hidden")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Whether there had been a dip in new shows released during and 
#after the commencement of COVID-19

#release date for new shows on platforms
ggplot(df, aes(x=release_year, fill=platform))+
  geom_histogram(stat="count")+
  scale_fill_manual( values = c("Netflix"= "#ff0000", "Hulu" = "#37ff91", "Disney+" = "#001f60",  "Amazon Prime" = "#00b0f1"))+
  facet_grid(~ platform)+
  theme_dark()+
  labs(x="Year Added to Platform",
       y="Year of Original Release",
       title="Release Year vs Year Added",
       subtitle="compares release year to when a show was added to a platform",
       color="Platform",
       shape="Type")+
  theme(axis.text.x=element_text(angle=67.5,hjust=1, vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5))

#unused
#close look at the content created between 2017 and 2021
ggplot(df, aes(x=release_year, fill=platform))+
  geom_histogram(stat="count")+
  scale_x_continuous(limits = c(2016, 2022))+
  scale_fill_manual( values = c("Netflix"= "#ff0000", "Hulu" = "#37ff91", "Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  facet_grid(~ platform)+
  theme_dark()+
  labs(x="Year Added to Platform",
       y="Year of Original Release",
       title="Release Year vs Year Added",
       subtitle="compares release year to when a show was added to a platform",
       color="Platform",
       shape="Type")+
  theme(axis.text.x=element_text(angle=67.5,hjust=1, vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "hidden")

#used
#close look at the content created between 2017 and 2021
ggplot(df, aes(x=release_year, color=platform))+
  geom_line(stat="count", size=2)+
  scale_x_continuous(limits = c(2016, 2021))+
  scale_color_manual( values = c("Netflix"= "#ff0000", "Hulu" = "#37ff91", "Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  geom_vline(xintercept=2019,lwd=1,colour="black", linetype="dashed")+
  #geom_rect(aes(xmin=2019, xmax=2021, ymin=0, ymax=1135), fill="lightblue", alpha=0.01)+
  labs(x="Number of New Releases",
       y="Year",
       title="New Releases added during the Covid-19 Period",
       subtitle="",
       color="Platform",
       shape="Type")+
  theme(axis.text.x=element_text(vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5))



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Time between actual movie/show release and their releases on the 
#streaming platforms

#taking the year(year added) and taking it away from the release year,
#this should show how long it takes form release to being added to the 
#streaming service.
df$release_diff <- df$year - df$release_year

#used
#plot for different paltform ages of content when it was added
ggplot(df, aes(y=release_diff, color=platform))+
  geom_boxplot()+
  facet_grid(~platform)+
  scale_color_manual( values = c("Netflix"= "#ff0000", "Hulu" = "#37ff91", "Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  labs(x="",
       y="Year of Original Release",
       title="Year that Content was Released on each Platform",
       color="Platform")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "hidden",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#Release Year vs Year Added plot
ggplot(df, aes(x=year, y=release_year, color=platform))+
  geom_jitter(aes(shape=type))+
  scale_y_reverse()+
  scale_color_manual( values = c("Netflix"= "#ff0000", "Hulu"   = "#37ff91", "Disney+" = "#001f60", "Amazon Prime" = "#00b0f1"))+
  theme_dark()+
  #theme(panel.background = element_rect(fill = 'lightblue', color = 'purple'))+
  facet_grid(~ platform)+
  labs(x="Year Added to Platform",
       y="Year of Original Release",
       title="Release Year vs Year Added",
       subtitle="compares release year to when a show was added to a platform",
       color="Platform",
       shape="Type")+
  theme(axis.text.x=element_text(angle=67.5,hjust=1, vjust=1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5))

