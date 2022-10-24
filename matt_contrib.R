# 11464 AR/VR for Data Analysis and Communication
# Project on Data Visualisation 
#
# Group: (v) forgo(r) 
# Tutorial: Tuesday 3:30-5.30
#
# Viriya Duch Sam u3211355
# Matthew Borowski u3214653
# Ethan Bendeich u3216357
#created on: 17/10/2022

#libraries
library(dplyr)
library(ggplot2)
library(lubridate)

#colour pallette for data
#hulu_colour <- #37ff91
#disney_colour <- #001f60
#netflix_colour <- #ff0000
#amazon_colour <- #00b0f1

#import dataset
netflix <- read.csv("Dataset/netflix_titles.csv")
hulu <- read.csv("Dataset/hulu_titles.csv")
disney <- read.csv("Dataset/disney_plus_titles.csv")
amazon <- read.csv("Dataset/amazon_prime_titles.csv")


# Add new column for each streaming platform
netflix <- netflix %>% mutate(platform = "Netflix")
hulu <- hulu %>% mutate(platform = "Hulu")
disney <- disney %>% mutate(platform = "Disney+")
amazon <- amazon %>% mutate(platform = "Amazon Prime")


#concatenate all the data into one dataframe
df <- do.call("rbind", list(netflix, disney, hulu, amazon))


#information on dataset
head(df, 10)
str(df)


#as the date formatting didnt work on a "non-NA" value, they need to be changed manually
df <- df %>% mutate_all(na_if, "")


#lubridate the date column so its recognised as a date
df$date_added <- as.Date.character(df$date_added, format = "%B %d, %Y")


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



#get average "release_diff" time across "year" for each platform
#look at trends across years, especially with netflix and hulu as they ahve more data available.

  

