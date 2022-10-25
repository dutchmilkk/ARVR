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

ap_df <- amazon
dp_df <- disney
nf_df <- netflix
hu_df <- hulu
## ---------------------- ETHANS WORK ------------------------
library(plotly)
library(stringr)
             
# ----------- ADDED PER YEAR ------------            
                      
ap_TV <- ap_df[ap_df$type == 'TV Show', ]
dp_TV <- dp_df[dp_df$type == 'TV Show', ]
nf_TV <- nf_df[nf_df$type == 'TV Show', ]
hu_TV <- hu_df[hu_df$type == 'TV Show', ]

library(plotly)
library(stringr)

# Retrive date vector from dataset

ap_datesAdded_raw <- ap_TV$date_added
dp_datesAdded_raw <- dp_TV$date_added
nf_datesAdded_raw <- nf_TV$date_added
hu_datesAdded_raw <- hu_TV$date_added

#Get total empty values in Prime Video dates vector
ap_includes_empty <- table(ap_datesAdded_raw)
View(ap_includes_empty)

# Remove empty date instances
ap_datesAdded <- ap_datesAdded_raw[!ap_datesAdded_raw %in% ""]
dp_datesAdded <- dp_datesAdded_raw[!dp_datesAdded_raw %in% ""]
nf_datesAdded <- nf_datesAdded_raw[!nf_datesAdded_raw %in% ""]
hu_datesAdded <- hu_datesAdded_raw[!hu_datesAdded_raw %in% ""]

# Split date in half and add to vector
ap_splitDates <- c()
dp_splitDates <- c()
nf_splitDates <- c()
hu_splitDates <- c()

for (value in ap_datesAdded) {
  split_value <- str_split(value, ", ", n = Inf, simplify = FALSE)
  ap_splitDates <- append(ap_splitDates, split_value)
}
for (value in dp_datesAdded) {
  split_value <- str_split(value, ", ", n = Inf, simplify = FALSE)
  dp_splitDates <- append(dp_splitDates, split_value)
}
for (value in nf_datesAdded) {
  split_value <- str_split(value, ", ", n = Inf, simplify = FALSE)
  nf_splitDates <- append(nf_splitDates, split_value)
}
for (value in hu_datesAdded) {
  split_value <- str_split(value, ", ", n = Inf, simplify = FALSE)
  hu_splitDates <- append(hu_splitDates, split_value)
}

# Retrieve year from each split date
ap_years <- c()
dp_years <- c()
nf_years <- c()
hu_years <- c()
for (value in ap_splitDates) {
  ap_years <- append(ap_years, value[2])
}
for (value in dp_splitDates) {
  dp_years <- append(dp_years, value[2])
}
for (value in nf_splitDates) {
  nf_years <- append(nf_years, value[2])
}
for (value in hu_splitDates) {
  hu_years <- append(hu_years, value[2])
}
dp_showsAdded_perYear <- data.frame(table(dp_years))
ap_showsAdded_perYear <- data.frame(table(ap_years))
nf_showsAdded_perYear <- data.frame(table(nf_years))
hu_showsAdded_perYear <- data.frame(table(hu_years))

# Print Amazon Prime Table (only one year is present which is 2021)
View(ap_showsAdded_perYear)

# Plot Line Graph
library(plotly)
fig <- plot_ly(type = 'scatter', mode = 'lines', line = list(width = 4))
fig <- fig %>% layout(plot_bgcolor = "#7f7f7f", title = "Shows added per year", xaxis = list(title = "Year"), yaxis = list(title = "Shows Added"))
fig <- fig %>% add_trace(data = nf_showsAdded_perYear, x = ~nf_years, y = ~Freq, name = "Netflix", line = list(color = "rgb(255, 28, 28)"))
fig <- fig %>% add_trace(data = dp_showsAdded_perYear, x = ~dp_years, y = ~Freq, name = "Disney Plus", line = list(color = "rgb(11, 0, 161)"))
fig <- fig %>% add_trace(data = hu_showsAdded_perYear, x = ~hu_years, y = ~Freq, name = "Hulu", line = list(color = "rgb(0, 255, 162)"))

fig         

## ---------- LONGEST AND SHORTEST TV SHOWS BY SEASON -------------             
nf_TV <- nf_df[nf_df$type == 'TV Show', ]
dp_TV <- dp_df[dp_df$type == 'TV Show', ]
ap_TV <- ap_df[ap_df$type == 'TV Show', ]
hu_TV <- hu_df[hu_df$type == 'TV Show', ]


# Get titles and season duration
nf_shows <- nf_TV$title
nf_seasons <- nf_TV$duration

dp_shows <- dp_TV$title
dp_seasons <- dp_TV$duration

ap_shows <- ap_TV$title
ap_seasons <- ap_TV$duration

hu_shows <- hu_TV$title
hu_seasons <- hu_TV$duration

# Retrieve number from seasons for each instance 
nf_season_no <- c()
dp_season_no <- c()
ap_season_no <- c()
hu_season_no <- c()

for (value in nf_seasons) {
  temp_vec <- unlist(strsplit(value, split = " "))
  nf_season_no <- append(nf_season_no, temp_vec[1])
}

for (value in dp_seasons) {
  temp_vec <- unlist(strsplit(value, split = " "))
  dp_season_no <- append(dp_season_no, temp_vec[1])
}
for (value in ap_seasons) {
  temp_vec <- unlist(strsplit(value, split = " "))
  ap_season_no <- append(ap_season_no, temp_vec[1])
}
for (value in hu_seasons) {
  temp_vec <- unlist(strsplit(value, split = " "))
  hu_season_no <- append(hu_season_no, temp_vec[1])
}

nf_TV_shows_seasons <- data.frame(nf_shows,nf_season_no)
nf_TV_shows_seasons <- transform(nf_TV_shows_seasons, nf_season_no = as.numeric(nf_season_no))
dp_TV_shows_seasons <- data.frame(dp_shows,dp_season_no)
dp_TV_shows_seasons <- transform(dp_TV_shows_seasons, dp_season_no = as.numeric(dp_season_no))
ap_TV_shows_seasons <- data.frame(ap_shows,ap_season_no)
ap_TV_shows_seasons <- transform(ap_TV_shows_seasons, ap_season_no = as.numeric(ap_season_no))
hu_TV_shows_seasons <- data.frame(hu_shows,hu_season_no)
hu_TV_shows_seasons <- transform(hu_TV_shows_seasons, hu_season_no = as.numeric(hu_season_no))

View(dp_TV_shows_seasons)


# Get mean of all season number instances
nf_mean_seasons <- mean(nf_TV_shows_seasons$nf_season_no)
dp_mean_seasons <- mean(dp_TV_shows_seasons$dp_season_no)
ap_mean_seasons <- mean(ap_TV_shows_seasons$ap_season_no)
hu_mean_seasons <- mean(hu_TV_shows_seasons$hu_season_no)


# Print All
nf_mean_seasons
dp_mean_seasons
ap_mean_seasons
hu_mean_seasons



# --- SHINY APP ---
# Dropdown vectors
platforms <- c("Netflix", "Disney Plus", "Prime Video", "Hulu")
sortingOrder <- c("Longest to Shortest Shows", "Shortest to Longest Shows") 

install.packages("shiny")
install.packages("plotly")

library(shiny)
library(plotly)


# Page Layout
ui <- fluidPage( 
  
  titlePanel("Longest/Shortest Shows (by Season)"), 
  sidebarLayout( 
    sidebarPanel(  # INPUTS  
      selectInput("platformInput",  
                  "Select Platform:", 
                  choices = platforms),
      selectInput("sortInput",  
                  "Select Sorting Order:", 
                  choices = sortingOrder),
      sliderInput("showNumberInput", "Number of Shows:",  
                  value=5, min = 5,  
                  max = 5, step = 5)), 
    mainPanel(
      plotlyOutput("showHist")
    ) 
  ) 
)

library(plotly)
# Server Function
server <- function(session, input, output){
  platformInput <- reactive(input$platformInput)
  sortInput <- reactive(input$sortInput)
  showNum <- reactive(input$showNumberInput)
  
  # Observe function monitoring dropdown and slider input changes
  # If the platform dropdown changes, change to the corresponding platform selected
  # If the sorting dropdown changes, change the order of the selected platform graph 
  # Create corresponding graph when the specific conditions are met. 
  observe({
    output$showHist <- renderPlotly({
      if (platformInput() == "Netflix") {
        updateSliderInput(session, "showNumberInput", max = length(nf_TV_shows_seasons$nf_shows))
        if (sortInput() == "Longest to Shortest Shows") {
          nf_TV_shows_seasons <- nf_TV_shows_seasons[order(nf_TV_shows_seasons$nf_season_no, decreasing = TRUE),]
          x <- nf_TV_shows_seasons$nf_season_no[1:showNum()]
          y <- nf_TV_shows_seasons$nf_shows[1:showNum()]
          nf_bar_desc <- plot_ly() %>%
            add_trace(type = "bar",
                      orientation = "h",
                      x = x,
                      y = y,
                      text = ~paste(x, 'Seasons'),
                      marker = list(color = "rgb(255, 0, 0)")) %>%
            layout(title = "TV Show Duration (By Season)",
                   height = 750,
                   xaxis = list(title = "Seasons"),
                   yaxis = list(title = "TV Show", categoryorder = "total ascending"))
          nf_bar_desc
        }
        else {
          nf_TV_shows_seasons <- nf_TV_shows_seasons[order(nf_TV_shows_seasons$nf_season_no, decreasing = FALSE),]
          x <- nf_TV_shows_seasons$nf_season_no[1:showNum()]
          y <- nf_TV_shows_seasons$nf_shows[1:showNum()]
          nf_bar_asc <- plot_ly() %>%
            add_trace(type = "bar",
                      x = x,
                      y = y,
                      orientation = "h",
                      text = ~paste(x, 'Seasons'),
                      marker = list(color = "rgb(255, 0, 0)")) %>%
            layout(title = "TV Show Duration (By Season)",
                   height = 750,
                   xaxis = list(title = "Seasons"),
                   yaxis = list(title = "TV Shows", categoryorder = "total descending"))
          nf_bar_asc
        }
      }
      else if (platformInput() == "Disney Plus") {
        updateSliderInput(session, "showNumberInput", max = length(dp_TV_shows_seasons$dp_shows))
        if (sortInput() == "Longest to Shortest Shows") {
          dp_TV_shows_seasons <- dp_TV_shows_seasons[order(dp_TV_shows_seasons$dp_season_no, decreasing = TRUE),]
          x <- dp_TV_shows_seasons$dp_season_no[1:showNum()]
          y <- dp_TV_shows_seasons$dp_shows[1:showNum()]
          dp_bar_desc <- plot_ly() %>%
            add_trace(type = "bar",
                      orientation = "h",
                      x = x,
                      y = y,
                      text = ~paste(x, 'Seasons'),
                      marker = list(color = "rgb(11, 0, 161)")) %>%
            layout(title = "TV Show Duration (By Season)",
                   height = 750,
                   xaxis = list(title = "Seasons"),
                   yaxis = list(title = "TV Show", categoryorder = "total ascending"))
          dp_bar_desc
        }
        else {
          dp_TV_shows_seasons <- dp_TV_shows_seasons[order(dp_TV_shows_seasons$dp_season_no, decreasing = FALSE),]
          x <- dp_TV_shows_seasons$dp_season_no[1:showNum()]
          y <- dp_TV_shows_seasons$dp_shows[1:showNum()]
          dp_bar_asc <- plot_ly() %>%
            add_trace(type = "bar",
                      x = x,
                      y = y,
                      orientation = "h",
                      text = ~paste(x, 'Seasons'),
                      marker = list(color = "rgb(11, 0, 161)")) %>%
            layout(title = "TV Show Duration (By Season)",
                   height = 750,
                   xaxis = list(title = "Seasons"),
                   yaxis = list(title = "TV Shows", categoryorder = "total descending"))
          dp_bar_asc
        }
      }
      else if (platformInput() == "Prime Video") {
        updateSliderInput(session, "showNumberInput", max = length(ap_TV_shows_seasons$ap_shows))
        if (sortInput() == "Longest to Shortest Shows") {
          ap_TV_shows_seasons <- ap_TV_shows_seasons[order(ap_TV_shows_seasons$ap_season_no, decreasing = TRUE),]
          x <- ap_TV_shows_seasons$ap_season_no[1:showNum()]
          y <- ap_TV_shows_seasons$ap_shows[1:showNum()]
          ap_bar_desc <- plot_ly() %>%
            add_trace(type = "bar",
                      orientation = "h",
                      x = x,
                      y = y,
                      text = ~paste(x, 'Seasons'),
                      marker = list(color = "rgb(0, 237, 245)")) %>%
            layout(title = "TV Show Duration (By Season)",
                   height = 750,
                   xaxis = list(title = "Seasons"),
                   yaxis = list(title = "TV Show", categoryorder = "total ascending"))
          ap_bar_desc
        }
        else {
          ap_TV_shows_seasons <- ap_TV_shows_seasons[order(ap_TV_shows_seasons$ap_season_no, decreasing = FALSE),]
          x <- ap_TV_shows_seasons$ap_season_no[1:showNum()]
          y <- ap_TV_shows_seasons$ap_shows[1:showNum()]
          ap_bar_asc <- plot_ly() %>%
            add_trace(type = "bar",
                      x = x,
                      y = y,
                      orientation = "h",
                      text = ~paste(x, 'Seasons'),
                      marker = list(color = "rgb(0, 237, 245)")) %>%
            layout(title = "TV Show Duration (By Season)",
                   height = 750,
                   xaxis = list(title = "Seasons"),
                   yaxis = list(title = "TV Shows", categoryorder = "total descending"))
          ap_bar_asc
        }
      }
      else if (platformInput() == "Hulu") {
        updateSliderInput(session, "showNumberInput", max = length(hu_TV_shows_seasons$hu_shows))
        if (sortInput() == "Longest to Shortest Shows") {
          hu_TV_shows_seasons <- hu_TV_shows_seasons[order(hu_TV_shows_seasons$hu_season_no, decreasing = TRUE),]
          x <- hu_TV_shows_seasons$hu_season_no[1:showNum()]
          y <- hu_TV_shows_seasons$hu_shows[1:showNum()]
          hu_bar_desc <- plot_ly() %>%
            add_trace(type = "bar",
                      orientation = "h",
                      x = x,
                      y = y,
                      text = ~paste(x, 'Seasons'),
                      marker = list(color = "rgb(0, 227, 26)")) %>%
            layout(title = "TV Show Duration (By Season)",
                   height = 750,
                   xaxis = list(title = "Seasons"),
                   yaxis = list(title = "TV Show", categoryorder = "total ascending"))
          hu_bar_desc
        }
        else {
          hu_TV_shows_seasons <- hu_TV_shows_seasons[order(hu_TV_shows_seasons$hu_season_no, decreasing = FALSE),]
          x <- hu_TV_shows_seasons$hu_season_no[1:showNum()]
          y <- hu_TV_shows_seasons$hu_shows[1:showNum()]
          hu_bar_asc <- plot_ly() %>%
            add_trace(type = "bar",
                      x = x,
                      y = y,
                      orientation = "h",
                      text = ~paste(x, 'Seasons'),
                      marker = list(color = "rgb(0, 227, 26)")) %>%
            layout(title = "TV Show Duration (By Season)",
                   height = 750,
                   xaxis = list(title = "Seasons"),
                   yaxis = list(title = "TV Shows", categoryorder = "total descending"))
          hu_bar_asc
        }
      }
    })
  })
  
} 

shinyApp(ui = ui, server = server)              
             
# -------  TOP GENRES ----------            
nf_TV <- nf_df[nf_df$type == 'TV Show', ]
nf_movies <- nf_df[nf_df$type == 'Movie', ]
ap_TV <- ap_df[ap_df$type == 'TV Show', ]
ap_movies <- ap_df[ap_df$type == 'Movie', ]
dp_TV <- dp_df[dp_df$type == 'TV Show', ]
dp_movies <- dp_df[dp_df$type == 'Movie', ]
hu_TV <- hu_df[hu_df$type == 'TV Show', ]
hu_movies <- hu_df[hu_df$type == 'Movie', ]


# Get listed in values separated by movie and TV Shows
nf_TV_li <- nf_TV$listed_in
nf_movie_li <- nf_movies$listed_in

ap_TV_li <- ap_TV$listed_in
ap_movie_li <- ap_movies$listed_in

dp_TV_li <- dp_TV$listed_in
dp_movie_li <- dp_movies$listed_in

hu_TV_li <- hu_TV$listed_in
hu_movie_li <- hu_movies$listed_in


# Listed in vectors
nf_listed_in_TV_list <- c()
nf_listed_in_movie_list <- c()

ap_listed_in_TV_list <- c()
ap_listed_in_movie_list <- c()

dp_listed_in_TV_list <- c()
dp_listed_in_movie_list <- c()

hu_listed_in_TV_list <- c()
hu_listed_in_movie_list <- c()

# Get all individual genre values into listed in vectors for both TV Shows and movies
# NETFLIX
for (value in nf_TV_li) {
  temp_vec <- str_split(value, ", ", n = Inf, simplify = FALSE)
  for (split_value in temp_vec) {
    nf_listed_in_TV_list <- append(nf_listed_in_TV_list, split_value)
  }
}
for (value in nf_movie_li) {
  temp_vec <- str_split(value, ", ", n = Inf, simplify = FALSE)
  for (split_value in temp_vec) {
    nf_listed_in_movie_list <- append(nf_listed_in_movie_list, split_value)
  }
}
# AMAZON PRIME VIDEO
for (value in ap_TV_li) {
  temp_vec <- str_split(value, ", ", n = Inf, simplify = FALSE)
  for (split_value in temp_vec) {
    ap_listed_in_TV_list <- append(ap_listed_in_TV_list, split_value)
  }
}
for (value in ap_movie_li) {
  temp_vec <- str_split(value, ", ", n = Inf, simplify = FALSE)
  for (split_value in temp_vec) {
    ap_listed_in_movie_list <- append(ap_listed_in_movie_list, split_value)
  }
}
# DISNEY PLUS
for (value in dp_TV_li) {
  temp_vec <- str_split(value, ", ", n = Inf, simplify = FALSE)
  for (split_value in temp_vec) {
    dp_listed_in_TV_list <- append(dp_listed_in_TV_list, split_value)
  }
}

for (value in dp_movie_li) {
  temp_vec <- str_split(value, ", ", n = Inf, simplify = FALSE)
  for (split_value in temp_vec) {
    dp_listed_in_movie_list <- append(dp_listed_in_movie_list, split_value)
  }
}
# HULU
for (value in hu_TV_li) {
  temp_vec <- str_split(value, ", ", n = Inf, simplify = FALSE)
  for (split_value in temp_vec) {
    hu_listed_in_TV_list <- append(hu_listed_in_TV_list, split_value)
  }
}

for (value in hu_movie_li) {
  temp_vec <- str_split(value, ", ", n = Inf, simplify = FALSE)
  for (split_value in temp_vec) {
    hu_listed_in_movie_list <- append(hu_listed_in_movie_list, split_value)
  }
}

# The genre "Arts, Entertainment, and Culture" cannot be properly seperated by ", " a solution is the following:
# Replace all "and Culture" occurrences in the listed in vector with the full genre name.
# Remove all occurrences of "Arts" and "Entertainment from listed in vector
ap_listed_in_TV_list <- replace(ap_listed_in_TV_list, ap_listed_in_TV_list == "and Culture", "Arts, Entertainment, and Culture")
ap_listed_in_TV_list <- ap_listed_in_TV_list[! ap_listed_in_TV_list %in% c("Arts", "Entertainment")]

ap_listed_in_movie_list <- replace(ap_listed_in_movie_list, ap_listed_in_movie_list == "and Culture", "Arts, Entertainment, and Culture")
ap_listed_in_movie_list <- ap_listed_in_movie_list[! ap_listed_in_movie_list %in% c("Arts", "Entertainment")]





# Turn listed in vectors into frequency table, listing each genre and their frequency of occurrence
# Sort by decreasing
nf_listed_in_TV_list.freq = table(nf_listed_in_TV_list)
nf_sorted_TV <- nf_listed_in_TV_list.freq[order(nf_listed_in_TV_list.freq, decreasing = TRUE)]

nf_listed_in_movie_list.freq = table(nf_listed_in_movie_list)
nf_sorted_movie <- nf_listed_in_movie_list.freq[order(nf_listed_in_movie_list.freq, decreasing = TRUE)]

ap_listed_in_TV_list.freq = table(ap_listed_in_TV_list)
ap_sorted_TV <- ap_listed_in_TV_list.freq[order(ap_listed_in_TV_list.freq, decreasing = TRUE)]

ap_listed_in_movie_list.freq = table(ap_listed_in_movie_list)
ap_sorted_movie <- ap_listed_in_movie_list.freq[order(ap_listed_in_movie_list.freq, decreasing = TRUE)]

dp_listed_in_TV_list.freq = table(dp_listed_in_TV_list)
dp_sorted_TV <- dp_listed_in_TV_list.freq[order(dp_listed_in_TV_list.freq, decreasing = TRUE)]

dp_listed_in_movie_list.freq = table(dp_listed_in_movie_list)
dp_sorted_movie <- dp_listed_in_movie_list.freq[order(dp_listed_in_movie_list.freq, decreasing = TRUE)]

hu_listed_in_TV_list.freq = table(hu_listed_in_TV_list)
hu_sorted_TV <- hu_listed_in_TV_list.freq[order(hu_listed_in_TV_list.freq, decreasing = TRUE)]

hu_listed_in_movie_list.freq = table(hu_listed_in_movie_list)
hu_sorted_movie <- hu_listed_in_movie_list.freq[order(hu_listed_in_movie_list.freq, decreasing = TRUE)]



# Turn frequency tables into DataFrames
nf_li_TV_df <- data.frame(nf_sorted_TV)
nf_li_movie_df <- data.frame(nf_sorted_movie)


ap_li_TV_df <- data.frame(ap_sorted_TV)
ap_li_movie_df <- data.frame(ap_sorted_movie)

dp_li_TV_df <- data.frame(dp_sorted_TV)
dp_li_movie_df <- data.frame(dp_sorted_movie)

hu_li_TV_df <- data.frame(hu_sorted_TV)
hu_li_movie_df <- data.frame(hu_sorted_movie)



# Color palette for wordcloud and pie chart
nf_colfunc_TV <- colorRampPalette(c("#ff0000", "#ffbf00"))
nf_colfunc_movie <- colorRampPalette(c("#fc0000", "#707070"))

ap_colfunc_TV <- colorRampPalette(c("#00ffc8", "#dd00ff"))
ap_colfunc_movie <- colorRampPalette(c("#008f8a", "#64008f"))

dp_colfunc_TV <- colorRampPalette(c("#0062ff", "#54575c"))
dp_colfunc_movie <- colorRampPalette(c("#0d00ff", "#3decff"))

hu_colfunc_TV <- colorRampPalette(c("#00de1a", "#00de9f"))
hu_colfunc_movie <- colorRampPalette(c("#008c1e", "#008c8c"))

# Get top 10 highest occurring genres, get the sum of the frequency of the genres below the top 10
# Concatenate that to the top 10 occurring genres as a data frame, labeling the other frequency sum as "other"

# NETFLIX
nf_top_TV_li <- nf_li_TV_df[1:10,]

nf_top_TV_shows <- as.character(nf_top_TV_li$nf_listed_in_TV_list)
nf_top_TV_shows.freq <- nf_top_TV_li$Freq

nf_other_TV_li <- nf_li_TV_df[11:length(nf_li_TV_df$nf_listed_in_TV_list),]
nf_other_TV_shows.freq <- nf_other_TV_li$Freq
nf_other_TV_sum <- sum(nf_other_TV_shows.freq)

nf_listed_in_TV_list <- append(nf_top_TV_shows, "Other")
Freq <- append(nf_top_TV_shows.freq, nf_other_TV_sum)
nf_top_TV_li <- data.frame(nf_listed_in_TV_list, Freq)

nf_top_movie_li <- nf_li_movie_df[1:10,]

nf_top_movie <- as.character(nf_top_movie_li$nf_listed_in_movie_list)
nf_top_movie.freq <- nf_top_movie_li$Freq

nf_other_movie_li <- nf_li_movie_df[11:length(nf_li_movie_df$nf_listed_in_movie_list),]
nf_other_movie.freq <- nf_other_movie_li$Freq
nf_other_movie_sum <- sum(nf_other_movie.freq)

nf_listed_in_movie_list <- append(nf_top_movie, "Other")
Freq <- append(nf_top_movie.freq, nf_other_movie_sum)
nf_top_movie_li <- data.frame(nf_listed_in_movie_list, Freq)

# AMAZON PRIME VIDEO
ap_top_TV_li <- ap_li_TV_df[1:10,]

ap_top_TV_shows <- as.character(ap_top_TV_li$ap_listed_in_TV_list)
ap_top_TV_shows.freq <- ap_top_TV_li$Freq

ap_other_TV_li <- ap_li_TV_df[11:length(ap_li_TV_df$ap_listed_in_TV_list),]
ap_other_TV_shows.freq <- ap_other_TV_li$Freq
ap_other_TV_sum <- sum(ap_other_TV_shows.freq)

ap_listed_in_TV_list <- append(ap_top_TV_shows, "Other")
Freq <- append(ap_top_TV_shows.freq, ap_other_TV_sum)
ap_top_TV_li <- data.frame(ap_listed_in_TV_list, Freq)

ap_top_movie_li <- ap_li_movie_df[1:10,]

ap_top_movie <- as.character(ap_top_movie_li$ap_listed_in_movie_list)
ap_top_movie.freq <- ap_top_movie_li$Freq

ap_other_movie_li <- ap_li_movie_df[11:length(ap_li_movie_df$ap_listed_in_movie_list),]
ap_other_movie.freq <- ap_other_movie_li$Freq
ap_other_movie_sum <- sum(ap_other_movie.freq)

ap_listed_in_movie_list <- append(ap_top_movie, "Other")
Freq <- append(ap_top_movie.freq, ap_other_movie_sum)
ap_top_movie_li <- data.frame(ap_listed_in_movie_list, Freq)

# DISNEY PLUS


dp_top_TV_li <- dp_li_TV_df[1:10,]
dp_top_TV_shows <- as.character(dp_top_TV_li$dp_listed_in_TV_list)
dp_top_TV_shows.freq <- dp_top_TV_li$Freq

dp_other_TV_li <- dp_li_TV_df[11:length(dp_li_TV_df$dp_listed_in_TV_list),]
dp_other_TV_shows.freq <- dp_other_TV_li$Freq
dp_other_TV_sum <- sum(dp_other_TV_shows.freq)

dp_listed_in_TV_list <- append(dp_top_TV_shows, "Other")
Freq <- append(dp_top_TV_shows.freq, dp_other_TV_sum)
dp_top_TV_li <- data.frame(dp_listed_in_TV_list, Freq)

dp_top_movie_li <- dp_li_movie_df[1:10,]

dp_top_movie <- as.character(dp_top_movie_li$dp_listed_in_movie_list)
dp_top_movie.freq <- dp_top_movie_li$Freq

dp_other_movie_li <- dp_li_movie_df[11:length(dp_li_movie_df$dp_listed_in_movie_list),]
dp_other_movie.freq <- dp_other_movie_li$Freq
dp_other_movie_sum <- sum(dp_other_movie.freq)

dp_listed_in_movie_list <- append(dp_top_movie, "Other")
Freq <- append(dp_top_movie.freq, dp_other_movie_sum)
dp_top_movie_li <- data.frame(dp_listed_in_movie_list, Freq)

# HULU

hu_top_TV_li <- hu_li_TV_df[1:10,]

hu_top_TV_shows <- as.character(hu_top_TV_li$hu_listed_in_TV_list)
hu_top_TV_shows.freq <- hu_top_TV_li$Freq

hu_other_TV_li <- hu_li_TV_df[11:length(hu_li_TV_df$hu_listed_in_TV_list),]
hu_other_TV_shows.freq <- hu_other_TV_li$Freq
hu_other_TV_sum <- sum(hu_other_TV_shows.freq)

hu_listed_in_TV_list <- append(hu_top_TV_shows, "Other")
Freq <- append(hu_top_TV_shows.freq, hu_other_TV_sum)
hu_top_TV_li <- data.frame(hu_listed_in_TV_list, Freq)

hu_top_movie_li <- hu_li_movie_df[1:10,]

hu_top_movie <- as.character(hu_top_movie_li$hu_listed_in_movie_list)
hu_top_movie.freq <- hu_top_movie_li$Freq

hu_other_movie_li <- hu_li_movie_df[11:length(hu_li_movie_df$hu_listed_in_movie_list),]
hu_other_movie.freq <- hu_other_movie_li$Freq
hu_other_movie_sum <- sum(hu_other_movie.freq)

hu_listed_in_movie_list <- append(hu_top_movie, "Other")
Freq <- append(hu_top_movie.freq, hu_other_movie_sum)
hu_top_movie_li <- data.frame(hu_listed_in_movie_list, Freq)

# Plot Pie charts
# NETFLIX

nf_tv_pie <- plot_ly(nf_top_TV_li, labels = ~nf_listed_in_TV_list, values = ~Freq, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = "#FFFFFF"),
                     hoverinfo = "text",
                     text = ~paste(Freq, 'occurences'),
                     marker = list(colors = nf_colfunc_TV(11),
                                   line = list(color = '#FFFFFF', width = 1)),
                     showlegend = FALSE)

nf_tv_pie <- nf_tv_pie %>% layout(title = 'Top TV Show Genres Netflix', plot_bgcolor = "#7f7f7f",
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


nf_movie_pie <- plot_ly(nf_top_movie_li, labels = ~nf_listed_in_movie_list, values = ~Freq, type = 'pie',
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = "#FFFFFF"),
                        hoverinfo = "text",
                        text = ~paste(Freq, 'occurences'),
                        marker = list(colors = nf_colfunc_movie(11),
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE)

nf_movie_pie <- nf_movie_pie %>% layout(title = 'Top Movie Genres Netflix',
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# AMAZON PRIME VIDEO
ap_tv_pie <- plot_ly(ap_top_TV_li, labels = ~ap_listed_in_TV_list, values = ~Freq, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = "#FFFFFF"),
                     hoverinfo = "text",
                     text = ~paste(Freq, 'occurences'),
                     marker = list(colors = ap_colfunc_TV(11),
                                   line = list(color = '#FFFFFF', width = 1)),
                     showlegend = FALSE)

ap_tv_pie <- ap_tv_pie %>% layout(title = 'Top TV Show Genres Amazon Prime',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


ap_movie_pie <- plot_ly(ap_top_movie_li, labels = ~ap_listed_in_movie_list, values = ~Freq, type = 'pie',
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = "#FFFFFF"),
                        hoverinfo = "text",
                        text = ~paste(Freq, 'occurences'),
                        marker = list(colors = ap_colfunc_movie(11),
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE)

ap_movie_pie <- ap_movie_pie %>% layout(title = 'Top Movie Genres Amazon Prime',
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# DISNEY PLUS
dp_tv_pie <- plot_ly(dp_top_TV_li, labels = ~dp_listed_in_TV_list, values = ~Freq, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = "#FFFFFF"),
                     hoverinfo = "text",
                     text = ~paste(Freq, 'occurences'),
                     marker = list(colors = dp_colfunc_TV(11),
                                   line = list(color = '#FFFFFF', width = 1)),
                     showlegend = FALSE)

dp_tv_pie <- dp_tv_pie %>% layout(title = 'Top TV Show Genres Disney Plus',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

dp_movie_pie <- plot_ly(dp_top_movie_li, labels = ~dp_listed_in_movie_list, values = ~Freq, type = 'pie',
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = "#FFFFFF"),
                        hoverinfo = "text",
                        text = ~paste(Freq, 'occurences'),
                        marker = list(colors = dp_colfunc_movie(11),
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE)

dp_movie_pie <- dp_movie_pie %>% layout(title = 'Top Movie Genres Disney Plus',
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# HULU
hu_tv_pie <- plot_ly(hu_top_TV_li, labels = ~hu_listed_in_TV_list, values = ~Freq, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = "#FFFFFF"),
                     hoverinfo = "text",
                     text = ~paste(Freq, 'occurences'),
                     marker = list(colors = hu_colfunc_TV(11),
                                   line = list(color = '#FFFFFF', width = 1)),
                     showlegend = FALSE)

hu_tv_pie <- hu_tv_pie %>% layout(title = 'Top TV Show Genres Hulu',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


hu_movie_pie <- plot_ly(hu_top_movie_li, labels = ~hu_listed_in_movie_list, values = ~Freq, type = 'pie',
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = "#FFFFFF"),
                        hoverinfo = "text",
                        text = ~paste(Freq, 'occurences'),
                        marker = list(colors = hu_colfunc_movie(11),
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE)

hu_movie_pie <- hu_movie_pie %>% layout(title = 'Top Movie Genres Hulu',
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# --- SHINY APP ---

# Dropdown inputs
platforms = c("Netflix", "Disney Plus", "Amazon Prime", "Hulu")
default = c("Select Platform")
netflixPies = c("Netflix TV Shows", "Netflix Movies")
dpPies = c("Disney Plus TV Shows", "Disney Plus Movies")
apPies = c("Amazon Prime Video TV Shows", "Amazon Prime Video Movies")
huluPies = c("Hulu TV Shows", "Hulu Movies")

# Shiny App page inputs and output
ui <- fluidPage( 
  titlePanel("Top Genres by each Platform"), 
  sidebarLayout( 
    sidebarPanel(  # INPUTS  
      selectInput("platformInput",  
                  "Select Platform:", 
                  choices =platforms),
      selectInput("genreInput",  
                  "Select Genre:", 
                  choices = default)),
    mainPanel(  # OUTPUTS 
      plotlyOutput("pie")),
  ) 
) 
# Server function
server <- function(input, output, session){ 
  platform <- reactive(input$platformInput)
  pie_type <- reactive(input$genreInput)

  # Functions to output specific pie charts when called
  netflix_TV <- reactive({
    nf_tv_pie
  })
  netflix_movie <- reactive({
    nf_movie_pie
  })
  dp_TV <- reactive({
    dp_tv_pie
  })
  dp_movie <- reactive({
    dp_movie_pie
  })
  ap_TV <- reactive({

    ap_tv_pie
  })
  ap_movie <- reactive({
    ap_movie_pie
  })
  hulu_TV <- reactive({
    hu_tv_pie
  })
  hulu_movie <- reactive({
    hu_movie_pie
  })
  # Observe change in platform input, change TV show/movie dropdown respectively
  observe({
    if (platform() == "Netflix") {
      updateSelectInput(session, "genreInput", choices = netflixPies)
    }
    if (platform() == "Disney Plus") {
      updateSelectInput(session, "genreInput", choices = dpPies)
    }
    if (platform() == "Amazon Prime") {
      updateSelectInput(session, "genreInput", choices = apPies)
    }
    if (platform() == "Hulu") {
      updateSelectInput(session, "genreInput", choices = huluPies)
    }
  })
  # Call pie chart functions when dropdown input changes
  graphInput <- reactive({
    switch(input$genreInput,
           "Netflix TV Shows" = netflix_TV(),
           "Netflix Movies" = netflix_movie(),
           "Disney Plus TV Shows" = dp_TV(),
           "Disney Plus Movies" = dp_movie(),
           "Amazon Prime Video TV Shows" = ap_TV(),
           "Amazon Prime Video Movies" = ap_movie(),
           "Hulu TV Shows" = hulu_TV(),
           "Hulu Movies" = hulu_movie()
    )
  })
  
  output$pie <- renderPlotly({
    graphInput()
  })
} 
shinyApp(ui = ui, server = server) 

library(wordcloud)
# WORD CLOUDS
# ----- Netflix -----
nf_TV_wordcloud <- wordcloud(words = nf_li_TV_df$nf_listed_in_TV_list, freq = nf_li_TV_df$Freq, min.freq = 1,
                             max.words = 200, random.order = FALSE, rot.per = 0.28,
                             colors=nf_colfunc_TV(10))

nf_movie_wordcloud <- wordcloud(words = nf_li_movie_df$nf_listed_in_movie_list, freq = nf_li_movie_df$Freq, min.freq = 1,
                                max.words = 200, random.order = FALSE, rot.per = 0.28,
                                colors=nf_colfunc_movie(10))


# ----- Disney Plus -----
dp_TV_wordcloud <- wordcloud(words = dp_li_TV_df$dp_listed_in_TV_list, freq = dp_li_TV_df$Freq, min.freq = 1,
                             max.words = 200, random.order = FALSE, rot.per = 0.28,
                             colors=dp_colfunc_TV(10))

dp_movie_wordcloud <- wordcloud(words = dp_li_movie_df$dp_listed_in_movie_list, freq = dp_li_movie_df$Freq, min.freq = 1,
                                max.words = 200, random.order = FALSE, rot.per = 0.28,
                                colors=dp_colfunc_movie(10))

# ----- Amazon Prime Video -----
ap_TV_wordcloud <- wordcloud(words = ap_li_TV_df$ap_listed_in_TV_list, freq = ap_li_TV_df$Freq, min.freq = 1,
                             max.words = 200, random.order = FALSE, rot.per = 0.28,
                             colors=ap_colfunc_TV(10))


ap_movie_wordcloud <- wordcloud(words = ap_li_movie_df$ap_listed_in_movie_list, freq = ap_li_movie_df$Freq, min.freq = 1,
                                max.words = 200, random.order = FALSE, rot.per = 0.28,
                                colors=ap_colfunc_movie(10))


# ----- Hulu ------
hu_TV_wordcloud <- wordcloud(words = hu_li_TV_df$hu_listed_in_TV_list, freq = hu_li_TV_df$Freq, min.freq = 1,
                             max.words = 200, random.order = FALSE, rot.per = 0.28,
                             colors=hu_colfunc_TV(10))


hu_movie_wordcloud <- wordcloud(words = hu_li_movie_df$hu_listed_in_movie_list, freq = hu_li_movie_df$Freq, min.freq = 1,
                                max.words = 200, random.order = FALSE, rot.per = 0.28,
                                colors=hu_colfunc_movie(10))             

             
-----------matt part-----------
             
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

temp <- df %>%
  group_by(platform,year) %>%
  count(year)

#used
#close look at the content created between 2017 and 2021
ggplot(temp, aes(x=year, y=n, color=platform))+
  geom_line( size=2)+
  #scale_x_continuous(limits = c(2016, 2021))+
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
