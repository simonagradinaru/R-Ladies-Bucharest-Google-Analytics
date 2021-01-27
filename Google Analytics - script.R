
# ---- 0. Setup ----
rm(list = ls())
library(googleAnalyticsR)
#library(googleAuthR)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggrepel)
library(modelr)
library(viridis)
library(scales)
library(forcats)
rerun_yn <- FALSE


# ---- 1. Get data from Google Analytics ---- 

if(rerun_yn == TRUE){
  # Connect R and Google Analytics
  ga_auth()
  
  # Generate a list of all the Google Analytics accounts you have access to
  ga_accounts <- ga_account_list()
  
  # ga_id contains the View ID that you want to query
  ga_id <- pull(ga_accounts[2,1])
  ga_id <- "ga:228717430"
  
  # Download all data and store it in a dataframe
  # blog was launched on September 11, 2020
  ga_df <- google_analytics(ga_id,
                            date_range = c("2020-09-11", as.character(Sys.Date()-1)), 
                            metrics = c("users", "sessions"),
                            dimensions = "date", 
                            anti_sample = TRUE) # avoids sampling data

  # add blog posts when they were launched
  ga_df$event <- ""
  ga_df %<>% 
    mutate(event = ifelse(date == "2020-09-11", 
                          "You can't put a number to love, but researchers estimated one (blog launched)",
                          event),
           event = ifelse(date == "2020-10-05",
                          "Are we chasing people 'out of our league'? (share in Instagram & Facebook story)",
                          event),
           event = ifelse(date == "2020-10-12",
                          "You can't put a number to love, but researchers estimated one (share in Facebook post)",
                          event),
           event = ifelse(date == "2020-10-23", 
                          "Are we chasing people 'out of our league'? (share in Facebook post)",
                          event),
           event = ifelse(date == "2020-11-20", 
                          "People use online dating platforms just for fun, *but lying is fun too* (share in Facebook post)",
                          event),
           event = ifelse(date == "2020-11-23", 
                          "People use online dating platforms just for fun, *but lying is fun too* (share in Instagram & Facebook story)",
                          event),
           event = ifelse(date == "2020-12-24", 
                          "Things you need to know about Christmas gift-giving (share in Instagram & Facebook story)", 
                          event),
           event = ifelse(date == "2020-12-28", 
                          "Things you need to know about Christmas gift-giving (share in Facebook post)", 
                          event)
           )
  
  # Check out all available segments 
  segments <- c(c("All Users", 
                  "New Users", 
                  "Returning Users", 
                  "Organic Traffic", 
                  "Search Traffic", 
                  "Direct Traffic", 
                  "Referral Traffic", 
                  "Mobile and Tablet Traffic", 
                  "Mobile Traffic", 
                  "Tablet and Desktop Traffic", 
                  "Android Traffic", 
                  "iOS Traffic", 
                  "Single Session Users", 
                  "Multi-session Users", 
                  "Romanian users"))
  segments_list <- ga_segment_list() %>% 
    filter(name %in% segments)
  
  # Add segment ID to the segment_ga4 function
  bySegment_ga4 <- lapply(segments_list$name, function(x) 
    segment_ga4(x, 
                segment_id = segments_list %>% 
                  filter(name == x) %>% 
                  pull(segmentId)
                )
    ) %>% magrittr::set_names(segments)
                              
  # Query Google Analytics data using the segment
  bySegment_data <- lapply(bySegment_ga4, function(x)
    google_analytics(ga_id,
                     c("2020-09-11", "2021-01-31"), 
                     dimensions = c("date", "segment"), 
                     segments = x, 
                     metrics = c("sessions", "users", "bounces")
                     ) 
  )
  
  # Obtain Google Analytics data by day of week
  ga_byDayofWeek_df <- google_analytics(ga_id, 
                                        date_range = c("2020-09-11", as.character(Sys.Date()-1)),
                                        metrics = "sessions", 
                                        dimensions = c("dayOfWeek","date")) %>% 
    mutate(dayOfWeek = factor(dayOfWeek, 
                              levels = c(0:6),
                              labels = c("Sunday", 
                                         "Monday", 
                                         "Tuesday", 
                                         "Wednesday", 
                                         "Thursday", 
                                         "Friday", 
                                         "Saturday")
                              )
    )
  
  # Obtain Google Analytics data by device category
  ga_byDevice_df  <- google_analytics(ga_id, 
                                      date_range = c("2020-09-11", as.character(Sys.Date()-1)), 
                                      metrics = c("sessions", "avgSessionDuration"),
                                      dimensions = c("date", "deviceCategory")
                                      )
  
  # Obtain Google Analytics data by browser
  ga_byBrowser_df <- google_analytics(ga_id, 
                                      date_range = c("2020-09-11",as.character(Sys.Date()-1)),
                                      metrics = c("sessions", "users", "goalCompletionsAll"),
                                      dimensions = c("browser", 
                                                     "browserVersion", 
                                                     "browserSize", 
                                                     "screenResolution", 
                                                     "mobileDeviceinfo")
                                      )
  
  # Obtain Google Analytics data by channel
  ga_byChannel_df <- google_analytics(ga_id, 
                                      date_range = c("2020-09-11",as.character(Sys.Date()-1)),
                                      metrics = "users",
                                      dimensions = c("date",
                                                     "deviceCategory", 
                                                     "channelGrouping")
                                      )
  
  # Obtain Google Analytics data by continent and country
  ga_byCountry_df <- google_analytics(ga_id, 
                                      date_range = c("2020-09-11",as.character(Sys.Date()-1)),
                                      metrics = "sessions",
                                      dimensions = c("date",
                                                     "continent",
                                                     "country", 
                                                     "city", 
                                                     "latitude", 
                                                     "longitude"
                                                     )
                                      )
  # Some observations have missing latitude and longitude (no city was recorded)
  
  # Recode missing coordinates for Romania
  ga_byCountry_df$latitude <- ifelse(ga_byCountry_df$latitude == "0.0000" & 
                                       ga_byCountry_df$country == "Romania", 
                                     "45.9443",
                                     ga_byCountry_df$latitude)
  ga_byCountry_df$longitude<- ifelse(ga_byCountry_df$longitude == "0.0000" & 
                                       ga_byCountry_df$country == "Romania", 
                                     "25.0094",
                                     ga_byCountry_df$longitude)
  
  # Recode missing coordinates for United States
  ga_byCountry_df$latitude <- ifelse(ga_byCountry_df$latitude == "0.0000" & 
                                       ga_byCountry_df$country == "United States", 
                                     "37.6000",
                                     ga_byCountry_df$latitude)
  ga_byCountry_df$longitude<- ifelse(ga_byCountry_df$longitude == "0.0000" & 
                                       ga_byCountry_df$country == "United States", 
                                     "-95.6650",
                                     ga_byCountry_df$longitude)
  
  # Recode missing coordinates for Trinidad & Tobago
  ga_byCountry_df$latitude <- ifelse(ga_byCountry_df$latitude == "0.0000" & 
                                       ga_byCountry_df$country == "Trinidad & Tobago", 
                                     "10.4437",
                                     ga_byCountry_df$latitude)
  ga_byCountry_df$longitude<- ifelse(ga_byCountry_df$longitude == "0.0000" & 
                                       ga_byCountry_df$country == "Trinidad & Tobago", 
                                     "-61.4191",
                                     ga_byCountry_df$longitude)
  
  # Recode missing coordinates for United Kingdom
  ga_byCountry_df$latitude <- ifelse(ga_byCountry_df$latitude == "0.0000" & 
                                       ga_byCountry_df$country == "United Kingdom", 
                                     "55.3618",
                                     ga_byCountry_df$latitude)
  ga_byCountry_df$longitude<- ifelse(ga_byCountry_df$longitude == "0.0000" & 
                                       ga_byCountry_df$country == "United Kingdom", 
                                     "-3.4433",
                                     ga_byCountry_df$longitude)
  
  # Recode missing coordinates for Moldova
  ga_byCountry_df$latitude <- ifelse(ga_byCountry_df$latitude == "0.0000" & 
                                       ga_byCountry_df$country == "Moldova", 
                                     "46.9804",
                                     ga_byCountry_df$latitude)
  ga_byCountry_df$longitude<- ifelse(ga_byCountry_df$longitude == "0.0000" & 
                                       ga_byCountry_df$country == "Moldova", 
                                     "28.3897",
                                     ga_byCountry_df$longitude)
  
  # Recode missing coordinates for Colombia
  ga_byCountry_df$latitude <- ifelse(ga_byCountry_df$latitude == "0.0000" & 
                                       ga_byCountry_df$country == "Colombia", 
                                     "4.1157",
                                     ga_byCountry_df$latitude)
  ga_byCountry_df$longitude<- ifelse(ga_byCountry_df$longitude == "0.0000" & 
                                       ga_byCountry_df$country == "Colombia", 
                                     "-72.9301",
                                     ga_byCountry_df$longitude)

  # Recode missing coordinates for Norway
  ga_byCountry_df$latitude <- ifelse(ga_byCountry_df$latitude == "0.0000" & 
                                       ga_byCountry_df$country == "Norway", 
                                     "64.5783",
                                     ga_byCountry_df$latitude)
  ga_byCountry_df$longitude<- ifelse(ga_byCountry_df$longitude == "0.0000" & 
                                       ga_byCountry_df$country == "Norway", 
                                     "17.8882",
                                     ga_byCountry_df$longitude)
  
  ga_byCountry_df %<>% 
    mutate(latitude = as.numeric(latitude), 
           longitude = as.numeric(longitude))

  # Save data
  save(ga_df, 
       ga_byDayofWeek_df,
       ga_byDevice_df,
       ga_byBrowser_df,
       ga_byChannel_df,
       ga_byCountry_df,
       file = "Google Analytics data.Rdata"
  )
  
} else load("Google Analytics data.Rdata")


# ---- 2. Analyze Google analytics data ----

# ---- Sessions evolution ----

# How did sessions evolve through time? 
sessions.plot <- ga_df %>%
  ggplot(aes(x = date, y = sessions, label = event)) +
  geom_line() +
  theme_light() +   
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
plotly::ggplotly(sessions.plot)
# peaks were achieved when sharing the post on Facebook

# Daily users with labels
ggplot(ga_df, aes(x = date, y = users, label = users)) + 
  geom_line() + 
  geom_point()+
  geom_label(aes(label = users), 
             size = 2, 
             fill = viridis(3)[2], 
             colour = "white", 
             fontface = "bold") + 
  scale_x_date(labels = scales::date_format("%m-%Y")) +
  theme_light() + 
  ylab("Users") + 
  xlab("Date") +
  ggtitle("Daily users between September 2020 and February 2021") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) 


# ---- Day of week ----

# Run a linear regression to predict sessions based on day of week
DayofWeek.lm <- lm(sessions ~ dayOfWeek, data = ga_byDayofWeek_df)
summary(DayofWeek.lm) 
# Monday is significant at 5%, Friday is significant at 10%

# Get predictions by day of week
DayofWeek.df <- ga_byDayofWeek_df %>% 
  data_grid(dayOfWeek) %>% 
  add_predictions(DayofWeek.lm, "sessions")

# Plot predictions
ga_byDayofWeek_df %>%
  ggplot(aes(x = dayOfWeek, y = sessions)) + 
  geom_boxplot()+
  geom_point(grid = DayofWeek.df, colour = viridis::viridis(3)[2], size = 2) + 
  theme_light()


# ---- Devices ----

# Plot sessions with deviceCategory
ga_byDevice_df %>%
  ggplot(aes(x = deviceCategory, y = sessions, fill = deviceCategory)) +   
  geom_bar(stat = "identity", position = position_dodge(-.9), show.legend = TRUE) + 
  theme_light() +
  scale_fill_manual(values = viridis::viridis(5), aesthetics = "fill") + 
  coord_flip() +
  theme(panel.grid = element_blank()) 
# mostly mobile users
  
# Plot average session duration and device category
ga_byDevice_df %>% 
  ggplot(aes(x = deviceCategory, y = avgSessionDuration, fill = deviceCategory)) + 
  geom_bar(stat = "identity", position = position_dodge(-.9)) + 
  theme_light() +
  scale_fill_manual(values = viridis::viridis(5), aesthetics = "fill") + 
  coord_flip() + 
  theme(panel.grid = element_blank()) 
# desktop users have a higher average session duration - do they read more carefully or just forget the tab open?


# ---- Browser ---- 

# Plot browser and sessions count
ga_byBrowser_df %>% 
  ggplot(aes(x = browser)) +
  geom_bar(aes(fill = browser)) +
  theme_minimal() +
  scale_fill_manual(values = viridis::viridis(7), aesthetics = "fill")

# Plot ordered by browser count
ga_byBrowser_df %>%
  count(browser) %>%
  mutate(browser = forcats::fct_reorder(browser, n, .desc = TRUE)) %>%
  ggplot(aes(x = browser, y = n)) + 
  geom_bar(aes(fill = browser), stat = "identity") + 
  theme_light() +
  scale_fill_manual(values = viridis::viridis(7), aesthetics = "fill") + 
  ylab("Sessions by browser") + 
  theme(panel.grid = element_blank()) 


# ---- Channel ---- 

# Plot                             
ga_byChannel_df %>% 
  ggplot(aes(x = channelGrouping)) +
  geom_bar(aes(fill = channelGrouping), show.legend = TRUE) +
  facet_wrap(~ deviceCategory, ncol = 1) +
  theme_light() +
  scale_fill_manual(values = viridis::viridis(7), aesthetics = "fill") + 
  coord_flip() + 
  theme(panel.grid = element_blank()) 


# ---- Country ---- 

# Plot ordered by continent count
ga_byCountry_df %>%
  count(continent) %>%
  mutate(continent = forcats::fct_reorder(continent, n, .desc = TRUE)) %>%
  ggplot(aes(x = continent, y = n)) + 
  geom_bar(stat = "identity", show.legend = FALSE, fill = viridis::viridis(3)[2]) + 
  geom_text(aes(x = continent, y = n, label = n),
            colour = "white",
            size = 3.3, 
            fontface = "bold", 
            hjust = 0.5, 
            vjust = 1.5) +
  theme_light() +
  theme(panel.grid = element_blank()) + 
  ylab("Sessions by continent")

# Plot ordered by country count
ga_byCountry_df %>%
  count(country) %>%
  mutate(country = forcats::fct_reorder(country, n, .desc = FALSE)) %>%
  ggplot(aes(x = country, y = n)) + 
  geom_bar(stat = "identity", show.legend = FALSE, fill = viridis::viridis(3)[2]) + 
  geom_text(aes(x = country, y = n, label = n),
            size = 3.3, 
            fontface = "bold", 
            hjust = 0, 
            vjust = +.45) +
  theme_light() +
  coord_flip() + 
  theme(panel.grid = element_blank()) + 
  ylab("Sessions by country")

# # Map 
# library(leaflet)
# library(widgetframe)
# leaflet() %>%
#   addTiles()
# 
# 
# pal <- colorBin("RdYlGn", domain = ga_byCountry_df$country, bins = bins)
# leaflet(data = ga_byCountry_df) %>% 
#   addPolygons(data = ga_byCountry_df,
#               fillColor = ~pal(country))
#   
#   
#   addPolygons(stroke = TRUE, 
#               color = "white", 
#               weight = 1, 
#               smoothFactor = 0.2, 
#               fillOpacity = .8,) %>%
#     addTiles() 
