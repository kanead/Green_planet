#' Anomaly Detection for The Green Planet
#' Get page views from Wikipedia and examine anomalies in the time series
#' 5th October 2022
#' Adam Kane
#' Code extracts the hits from Wikipedia for a given article and then determines if they're outliers
#' a positive outlier would suggest something happened on the day to increase traffic to the page
#' i.e. the broadcast of the documentary 
#' we do this for a single article/ species as a sanity check and then run it over
#' multiple articles 

#' housekeeping
rm(list = ls())
graphics.off()

#' load the packages
#' might need to install R tools from the site for these
library(tidyverse)
library(pageviews)
# install.packages("devtools")
# devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

#####' GET THE WIKIPEDIA HITS ----
#' you can specify the language of the wiki page
#' the start and end dates, I usually run it for the year
#' the user type which can be a person (user) or a bot (automated)
#' then the platform people used to access the page - desktop or mobile-web
#' done for both here
#' Good to check one against the output of the function when we run multiple
#' articles 
mydata <-
  article_pageviews(
    project = "en.wikipedia",
    article = "Durian",
    start = as.Date('2021-06-01'),
    end = as.Date("2022-04-30"),
    user_type = c("user"),
    platform = "all"
  )

head(mydata)

#' have a look at the data 
# ggplot(mydata, aes(x = date, y = views)) + geom_point()

#' export if needed
# write.csv(x = mydata, file = "Miconia.csv", row.names = F)

#' extract the data we want i.e. just the columns with dates and page views
subdata <- 
  dplyr::select(mydata, date, views)
subdata <- data.frame(subdata)
head(subdata)

####' RUN THE ANOMALY DETECTION ----
res = AnomalyDetectionTs(subdata,
                         max_anoms = 0.01,
                         # max number of values to detect as a % of the data
                         direction = 'pos',
                         #' only want to look at positive anomalies
                         plot = TRUE)
res$plot # plot it
res$anoms # look at the dates that have anomalies and the Wiki page views
# on those dates, you could compare these to the dates of interest


####' GET DATA FOR MULTIPLE WIKIPEDIA ARTICLES ----
#' create a function to extract data for multiple articles
get_wiki <-
  function(x) {
    article_pageviews(
      project = "en.wikipedia",
      article = x      ,
      start = as.Date('2021-06-01'),
      end = as.Date("2022-04-30"),
      user_type = "user",
      platform = c("all") # only mobile access here
    )
  }

#' give a vector of names of articles you want to pull from Wiki
#' could load this in from a spreadsheet if easier
#' these names are control time series for the Green Planet to check against
#' the featured species 
species_names <-
  c("Durian",
    "Elaeocarpus_bojeri",
    "Hevea_brasiliensis",
    "Passiflora",
    "Heliconia",
    "Dracula_simia",
    "Jabuticaba")

# extract the article views based on the vector of names 
output <- species_names %>% get_wiki
levels(as.factor(output$article))


anom_func <-  function(x) {
  res = AnomalyDetectionTs((data.frame(x[6:7])),
                           # 6 and 7 are the date and views column
                           max_anoms = 0.01,
                           # play around with this value
                           direction = 'pos',
                           plot = FALSE
  )
}

anoms <- output %>%
  group_by(article) %>% nest() %>%
  mutate(count_anom = map(data, ~ anom_func(.)))

names(anoms$count_anom) <- species_names # this puts the names of the species on the list
# anoms$count_anom 

#' now pull out the number of anomalies by article
#' we don't need the other info
anom_by_species <- map(anoms$count_anom, 1,)
# anom_by_species


anom_by_speciesdf <- anom_by_species %>% enframe() %>% # this converts from a list to a data frame type
  unnest()                              # and then unnests it so you can see everything
anom_by_speciesdf


#' make sure it matches to when we tried a single article (Durian)
res$anoms # original 
check <- anom_by_speciesdf %>% filter(.,name == "Durian"); check # from group
res$anoms$anoms == check$anoms

write.csv(x = anom_by_speciesdf, file = "control_anomalies.csv", row.names = F)
