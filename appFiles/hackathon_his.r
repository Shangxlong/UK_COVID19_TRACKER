library(tidyverse)      # data manipulation and visualization
library(lubridate)      # easily work with dates and times
require(fpp2)           # working with time series data
library(zoo)            # working with time series data
library(plotly)
if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')

devtools::install_github('bbc/bbplot')
library("bbplot") #colour scheme


#read the data set 
covid_ts <- read_csv("data/COVID19_UK_positive_cases_ts.csv")
#preview dataset
glimpse(covid_ts)
covid_ts1 <- covid_ts %>% rename(daily_pos_num = `Daily number of positive cases (new methodology)`,
                    cumla_pos_num = `Cumulative number of positive cases (new methodology)`,
                    date = `Earliest Specimen Date`) %>% 
  mutate(daily_pos_num = na_if(daily_pos_num, "Not available"),
         cumla_pos_num = na_if(cumla_pos_num, "Not available")) #parse "not available" into NA
covid_ts1$daily_pos_num <- as.numeric(covid_ts1$daily_pos_num)
covid_ts1$cumla_pos_num <- as.numeric(covid_ts1$cumla_pos_num)
#change the date into date format  
  covid_ts1$date <- dmy(covid_ts1$date)
# calculate moving weighted average
rollmean_covid  <- covid_ts1  %>% filter(Nation == "UK") %>% select(date, daily_pos_num) %>% 
    mutate(pos_num01 = rollmean(daily_pos_num, k = 3, fill = NA),
           pos_num02 = rollmean(daily_pos_num, k = 5, fill = NA),
           pos_num03 = rollmean(daily_pos_num, k = 7, fill = NA),
           pos_num04 = rollmean(daily_pos_num, k = 10, fill = NA),
           pos_num05 = rollmean(daily_pos_num, k = 14, fill = NA))

rollmean_covid_metric <- rollmean_covid %>% gather(metric, number, pos_num01:pos_num05)
write.csv(rollmean_covid_metric,"Dailycases")

g1<- rollmean_covid_metric %>% 
  filter(metric == "pos_num04") %>% 
  ggplot(aes(date, number)) + 
  geom_line(color = "#db4259", alpha = 0.6) +
  geom_col(position = "identity", fill = "orange", alpha = 0.6) +
  labs(title = "Daily recorded cases", y = "confirmes cases") 

ggplotly(g1)
# ssection 2 (the other dataset) -------------------------------------------
#read the data set 
deaths_ts <- read_csv("data/COVID-19_UK_deaths_ts.csv")
#preview dataset
glimpse(deaths_ts)
deaths_ts1 <- deaths_ts %>% rename(daily_d_num = `UK Daily count of deaths in all settings`,
                                 date = `Date published by DHSC`) %>% 
  mutate(daily_d_num = na_if(daily_d_num, "Not available")) #parse "not available" into NA
deaths_ts1$daily_d_num <- as.numeric(deaths_ts1$daily_d_num)

#change the date into date format  
deaths_ts1$date <- dmy(deaths_ts1$date)
# calculate moving weighted average
rollmean_deaths  <- deaths_ts1   %>% select(date, daily_d_num) %>% 
  mutate(d_num01 = rollmean(daily_d_num, k = 3, fill = NA),
         d_num02 = rollmean(daily_d_num, k = 5, fill = NA),
         d_num03 = rollmean(daily_d_num, k = 7, fill = NA),
         d_num04 = rollmean(daily_d_num, k = 10, fill = NA),
         d_num05 = rollmean(daily_d_num, k = 14, fill = NA))

rollmean_d_metric <- rollmean_deaths %>% gather(metric, number, d_num01:d_num05)
rollmean_d_metric

write.csv(rollmean_d_metric,"Deaths")

g2<- rollmean_d_metric %>% 
  filter(metric == "d_num04") %>% 
  ggplot(aes(date, number)) + 
  geom_line(color = "orange", alpha = 0.6) +
  geom_col(position = "identity", fill = "#db4259", alpha = 0.6) +
  labs(title = "Daily deaths published by DHSC", y = "confirmes deaths") 

ggplotly(g2)


  