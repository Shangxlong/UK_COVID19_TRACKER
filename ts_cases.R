library(ggplot2)
library(scales)
library(tidyverse)
library(hrbrthemes)

positive_cases <- read.csv("data/UKcases.csv", sep=",", header=T)
# covert date to Date class
positive_cases$Date <- as.Date(positive_cases$Date, "%d/%m/%Y")

P_bar <- ggplot(positive_cases, aes(x = Date, y = cases)) +
  geom_histogram(stat = "sum") +
  geom_smooth(size = 1.1, se=FALSE )
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
P_bar
