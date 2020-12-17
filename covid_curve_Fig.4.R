

#code for the Maldives covid curve
#code taken from 
#https://joachim-gassen.github.io/tidycovid19/


install.packages("tidyverse")
remotes::install_github("joachim-gassen/tidycovid19")
#this might request to load first
#library(remotes)
install.packages("zoo")

library(tidyverse)
library(tidycovid19)
library(zoo)

#use a function to open data from Johns Hopking Covid Dataset

df <- download_merged_data(cached = TRUE, silent = TRUE)

#plot the covid curve for maldives
#including the daily new cases from the confirmed cases
#includes a roll mean of 7 days.
#
df %>%
  filter(country == "Maldives") %>%
  mutate(
    new_cases = confirmed - lag(confirmed),
    ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right")
  ) %>%
  filter(!is.na(new_cases), !is.na(ave_new_cases)) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = new_cases), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = ave_new_cases), color ="red") +
  scale_x_date(breaks = as.Date(c("2020-03-08", "2020-04-15", "2020-06-08", "2020-07-08","2020-07-23","2020-09-27","2020-12-07")),
               minor_breaks = as.Date(c("2020-04-15", "2020-07-08")))+
  theme_minimal()

