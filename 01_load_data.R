# Load required packages ----
library(tidyverse)  # for read_csv, dplyr, ggplot2
library(janitor)    # for clean_names()
sleep_raw <- read_csv("data/Sleep_health_and_lifestyle_dataset.csv") #load the sleep dataset
sleep <- sleep_raw %>%
  clean_names()
# Quick look at the data ----
glimpse(sleep)      # see column names and types
head(sleep, 10)     # first 10 rows
summary(sleep)      # basic stats for each column
