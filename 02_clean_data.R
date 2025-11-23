library(tidyverse)
library(janitor)

# 1) Load original data and clean column names ----
sleep <- read_csv("data/Sleep_health_and_lifestyle_dataset.csv") %>% 
  clean_names()

# 2) Convert categorical variables to factors ----
sleep <- sleep %>%
  mutate(
    gender = as.factor(gender),
    occupation = as.factor(occupation),
    bmi_category = factor(
      bmi_category,
      levels = c("Underweight", "Normal", "Overweight", "Obese")
    ),
    sleep_disorder = as.factor(sleep_disorder)
  )

# 3) Split blood pressure into systolic / diastolic ----
sleep <- sleep %>%
  separate(
    blood_pressure,
    into = c("systolic", "diastolic"),
    sep = "/",
    convert = TRUE
  )

# 4) Put systolic/diastolic next to heart_rate ----
sleep <- sleep %>%
  relocate(systolic, diastolic, .after = heart_rate)

# 5) Quick checks ----
print(colSums(is.na(sleep)))
glimpse(sleep)

# 6) Save cleaned data for Shiny app ----
write_csv(sleep, "data/sleep_clean.csv")
