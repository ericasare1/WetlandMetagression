
library("tidyverse")

#.......... Summary statistics for wetland meta-data ________
#..Whole data
meta_data <- read.csv("data/finalData_24_8.csv")

summary_whole <-data.frame(psych::describe(meta_data))
data2 %>% View()

#Summary by category

sumary_wlfresh <- data.frame(psych::describeBy(meta_data, meta_data$wlfresh))
sumary_canada <- data.frame(psych::describeBy(meta_data, meta_data$canada))

