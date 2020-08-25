
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, dplyr, summarytools)

# Import data
#-----------------------------------------------
meta_data <- read.csv("data/finalData_24_8.csv")

#Summary Stats for whole data
sum_whole <- meta_data %>% descr(stats = "fivenum") %>% tb()

# Grouped summary statis
grouped_wlfresh <- meta_data %>% group_by(wlfresh) %>% descr(stats = "fivenum") %>% tb()
grouped_canada <- meta_data %>% group_by(canada) %>% descr(stats = "fivenum") %>% tb()

#saving data
write_csv(sum_whole, "data/sum_whole.csv")
write_csv(grouped_wlfresh , "data/grouped_wlfresh .csv")
write_csv(grouped_canada, "data/grouped_canada.csv")







