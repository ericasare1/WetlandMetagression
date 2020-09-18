
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, dplyr, summarytools)

# Import data
#-----------------------------------------------
meta_data <- read_csv("data/metadata2.csv")

#Summary Stats for whole data
sum_whole <- meta_data %>% descr(stats = "common") %>% tb()

# Grouped summary statis
grouped_wlfresh <- meta_data %>% group_by(wlfresh) %>% descr(stats = "common") %>% tb()
grouped_canada <- meta_data %>% group_by(canada) %>% descr(stats = "common") %>% tb()

#saving data
write_csv(sum_whole, "output/sum_whole.csv")
write_csv(grouped_wlfresh , "output/grouped_wlfresh.csv")
write_csv(grouped_canada, "output/grouped_canada.csv")

	
	





