## Dan Smilowitz
## DATA 608 Assignment 2


library(bigvis)
library(ggplot2)
library(dplyr)
library(scales)


# load data ####
github_folder <- "https://raw.githubusercontent.com/dsmilo/DATA608/master/mod2/data/"
boros <- c("BK", "BX", "MN", "QN", "SI")
by_boro <- list("BK" = data.frame(), "BX" = data.frame(), "MN" = data.frame(), "QN" = data.frame(), "SI" = data.frame())

for (i in 1:length(boros)){
	by_boro[[boros[i]]] <- read.csv(paste0(github_folder, boros[i], ".csv"), stringsAsFactors = FALSE)
}

nyc <- data.frame(stringsAsFactors = FALSE)
for(i in 1:length(boros)) {
	nyc <- rbind(nyc, by_boro[[boros[i]]])
}


# Question 1 ####
nyc <- nyc %>% filter(YearBuilt > 1850, YearBuilt < 2017, LotArea > 100, AssessTot < 10000000, NumFloors > 0)

ggplot(nyc, aes(x=YearBuilt)) + geom_histogram(alpha = 0.75, binwidth = 1, col = "black") + scale_x_reverse("", breaks = seq(1850, 2020,10)) + scale_y_continuous("") + coord_flip() + ggtitle("Number of Buildings Constructed By Year\n")
# There are suspiciously high numbers of buildings reported as built 10-year intervals before roughly 1985; this suggests that the data may be inaccurate
ggsave("Figure1.png", height = 9, width = 6)


# Question 2 ####
floors_by_year <- condense(bin(nyc$NumFloors, 10, origin = 0), bin(nyc$YearBuilt, 1, origin = 1850))

autoplot(floors_by_year) + scale_y_reverse("Year Built", breaks = seq(1850, 2020, 10)) + scale_x_continuous("Number of Floors", breaks = seq(0, 120, 20)) + scale_fill_gradient("Buildings", trans = "log", low = "red", high = "green", breaks = 10^(0:5)) + theme(legend.position = "bottom", legend.key.width = unit(0.75, "in")) + ggtitle("Number of Buildings per Year by Number of Floors\n")
ggsave("Figure2.png", height = 9, width = 6)


# Question 3 ####
nyc$AssessPerFloor <- nyc$AssessTot / nyc$NumFloors
per_floor_by_year <- condense(bin(nyc$YearBuilt, 1), z = nyc$AssessPerFloor)
autoplot(per_floor_by_year) + geom_smooth(se = FALSE, lty = 3, col = "red") + xlim(1930, 1955) + theme(legend.position = "none") + ggtitle("Mean Assessed Value per Floor by Year\n(Surrounding World War II)\n") + scale_y_continuous("Value per Floor", limits = c(0, 600000), labels = comma)
ggsave("Figure3.png", height = 9, width = 6)
