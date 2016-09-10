## Dan Smilowitz
## DATA 608 Assignment 1


# load data ####
companies <- read.csv('https://raw.githubusercontent.com/jlaurito/CUNY_IS608/master/lecture1/data/inc5000_data.csv', header = TRUE, stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)


# Question 1 ####
by_state <- companies %>% group_by(State) %>% summarize(Count = n()) %>% arrange(desc(Count))

ordered_states <- rev(by_state$State)  # includes DC & Puerto Rico
by_state$State <- factor(by_state$State, levels = ordered_states)

ggplot(by_state, aes(x = State, y = Count)) + geom_bar(stat = "identity", alpha = 0.5, col = "black") + coord_flip() + ggtitle("Number of Fast-Growing Companies per State\n") + scale_y_continuous("Number of Companies") + scale_x_discrete("")

ggsave('Figure1.png', height = 16, width = 9)


# Question 2 ####
third <- by_state$State[3]  # NY
newyork <- subset(companies, State == "NY" & complete.cases(companies))
newyork <- newyork[!newyork$Employees %in% boxplot.stats(newyork$Employees)$out, ]

by_industry <- newyork %>% group_by(Industry) %>% 
  summarize(Avg_Emp = median(Employees)) %>% arrange(desc(Avg_Emp))

ordered_industries <- rev(by_industry$Industry) # 25 industries
newyork$Industry <- factor(newyork$Industry, levels = ordered_industries)

ggplot(newyork, aes(x = Industry, y = Employees)) + geom_boxplot(outlier.shape = NA, alpha = 0.5) + coord_flip() + ggtitle("Employment by Industry in New York\n") + scale_x_discrete("")

ggsave('Figure2.png', height = 16, width = 9)


# Question 3 ####
per_employee <- companies %>% group_by(Industry) %>% mutate(Rev_per_Emp = Revenue / Employees / 1000)
per_employee <- per_employee[
  !per_employee$Rev_per_Emp %in% boxplot.stats(per_employee$Rev_per_Emp)$out &
    !per_employee$Employees %in% boxplot.stats(per_employee$Employees)$out &
    !per_employee$Revenue %in% boxplot.stats(per_employee$Revenue)$out, ]

per_employee <- per_employee[complete.cases(per_employee), ]
by_industry_2 <- per_employee %>% summarize(med = median(Rev_per_Emp)) %>% arrange(desc(med))

ordered_industries_2 <- rev(by_industry_2$Industry)
per_employee$Industry <- factor(per_employee$Industry, levels = ordered_industries_2)

ggplot(per_employee, aes(x = Industry, y = Rev_per_Emp)) + geom_boxplot(outlier.shape = NA, alpha = 0.5) + coord_flip() + ggtitle("Revenue Generated per Employee by Industry\n") + scale_y_continuous("Revenue per Employee ($k)") + scale_x_discrete("")

ggsave('Figure3.png', height = 16, width = 9)
