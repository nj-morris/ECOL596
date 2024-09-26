#### Titanic 
library(ggplot2)
library(dplyr)
library(tidyr)

# read in data
data <- read.csv("titanic_tidy.csv")
View(data)

# add column with Survived or Died
data2 <- data %>%
  mutate(Status = ifelse(Survived == 1, "Survived", "Died"))

# separate name into title and rest of name
data3 <- data2 %>%
  separate(Name, into = c("Title", "Full_Name"), sep = "\\.", extra = "merge")

# create age groups
data3$AgeGroup <- cut(data4$Age, breaks=c(0, 18, 35, 50, 65, Inf), 
                   labels=c("0-18", "19-35", "36-50", "51-65", "65+"))

# filter to only indv that survived
survivors <- data3 %>% filter(Survived == 1)

# assign colors to died and survived
 # scale_fill_manual(values = survival_colors)
survival_colors <- c("black", "#B31B1B")

# plot number of indv that survived or died
ggplot(data2, aes(x= Status, fill = Status)) + geom_bar() + scale_fill_manual(values = survival_colors)

# plot number of indv (stacked) per fare
ggplot(data2, aes(x = Fare, fill = Status)) +
  geom_histogram(binwidth = 5) +  
  labs(title = "Survival by Fare Price",
       x = "Fare Price",
       y = "Count",
       fill = "Status") + theme_minimal() + scale_fill_manual(values = survival_colors)

ggplot(survivors, aes(x = Fare)) +
  geom_histogram(color = "#B31B1B", binwidth = 5) +  
  labs(title = "Survival by Fare Price",
       x = "Fare Price",
       y = "Count") + theme_minimal() 

# plot number of indv that survived by age
ggplot(data2, aes(x = Age, fill = Status)) +
  geom_histogram(binwidth = 1) + 
  labs(title = "Survival Count by Age",
       x = "Age",
       y = "Count",
       fill = "") +
  theme_minimal() + scale_fill_manual(values = survival_colors)


# plot # indv survival by title
ggplot(data3, aes(x = Title, fill = Status)) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) + 
  labs(title = "Survival Count by Title",
       x = "Title",
       y = "Count",
       fill = "") +
  theme_minimal() + scale_fill_manual(values = survival_colors)

# plot # indv survival by title - only survivors
ggplot(survivors, aes(x = Title, fill = Sex)) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) + 
  labs(title = "Survival Count by Title",
       x = "Title",
       y = "Count",
       fill = "") +
  theme_minimal() 

# plot # indv survival by class
ggplot(data2, aes(x = Pclass, fill = Status)) +
  geom_bar(position = "dodge") +  # Use dodge to show bars side by side
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Survival of Passengers by Class",
       x = "Passenger Class",
       y = "Number of Passengers",
       fill = "Status") + theme_minimal() + scale_fill_manual(values = survival_colors)

# create pie chart of survived/died by age group
data_summary <- data3 %>%
  group_by(AgeGroup, Survived) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) # Calculate percentages for each group

ggplot(data_summary, aes(x = "", y = Percentage, fill = factor(Survived))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~AgeGroup) + # Separate pie charts by Age Group
  labs(title = "Titanic Survival by Age Group",
       fill = "Survived?",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("0" = "black", "1" = "#B31B1B"), # Custom colors for survived and died
                    labels = c("0" = "Died", "1" = "Survived")) + # Custom legend labels
  theme_void() # Clean the chart by removing axes

# create pie chart of survived / died by age group and sex
data_summary2 <- data3 %>%
  group_by(AgeGroup, Survived, Sex) %>%
  summarize(Count = n()) %>%
  group_by(AgeGroup) %>% # Group by AgeGroup to calculate percentage within each group
  mutate(Percentage = Count / sum(Count) * 100) # Calculate percentages for each subgroup

ggplot(data_summary2, aes(x = "", y = Percentage, fill = interaction(Survived, Sex))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~AgeGroup) + # Separate pie charts by Age Group
  labs(title = "Survival and Sex by Age Group",
       fill = "Survival and Sex", # Legend title
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("0.male" = "black", "0.female" = "darkgrey", 
                               "1.male" = "#B31B1B", "1.female" = "red"), 
                    labels = c("0.male" = "Died (Male)", "0.female" = "Died (Female)",
                               "1.male" = "Survived (Male)", "1.female" = "Survived (Female)")) + # Custom labels
  theme_void() # Clean the chart by removing axes

# create pie chart of survived / died by age group and sex - reordered
data_summary2$Survived_Sex <- factor(interaction(data_summary2$Survived, data_summary2$Sex),
                                  levels = c("0.male", "0.female", "1.male", "1.female")) 

ggplot(data_summary2, aes(x = "", y = Percentage, fill = Survived_Sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~AgeGroup) + # Separate pie charts by Age Group
  labs(title = "Survival and Sex by Age Group",
       fill = "Survival and Sex", # Legend title
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("0.male" = "black", "0.female" = "darkgrey", 
                               "1.male" = "#B31B1B", "1.female" = "red"), 
                    labels = c("0.male" = "Died (Male)", "0.female" = "Died (Female)",
                               "1.male" = "Survived (Male)", "1.female" = "Survived (Female)")) + # Custom labels
  theme_void() # Clean the chart by removing axes
