#### Titanic 
library(ggplot2)
library(dplyr)
library(tidyr)

# read in data
data <- read.csv("titanic_tidy.csv")

# add column with Survived or Died
data2 <- data %>%
  mutate(Status = ifelse(Survived == 1, "Survived", "Died"))

# separate name into title and rest of name
data4 <- data2 %>%
  separate(Name, into = c("Title", "Full_Name"), sep = "\\.", extra = "merge")

# assign colors to died and survived
scale_fill_manual(values = location_colors)
location_colors <- c("black", "#B31B1B")

# plot number of indv that survived or died
ggplot(data2, aes(x= Status)) + geom_bar()

# plot number of indv (stacked) per fare
ggplot(data2, aes(x = Fare, fill = Status)) +
  geom_histogram(binwidth = 5) +  # Use dodge to show bars side by side
  labs(title = "Survival Count by Fare Price",
       x = "Fare Price",
       y = "Count",
       fill = "Status") + theme_minimal()

# plot number of indv that survived by age
ggplot(data2, aes(x = Age, fill = Status)) +
  geom_histogram(binwidth = 1) + 
  labs(title = "Survival Count by Age",
       x = "Age",
       y = "Count",
       fill = "") +
  theme_minimal()


# plot # indv survival by title
ggplot(data4, aes(x = Title, fill = Status)) +
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
  labs(title = "Survival of Passengers by Class",
       x = "Passenger Class",
       y = "Number of Passengers",
       fill = "Status") + theme_minimal() + scale_fill_manual(values = location_colors)


