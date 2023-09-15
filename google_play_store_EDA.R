#### Google Play Store - Exploratory Data Analysis ----

## Jehangeer Aswani ----

# install.packages("tidyverse")


library(tidyverse)

# Setting theme
theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))


# Importing the data set
ps_data <- read_csv("googleplaystore.csv")

# Column names
names(ps_data)

# Dimension
dim(ps_data)

## We have 13 variables with 10840 observations

# Glimpse of the dataset
glimpse(ps_data)

# Checking missing values
sum(is.na(ps_data))

## We have 1475 missing values in this data set

# First six observation of the data set
head(ps_data)

# Changing few variables data type to factor
ps_data$Category <- as.factor(ps_data$Type)
ps_data$Type <- as.factor(ps_data$Type)
ps_data$`Content Rating` <- as.factor(ps_data$`Content Rating`)
ps_data$Genres <- as.factor(ps_data$Genres)

# Excluding all missing values 
ps_data <- na.omit(ps_data)

# Dimension
dim(ps_data)

## Now we have 9365 observation in our data set

## The app rating mirrors users' feedback and significantly influences the app's success on the Google Play Store.

# Checking the distribution of Rating variable

summary(ps_data$Rating)

ps_data |> 
  ggplot(aes(Rating)) +
  geom_histogram() +
  labs(title = "Play Store Apps Ratings Distribution")

ps_data %>%
  ggplot(aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "blue", alpha = 0.7) +
  geom_density(aes(y = after_stat(count * (0.5 * sum(..count..)))), color = "red", alpha = 0.5) +
  labs(title = "Play Store Apps Ratings Distribution")

# Rating average

mean(ps_data$Rating)

## The distribution of app ratings on the Play Store is negatively skewed, indicating that a majority of the apps receive higher ratings. On average, apps tend to have favorable ratings, with the mean rating hovering around 4.2.


# Paid vs Free Apps

ps_data %>%
  group_by(Type) %>%
  summarise(Total_Apps = n()) %>%
  mutate(Percentage = (Total_Apps / sum(Total_Apps)) * 100) %>%
  ggplot(aes(x = Type, y = Total_Apps)) +
  geom_bar(stat = "identity", fill = c("yellow", "lightblue")) +
  geom_text(aes(label = paste0(sprintf("%.1f", Percentage), "%")), 
            position = position_stack(vjust = 1), 
            fontface = "bold") +
  labs(title = "Total Apps by Type", y = "Total Apps", x = "Type")

# Content ratings provide information about the minimum level of maturity required to access the content in apps. However, they do not indicate whether an app is specifically tailored for users of a certain age group.


content_ratings <- ps_data |> 
  group_by(`Content Rating`) |> 
  summarise(Total_Apps = n())

# Calculate percentages
content_ratings <- content_ratings %>%
  mutate(percentage = paste(round((Total_Apps / sum(Total_Apps)) * 100, 2), "%"))

content_ratings |> 
  ggplot(aes(x = `Content Rating`, y = Total_Apps)) +
  geom_bar(stat = 'identity', fill = c("black", "#E74C8C", "yellow", "lightblue", "#229954", "#2ECC71")) +
  geom_text(aes(label = paste0(sprintf("%s", percentage))), 
            position = position_stack(vjust = 1), 
            fontface = "bold") +
  ylim(0, 8000) +
  labs(x = "Content Rating category", 
       y = "No. of Apps", 
       title = "Application Counts vs. Content Rating")
  


#  Comparing Apps Based on Installations
# The number of installs indicates an app's popularity in the Play Store.

install_apps <- ps_data |> 
  group_by(Installs) |> 
  summarise(total_installs = n())


# Count and categorize Installs
ps_data %>%
  mutate(Installs_start = gsub("[^0-9]", "", Installs),  
         Installs_start = as.numeric(ifelse(Installs_start == "", NA, Installs_start)),
         Installs_category = case_when(
           is.na(Installs_start) | Installs_start < 1000 ~ "<1k",
           Installs_start < 10000 ~ "1k-10k",
           Installs_start < 100000 ~ "10k-100k",
           Installs_start < 1000000 ~ "100k-1M",
           Installs_start < 10000000 ~ "1M-10M",
           Installs_start < 100000000 ~ "10M-100M",
           TRUE ~ ">100M"
         )) %>%
  group_by(Installs_category) %>%
  summarise(count_installs = n()) %>%
  mutate(Percentage = paste(round((count_installs / sum(count_installs)) * 100, 2), "%")) |>
  ggplot(aes(x = Installs_category, y = count_installs)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  geom_text(aes(label = paste0(sprintf("%s", Percentage))), 
            position = position_stack(vjust = 1.02), 
            fontface = "bold") +
  labs(x = "No: Downlaods", y = "No: of Apps", 
       title = "Total Numbers of Apps Downloaded")
  











