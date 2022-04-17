install.packages("tidyverse")

#Data Importing
library(dplyr) #provides "grammar" for data manipulation and for operating on data frames.
library(tidyr) #to "tidy" the data
library(tidyverse)
players <- read.csv("players.csv",TRUE,",") #import and load dataset
players #view dataset
head(players, n=5) #returns first 5 rows of the dataset
tail(players, n=5) #returns last 5 rows of the dataset
str(players) #displays the internal structure
dim(players) #returns the dimension of the dataset

#Data Cleaning
is.na(players)  #checking for missing values 
bad <- is.na(players) #another method to check for missing values
print(bad)
sum(is.na(players)) #returns the count of missing values

#rename the column "Markey.Value.In.Millions.A.." to "Market.Value"
players <- rename(players, Market.Value = Markey.Value.In.Millions.Â..) 
players


#this method drops the columns, "Number.Of.Substitute.In" and "Number.of.Substitute.Out"
# These columns will not be needed and included in the analysis. 
new_df <- select(players,-starts_with("Number")) 
new_df                         

str(new_df)
summary(new_df) #summary information of the variables. 

#Histogram
hist(new_df$Age) #Histogram of age of players, aes=aesthetics 
ggplot(new_df, aes(x=Age)) +
  geom_histogram(bins=15, color="black", fill="lightblue")

hist(new_df$Goals) #Histogram of goals of players
ggplot(new_df, aes(x=Goals)) +
  geom_histogram(bins=15, color="black", fill="lightblue")


#Exploratory Data Analysis

# Add a new variable AgeClass, "Old" if a player is 28 or older, "young" if player is younger than 28. 

new_df$AgeClass <- as.factor(ifelse(new_df$Age >= 28,"Old","Young"))
(new_df)

#finding the average, minimum, and maximum age for "Old" and "Young"
with(new_df, tapply(Age,AgeClass, mean))
with(new_df, tapply(Age,AgeClass, min))
with(new_df, tapply(Age,AgeClass, max))

#outputs the Maximum and Minimum age of players in their respective countries
with(new_df, tapply(Age, list(AgeClass, Country), FUN=max))

#display the Name, Age, and number of Goals by players age 28 and over
new_df[new_df$Age >=28, c("Name", "Age", "Goals")]

#display the Name, Age, and number of Goals by players under 28
new_df[new_df$Age <28, c("Name", "Age", "Goals")]


names(new_df) #display names of the column 

summary(new_df$Market.Value)

#scatter plot of players market value 
plot <- ggplot(new_df, aes(x=Age, y=Market.Value, col=AgeClass)) +
  geom_point()
print(plot)


cor(new_df$Age, new_df$Market.Value)

cor(new_df$Goals, new_df$Market.Value)





