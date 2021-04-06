library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(magrittr)
tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

unvotes <- tuesdata$unvotes
roll_calls<- tuesdata$roll_calls
issues<- tuesdata$issues

head(unvotes)
summary(unvotes)

head(roll_calls)
summary(roll_calls)

head(issues)
summary(issues)


#number of countries involved
all<-distinct(unvotes,country)
count(all)

#join all the dataframes
data_unvotes_roll_calls_issues <- unvotes %>%  
  inner_join(roll_calls)%>%
  inner_join(issues)
head(data_unvotes_roll_calls_issues)
summary(data_unvotes_roll_calls_issues)


# **** Begin -> % of votes that are "Yes" for the countries: France, India, United States of America *****


#create subsets for each country (US, FR, IN), with only three columns, 
  #country, vote, year <- (retrived from the date)

data_unvotes_roll_calls_issues_US <- data_unvotes_roll_calls_issues %>%
  filter(country == "United States") %>%
  mutate(year = format(date, format = "%Y")) %>%
  select(country, vote, year)
head(data_unvotes_roll_calls_issues_US)

data_unvotes_roll_calls_issues_FR <- data_unvotes_roll_calls_issues %>%
  filter(country == "France") %>%
  mutate(year = format(date, format="%Y")) %>%
  select(country, vote, year)
head(data_unvotes_roll_calls_issues_FR)

data_unvotes_roll_calls_issues_ID <- data_unvotes_roll_calls_issues %>%
  filter(country == "India") %>%
  mutate(year=format(date, format = "%Y")) %>%
  select(country, vote, year)
head(data_unvotes_roll_calls_issues_ID)

# create a function to calculate the percentage of yes, and a new data frame with three columns
 # country, year and percentage_yes

calculate_yes_percentage <- function(data, df_name) {
  df_name <- data.frame(matrix(vector(), ncol = 3))
  colnames(df_name) <-c("country","year","percentage_yes")
  for (i in unique(data$year)){
    subset_year=filter( data, year==i)
    count_votes=count(subset_year, vote)
    percentage_yes=round(((count_votes[count_votes$vote=="yes","n"]/(sum(count_votes[ ,"n"])))*100),digits=2)
    print(paste("In the", i, "the percentage of yes were", percentage_yes, "in",data[1, "country"] ))
    newLine <- c(data[1, "country"], i, percentage_yes)
    df_name[nrow(df_name) + 1, ] <- newLine  
  }  
  return(df_name)
}

# run the previous function for each subset previous created for each country

df_percentage_US <- calculate_yes_percentage(data_unvotes_roll_calls_issues_US, df)
df_percentage_FR <- calculate_yes_percentage(data_unvotes_roll_calls_issues_FR, df)
df_percentage_ID <- calculate_yes_percentage(data_unvotes_roll_calls_issues_ID, df)


#join all the dataframes generated in the previous run

data_US_FR_ID_percentage_yes <- df_percentage_US %>%
  rbind(df_percentage_FR) %>%
  rbind(df_percentage_ID)
head(data_US_FR_ID_percentage_yes)
summary(data_US_FR_ID_percentage_yes)

# change year for a numeric to use for x axis 

data_US_FR_ID_percentage_yes <- data_US_FR_ID_percentage_yes %>%
  mutate(year=as.numeric(year)) 

# generate the graphic 

graph_percentage_yes_FR_US_ID <- ggplot(data_US_FR_ID_percentage_yes, aes(x=year, y=percentage_yes))+ 
  geom_line(aes(colour=country))+
  xlim(1940, 2020)
graph_percentage_yes_FR_US_ID 

ggsave(plot=graph_percentage_yes_FR_US_ID, "2021.03.30-Percentage_Yes_FR_US_ID.jpg",  dpi = 180, width = 8, height = 7)
# ****** End -> % of votes that are "Yes" for the countries: France, India, United States of America*********


