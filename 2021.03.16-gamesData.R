library(tidyverse)
library(tidytuesdayR)
library(dplyr)

tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games

#data inspection
head(games)
summary(games)
str(games)

#PLOTS

##bar plot months
bar_months<- ggplot(games, aes(x=factor(month,levels=month.name))) + 
  geom_bar(colour=9, fill=9)+
  labs(x="Months", y="Counts", title="Number of games analysed per month since 2012 to 2021")
bar_months

    #the plot with the months doesn't show any relevant information, only the number of analysis made
     #in each months, without differentiate the years in the data collection was made

##bar plot years

bar_years<- ggplot(games, aes(x=year)) + 
  geom_bar(colour=9, fill=9)+
  labs(x="Years", y="Counts", title="Number of games analysed in each year")
bar_years

      # the plot with the years show how the number of counts had increase during the years, which can be related 
          # with the increase of number of games. 

#try plot by year (2019) and month
data_only2019<- games%>%
  filter(year==2019)
data_only2019

data_only2019$month = factor(data_only2019$month, levels = month.abb)
bar_months_year2019<- ggplot(games, aes(x=factor(month,levels=month.name))) + 
  geom_bar(colour=9, fill=9)+
  labs(x="Months", y="Counts per Month", title="Number of games analysed per month in 2019")
bar_months_year2019
#number of collection per month on the year of 2019. 

#in each years were data collected
unique(games$year)

#in 2012 in which month the data collected started?

data_only2012<- games%>%
  filter(year==2012)
data_only2012
unique(data_only2012$month)

    #In 2012, the data started to be collected in July


#plot month, year, avg_peak_perc
# change the values on the avg_peak_perc from character to numeric
games<- games%>%
  mutate(avg_peak_perc=as.numeric(gsub("%", "",avg_peak_perc)))  
str(games)

games$year <- factor(games$year, levels=c("2012","2013","2014","2015",
                                                    "2016","2017","2018","2019","2020","2021"))
#tile plot with all the years
tilePlot_allYears<-ggplot(games, aes(x=factor(month,levels=month.name), y=year))+
  geom_tile(aes(fill=avg_peak_perc))
  labs(x='Months', y='Years', title='Share of the average in the maximum value (avg / peak) in %, in each month and year', 
       subtitle ="Data from 2012 to 2021",
     color="(avg / peak) in %")+
tilePlot_allYears

# tile plot with only the years with information for all months
data_onlyCompleteMonths<- games%>%
  filter(year!=2012, year!=2021)
data_onlyCompleteMonths
unique(data_onlyCompleteMonths$year)

tilePlot_CompleteYears<-ggplot(data_onlyCompleteMonths, aes(x=factor(month,levels=month.name), y=year))+
  geom_tile(aes(fill=avg_peak_perc))+
  labs(x='Months', y='Years', title='Share of the average in the maximum value (avg / peak) in %, in each month and year',
       subtitle ="Data from  to 2013 to 2020", fill="(avg / peak) in %") +
  scale_fill_distiller(palette = "Oranges")
tilePlot_CompleteYears


# *********Begin -> Plot, Top10 Games with the higher sum_peak *********

#create a dataframe with two columns (gamename, and peak sum)

df_sum_peak_all_games <- data.frame(matrix(vector(), ncol = 2)) #to creat an empty data frame
colnames(df_sum_peak_all_games) <- c("gamename","peak_sum") #add columns names

#cicle to do the sum of the peaks per game and add the information to the previous empty df
for(gameid in unique(games$gamename)){
  subset <- filter(games, gamename == gameid)
  total_peak <-sum(subset$peak)
  newLine <- c(gameid, total_peak)
  df_sum_peak_all_games[nrow(df_sum_peak_all_games) + 1, ] <- newLine  
}

head(df_sum_peak_all_games)

# change column to numeric
df_sum_peak_all_games<- df_sum_peak_all_games %>%
  mutate(peak_sum=as.numeric(peak_sum))

#order the df from the game with the higher number of peaks to the lower
df_sum_peak_all_games <- df_sum_peak_all_games[order(df_sum_peak_all_games$peak_sum, decreasing=TRUE),]
head(df_sum_peak_all_games)

#create a subdataframe with only the top10
top10_subset <- head(df_sum_peak_all_games ,10)

#factorize the "gamenames" to keep the correct order in the graphic on the x axis
top10_subset$gamename <- factor(top10_subset$gamename, levels = top10_subset$gamename[order(top10_subset$peak_sum, decreasing=TRUE)])

#generate the graphic
graphic_top10_sum_peak <- ggplot(top10_subset, aes(x=gamename, y=peak_sum, fill=gamename)) +
  geom_col()+
  theme(axis.text.x = element_text(angle = 45,  hjust = 1))+
  scale_y_continuous( breaks=c(0, 20000000,40000000,60000000,80000000), 
                      labels=c("0","20M", "40M", "60M", "80M")) +
  labs(x = "Games' Name", y = "Peak Sum", 
         fill = "Games' Name", title = "Top 10 Games based on total sum of their streams")
  
graphic_top10_sum_peak


ggsave(plot=graphic_top10_sum_peak, "2021.03.16-games_sum_peak.jpg",  dpi = 180, width = 8, height = 7)

# *********End -> Plot, Top10 Games with the higher sum_peak *********
