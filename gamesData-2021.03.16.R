library(tidyverse)
library(tidytuesdayR)

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
