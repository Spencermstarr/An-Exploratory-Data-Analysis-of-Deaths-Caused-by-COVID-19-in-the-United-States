# R script for the final project in STAT 515

# make sure my working directory is correct
getwd()
setwd('C:/Users/spenc/OneDrive/Documents/George Mason University/Fall 2020/STAT 515/Project 2')

# load useful packages
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(randomForest)
library(class)
library(scales)



# import and load the data sets
# This first dataset is from the CDC; it contains data on cumulative 
# deaths from COVID-19, total deaths from all causes, Pneumonia deaths, 
# and Influenza deaths in the United States starting at 2/1/20, 
# the beginning of the pandemic and going until 11/18/20. The deaths 
# are broken down by sex (male/female), age groups, and state.
deaths <- read.csv('Provisional_COVID-19_Deaths_by_Sex_Age_&_State.csv')
str(deaths)
summary(deaths)
head(deaths)
tail(deaths)
class(deaths)


# The second dataset, which is also from the CDC, contains data on
# weekly deaths in the US from all causes including COVID-19 from
# 1/5/19 until 11/7/20 which is also broken down by state.
weeklydeaths <- read.csv('Weekly_Counts_of_Deaths_by_State_&_Select_Causes_2019-2020.csv')
str(weeklydeaths)
# convert the Week Ending Date field in the dataframe from a chr to a date
weeklydeaths$Week.Ending.Date <- mdy(weeklydeaths$Week.Ending.Date)
str(weeklydeaths)
summary(weeklydeaths)
head(weeklydeaths)
tail(weeklydeaths)
class(weeklydeaths)


# The third dataset contains the raw weekly counts of excess
# mortality for a multitude of different countries around the world
# starting on 1/5/20 and going until 10/18/20.
raw_excess_mortality <- read.csv('excess mortality raw death counts.csv')
str(raw_excess_mortality)
# convert the Date field in the dataframe from a string to a date
raw_excess_mortality$Date <- mdy(raw_excess_mortality$Date)
str(raw_excess_mortality)
summary(raw_excess_mortality)
head(raw_excess_mortality)
tail(raw_excess_mortality)
class(raw_excess_mortality)

# The fourth dataset has data on the P-scores of excess mortality
# in the US broken down by age buckets.
emPs <- read.csv('excess mortality P-scores by age.csv')
str(emPs)
emPs$Date <- mdy(emPs$Date)
str(emPs)
length(emPs)
dim.data.frame(emPs)

# The fifth dataset is an international dataset so I can make 
# international comparisons, it is from ourworldindata.org, 
# so I will name is owid.
owid <- read.csv('owid covid data.csv')
str(owid)
# convert the date field in the dataframe from a string to a date
owid$date <- ymd(owid$date)
str(owid)
summary(owid)
head(owid)
tail(owid)
class(owid)
typeof(owid)
# add a death rate (defined as deaths per 100,000 population)
# to the owid dataframe that I can use later
dphth <- owid$total_deaths_per_million*10
tail(dphth)
class(dphth)
typeof(dphth)
length(dphth)
owid$death_rate <- dphth
owid$death_rate



# create a dataframe of the excess mortality data just for the US
em.USA <- raw_excess_mortality[raw_excess_mortality$Entity == "United States", ]
em.USA
class(em.USA)
str(em.USA)
tail(em.USA)
em.USA$Date <- ymd(em.USA$Date)
str(em.USA)

# line chart of excess mortality in the US during the COVID-19 pandemic
ggplot(data = em.USA, aes(x = Date)) +
  geom_line(aes(y = Deaths..2020..all.ages, group = 1, color = 'red')) +
  geom_point(aes(y = Deaths..2020..all.ages, group = 1, color = 'red')) +
  geom_line(aes(y = Average_deaths_from_2015_to_2019_all_ages, group = 1, 
                color = 'black'), size = 1) +
  geom_line(aes(y = deaths_2019_all_ages, group = 1, color = 'grey')) +
  geom_line(aes(y = deaths_2018_all_ages, group = 1, color = 'blue')) +
  geom_line(aes(y = deaths_2017_all_ages, group = 1, color = '#006600')) +
  geom_line(aes(y = deaths_2016_all_ages, group = 1, color = '#660099')) +
  geom_line(aes(y = deaths_2015_all_ages, group = 1, color = 'brown')) +
  scale_color_identity(name = 'Legend',
                       breaks = c('red', 'black', 'grey', 'blue', 
                                  '#006600', '#660099', 'brown'),
                       labels = c('2020', 'Average, 2015-2019', '2019',
                                  '2018', '2017', '2016', '2015'), 
                       guide = 'legend') +
  scale_x_date(labels = date_format("%m/%d"), 
               breaks = "8 weeks", minor_breaks = "8 weeks",
               limits = as.Date(c('2020-01-05', '2020-11-29'))) +
  scale_y_continuous(label = comma, breaks = seq(50000, 80000, 5000), 
                     minor_breaks = seq(50000, 80000, 5000)) +
  coord_cartesian(ylim = c(48000, 80000)) +
  labs(x = '', y = 'Total deaths from all causes',
       title = 'Excess Mortality in the United States during the COVID-19 pandemic:
Raw number of deaths from all causes compared to recent years', 
       subtitle = 'Shown is how the raw number of weekly deaths in 2020 differs from the number of deaths in the previous five years (2015-2019).',
       caption=expression(paste('Sources: Human Mortality Database (2020), OurWorldinData.org'))) + 
  theme_light() +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 8),
        plot.caption = element_text(size = 9, hjust = 0))


emPs_USA <- subset(emPs, Code == 'USA')
oldest <- emPs_USA$Excess_mortality_P.scores._ages_85.
length(oldest)
older <- emPs_USA$Excess_mortality_P.scores._ages_75.84
old <- emPs_USA$Excess_mortality_P.scores._ages_65.74
middle <- emPs_USA$Excess_mortality_P.scores._ages_15.64
# line chart of excess mortality P-scores in the US during the 
# COVID-19 pandemic which is broken down by bracketed age groups
ggplot(data = emPs_USA, aes(x = Date)) +
  geom_line(aes(y = oldest, group = 1, color = 'red')) +
  geom_line(aes(y = older, group = 1, color = 'black')) +
  geom_line(aes(y = old, group = 1, color = '#330066')) +
  geom_line(aes(y = middle, group = 1, color = 'blue')) +
  scale_color_identity(name = 'Legend',
                       breaks = c('red', 'black', '#330066', 'blue'),
                       labels = c('USA ~ Ages 85+', 'USA ~ Ages 75-84', 
                                  'USA ~ Ages 65-74', 'USA ~ Ages 15-64'), 
                       guide = 'legend') +
  scale_x_date(labels = date_format("%m/%d"), 
               breaks = "8 weeks", minor_breaks = "8 weeks",
               limits = as.Date(c('2020-01-05', '2020-11-29'))) +
  scale_y_continuous(labels = function(y) paste0(y, "%"), 
                     breaks = seq(0, 50, 10), minor_breaks = seq(0, 50, 5)) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(x = '', y = 'Total deaths from all causes',
       title = 'Excess Mortality in the United States during the COVID-19 pandemic:
Deaths from all causes compared to recent years, by age', 
       subtitle = 'Shown is how the weekly number of deaths in 2020, broken down by age brackets, differs as a percentage from the 
average number of deaths in the same week over the most recent five years (2015-2019); this metric is known as the P-score. ',
       caption=expression(paste('Sources: Human Mortality Database (2020), OurWorldinData.org'))) + 
  theme_light() +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 8),
        plot.caption = element_text(size = 9, hjust = 0))







# calculate the case fatality rate for the US
owid.USA <- owid[owid$location == "United States", ]
td_USA <- owid.USA$total_deaths
tc_USA <- owid.USA$total_cases
CFR_USA <- td_USA/tc_USA
CFR_USA

owid.Mex <- owid[owid$location == 'Mexico', ]
td_Mex <- owid.Mex$total_deaths
tc_Mex <- owid.Mex$total_cases
CFR_Mex <- td_Mex/tc_Mex
tail(CFR_Mex)

owid.Can <- owid[owid$location == 'Canada', ]
td_Can <- owid.Can$total_deaths
tc_Can <- owid.Can$total_cases
CFR_Can <- td_Can/tc_Can
tail(CFR_Can)


# Create a line chart of the case fatality rates of 
# covid in the United States, Mexico, & Canada over time.
ggplot(data = owid, aes(x = date)) +
  geom_line(aes(y = CFR_USA, group = 1, color = 'red'),
            subset(owid, location %in% c('United States')), size = 1) +
  geom_line(aes(y = CFR_Mex, group = 1, color = '#006600'),
            subset(owid, location %in% c('Mexico')), size = 1) +
  geom_line(aes(y = CFR_Can, group = 1, color = 'black'),
            subset(owid, location %in% c('Canada')), size = 1) +
  scale_color_identity(name = 'Legend', breaks = c('red', '#006600', 'black'),
                       labels = c('USA', 'Mexico', 'Canada'), guide = 'legend') +
  scale_x_date(labels = date_format("%m/%d"), 
               breaks = "6 weeks", minor_breaks = "6 weeks",
               limits = as.Date(c('2020-03-23', '2020-12-01'))) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Sources: Johns Hopkins University's CCSE COVID-19 data, OurWorldinData.org", 
       y = 'CFR', title = 'Case Fatality Rate of COVID-19 over time',
       subtitle = 'The Case Fatality Rate (CFR) is just confirmed deaths divided by confirmed cases.') +
  theme_light() + 
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 9, hjust = 0),
        axis.title.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 9))



# create a dataframe of the owid data just for North America
owid.NA <- owid[owid$continent == "North America", ]
str(owid.NA)

# Create a chart comparing total deaths per million from covid
# in the United States over time to that of its two neighbors,
# Mexico and Canada.
ggplot(data = owid.NA, aes(x = date)) +
  geom_line(aes(y = total_deaths_per_million, group = 1, color = 'red'), 
            subset(owid, location %in% c('United States'))) +
  geom_line(aes(y = total_deaths_per_million, group = 1, 
                color = '#006600'), subset(owid, location %in% c('Mexico'))) +
  geom_line(aes(y = total_deaths_per_million, group = 1, 
                color = 'black'), subset(owid, location %in% c('Canada'))) +
  scale_color_identity(name = 'Legend', breaks = c('red', '#006600', 'black'),
                       labels = c('USA', 'Mexico', 'Canada'), guide = 'legend') +
  scale_x_date(labels = date_format("%m/%d"), 
               breaks = "6 weeks", minor_breaks = "6 weeks",
               limits = as.Date(c('2020-03-01', '2020-12-01'))) +
  labs(x = '', y = 'covid deaths per million',
       title = 'Cumulative confirmed deaths from COVID-19 per million people',
       caption=expression(paste("Sources: Johns Hopkins University's CCSE COVID-19 data, OurWorldinData.org"))) +
  theme_light() + 
  theme(plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 8),
    plot.caption = element_text(size = 9, hjust = 0))

ggplot(data = owid, aes(x = date)) +
  geom_line(aes(y = total_deaths_per_million, group = 1, color = 'red'), 
            subset(owid, location %in% c('United States')), size = 1) +
  geom_line(aes(y = total_deaths_per_million, group = 1, color = '#006600'), 
            subset(owid, location %in% c('Mexico')), size = 1) +
  geom_line(aes(y = total_deaths_per_million, group = 1, color = 'black'), 
            subset(owid, location %in% c('Canada')), size = 1) +
  scale_color_identity(name = 'Legend', breaks = c('red', '#006600', 'black'),
                       labels = c('USA', 'Mexico', 'Canada'), guide = 'legend') +
  scale_x_date(labels = date_format("%m/%d"), 
               breaks = "6 weeks", minor_breaks = "6 weeks",
               limits = as.Date(c('2020-03-01', '2020-12-01'))) +
  labs(x = '', y = 'covid deaths per million',
       title = 'Cumulative confirmed deaths from COVID-19 per million people',
       caption=expression(paste("Sources: Johns Hopkins University's CCSE COVID-19 data, OurWorldinData.org"))) +
  theme_light() + 
  theme(plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 9),
        plot.caption = element_text(size = 9, hjust = 0))


library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

# create a plot of weekly covid deaths in Florida over time
ggplot(data = weeklydeaths, aes(x = MMWR.Week)) +
  geom_col(aes(y = COVID.Underlying.Cause.of.Death.),
            subset(weeklydeaths, Jurisdiction.of.Occurrence %in% c('Florida'))) +
  labs(x = 'Week number', y = 'Deaths', title = 'Weekly deaths in Florida') +
  theme_light()
# create a plot of weekly covid deaths in Texas over time
ggplot(data = weeklydeaths, aes(x = MMWR.Week)) +
  geom_col(aes(y = COVID.Underlying.Cause.of.Death.),
           subset(weeklydeaths, Jurisdiction.of.Occurrence %in% c('Texas'))) +
  labs(x = 'Week number', y = 'Deaths', title = 'Weekly deaths in Texas') +
  theme_light()
# create a plot of weekly covid deaths in California over time
ggplot(data = weeklydeaths, aes(x = MMWR.Week)) +
  geom_col(aes(y = COVID.Underlying.Cause.of.Death.),
           subset(weeklydeaths, Jurisdiction.of.Occurrence %in% c('California'))) +
  labs(x = 'Week number', y = 'Deaths', title = 'Weekly deaths in California') +
  theme_light()
# create a plot of weekly covid deaths in Virginia over time
ggplot(data = weeklydeaths, aes(x = MMWR.Week)) +
  geom_col(aes(y = COVID.Underlying.Cause.of.Death.),
           subset(weeklydeaths, Jurisdiction.of.Occurrence %in% c('Virginia'))) +
  labs(x = 'Week number', y = 'Deaths', title = 'Weekly deaths in Virginia') +
  theme_light()


# create a plot comparing California, Florida, Virginia, Oregon, & Texas
# over time in terms of their cumulative covid deaths
ggplot(data = weeklydeaths, aes(x = Week.Ending.Date)) + 
  geom_line(aes(y = Cumulative_COVID_deaths, color = '#0000FF'),
            subset(weeklydeaths, Jurisdiction.of.Occurrence == 'California')) +
  geom_line(aes(y = Cumulative_COVID_deaths, color = '#003300'),
            subset(weeklydeaths, Jurisdiction.of.Occurrence == 'Florida')) +
  geom_line(aes(y = Cumulative_COVID_deaths, color = 'black'),
            subset(weeklydeaths, Jurisdiction.of.Occurrence == 'Virginia')) +
  geom_line(aes(y = Cumulative_COVID_deaths, color = '#660033'),
            subset(weeklydeaths, Jurisdiction.of.Occurrence == 'Oregon')) +
  geom_line(aes(y = Cumulative_COVID_deaths, color = 'red'),
            subset(weeklydeaths, Jurisdiction.of.Occurrence == 'Texas')) +
  scale_color_identity(name = 'Legend', breaks = c('#0000FF', '#003300', 'black', '#660033', 'red'),
                       labels = c('California', 'Florida', 'Virginia', 'Oregon', 'Texas'), 
                       guide = 'legend') +
  scale_y_continuous(label = comma) +
  scale_x_date(labels = date_format("%m/%d"), 
               breaks = "4 weeks", minor_breaks = "4 weeks",
               limits = as.Date(c('2020-03-07', '2020-11-07'))) +
  labs(x = '', y = 'Deaths', title = 'Cumulative deaths from COVID-19 in five states',
       caption=expression(paste('Source: Center for Disease Control (CDC)'))) +
  theme_light()+ 
  theme(plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 9),
        plot.caption = element_text(size = 9, hjust = 0))



# create a bar chart of the 10 states in the US with the 
# most total deaths from covid
df10 <- read.csv('Data for top 10 deaths states bar chart.csv')
ggplot(data = df10) +
  geom_bar(stat = 'identity', aes(x = reorder(State, -COVID19_Deaths), 
                                  y = COVID19_Deaths), fill = 'cyan') +
  scale_y_continuous(label = comma, breaks = seq(0, 35000, 5000),
                     minor_breaks = seq(0, 35000, 5000)) +
  coord_cartesian(ylim = c(0, 35000)) +
  labs(x = '', y = 'Deaths',
       title = '10 States with the most deaths from COVID-19',
       caption=expression(paste('Source: Center for Disease Control (CDC)'))) +
  theme_light() +
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        plot.caption = element_text(size = 8, hjust = 0))

df10 <- read.csv('Data for top 10 deaths states bar chart.csv')
ggplot(data = df10) +
  geom_bar(stat = 'identity', aes(x = reorder(State, -COVID_death_rate), 
                                  y = COVID_death_rate), fill = 'red') +
  geom_text(aes(x = State, y = COVID_death_rate, label = COVID_death_rate), 
            position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_y_continuous(breaks = seq(0, 180, 30),
                     minor_breaks = seq(0, 180, 15)) +
  coord_cartesian(ylim = c(0, 180)) +
  labs(x = '', y = 'Crude Death Rate',
       title = '10 States ranked by deaths per hundred thousand of population from COVID-19',
       caption=expression(paste('Source: Center for Disease Control (CDC)'))) +
  theme_light() +
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        plot.caption = element_text(size = 9, hjust = 0))

df5 <- read.csv('Data for top 5 deaths states bar chart.csv')
ggplot(data = df5) +
  geom_bar(stat = 'identity', aes(x = reorder(State, -COVID19_Deaths), 
                                  y = COVID19_Deaths), fill = 'magenta') +
  scale_y_continuous(label = comma, breaks = seq(0, 35000, 5000),
                     minor_breaks = seq(0, 35000, 5000)) +
  coord_cartesian(ylim = c(0, 35000)) +
  labs(x = '', y = 'Deaths',
       title = '5 States with the most deaths from COVID-19',
       caption=expression(paste('Source: Center for Disease Control (CDC)'))) +
  theme_light() +
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        plot.caption = element_text(size = 8, hjust = 0))


# another bar chart breaking down total covid deaths in the US by age
ggplot(data = subset(deaths, State == 'United States'), x = Age_group) +
  geom_bar(stat = 'identity', aes(x = Age_group, y = COVID19_Deaths), 
           fill = 'grey', subset(deaths, subset = Age_group %in% c('15-24 years', '25-34 years', '35-44 years', '45-54 years', '55-64 years', '65-74 years', '75-84 years', '85 years and over'))) +
  scale_y_continuous(label = comma) +
  labs(x = '', y = 'Total Confirmed Deaths',
       title = 'COVID-19 Deaths in the United States by Age',
       caption=expression(paste('Source: Center for Disease Control (CDC)'))) +
  theme_light() +
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 8),
        plot.caption = element_text(size = 8, hjust = 0))




# Run a random forest regression on the to try to find
# a decent set of predictors for total deaths from COVID-19 
# from the variables included in our world in data's main dataset.
colnames(owid)
attach(owid)
library(caTools)
set.seed(1)
split = sample.split(owid$total_deaths, SplitRatio = 0.7)
owid.train = subset(owid, split == TRUE)
owid.test = subset(owid, split == FALSE)
sapply(owid, class)
owid.train
class(owid.train)
class(owid.test)
dim(owid.train)
dim(owid.test)
dim(owid)

modelRF <- randomForest(total_deaths ~ stringency_index+population+
population_density+median_age+aged_70_older+gdp_per_capita+extreme_poverty+
cardiovasc_death_rate+diabetes_prevalence+handwashing_facilities+
hospital_beds_per_thousand+life_expectancy+human_development_index,
    data = owid.train, mtry = 7, importance = TRUE, na.action = na.omit)

modelRF
yhat.modelRF = predict(modelRF, newdata = owid.test)
mean((yhat.modelRF - owid.test)^2)


summary(modelRF)
plot(modelRF)
importance(modelRF)
varImpPlot(modelRF)


df <- read.csv('RFdata.csv')
set.seed(12)
split = sample.split(df, SplitRatio = 0.7)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)
dim(train)
dim(test)
dim(df)

modelRF2 <- randomForest(total_deaths ~ ., data = train, 
                         importance = TRUE, na.action = na.omit)
